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

import static com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity.FIELD_NAME_ALT_REPORT_CARD;
import static com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity.FLAG_T;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolGradeTermDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolTranscriptRubric;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnRubricAssessmentPerformance;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnRubricCriterion;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The Class OnsisReportCardSkill.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisReportCardSkill extends OnsisStateReportData {

    /**
     * The Class OnsisReportCardSkillEntity.
     */
    public static class OnsisReportCardSkillEntity extends OnsisStateReportEntity {
        private List<OnRubricAssessmentPerformance> m_rubricAssessmentPerformances;

        /**
         * Gets the learning skill code.
         *
         * @return the learning skill code
         */
        public String getLearningSkillCode() {
            OnRubricAssessmentPerformance assessmentPerformance = getRubricAssessmentPerformance();
            OnRubricCriterion rubricCriterion = (OnRubricCriterion) assessmentPerformance.getRubricCriterion(getBroker());

            return rubricCriterion.getLearningSkills();
        }

        /**
         * Gets the level.
         *
         * @return the level
         */
        public String getLevel() {
            Optional<ToolTranscriptRubric> rubric =
                    getRubricAssessmentPerformance().getRubricAssessment(getBroker()).getTranscriptRubrics(getBroker()).stream()
                            .filter(rap -> rap.getTranscript(getBroker()) != null
                                    && rap.getTranscript(getBroker()).getSchool(getBroker()) != null)
                            .findFirst();
            String schoolLanguageType =
                    rubric.isPresent()
                            ? ((OnSchool) rubric.get().getTranscript(getBroker()).getSchool(getBroker())).getLanguageType()
                            : null;
            schoolLanguageType = StringUtils.isBlank(schoolLanguageType)
                    ? "E"
                    : schoolLanguageType;
            OnRubricAssessmentPerformance assessmentPerformance = getRubricAssessmentPerformance();
            return schoolLanguageType + assessmentPerformance.getId();
        }

        /**
         * Gets the report data.
         *
         * @return the report data
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisReportCardSkill getReportData() {
            return (OnsisReportCardSkill) super.getReportData();
        }

        /**
         * Gets the rubric assessment performance.
         *
         * @return the rubric assessment performance
         */
        public OnRubricAssessmentPerformance getRubricAssessmentPerformance() {
            return m_rubricAssessmentPerformances.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data the data
         * @param bean the bean
         * @throws X2BaseException the x 2 base exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisReportCardSkill reportData = getReportData();
            OnsisReportCardTermEntity parentEntity = reportData.getParentEntity();

            /*
             * If the TERM's Alt Report Card Flag is T,
             * then no Subject Strands or Learning Skills should publish
             */
            boolean isAltReportCard = FLAG_T.equals(parentEntity.getFieldValue(FIELD_NAME_ALT_REPORT_CARD));
            if (isAltReportCard) {
                setRowCount(0);
                return;
            }

            OnsisStudent student = parentEntity.getStudent();
            ToolGradeTermDate gradeTerm = parentEntity.getGradeTermDate();

            m_rubricAssessmentPerformances =
                    student.getRubricAssessmentPerformance(getBroker(), gradeTerm.getGradeTermId()).stream()
                            .filter(assessmentPerformance -> {
                                OnRubricCriterion rubricCriterion =
                                        (OnRubricCriterion) assessmentPerformance.getRubricCriterion(getBroker());
                                if (rubricCriterion == null) {
                                    return false;
                                }

                                String stateValue = rubricCriterion.getLearningSkills();

                                return !StringUtils.isBlank(stateValue);
                            })
                            .map(rap -> (OnRubricAssessmentPerformance) rap)
                            .sorted(new Comparator<OnRubricAssessmentPerformance>() {

                                @Override
                                public int compare(OnRubricAssessmentPerformance o1, OnRubricAssessmentPerformance o2) {
                                    OnRubricCriterion rac1 = (OnRubricCriterion) o1.getRubricCriterion(getBroker());
                                    OnRubricCriterion rac2 = (OnRubricCriterion) o2.getRubricCriterion(getBroker());
                                    String str1 = StringUtils.unNullify(rac1 == null ? null : rac1.getLearningSkills());
                                    String str2 = StringUtils.unNullify(rac2 == null ? null : rac2.getLearningSkills());
                                    int result = str1.compareTo(str2);
                                    if (result == 0) {
                                        result = o1.getOid().compareTo(o2.getOid());
                                    }
                                    return result;
                                }
                            })
                            .collect(Collectors.toList());

            setRowCount(m_rubricAssessmentPerformances.size());
        }
    }

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
     * Gets the parent entity.
     *
     * @return the parent entity
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisReportCardTermEntity getParentEntity() {
        return (OnsisReportCardTermEntity) super.getParentEntity();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisReportCardSkillEntity.class);
    }

}
