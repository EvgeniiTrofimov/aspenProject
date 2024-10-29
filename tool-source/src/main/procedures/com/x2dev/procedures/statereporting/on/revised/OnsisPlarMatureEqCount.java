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

import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnGraduationStudentProgram;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolData.OnsisSchoolEntity;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.GraduationStudentProgram;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.w3c.dom.Element;

/**
 * The Class OnsisPlarMatureEqCount.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisPlarMatureEqCount extends OnsisStateReportData {

    public static final String ELEMENT_STUDENT_HEAD_COUNT = "STUDENT_HEAD_COUNT";

    /**
     * The Class OnsisPlarMatureEqCountEntity.
     */
    public static class OnsisPlarMatureEqCountEntity extends OnsisStateReportEntity {
        private static final String COUNTER_TYPE_2A = "2a";
        private static final String COUNTER_TYPE_2B = "2b";
        private static final String COUNTER_TYPE_3 = "3";
        private static final String COUNTER_TYPE_4 = "4";
        private static final String COUNTER_TYPE_5A = "5a";
        private static final String COUNTER_TYPE_5B = "5b";
        private static final String COUNTER_TYPE_5C = "5c";

        /**
         * The Class CountersGroup.
         */
        private class CountersGroup {
            final ToolBeanColumn m_fieldToJoinBy;
            final Filterable<? extends ToolBean> m_filterable;
            final Map<String, List<String>> m_valuesToJoinBy;

            /**
             * Instantiates a new counters group.
             *
             * @param filterable Filterable<? extends ToolBean>
             * @param fieldPlarType ToolBeanColumn
             * @param valuesToJoinBy Map<String,List<String>>
             */
            CountersGroup(Filterable<? extends ToolBean> filterable, ToolBeanColumn fieldPlarType,
                    Map<String, List<String>> valuesToJoinBy) {
                m_filterable = filterable;
                m_fieldToJoinBy = fieldPlarType;
                m_valuesToJoinBy = valuesToJoinBy;
            }
        }

        private List<CountersGroup> m_countersGroup = new ArrayList<>();
        private List<String> m_counterTypes = Arrays.asList(
                COUNTER_TYPE_2A,
                COUNTER_TYPE_2B,
                COUNTER_TYPE_3,
                COUNTER_TYPE_4,
                COUNTER_TYPE_5A,
                COUNTER_TYPE_5B,
                COUNTER_TYPE_5C);

        /**
         * Gets the counter type.
         *
         * @return String
         */
        public String getCounterType() {
            return m_counterTypes.get(getCurrentRow());
        }

        /**
         * Gets the counter value.
         *
         * @return int
         */
        public int getCounterValue() {
            for (final CountersGroup group : m_countersGroup) {
                if (group.m_valuesToJoinBy.containsKey(getCounterType())) {
                    return group.m_filterable
                            .filter(new Filter() {
                                @Override
                                public boolean isFiltered(Object toFilter) {
                                    ToolBean bean = (ToolBean) toFilter;
                                    return group.m_valuesToJoinBy.get(getCounterType())
                                            .contains(bean.getValueReferenceState(group.m_fieldToJoinBy));
                                }
                            })
                            .distinct(Transcript.COL_STUDENT_OID)
                            .count();
                }
            }
            return 0;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean ToolBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            OnsisSchoolEntity parentEntity = (OnsisSchoolEntity) getReportData().getParentEntity();

            List<CountersGroup> counterGroups = new ArrayList<>();

            {
                Map<String, List<String>> trnValuesToJoinBy = new HashMap<>();
                trnValuesToJoinBy.put(COUNTER_TYPE_2A, Arrays.asList(OnTranscript.PLAR_TYPE_GRADE_IND_9_10));
                trnValuesToJoinBy.put(COUNTER_TYPE_2B, Arrays.asList(OnTranscript.PLAR_TYPE_GRADE_CHA_10));
                trnValuesToJoinBy.put(COUNTER_TYPE_3, Arrays.asList(OnTranscript.PLAR_TYPE_GRADE_EQU_11_12));
                trnValuesToJoinBy.put(COUNTER_TYPE_4, Arrays.asList(OnTranscript.PLAR_TYPE_GRADE_CHA_11_12));

                CountersGroup transcripts =
                        new CountersGroup(parentEntity.getMaturePlarTranscripts(),
                                OnTranscript.FIELD_PLAR_TYPE, trnValuesToJoinBy);

                counterGroups.add(transcripts);
            }

            {
                String diplomaField = GraduationStudentProgram.REL_PROGRAM_STUDIES
                        + ModelProperty.PATH_DELIMITER + GraduationProgram.COL_DIPLOMA_TYPE;

                Map<String, List<String>> gsrValuesToJoinBy = new HashMap<>();
                gsrValuesToJoinBy.put(COUNTER_TYPE_5A,
                        Arrays.asList(OnGraduationStudentProgram.DIPLOMA_TYPE_COLLEGE_PREP,
                                OnGraduationStudentProgram.DIPLOMA_TYPE_HONORS_COLLEGE_PREP,
                                OnGraduationStudentProgram.DIPLOMA_TYPE_OSSD_SHSM));
                gsrValuesToJoinBy.put(COUNTER_TYPE_5B, Arrays.asList("*****dummy*****"));
                gsrValuesToJoinBy.put(COUNTER_TYPE_5C,
                        Arrays.asList(OnGraduationStudentProgram.DIPLOMA_TYPE_SSGD));

                CountersGroup graduationStudentPrograms =
                        new CountersGroup(
                                parentEntity.getMatureStudentsPrograms(),
                                OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE,
                                gsrValuesToJoinBy);

                counterGroups.add(graduationStudentPrograms);
            }

            if (getGlobalData().getDebugDetail()) {
                StringBuilder debugOutput = new StringBuilder();
                debugOutput.append("OnsisPlarMatureEqCountEntity.intitialize - getMaturePlarTranscripts:\n");
                parentEntity.getMaturePlarTranscripts().extract().forEach(trn -> {
                    OnsisStudent student = (OnsisStudent) trn.getStudent(getBroker());
                    debugOutput.append(student.getNameView() + "[" + student.getOen() + "]" + trn.toString() + "/n");
                });

                debugOutput.append("OnsisPlarMatureEqCountEntity.intitialize - getMatureStudentsPrograms:\n");
                parentEntity.getMatureStudentsPrograms().extract().forEach(trn -> {
                    OnsisStudent student = (OnsisStudent) trn.getStudent(getBroker());
                    debugOutput.append(student.getNameView() + "[" + student.getOen() + "]" + trn.toString() + "\n");
                });
                getReportData().log(debugOutput.toString());
            }

            m_countersGroup.addAll(counterGroups);

            setRowCount(m_counterTypes.size());
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement Element
         * @param parentElement Element
         * @return true, if is row canceled
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.w3c.dom.Element,
         *      org.w3c.dom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            boolean isCanceled = "0".equals(getChildText(ELEMENT_STUDENT_HEAD_COUNT, entityElement));
            return isCanceled;
        }
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisPlarMatureEqCountEntity.class);
    }
}
