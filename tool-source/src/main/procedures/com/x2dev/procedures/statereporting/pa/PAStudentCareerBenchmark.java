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

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

/**
 * The Class PAStudentCareerBenchmark.
 */
public class PAStudentCareerBenchmark extends StateReportData {
    /**
     * The Class SCBEntity.
     */
    public static class SCBEntity extends StateReportEntity {
        public static final String GRADE_LEVEL_CODE_05 = "005";
        public static final String GRADE_LEVEL_CODE_08 = "008";
        public static final String GRADE_LEVEL_CODE_11 = "011";

        private static final Collection<String> m_reportableGrades =
                Arrays.asList(GRADE_LEVEL_CODE_05, GRADE_LEVEL_CODE_08, GRADE_LEVEL_CODE_11);

        private PAStudentCareerBenchmark m_data = null;
        private String m_gradeLevel = null;
        private SisStudent m_student = null;

        /**
         * Instantiates a new SCB entity.
         */
        public SCBEntity() {
            // empty public constructor to instantiate dynamically
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

            m_data = (PAStudentCareerBenchmark) data;
            m_student = (SisStudent) bean;
            m_gradeLevel = getGradeLevelOnReportDate();

            if (!m_reportableGrades.contains(m_gradeLevel)) {
                setRowCount(0);
            }
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevelOnReportDate() {
            String gradeLevel = null;
            StudentEnrollmentSpan actualSpan = null;

            // Determine student enrollment span on report date. It may be actual span or most
            // recent one on the report date.
            Collection<StudentEnrollmentSpan> spans = m_data.m_helper.getStudentEnrollmentSpans(m_student, true);
            for (StudentEnrollmentSpan curSpan : spans) {
                PlainDate entryDate = curSpan.getFirstActiveDate();
                PlainDate exitDate = curSpan.getLastActiveDate();
                if (!m_data.m_reportDate.before(entryDate)
                        && (exitDate == null || !m_data.m_reportDate.after(exitDate))) {
                    actualSpan = curSpan;
                    break;
                } else if (actualSpan == null || (curSpan.getLastActiveDate().before(m_data.m_reportDate)
                        && curSpan.getLastActiveDate().after(actualSpan.getLastActiveDate()))) {
                    actualSpan = curSpan;
                }
            }

            // Calculate grade level based on grade level of student or, if actual span is found and
            // student's yog differs from yog from the span, based on yog from the span.
            gradeLevel = m_student.getGradeLevel();
            ReferenceCode gradeCode = null;
            if (actualSpan != null && m_student.getYog() != actualSpan.getFirstActiveEnrollment().getYog()) {
                gradeCode = m_data.m_gradeMatcher.getReferenceCode(actualSpan.getFirstActiveEnrollment().getYog());
            } else {
                gradeCode = m_data.m_gradeMatcher.getReferenceCode(gradeLevel);
            }
            if (gradeCode != null) {
                gradeLevel = gradeCode.getStateCode();
            }
            return gradeLevel;
        }
    }

    /**
     * The Class CategorySetCodeRetriever.
     */
    public class CategorySetCodeRetriever implements FieldRetriever {
        private static final String CALC_ID = "CAT_SET_CODE";

        private static final String CATEGORY_SET_CODE_5 = "CSB05";
        private static final String CATEGORY_SET_CODE_8 = "CSB08";
        private static final String CATEGORY_SET_CODE_11 = "CSB11";

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
            String gradeLevel = ((SCBEntity) entity).m_gradeLevel;
            String value = null;
            switch (gradeLevel) {
                case (SCBEntity.GRADE_LEVEL_CODE_05):
                    value = CATEGORY_SET_CODE_5;
                    break;
                case (SCBEntity.GRADE_LEVEL_CODE_08):
                    value = CATEGORY_SET_CODE_8;
                    break;
                case (SCBEntity.GRADE_LEVEL_CODE_11):
                    value = CATEGORY_SET_CODE_11;
                    break;
                default:
                    break;
            }
            return value;
        }
    }

    /**
     * The Class IndicatorRetriever.
     */
    public class IndicatorRetriever implements FieldRetriever {
        private static final String CALC_ID = "INDICATOR";

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
            String gradeLevel = ((SCBEntity) entity).m_gradeLevel;
            SisStudent student = (SisStudent) entity.getBean();
            boolean value = false;
            String fieldToRetrieve = null;
            switch (gradeLevel) {
                case (SCBEntity.GRADE_LEVEL_CODE_05):
                    fieldToRetrieve = m_fieldBenchInd05;
                    break;
                case (SCBEntity.GRADE_LEVEL_CODE_08):
                    fieldToRetrieve = m_fieldBenchInd08;
                    break;
                case (SCBEntity.GRADE_LEVEL_CODE_11):
                    fieldToRetrieve = m_fieldBenchInd11;
                    break;
                default:
                    break;
            }
            if (fieldToRetrieve != null) {
                value = CODE_YES.equals(student.getFieldValueByBeanPath(fieldToRetrieve));
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * The Class GradeMatcher.
     */
    private class GradeMatcher {
        private int m_maxGradeLevel;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private TreeMap m_sortedGradeLevels;

        /**
         * Instantiates a new grade matcher.
         */
        public GradeMatcher() {
            DataDictionary dictionary = PAStudentCareerBenchmark.this.getDataDictionary();
            ModelProperty prop = new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    PAStudentCareerBenchmark.this.getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                m_referenceGradeCodeMap = referenceTable.getCodeMap();
            } else {
                throw new IllegalStateException("Grade Code Reference table cannot be built");
            }

            List<String> removedCodes = new LinkedList();
            for (Entry<String, ReferenceCode> entry : m_referenceGradeCodeMap.entrySet()) {
                if (entry.getValue().getDisabledIndicator()) {
                    removedCodes.add(entry.getKey());
                }
            }
            for (String code : removedCodes) {
                m_referenceGradeCodeMap.remove(code);
            }

            ModelBroker broker = new ModelBroker(PAStudentCareerBenchmark.this.getPrivilegeSet());
            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(broker);
        }

        /**
         * Gets the reference code.
         *
         * @param yog the yog
         * @return the reference code
         */
        public ReferenceCode getReferenceCode(int yog) {
            ReferenceCode gradeCode = null;
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(m_maxGradeLevel, yog,
                    PAStudentCareerBenchmark.this.getCurrentContext().getSchoolYear(), m_sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
            return gradeCode;
        }

        /**
         * Gets the reference code.
         *
         * @param code the code
         * @return the reference code
         */
        public ReferenceCode getReferenceCode(String code) {
            ReferenceCode gradeCode = m_referenceGradeCodeMap.get(code);
            return gradeCode;
        }
    }

    private static final String ALIAS_INDICATOR_LVL_05 = "all-std-CSB05BenchmarkMet";
    private static final String ALIAS_INDICATOR_LVL_08 = "all-std-CSB08BenchmarkMet";
    private static final String ALIAS_INDICATOR_LVL_11 = "all-std-CSB11BenchmarkMet";

    private static final String CODE_YES = "Y";

    private static final String PARAM_REPORT_DATE = "reportDate";

    private String m_fieldBenchInd05 = null;
    private String m_fieldBenchInd08 = null;
    private String m_fieldBenchInd11 = null;

    private GradeMatcher m_gradeMatcher = null;
    private StudentHistoryHelper m_helper = null;
    private PlainDate m_reportDate = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_fieldBenchInd05 = translateAliasToJavaName(ALIAS_INDICATOR_LVL_05, true);
        m_fieldBenchInd08 = translateAliasToJavaName(ALIAS_INDICATOR_LVL_08, true);
        m_fieldBenchInd11 = translateAliasToJavaName(ALIAS_INDICATOR_LVL_11, true);

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        m_gradeMatcher = new GradeMatcher();

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_helper.getStudentCriteria().addEqualTo(Student.COL_ENROLLMENT_STATUS, activeStatus);

        Map<String, FieldRetriever> calcs = new HashMap<>();
        calcs.put(CategorySetCodeRetriever.CALC_ID, new CategorySetCodeRetriever());
        calcs.put(IndicatorRetriever.CALC_ID, new IndicatorRetriever());
        addCalcs(calcs);

        setQuery(m_helper.getStudentQuery(false));
        setEntityClass(SCBEntity.class);
    }
}
