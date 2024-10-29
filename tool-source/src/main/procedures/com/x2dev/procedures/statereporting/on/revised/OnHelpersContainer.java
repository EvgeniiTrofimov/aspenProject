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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.FieldAliases;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

/**
 * The Class OnHelpersContainer.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnHelpersContainer {
    /**
     * The Interface OnsisBrokerProvider.
     */
    public interface OnBrokerProvider {

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        public X2Broker getBroker();
    }

    /**
     * The Interface OnsisCTXProvider.
     */
    public interface OnCTXProvider {

        /**
         * Gets the context.
         *
         * @return District school year context
         */
        public DistrictSchoolYearContext getContext();
    }

    /**
     * The Interface OnsisDateRangeProvider.
     */
    public interface OnDateRangeProvider {

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate();

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate();
    }

    /**
     * The Interface OnsisDictionaryExtractorProvider.
     */
    public interface OnDictionaryExtractorProvider {

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         */
        public DictionaryExtractor getDictionaryExtractor();
    }

    /**
     * The Interface OnsisSchoolDateRangeProvider.
     */
    public interface OnSchoolDateRangeProvider
            extends OnBrokerProvider, OnSchoolProvider, OnDateRangeProvider, OnCTXProvider,
            OnDictionaryExtractorProvider {
        // combine other providers
    }

    /**
     * The Interface OnsisSchoolProvider.
     */
    public interface OnSchoolProvider {

        /**
         * Gets the school.
         *
         * @return School
         */
        public OnSchool getSchool();
    }

    /**
     * The Class FilterablesHelper.
     */
    public static class FilterablesHelper {
        private X2Broker m_broker;
        private DictionaryExtractor m_dictionaryExtractor;
        private Map<String, Filterable> m_filterables = new HashMap<>();

        /**
         * Instantiates a new filterables helper.
         *
         * @param broker X2Broker
         * @param dictionaryExtractor DictionaryExtractor
         */
        public FilterablesHelper(X2Broker broker, DictionaryExtractor dictionaryExtractor) {
            m_broker = broker;
            m_dictionaryExtractor = dictionaryExtractor;
        }

        /**
         * Clear.
         */
        public void clear() {
            m_filterables.clear();
        }

        /**
         * Dump filterables.
         */
        public void dumpFilterables() {
            ToolBean.debug("m_filterables counts by class:");
            Map<String, Integer> occurrenceCounts = new HashMap<>();
            Map<String, Integer> objectCounts = new HashMap<>();

            Collection<String> shortKeys = new ArrayList<>();
            for (String longKey : m_filterables.keySet()) {
                String shortKey = longKey;
                if (longKey.contains("_")) {
                    shortKey = longKey.substring(0, longKey.indexOf("_"));
                }
                shortKeys.add(shortKey);

                /*
                 * Sum occurrences of shortKey
                 */
                Integer numOccurrence = occurrenceCounts.get(shortKey);
                if (numOccurrence == null) {
                    occurrenceCounts.put(shortKey, Integer.valueOf(1));
                } else {
                    occurrenceCounts.put(shortKey, numOccurrence++);
                }

                /*
                 * Sum object count for shortKey
                 */
                int numObjects = m_filterables.get(longKey).extract().size();
                Integer sumNumObjects = objectCounts.get(shortKey);
                if (sumNumObjects == null) {
                    objectCounts.put(shortKey, Integer.valueOf(numObjects));
                } else {
                    objectCounts.put(shortKey, sumNumObjects + numObjects);
                }

            }

            for (String key : occurrenceCounts.keySet()) {
                Integer occurrenceCount = occurrenceCounts.get(key);
                Integer objectCount = objectCounts.get(key);
                ToolBean.debug(key + ": occurrences[" + occurrenceCount
                        + "] objects[" + objectCount + "]");
            }
        }

        /**
         * Gets the Filterable.
         *
         * @param key String
         * @return Filterable
         */
        public Filterable getFilterable(String key) {
            return m_filterables.get(key);
        }

        /**
         * Gets the Filterable.
         *
         * @param <R> the generic type
         * @param clazz Class<T>
         * @return Filterable
         */
        public <R extends ToolBean> Filterable<R> getFilterable(Class<R> clazz) {
            return getFilterable(clazz, new X2Criteria());
        }

        /**
         * Gets the filterable.
         *
         * @param clazz Class
         * @param criteria X2Criteria
         * @return Filterable
         */
        public <R extends ToolBean> Filterable<R> getFilterable(Class<R> clazz, X2Criteria criteria) {
            String filterableKey = clazz.getName() + criteria.toString();
            Filterable filterable = m_filterables.get(filterableKey);
            if (filterable == null) {
                filterable = FilterableFactory.create(getBroker(), getDictionaryExtractor(), clazz, criteria, null);
                m_filterables.put(filterableKey, filterable);
            }
            return filterable;
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        private X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         */
        private DictionaryExtractor getDictionaryExtractor() {
            return m_dictionaryExtractor;
        }

    }

    /**
     * The Class Grades Student Helper.
     */
    public static class GradesHelper {
        private X2Broker m_broker;
        private DistrictSchoolYearContext m_context;
        private DictionaryExtractor m_dictionaryExtractor;
        private GradeMatcher m_gradeMatcher;

        /**
         * Instantiates a new grades helper.
         *
         * @param helpProvider OnsisSchoolDateRangeProvider
         */
        public GradesHelper(OnSchoolDateRangeProvider helpProvider) {
            super();
            m_broker = helpProvider.getBroker();
            m_context = helpProvider.getContext();
            m_dictionaryExtractor = helpProvider.getDictionaryExtractor();
        }

        /**
         * Instantiates a new grades helper.
         *
         * @param context the context
         * @param dictExtractor the dict extractor
         * @param broker the broker
         */
        public GradesHelper(DistrictSchoolYearContext context, DictionaryExtractor dictExtractor, X2Broker broker) {
            super();
            m_broker = broker;
            m_context = context;
            m_dictionaryExtractor = dictExtractor;
        }

        /**
         * Gets the grade level.
         *
         * @param yog int
         * @return String
         */
        public String getGradeLevel(int yog) {
            ReferenceCode gradeCode = getGradeMatcher().getReferenceCode(yog);
            return gradeCode.getStateCode();
        }

        /**
         * Gets the grade matcher.
         *
         * @return Grade matcher
         */
        public GradeMatcher getGradeMatcher() {
            if (m_gradeMatcher == null) {
                m_gradeMatcher = new GradeMatcher(m_context, m_dictionaryExtractor, m_broker);
            }
            return m_gradeMatcher;
        }
    }

    /**
     * The Class GradeMatcher.
     */
    public static class GradeMatcher {
        private X2Broker m_broker;
        private DistrictSchoolYearContext m_context;
        private DictionaryExtractor m_dictionaryExtractor;
        private int m_maxGradeLevel;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private TreeMap<Integer, List<String>> m_sortedGradeLevels;

        /**
         * Instantiates a new grade matcher.
         *
         * @param helpProvider OnsisSchoolDateRangeProvider
         */
        public GradeMatcher(OnSchoolDateRangeProvider helpProvider) {
            super();
            m_broker = helpProvider.getBroker();
            m_context = helpProvider.getContext();
            m_dictionaryExtractor = helpProvider.getDictionaryExtractor();
            init();
        }

        /**
         * Instantiates a new grade matcher.
         *
         * @param context the context
         * @param dictExtractor the dict extractor
         * @param broker the broker
         */
        public GradeMatcher(DistrictSchoolYearContext context, DictionaryExtractor dictExtractor, X2Broker broker) {
            super();
            m_broker = broker;
            m_context = context;
            m_dictionaryExtractor = dictExtractor;
            init();
        }

        /**
         * Gets the numeric grade.
         *
         * @param refCode ReferenceCode
         * @return Integer
         */
        public Integer getNumericGrade(ReferenceCode refCode) {
            String numericGradeLevel =
                    (String) refCode.getFieldValueByAlias(FieldAliases.GRADE_LEVEL_NUMERIC_VALUE);

            if (StringUtils.isEmpty(numericGradeLevel)) {
                return null;
            }

            return new Integer(numericGradeLevel);
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
                    m_context.getSchoolYear(), m_sortedGradeLevels);
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

        /**
         * Return the first ReferenceCode matching numericGradeLevel.
         *
         * @param numericGradeLevel int
         * @return Reference code
         */
        public ReferenceCode getReferenceCodeForNumericGradeLevel(int numericGradeLevel) {
            List<String> matchingGradeLevels = m_sortedGradeLevels.get(Integer.valueOf(numericGradeLevel));
            String matchingGradeCode = null;
            if (matchingGradeLevels != null && matchingGradeLevels.size() > 0) {
                matchingGradeCode = matchingGradeLevels.get(0);
            }
            if (matchingGradeCode == null) {
                return null;
            }

            return m_referenceGradeCodeMap.get(matchingGradeCode);
        }

        /**
         * Gets the reference code reverse lookup.
         *
         * @param stateCode String
         * @return Reference code
         */
        public ReferenceCode getReferenceCodeReverseLookup(String stateCode) {
            for (ReferenceCode refCode : m_referenceGradeCodeMap.values()) {
                if (stateCode.equals(refCode.getStateCode())) {
                    return refCode;
                }
            }
            return null;
        }

        /**
         * Inits the.
         */
        private void init() {
            DataDictionaryField field = ToolStudent.FIELD_GRADE_LEVEL.getField(m_dictionaryExtractor);
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

            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(m_broker);
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(m_broker);

        }
    }


}
