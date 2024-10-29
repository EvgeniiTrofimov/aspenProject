/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.common;

import static com.x2dev.sis.model.beans.StudentEnrollment.ENTRY;
import static com.x2dev.sis.model.beans.StudentEnrollment.WITHDRAWAL;
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.ParameterSelectionHandler;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster;
import com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster.JoinType;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.beans.path.BeanColumnPath;
import com.follett.fsc.core.k12.beans.path.BeanPath.RelationshipType;
import com.follett.fsc.core.k12.beans.path.BeanTablePath;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.utils.database.DBType;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition.JoinAdjusterPattern;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanQuery.ToolBeanQueryIterator;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanRelationship.Loader;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AspenSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Pair;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.ParentSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.sis.model.business.ContextAttributesManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.DecimalAsStringConverter;
import com.x2dev.utils.converters.TimeAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.collections.comparators.NullComparator;

/**
 * The Class ToolBean.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class ToolBean implements Map {

    /**
     * The Interface FieldConverter.
     */
    public interface FieldConverter {

        /**
         * Converted value.
         *
         * @param input Object
         * @return Object
         */
        Object convertedValue(Object input);
    }

    /**
     * The Enum PredefinedConverter.
     */
    public enum PredefinedConverter implements FieldConverter {
        BIG_DECIMAL {
            DecimalAsStringConverter m_converter = (DecimalAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER, Locale.getDefault(), true);


            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof BigDecimal) {
                        result = input;
                    } else if (input instanceof Number) {
                        result = BigDecimal.valueOf(((Number) input).doubleValue());
                    } else if (input instanceof String) {
                        result = m_converter.parseSystemString((String) input);
                    } else {
                        throw new IllegalStateException(
                                "Cannot convert " + input.getClass().getName() + "to BigDecimal");
                    }
                }
                return result;
            }
        },
        DOUBLE {

            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof Number) {
                        result = Double.valueOf(((Number) input).doubleValue());
                    } else if (input instanceof String) {
                        try {
                            result = Double.valueOf(Double.parseDouble(((String) input)));
                        } catch (NumberFormatException e) {
                            // do nothing
                        }
                    } else {
                        throw new IllegalStateException("Cannot convert " + input.getClass().getName() + "to Double");
                    }
                }
                return result;
            }
        },
        INTEGER {

            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof Number) {
                        result = Integer.valueOf(((Number) input).intValue());
                    } else if (input instanceof String) {
                        try {
                            result = Integer.valueOf(Integer.parseInt(((String) input)));
                        } catch (NumberFormatException e) {
                            // do nothing
                        }
                    } else {
                        throw new IllegalStateException("Cannot convert " + input.getClass().getName() + "to Integer");
                    }
                }
                return result;
            }
        },
        LOGICAL {

            BooleanAsStringConverter m_converter = (BooleanAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.BOOLEAN_CONVERTER, Locale.getDefault(), true);

            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof Boolean) {
                        result = input;
                    } else if (input instanceof String) {
                        result = m_converter.parseSystemString((String) input);
                    } else {
                        throw new IllegalStateException("Cannot convert " + input.getClass().getName() + "to Logical");
                    }
                }
                return result;
            }

        },
        LONG {

            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof Number) {
                        result = Long.valueOf(((Number) input).longValue());
                    } else if (input instanceof String) {
                        try {
                            result = Long.valueOf(Long.parseLong(((String) input)));
                        } catch (NumberFormatException e) {
                            // do nothing
                        }
                    } else {
                        throw new IllegalStateException("Cannot convert " + input.getClass().getName() + "to Long");
                    }
                }
                return result;
            }
        },
        PLAINDATE {

            DateAsStringConverter m_converter =
                    (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                            Locale.getDefault(),
                            true);

            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof PlainDate) {
                        result = input;
                    } else if (input instanceof java.util.Date) {
                        result = new PlainDate((java.util.Date) input);
                    } else if (input instanceof String) {
                        result = m_converter.parseSystemString((String) input);
                    } else {
                        throw new IllegalStateException(
                                "Cannot convert " + input.getClass().getName() + "to PlainDate");
                    }
                }
                return result;
            }

        },
        PLAINTIME {

            TimeAsStringConverter m_converter =
                    (TimeAsStringConverter) ConverterFactory.getConverterForClass(PlainTime.class.getName(),
                            Locale.getDefault(),
                            true);

            @Override
            public Object convertedValue(Object input) {
                Object result = null;
                if (input != null) {
                    if (input instanceof PlainTime) {
                        result = input;
                    } else if (input instanceof java.util.Date) {
                        result = new PlainTime((java.util.Date) input);
                    } else if (input instanceof String) {
                        result = m_converter.parseSystemString((String) input);
                    } else {
                        throw new IllegalStateException(
                                "Cannot convert " + input.getClass().getName() + "to PlainTime");
                    }
                }
                return result;
            }

        }
    }


    /**
     * The Class PreferenceManager.
     */
    public static class DistrictManager {
        public static final String DEFAULT_CALENDAR_ID = "Standard";

        private static Collection<String> m_activeStudentCodes;
        private static AnnualSpanFactory s_annualSpanFactory = new AnnualSpanFactory();
        private static Filterable<? extends ToolDistrictContext> m_contextBySchoolYear = null;
        private static List<String> m_defaultCalendarIds = null;
        private static Organization s_organization;
        private static Boolean s_preferenceMemberOnEntry;
        private static Boolean s_preferenceMemberOnWithdrawal;
        private static ScheduleManager s_scheduleMgr;
        private static StudentScheduleSpanFactory s_scheduleSpanFactory = new StudentScheduleSpanFactory();
        private static Filterable<ToolSchool> s_schoolsFilterable = null;
        private static ToolOrganization s_toolOrganization;

        /**
         * Gets the annual span factory.
         *
         * @return Annual span factory
         */
        public static AnnualSpanFactory getAnnualSpanFactory() {
            return s_annualSpanFactory;
        }

        /**
         * Gets the default calendar ids.
         *
         * @return List
         */
        public static List<String> getDefaultCalendarIds() {
            return m_defaultCalendarIds == null ? Collections.EMPTY_LIST : m_defaultCalendarIds;
        }

        /**
         * Gets the district school year context.
         *
         * @param broker X2Broker
         * @param whenDate PlainDate
         * @return ctx
         */
        public static ToolDistrictContext getDistrictSchoolYearContext(X2Broker broker, PlainDate whenDate) {
            if (whenDate == null) {
                return null;
            }

            /*
             * Walking backward in time, return the first context with StartDate on/before WhenDate
             */
            for (ToolDistrictContext context : getDistrictSchoolYearContexts(broker).extract()) {
                // Skip future year
                if (context.getStartDate().after(whenDate)) {
                    continue;
                }

                // Reasonableness check
                PlainDate startDate = context.getStartDate();
                PlainDate startDatePlusYear = DateUtils.add(startDate, Calendar.YEAR, 1);
                PlainDate startDatePlusYearLeeway = DateUtils.add(startDatePlusYear, Calendar.MONTH, 2);
                if (whenDate.after(startDatePlusYearLeeway)) {
                    throw new RuntimeException("Unable to determine District Context Year for " + whenDate);
                }

                return context;
            }
            return null;
        }

        /**
         * Gets the district school year contexts.
         *
         * @param broker X2Broker
         * @return Filterable
         */
        public static Filterable<? extends ToolDistrictContext> getDistrictSchoolYearContexts(X2Broker broker) {

            if (m_contextBySchoolYear == null) {
                Class<? extends ToolDistrictContext> classCTX =
                        ToolBean.getRegisteredClass(DistrictSchoolYearContext.class.getName(),
                                ToolDistrictContext.class);
                m_contextBySchoolYear =
                        FilterableFactory.create(broker, ToolBean.getDictionaryExtractor(), classCTX, new X2Criteria(),
                                Arrays.asList(ToolDistrictContext.FIELD_START_DATE_DESC));
                // Perform one time date check
                PlainDate historicalCheckDate = DateUtils.add(new PlainDate(), Calendar.YEAR, -13);

                // Populate internal map
                PlainDate nextYearStartDate = null;
                for (ToolDistrictContext districtSchoolYearContext : m_contextBySchoolYear.extract()) {
                    PlainDate startDate = districtSchoolYearContext.getStartDate();

                    /*
                     * Reasonableness check that context start dates are within 14 months of each
                     * other
                     * (only apply to last 12 years)
                     */
                    if (startDate.after(historicalCheckDate)) {
                        PlainDate startDatePlusYear = DateUtils.add(startDate, Calendar.YEAR, 1);
                        PlainDate startDatePlusYearLeeway = DateUtils.add(startDatePlusYear, Calendar.MONTH, 2);
                        if (nextYearStartDate != null && nextYearStartDate.after(startDatePlusYearLeeway)) {
                            throw new RuntimeException("Context year start dates are too far apart: "
                                    + startDate + ", " + nextYearStartDate);
                        }
                    }
                    nextYearStartDate = startDate;
                }

            }
            return m_contextBySchoolYear;
        }

        /**
         * Gets the organization.
         *
         * @param broker X2Broker
         * @return Organization
         */
        public static Organization getOrganization(X2Broker broker) {
            if (s_organization == null) {
                s_organization = OrganizationManager.getRootOrganization(broker);
            }
            return s_organization;
        }

        /**
         * Gets the organization tool bean.
         *
         * @param broker X2Broker
         * @return Tool organization
         */
        public static ToolOrganization getOrganizationToolBean(X2Broker broker) {
            if (s_toolOrganization == null) {
                s_toolOrganization = ToolBean.getBeanByOid(broker, getDictionaryExtractor(), ToolOrganization.class,
                        getOrganization(broker).getOid(), true);
            }
            return s_toolOrganization;
        }

        /**
         * Gets the schedule manager.
         *
         * @param broker X2Broker
         * @return Schedule manager
         */
        public static ScheduleManager getScheduleManager(X2Broker broker) {
            if (s_scheduleMgr == null) {
                s_scheduleMgr = new ScheduleManager(broker);
            }
            return s_scheduleMgr;
        }

        /**
         * Gets the schools filterable.
         *
         * @return the schools filterable
         */
        public static Filterable<? extends ToolSchool> getSchoolsFilterable() {
            if (s_schoolsFilterable == null) {
                Class clazzToolBean = getRegisteredClass(ToolSchool.getX2BaseClass());

                s_schoolsFilterable =
                        FilterableFactory.create(ToolBean.getBroker(true), ToolBean.getDictionaryExtractor(),
                                clazzToolBean, new X2Criteria(), null);

            }
            return s_schoolsFilterable;
        }

        /**
         * Gets the student schedule span factory.
         *
         * @return Student schedule span factory
         */
        public static StudentScheduleSpanFactory getStudentScheduleSpanFactory() {
            return s_scheduleSpanFactory;
        }

        /**
         * Checks if is active status.
         *
         * @param broker X2Broker
         * @param statusCode String
         * @return true, if is active status
         */
        public static boolean isActiveStatus(X2Broker broker, String statusCode) {
            if (m_activeStudentCodes == null) {
                m_activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization(broker));
            }
            return m_activeStudentCodes.contains(statusCode);
        }

        /**
         * Gets the member on entry.
         *
         * @param broker X2Broker
         * @return boolean
         */
        public static boolean isMemberOnEntry(X2Broker broker) {
            if (s_preferenceMemberOnEntry == null) {
                s_preferenceMemberOnEntry =
                        Boolean.valueOf(com.follett.fsc.core.k12.business.PreferenceManager.getPreferenceValue(
                                getOrganization(broker),
                                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE));
            }

            return s_preferenceMemberOnEntry.booleanValue();
        }

        /**
         * Gets the member on withdrawal.
         *
         * @param broker X2Broker
         * @return boolean
         */
        public static boolean isMemberOnWithdrawal(X2Broker broker) {
            if (s_preferenceMemberOnWithdrawal == null) {
                s_preferenceMemberOnWithdrawal =
                        Boolean.valueOf(com.follett.fsc.core.k12.business.PreferenceManager.getPreferenceValue(
                                getOrganization(broker),
                                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE));
            }

            return s_preferenceMemberOnWithdrawal.booleanValue();
        }

        /**
         * Sets the annual span factory.
         *
         * @param factory void
         */
        public static void setAnnualSpanFactory(AnnualSpanFactory factory) {
            s_annualSpanFactory = factory;
        }

        /**
         * Sets the default calendar ids.
         *
         * @param calendarIds void
         */
        public static void setDefaultCalendarIds(List<String> calendarIds) {
            m_defaultCalendarIds = calendarIds;
        }

        /**
         * Gets the organization.
         *
         * @param organization void
         * @return Organization
         */
        public static void setOrganization(Organization organization) {
            s_organization = organization;
        }

        /**
         * Sets the student schedule span factory.
         *
         * @param factory void
         */
        public static void setStudentScheduleSpanFactory(StudentScheduleSpanFactory factory) {
            s_scheduleSpanFactory = factory;
        }

    }

    /**
     * The Class Address. Empty class to support extensions;
     */
    public static class ToolAddress extends ToolBean {
        public static final ToolBeanColumn FIELD_CITY =
                new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.city());
        public static final ToolBeanColumn FIELD_LINE_1 =
                new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.addressLine01());
        public static final ToolBeanColumn FIELD_LINE_2 =
                new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.addressLine02());
        public static final ToolBeanColumn FIELD_LINE_3 =
                new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.addressLine03());
        public static final ToolBeanColumn FIELD_POSTAL_CODE =
                new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.postalCode());
        public static final ToolBeanColumn FIELD_STATE =
                new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.state());


        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION.expand(
                FIELD_CITY,
                FIELD_LINE_1,
                FIELD_LINE_2,
                FIELD_LINE_3,
                FIELD_POSTAL_CODE,
                FIELD_STATE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.PERSON_ADDRESS.getBeanType();
        }

        /**
         * Instantiates a new onsis course request.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolAddress(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the city.
         *
         * @return String
         */
        public String getCity() {
            return getValueString(FIELD_CITY);
        }

        /**
         * Gets the line 1.
         *
         * @return the line 1
         */
        public String getLine1() {
            return getValueString(FIELD_LINE_1);
        }

        /**
         * Gets the line 2.
         *
         * @return the line 2
         */
        public String getLine2() {
            return getValueString(FIELD_LINE_2);
        }

        /**
         * Gets the line 3.
         *
         * @return the line 3
         */
        public String getLine3() {
            return getValueString(FIELD_LINE_3);
        }

        /**
         * Gets the postal code.
         *
         * @return the postal code
         */
        public String getPostalCode() {
            return getValueString(FIELD_POSTAL_CODE);
        }

        /**
         * Gets the state.
         *
         * @return String
         */
        public String getState() {
            return getValueString(FIELD_STATE);
        }
    }

    /**
     * The Class ToolBeanCache.
     */
    public static class ToolBeanCache {

        /**
         * The Class BeanCacheItem.
         */
        private class BeanCacheItem {
            private String className;
            private Map<String, Integer> m_groupCounts = new HashMap();
            private Map<String, Map<String, ToolBean>> m_keysToBean = new HashMap();
            private Map<String, Map<String, Set<ToolBean>>> m_keysToBeans = new HashMap();

            /**
             * Instantiates a new bean cache item.
             *
             * @param className String
             */
            public BeanCacheItem(String className) {
                this.className = className;
            }

            /**
             * Adds the bean.
             *
             * @param definition ToolBeanDefinition
             * @param bean ToolBean
             * @param groupKey String
             */
            public void addBean(ToolBeanDefinition definition, ToolBean bean, String groupKey) {
                definition.getKeys().stream().forEach(column -> {
                    String key = column.resolve(null);
                    if (column.isUnique()) {
                        Map<String, ToolBean> keyToBean = m_keysToBean.get(key);
                        if (keyToBean == null) {
                            keyToBean = new LinkedHashMap();
                            m_keysToBean.put(key, keyToBean);
                        }
                        ToolBean oldBean = keyToBean.put(bean.getValueString(column), bean);
                        if (oldBean != null) {
                            throw new RuntimeException(
                                    "Old Bean " + oldBean.toString() + " replaced with " + bean.toString());
                        }
                    } else {
                        Map<String, Set<ToolBean>> keyToBeans = m_keysToBeans.get(key);
                        if (keyToBeans == null) {
                            keyToBeans = new LinkedHashMap();
                            m_keysToBeans.put(key, keyToBeans);
                        }
                        Set<ToolBean> beans = keyToBeans.get(key);
                        if (beans == null) {
                            beans = new LinkedHashSet();
                            keyToBeans.put(key, beans);
                        }
                        if (!beans.add(bean)) {
                            throw new RuntimeException("Bean " + bean.toString() + " already in " + beans.toString());
                        }
                    }
                    Integer currentCount = m_groupCounts.get(groupKey);
                    if (currentCount == null) {
                        m_groupCounts.put(groupKey, Integer.valueOf(1));
                    } else {
                        m_groupCounts.put(groupKey, Integer.valueOf(currentCount.intValue() + 1));
                    }
                });
            }

            /**
             * Clear.
             */
            public void clear() {
                m_groupCounts.clear();
                for (Map<String, ToolBean> map : m_keysToBean.values()) {
                    map.clear();
                }
            }

            /**
             * Gets the bean - search all keys.
             *
             * @param key String
             * @return ToolBean
             */
            public ToolBean get(String key) {
                ToolBean bean = null;
                for (Map<String, ToolBean> map : m_keysToBean.values()) {
                    bean = map.get(key);
                    if (bean != null) {
                        break;
                    }
                }
                return bean;
            }

            /**
             * Gets the bean - search all keys.
             *
             * @param column ToolBeanColumn
             * @param key String
             * @return ToolBean
             */
            public ToolBean get(ToolBeanColumn column, String key) {
                ToolBean bean = null;
                Map<String, ToolBean> map = m_keysToBean.get(column.resolve(null));
                if (map != null) {
                    bean = map.get(key);
                }
                return bean;
            }

            /**
             * Gets the bean set for non unique keys - search all keys.
             *
             * @param key String
             * @return Set<ToolBean>
             */
            public Set<ToolBean> getBeanSet(String key) {
                Set<ToolBean> beans = null;
                for (Map<String, Set<ToolBean>> map : m_keysToBeans.values()) {
                    beans = map.get(key);
                    if (beans != null) {
                        break;
                    }
                }
                return beans;
            }

            /**
             * Gets the class name.
             *
             * @return String
             */
            public String getClassName() {
                return className;
            }

            /**
             * Key set.
             *
             * @return Set
             */
            public Set<String> keySet() {
                return keySet(FIELD_OID);
            }

            /**
             * Key set.
             *
             * @param column ToolBeanColumn
             * @return Set
             */
            public Set<String> keySet(ToolBeanColumn column) {
                String key = column.resolve(null);
                if (column.isUnique()) {
                    Map<String, ToolBean> keyToBean = m_keysToBean.get(key);
                    return keyToBean == null ? Collections.EMPTY_SET : keyToBean.keySet();
                }
                Map<String, Set<ToolBean>> keysToBeans = m_keysToBeans.get(key);
                return keysToBeans == null ? Collections.EMPTY_SET : keysToBeans.keySet();
            }

            /**
             * Removes the.
             *
             * @param oid String
             * @param groupKey String
             * @return ToolBean
             */
            public ToolBean remove(String oid, String groupKey) {
                ToolBean toRemove = get(oid);
                if (toRemove != null) {
                    for (Entry<String, Map<String, ToolBean>> entry : m_keysToBean.entrySet()) {
                        String key = entry.getKey();
                        String keyValue = (String) toRemove.getFieldValueByColumnName(key);
                        entry.getValue().remove(keyValue);
                    }
                    Integer currentCount = m_groupCounts.get(groupKey);
                    if (currentCount != null && currentCount.intValue() > 0) {
                        m_groupCounts.put(groupKey, Integer.valueOf(currentCount.intValue() - 1));
                    } else {
                        Optional<Entry<String, Integer>> maxGroup = m_groupCounts.entrySet().stream()
                                .max((entry1, entry2) -> entry1.getValue().compareTo(entry2.getValue()));
                        if (maxGroup.isPresent() && maxGroup.get().getValue().intValue() > 0) {
                            m_groupCounts.put(maxGroup.get().getKey(),
                                    Integer.valueOf(maxGroup.get().getValue().intValue() - 1));
                        }
                    }
                }
                return toRemove;
            }

            /**
             * To string.
             *
             * @return String
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                output.append(className);
                output.append(" -");
                m_keysToBean.entrySet().stream().forEach(entry -> {
                    output.append(" ");
                    output.append(entry.getKey());
                    output.append(": ");
                    output.append(entry.getValue().keySet().size());
                });
                m_groupCounts.entrySet().stream().forEach(entry -> {
                    output.append(" ");
                    output.append(entry.getKey());
                    output.append(" ");
                    output.append(entry.getValue().toString());
                });
                return output.toString();
            }

            /**
             * Values.
             *
             * @param column ToolBeanColumn
             * @return Collection
             */
            public Collection<ToolBean> values(ToolBeanColumn column) {
                String key = column.resolve(null);
                if (column.isUnique()) {
                    Map<String, ToolBean> keyToBean = m_keysToBean.get(key);
                    return keyToBean == null ? Collections.EMPTY_LIST : keyToBean.values();
                }
                Map<String, Set<ToolBean>> keyToBeans = m_keysToBeans.get(key);
                return keyToBeans.values().stream()
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList());
            }
        }

        private final Map<String, BeanCacheItem> m_beanCache = new HashMap();

        /**
         * Cache tool bean.
         *
         * @param definition ToolBeanDefinition
         * @param bean ToolBean
         * @param groupKey String
         */
        public void cacheToolBean(ToolBeanDefinition definition, ToolBean bean, String groupKey) {
            String className = bean.getClass().getName();
            BeanCacheItem cache = getCacheItem(className);
            cache.addBean(definition, bean, groupKey);
        }

        /**
         * Clear all cached tool beans.
         *
         * @param clazzToolBean Class<? extends ToolBean>
         */
        public void clearAllCachedToolBeans(Class<? extends ToolBean> clazzToolBean) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            cache.clear();
        }

        /**
         * Gets the cached tool bean.
         *
         * @param <T> the generic type
         * @param clazzToolBean Class<T>
         * @param oid String
         * @return t
         */
        public <T extends ToolBean> T getCachedToolBean(final Class<T> clazzToolBean, final String oid) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            return (T) cache.get(oid);
        }

        /**
         * Gets the cached tool beans.
         *
         * @param <T> the generic type
         * @param clazzToolBean Class<T>
         * @return Collection
         */
        public <T extends ToolBean> Collection<T> getCachedToolBeans(final Class<T> clazzToolBean) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            return (Collection<T>) cache.values(FIELD_OID);
        }

        /**
         * Gets the cached tool bean oids.
         *
         * @param clazzToolBean Class<? extends ToolBean>
         * @return Sets the
         */
        public Set<String> getCachedToolBeanOids(final Class<? extends ToolBean> clazzToolBean) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            return cache.keySet();
        }

        /**
         * Gets the cached tool bean.
         *
         * @param <T> the generic type
         * @param clazzToolBean Class<T>
         * @param oid String
         * @return t
         */
        public <T extends ToolBean> Set<T> getCachedToolBeanSet(final Class<T> clazzToolBean, final String oid) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            return (Set<T>) cache.getBeanSet(oid);
        }

        /**
         * Removes the cached tool bean.
         *
         * @param <T> the generic type
         * @param clazzToolBean Class<T>
         * @param oid String
         * @param groupKey String
         * @return T
         */
        public <T extends ToolBean> T removeCachedToolBean(final Class<T> clazzToolBean,
                                                           final String oid,
                                                           String groupKey) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            return (T) cache.remove(oid, groupKey);
        }

        /**
         * Removes the cached tool beans.
         *
         * @param <T> the generic type
         * @param clazzToolBean Class<T>
         * @param oids List<String>
         * @param groupKey String
         * @return List
         */
        public <T extends ToolBean> List<T> removeCachedToolBeans(Class<T> clazzToolBean,
                                                                  List<String> oids,
                                                                  String groupKey) {
            String className = clazzToolBean.getName();
            BeanCacheItem cache = getCacheItem(className);
            List<T> beans = new ArrayList();
            oids.stream().forEach(oid -> {
                T bean = (T) cache.remove(oid, null);
                if (bean != null) {
                    beans.add(bean);
                }
            });
            return beans;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder output = new StringBuilder();
            m_beanCache.values().stream()
                    .sorted(new Comparator<BeanCacheItem>() {
                        @Override
                        public int compare(BeanCacheItem o1, BeanCacheItem o2) {
                            return o1.getClassName().compareTo(o2.getClassName());
                        }
                    })
                    .forEach(cacheItem -> {
                        output.append(cacheItem.toString());
                        output.append("\n");
                    });
            return output.toString();
        }

        /**
         * Gets the cache item.
         *
         * @param className String
         * @return Bean cache item
         */
        private BeanCacheItem getCacheItem(String className) {
            BeanCacheItem cacheItem = m_beanCache.get(className);
            if (cacheItem == null) {
                cacheItem = new BeanCacheItem(className);
                m_beanCache.put(className, cacheItem);
            }
            return cacheItem;
        }
    }


    /**
     * The Class RptBeanColumn.
     */
    public static class ToolBeanColumn {

        /**
         * The Class AliasDefinition.
         */
        public static class AliasDefinition {
            private final String m_alias;
            private final String m_ddxId;
            private final boolean m_isRequired;

            /**
             * Instantiates a new alias definition.
             *
             * @param alias String
             * @param ddxId String
             * @param isRequired boolean
             */
            public AliasDefinition(String alias, String ddxId, boolean isRequired) {
                this.m_alias = alias;
                this.m_ddxId = ddxId;
                this.m_isRequired = isRequired;
            }

            /**
             * Gets the alias.
             *
             * @return the alias
             */
            public String getAlias() {
                return m_alias;
            }

            /**
             * Gets the ddx id.
             *
             * @return the ddxId
             */
            public String getDdxId() {
                return m_ddxId;
            }

            /**
             * Checks if is required.
             *
             * @return the isRequired
             */
            public boolean isRequired() {
                return m_isRequired;
            }

            /**
             * To string.
             *
             * @return the string
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                if (!StringUtils.isEmpty(m_ddxId)) {
                    output.append(m_ddxId + "-");
                }
                output.append(m_alias);
                output.append("-");
                output.append(m_isRequired);
                return output.toString();
            }
        }

        private AliasDefinition m_aliasDefinition;
        private BeanColumnPath m_beanPath;
        private FieldConverter m_converter;
        private DataDictionaryField m_field;
        private boolean m_isUndefined = false;
        private boolean m_isUnique = true;
        private String m_resolved;
        private boolean m_sortAscending = true;
        private BeanTablePath m_tablePath;

        /**
         * Instantiates a new rpt bean column.
         *
         * @param beanPath String
         */
        public ToolBeanColumn(String beanPath) {
            this(beanPath, true);
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param beanPath String
         * @param sortAscending boolean
         */
        public ToolBeanColumn(String beanPath, boolean sortAscending) {
            m_resolved = beanPath;
            m_sortAscending = sortAscending;
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param beanPath String
         */
        public ToolBeanColumn(BeanColumnPath beanPath) {
            this(beanPath, true);
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param beanPath String
         * @param converter Function<Object,Object>
         */
        public ToolBeanColumn(BeanColumnPath beanPath, FieldConverter converter) {
            this(beanPath, converter, true);
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param beanPath String
         * @param sortAscending boolean
         */
        public ToolBeanColumn(BeanColumnPath beanPath, boolean sortAscending) {
            this(beanPath, null, sortAscending);
        }


        /**
         * Instantiates a new rpt bean column.
         *
         * @param beanPath String
         * @param converter Function<Object,Object>
         * @param sortAscending boolean
         */
        public ToolBeanColumn(BeanColumnPath beanPath, FieldConverter converter, boolean sortAscending) {
            super();
            m_beanPath = beanPath;
            m_converter = converter;
            m_sortAscending = sortAscending;
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param tablePath BeanTablePath
         * @param alias String
         */
        public ToolBeanColumn(BeanTablePath tablePath, String alias) {
            this(tablePath, alias, null, true);
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param tablePath BeanTablePath
         * @param alias String
         * @param ddxId String
         */
        public ToolBeanColumn(BeanTablePath tablePath, String alias, String ddxId) {
            this(tablePath, alias, ddxId, true);
        }

        /**
         * Instantiates a new rpt bean column.
         *
         * @param tablePath BeanTablePath
         * @param alias String
         * @param ddxId String
         * @param sortAscending boolean
         */
        public ToolBeanColumn(BeanTablePath tablePath, String alias, String ddxId, boolean sortAscending) {
            super();
            m_tablePath = tablePath;
            m_aliasDefinition = new AliasDefinition(alias, ddxId, true);
            m_sortAscending = sortAscending;
        }

        /**
         * Instantiates a new tool bean column.
         *
         * @param tablePath BeanTablePath
         * @param aliasDefinition AliasDefinition
         */
        public ToolBeanColumn(BeanTablePath tablePath, AliasDefinition aliasDefinition) {
            this(tablePath, aliasDefinition, true);
        }

        /**
         * Instantiates a new tool bean column.
         *
         * @param tablePath BeanTablePath
         * @param aliasDefinition AliasDefinition
         * @param sortAscending boolean
         */
        public ToolBeanColumn(BeanTablePath tablePath, AliasDefinition aliasDefinition, boolean sortAscending) {
            super();
            m_tablePath = tablePath;
            m_aliasDefinition = aliasDefinition;
            m_sortAscending = sortAscending;
        }

        /**
         * Instantiates a new tool bean column.
         */
        private ToolBeanColumn() {
            super();
        }

        /**
         * Adds the non unique.
         *
         * @return ToolBeanColumn
         */
        public ToolBeanColumn addNonUnique() {
            ToolBeanColumn column = copy();
            column.m_isUnique = false;
            return column;
        }

        /**
         * Gets the alias.
         *
         * @return String
         */
        /*
         * Gets the alias.
         *
         * @return String
         */
        public String getAlias() {
            return m_aliasDefinition == null ? EMPTY_STRING : m_aliasDefinition.getAlias();
        }

        /**
         * Gets the converter.
         *
         * @return Function
         */
        public FieldConverter getConverter() {
            return m_converter;
        }

        /**
         * Gets the field.
         *
         * @param extractor DictionaryExtractor
         * @return Data dictionary field
         */
        public DataDictionaryField getField(DictionaryExtractor extractor) {
            if (m_field == null) {
                if (m_beanPath != null) {
                    m_field = m_beanPath.getField(extractor.getBroker());
                } else {
                    m_field = extractor.getFieldByAlias(m_aliasDefinition.getAlias(), m_aliasDefinition.getDdxId(),
                            m_aliasDefinition.isRequired());
                }
            }
            return m_field;
        }

        /**
         * Gets the model property.
         *
         * @param extractor DictionaryExtractor
         * @return Model property
         */
        public ModelProperty getModelProperty(DictionaryExtractor extractor) {
            return new ModelProperty(getField(extractor), extractor.getDictionary(m_aliasDefinition.getDdxId()));
        }

        /**
         * Checks if is undefined.
         *
         * @return true, if is undefined
         */
        public boolean isUndefined() {
            if (m_resolved == null) {
                resolve(null);
            }
            return m_isUndefined;
        }

        /**
         * Checks if is unique.
         *
         * @return true, if is unique
         */
        public boolean isUnique() {
            return m_isUnique;
        }

        /**
         * Resolve.
         * Note that all columns are resolved when initial bean is instantiated, so
         * DictionaryExtractor can be null when calling for a bean
         *
         * @param extractor DictionaryExtractor
         * @return String
         */
        public String resolve(DictionaryExtractor extractor) {
            if (m_resolved == null) {
                if (m_beanPath != null) {
                    if (extractor != null) {
                        m_field = m_beanPath.getField(extractor.getBroker());
                    }
                    m_resolved = m_beanPath.toString();
                } else {
                    if (extractor == null) {
                        extractor = ToolBean.getDictionaryExtractor();
                    }
                    if (extractor == null) {
                        throw new IllegalStateException("DictionaryExtractor is required");
                    }
                    m_field = extractor.getFieldByAlias(m_aliasDefinition.getAlias(), m_aliasDefinition.getDdxId(),
                            m_aliasDefinition.isRequired());
                    if (m_field != null
                            && !m_field.getDataTable().getDatabaseName().equals(m_tablePath.getDatabaseName())) {
                        throw new IllegalStateException("Alias [" + m_aliasDefinition.getAlias() + "] found on table "
                                + m_field.getDataTable().getDatabaseName() + " was expected on table "
                                + m_tablePath.getDatabaseName());
                    }
                    if (m_field != null && m_field.isEnabled() && m_tablePath.getParent() == null) {
                        m_resolved = m_field.getJavaName();
                    } else if (m_field != null && m_field.isEnabled()) {
                        m_resolved = m_tablePath.toString() + ModelProperty.PATH_DELIMITER + m_field.getJavaName();
                    } else {
                        m_resolved = "";
                        m_isUndefined = true;
                    }
                }
            }
            return m_resolved;
        }

        /**
         * Sort ascending.
         *
         * @return true, if successful
         */
        public boolean sortAscending() {
            return m_sortAscending;
        }

        /**
         * To string.
         *
         * @return the string
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder output = new StringBuilder();
            if (m_aliasDefinition != null) {
                output.append(m_aliasDefinition.toString() + "-");
            }
            output.append(resolve(ToolBean.getDictionaryExtractor()));
            return output.toString();
        }

        /**
         * Copy.
         *
         * @return ToolBeanColumn
         */
        private ToolBeanColumn copy() {
            ToolBeanColumn column = new ToolBeanColumn();
            column.m_aliasDefinition = m_aliasDefinition;
            column.m_beanPath = m_beanPath;
            column.m_converter = m_converter;
            column.m_field = m_field;
            column.m_isUndefined = m_isUndefined;
            column.m_isUnique = m_isUnique;
            column.m_resolved = m_resolved;
            column.m_sortAscending = m_sortAscending;
            column.m_tablePath = m_tablePath;

            return column;
        }

    }


    /**
     * The Class RptBeanColumns.
     */
    public static class ToolBeanDefinition {
        private static Pattern s_innerJoinPattern = Pattern.compile(
                "INNER JOIN ([a-zA-Z0-9_]*) [a-zA-Z0-9_]* ON [a-zA-Z0-9_]*.([a-zA-Z0-9_]*)=[a-zA-Z0-9_\\.]*\\.([a-zA-Z0-9_]*)");

        /**
         * The Class AdvancedJoinAdjuster.
         */
        public static class AdvancedJoinAdjuster extends JoinAdjuster {
            /**
             * The type of join to change to
             */
            private JoinType m_join;

            /**
             * Persistence Key to check Database Type
             */
            private PersistenceKey m_persistenceKey;

            /**
             * The Database name of the object
             */
            private String m_tableName;

            /**
             * The Database name of the join column
             */
            private String m_columnName;



            /**
             * Instantiates a new advanced join adjuster.
             *
             * @param jointype the jointype
             * @param tableName the table name
             * @param columnName the column name
             * @param persistenceKey the persistence key
             */
            public AdvancedJoinAdjuster(JoinType jointype, String tableName, String columnName,
                    PersistenceKey persistenceKey) {
                super(jointype, tableName, persistenceKey);
                m_join = jointype;
                m_persistenceKey = persistenceKey;
                m_tableName = tableName;
                m_columnName = columnName;
            }

            /**
             * Fix sql.
             *
             * @param originalSql the original sql
             * @return the string
             * @see com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster#fixSql(java.lang.String)
             */
            @Override
            protected String fixSql(String originalSql) {
                if (m_join.equals(JoinType.FULL_OUTER) && DBType.MYSQL.equals(m_persistenceKey.getDBType())) {
                    return originalSql;
                }

                Matcher matcher = s_innerJoinPattern.matcher(originalSql);
                while (matcher.find()) {
                    if (m_tableName.equals(matcher.group(1))
                            && (m_columnName.equals(matcher.group(2)) || m_columnName.equals(matcher.group(3)))) {
                        String searchString = matcher.group();
                        String replaceString = searchString.replace("INNER JOIN", m_join.sql_value);
                        return originalSql.replace(searchString, replaceString);
                    }
                }

                return originalSql;
            }

        }

        /**
         * The Class JoinAdjusterPattern.
         */
        public static class JoinAdjusterPattern {
            public String columnName;
            public String tableName;
            public JoinType type;

            /**
             * Instantiates a new join adjuster pattern.
             *
             * @param type JoinType
             * @param tableName String
             */
            public JoinAdjusterPattern(JoinType type, String tableName) {
                this.type = type;
                this.tableName = tableName;
            }

            /**
             * Instantiates a new join adjuster pattern.
             *
             * @param type JoinType
             * @param tableName String
             * @param columnName String
             */
            public JoinAdjusterPattern(JoinType type, String tableName, String columnName) {
                this.type = type;
                this.tableName = tableName;
                this.columnName = columnName;
            }

            /**
             * Gets the join adjuster.
             *
             * @param persistenceKey the persistence key
             * @return the join adjuster
             */
            public JoinAdjuster getJoinAdjuster(PersistenceKey persistenceKey) {
                return StringUtils.isEmpty(columnName) ? new JoinAdjuster(type, tableName, persistenceKey)
                        : new AdvancedJoinAdjuster(type, tableName, columnName, persistenceKey);
            }
        }

        private Map<String, ToolBeanColumn> m_aliasMap;
        private List<BiFunction<X2Broker, X2Criteria, X2Criteria>> m_criteriaFunctions;
        private List<ToolBeanColumn> m_columns;
        private List<ToolBeanColumn> m_columnsResolved;
        private List<BiFunction<X2Broker, X2Criteria, List<String>>> m_extraColumnsFunctions;
        private List<Predicate<ToolBean>> m_filters;
        private List<ToolBeanColumn> m_keys;
        private Map<String, Integer> m_mapColumnToIndex;
        private Map<Integer, String> m_mapIndexToColumn;
        private List<JoinAdjusterPattern> m_joinAdjusters;
        private List<ToolBeanRelationship> m_relationships;
        private List<ToolBeanColumn> m_sortColumns;

        /**
         * Instantiates a new rpt bean columns.
         */
        public ToolBeanDefinition() {
            super();
            m_columns = new ArrayList();
        }

        /**
         * Instantiates a new rpt bean columns.
         *
         * @param c Collection<? extends RptBeanColumn>
         */
        public ToolBeanDefinition(Collection<ToolBeanColumn> c) {
            super();
            m_columns = new ArrayList(c);
            Collections.unmodifiableList(m_columns);
        }

        /**
         * Instantiates a new rpt bean columns.
         *
         * @param columns RptBeanColumn[]
         */
        public ToolBeanDefinition(ToolBeanColumn... columns) {
            super();
            m_columns = new ArrayList(columns.length);
            for (ToolBeanColumn column : columns) {
                m_columns.add(column);
            }
            Collections.unmodifiableList(m_columns);
        }

        /**
         * Adds the extra columns.
         *
         * @param broker X2Broker
         * @param criteria X2Criteria
         * @return List
         */
        public List<String> addExtraColumns(X2Broker broker, X2Criteria criteria) {
            List<String> newColumns = getExtraColumnsFunctions().stream()
                    .map(fn -> fn.apply(broker, criteria).stream())
                    .reduce(Stream.empty(), (output, element) -> Stream.concat(output, element))
                    .distinct()
                    .filter(colName -> !m_mapColumnToIndex.containsKey(colName))
                    .collect(Collectors.toList());
            int index = m_mapColumnToIndex.size();
            for (String colName : newColumns) {
                m_mapColumnToIndex.put(colName, Integer.valueOf(index));
                m_mapIndexToColumn.put(Integer.valueOf(index++), colName);
            }
            return newColumns;
        }

        /**
         * Apply criteria.
         *
         * @param broker X2Broker
         * @param criteria X2Criteria
         * @return X2Criteria
         */
        public X2Criteria applyCriteria(X2Broker broker, X2Criteria criteria) {
            for (BiFunction<X2Broker, X2Criteria, X2Criteria> fnCriteria : getCriteriaFunctions()) {
                criteria = fnCriteria.apply(broker, criteria);
            }
            return criteria;
        }

        /**
         * Expand.
         *
         * @param columns RptBeanColumn[]
         * @return RptBeanColumns
         */
        public ToolBeanDefinition expand(ToolBeanColumn... columns) {
            ToolBeanDefinition columnList = copy();

            for (ToolBeanColumn column : columns) {
                columnList.m_columns.add(column);
            }

            return columnList.unmodifiable();
        }

        /**
         * Expand.
         *
         * @param columns RptBeanColumns
         * @return Object
         */
        public ToolBeanDefinition expand(ToolBeanDefinition columns) {
            ToolBeanDefinition expandedColumns = copy();

            expandedColumns.m_columns.addAll(columns.m_columns);
            if (!columns.getJoinAdjusters().isEmpty()) {
                if (expandedColumns.m_joinAdjusters == null) {
                    expandedColumns.m_joinAdjusters = new ArrayList(columns.getJoinAdjusters());
                } else {
                    expandedColumns.m_joinAdjusters.addAll(columns.getJoinAdjusters());
                }
            }

            return expandedColumns.unmodifiable();
        }

        /**
         * Expand criteria functions.
         *
         * @param criteriaFunctions BiFunction<X2Broker,X2Criteria,X2Criteria>[]
         * @return ToolBeanDefinition
         */
        public ToolBeanDefinition expandCriteriaFunctions(BiFunction<X2Broker, X2Criteria, X2Criteria>... criteriaFunctions) {
            ToolBeanDefinition expandedColumns = copy();

            if (expandedColumns.m_criteriaFunctions == null) {
                expandedColumns.m_criteriaFunctions = new ArrayList(criteriaFunctions.length);
            }
            for (BiFunction<X2Broker, X2Criteria, X2Criteria> function : criteriaFunctions) {
                expandedColumns.m_criteriaFunctions.add(function);
            }

            return expandedColumns.unmodifiable();
        }

        /**
         * Expand filters. The filters will only be applied to beans that are not already cached
         *
         * @param extraColumnsFunctions BiFunction<X2Broker,X2Criteria,List<String>>[]
         * @return ToolBeanDefinition
         */
        public ToolBeanDefinition expandExtraColumnsFunctions(BiFunction<X2Broker, X2Criteria, List<String>>... extraColumnsFunctions) {
            ToolBeanDefinition expandedColumns = copy();

            if (expandedColumns.m_extraColumnsFunctions == null) {
                expandedColumns.m_extraColumnsFunctions = new ArrayList(extraColumnsFunctions.length);
            }
            for (BiFunction<X2Broker, X2Criteria, List<String>> function : extraColumnsFunctions) {
                expandedColumns.m_extraColumnsFunctions.add(function);
            }
            return expandedColumns.unmodifiable();
        }


        /**
         * Expand filters. The filters will only be applied to beans that are not already cached
         *
         * @param filterFunctions Predicate<ToolBean>[]
         * @return ToolBeanDefinition
         */
        public ToolBeanDefinition expandFilters(Predicate<ToolBean>... filterFunctions) {
            ToolBeanDefinition expandedColumns = copy();

            if (expandedColumns.m_filters == null) {
                expandedColumns.m_filters = new ArrayList(filterFunctions.length);
            }
            for (Predicate<ToolBean> function : filterFunctions) {
                expandedColumns.m_filters.add(function);
            }
            return expandedColumns.unmodifiable();
        }


        /**
         * Expand join adjusters.
         *
         * @param adjusters JoinAdjusterPattern[]
         * @return ToolBeanColumns
         */
        public ToolBeanDefinition expandJoinAdjusters(JoinAdjusterPattern... adjusters) {
            ToolBeanDefinition expandedColumns = copy();

            if (expandedColumns.m_joinAdjusters == null) {
                expandedColumns.m_joinAdjusters = new ArrayList(adjusters.length);
            }
            for (JoinAdjusterPattern adjuster : adjusters) {
                expandedColumns.m_joinAdjusters.add(adjuster);
            }

            return expandedColumns.unmodifiable();
        }

        /**
         * Expand keys.
         *
         * @param columns ToolBeanColumn[]
         * @return ToolBeanDefinition
         */
        public ToolBeanDefinition expandKeys(ToolBeanColumn... columns) {
            ToolBeanDefinition columnList = copy();

            if (columnList.m_keys == null) {
                columnList.m_keys = new ArrayList(columns.length);
            }

            for (ToolBeanColumn column : columns) {
                columnList.m_keys.add(column);
                columnList.m_columns.add(column);
            }

            return columnList.unmodifiable();
        }

        /**
         * Expand relationships.
         *
         * @param relationships ToolBeanRelationship[]
         * @return ToolBeanColumns
         */
        public ToolBeanDefinition expandRelationships(ToolBeanRelationship... relationships) {
            ToolBeanDefinition expandedRelationships = copy();

            if (expandedRelationships.m_relationships == null) {
                expandedRelationships.m_relationships = new ArrayList(relationships.length);
            }
            for (ToolBeanRelationship relationship : relationships) {
                expandedRelationships.m_relationships.add(relationship);
            }

            return expandedRelationships.unmodifiable();
        }

        /**
         * Expand relationships.
         *
         * @param sortColumns ToolBeanColumns
         * @return ToolBeanColumns
         */
        public ToolBeanDefinition expandSort(ToolBeanColumn... sortColumns) {
            ToolBeanDefinition adjustedColumns = copy();

            if (adjustedColumns.m_sortColumns == null) {
                adjustedColumns.m_sortColumns = new ArrayList(sortColumns.length);
            }

            for (ToolBeanColumn sortColumn : sortColumns) {
                adjustedColumns.m_sortColumns.add(sortColumn);
            }

            return adjustedColumns.unmodifiable();
        }

        /**
         * Filters OK.
         *
         * @param bean ToolBean
         * @return true, if successful
         */
        public boolean filtersOK(ToolBean bean) {
            for (Predicate<ToolBean> function : getFilters()) {
                if (!function.test(bean)) {
                    return false;
                }
            }
            return true;
        }

        /**
         * Gets the column by alias.
         *
         * @param alias the alias
         * @return the column by alias
         */
        public ToolBeanColumn getColumnByAlias(String alias) {
            if (m_aliasMap == null) {
                m_aliasMap = getColumns().stream()
                        .filter(column -> !StringUtils.isEmpty(column.getAlias()))
                        .collect(Collectors.toMap(ToolBeanColumn::getAlias, Function.identity()));
            }
            return m_aliasMap.get(alias);
        }

        /**
         * Gets the columns.
         *
         * @return List
         */
        public List<ToolBeanColumn> getColumns() {
            return m_columns;
        }

        /**
         * Gets the criteria functions.
         *
         * @return List
         */
        public List<BiFunction<X2Broker, X2Criteria, X2Criteria>> getCriteriaFunctions() {
            return m_criteriaFunctions == null ? Collections.EMPTY_LIST : m_criteriaFunctions;
        }

        /**
         * Gets the criteria functions.
         *
         * @return List
         */
        public List<BiFunction<X2Broker, X2Criteria, List<String>>> getExtraColumnsFunctions() {
            return m_extraColumnsFunctions == null ? Collections.EMPTY_LIST : m_extraColumnsFunctions;
        }

        /**
         * Gets the filters.
         *
         * @return List
         */
        public List<Predicate<ToolBean>> getFilters() {
            return m_filters == null ? Collections.EMPTY_LIST : m_filters;
        }

        /**
         * Gets the index.
         *
         * @param key String
         * @return int
         */
        public int getIndex(String key) {
            Integer index = m_mapColumnToIndex.get(key);
            return index == null ? -1 : index.intValue();
        }

        /**
         * Gets the join adjusters.
         *
         * @return List
         */
        public List<JoinAdjusterPattern> getJoinAdjusters() {
            return m_joinAdjusters == null ? Collections.EMPTY_LIST : m_joinAdjusters;
        }

        /**
         * Gets the keys.
         *
         * @return List
         */
        public List<ToolBeanColumn> getKeys() {
            return m_keys == null ? Collections.EMPTY_LIST : m_keys;
        }

        /**
         * Gets the relationships.
         *
         * @return List
         */
        public List<ToolBeanRelationship> getRelationships() {
            return m_relationships == null ? Collections.EMPTY_LIST : m_relationships;
        }

        /**
         * Gets the relationship for class.
         *
         * @param x2Clazz Class<? extends X2BaseBean>
         * @return Tool bean relationship
         */
        public ToolBeanRelationship getRelationshipForClass(Class<? extends X2BaseBean> x2Clazz) {
            return getRelationships().stream().filter(rel -> rel.getToClass().equals(x2Clazz)).findAny().orElse(null);
        }

        /**
         * Gets the resolved columns.
         *
         * @return List
         */
        public List<ToolBeanColumn> getResolvedColumns() {
            if (m_columnsResolved == null) {
                resolve(null);
            }
            return m_columnsResolved;
        }

        /**
         * Gets the resolved columns.
         *
         * @param extractor DictionaryExtractor
         * @return List
         */
        public List<ToolBeanColumn> getResolvedColumns(DictionaryExtractor extractor) {
            if (m_columnsResolved == null) {
                resolve(extractor);
            }
            return m_columnsResolved;
        }

        /**
         * Gets the sort.
         *
         * @return Tool bean columns
         */
        public List<ToolBeanColumn> getSortColumns() {
            return m_sortColumns;
        }

        /**
         * Checks if is resolved.
         *
         * @return true, if is resolved
         */
        public boolean isResolved() {
            return m_mapColumnToIndex != null;
        }

        /**
         * Resolve.
         *
         * @param extractor DictionaryExtractor
         * @return String[]
         */
        public List<String> resolve(DictionaryExtractor extractor) {
            if (m_mapColumnToIndex == null) {
                m_columnsResolved = getColumns().stream()
                        .filter(column -> !StringUtils.isEmpty(column.resolve(extractor))).collect(Collectors.toList());
                m_mapColumnToIndex = new LinkedHashMap();
                m_mapIndexToColumn = new HashMap();
                int index = 0;
                for (ToolBeanColumn column : this.m_columnsResolved) {
                    String name = column.resolve(extractor);
                    if (!m_mapColumnToIndex.containsKey(name)) {
                        m_mapColumnToIndex.put(name, Integer.valueOf(index));
                        m_mapIndexToColumn.put(Integer.valueOf(index++), name);
                    }
                }
            }
            return m_mapColumnToIndex.keySet().stream().collect(Collectors.toList());
        }

        /**
         * Copy.
         *
         * @return ToolBeanColumns
         */
        private ToolBeanDefinition copy() {
            ToolBeanDefinition columns = new ToolBeanDefinition();
            columns.m_columns = new ArrayList(this.m_columns);
            if (!getKeys().isEmpty()) {
                columns.m_keys = new ArrayList(getKeys());
            }
            if (!getCriteriaFunctions().isEmpty()) {
                columns.m_criteriaFunctions = new ArrayList(getCriteriaFunctions());
            }
            if (!getExtraColumnsFunctions().isEmpty()) {
                columns.m_criteriaFunctions = new ArrayList(getExtraColumnsFunctions());
            }
            if (!getFilters().isEmpty()) {
                columns.m_filters = new ArrayList(getFilters());
            }
            if (!getJoinAdjusters().isEmpty()) {
                columns.m_joinAdjusters = new ArrayList(getJoinAdjusters());
            }
            if (!getRelationships().isEmpty()) {
                columns.m_relationships = new ArrayList(getJoinAdjusters());
            }
            columns.m_sortColumns = getSortColumns();
            return columns;
        }

        /**
         * Unmodifiable.
         *
         * @return ToolBeanColumns
         */
        private ToolBeanDefinition unmodifiable() {
            Collections.unmodifiableList(m_columns);
            if (m_criteriaFunctions != null) {
                Collections.unmodifiableList(m_criteriaFunctions);
            }
            if (m_extraColumnsFunctions != null) {
                Collections.unmodifiableList(m_extraColumnsFunctions);
            }
            if (m_filters != null) {
                Collections.unmodifiableList(m_filters);
            }
            if (m_joinAdjusters != null) {
                Collections.unmodifiableList(m_joinAdjusters);
            }
            if (m_relationships != null) {
                Collections.unmodifiableList(m_relationships);
            }
            if (m_sortColumns != null) {
                Collections.unmodifiableList(m_sortColumns);
            }
            return this;
        }
    }


    /**
     * The Class ToolBeanQuery.
     *
     * @param <R> the generic type
     */
    public static class ToolBeanQuery<R extends ToolBean> {

        /**
         * The Class ToolBeanQueryIterator.
         *
         * @param <T> the generic type
         */
        public class ToolBeanQueryIterator<T extends ToolBean> implements Iterator<R>, AutoCloseable {
            private String m_groupKey;
            private boolean m_isCached = false;
            private QueryIterator m_iterator;
            private ToolBean m_next = null;

            /**
             * Instantiates a new tool bean query iterator.
             *
             * @param iterator QueryIterator
             * @param groupKey String
             */
            public ToolBeanQueryIterator(QueryIterator iterator, String groupKey) {
                m_iterator = iterator;
                m_groupKey = groupKey;
            }

            /**
             * Checks for next.
             * This method needs to materialize the next bean so that it can test the filters
             *
             * @return true, if successful
             * @see java.util.Iterator#hasNext()
             */
            @Override
            public boolean hasNext() {
                if (m_next != null) {
                    return true;
                }
                m_next = getNextBean();
                return m_next == null ? false : true;
            }

            /**
             * Next.
             *
             * @return R
             * @see java.util.Iterator#next()
             */
            @Override
            public R next() {
                R bean = null;
                if (m_next == null) {
                    bean = (R) getNextBean();
                } else {
                    bean = (R) m_next;
                    m_next = null;
                }

                if (!m_isCached && bean != null) {
                    cacheToolBean(definition, bean, m_groupKey);
                }
                return bean;
            }

            /**
             * Close.
             *
             * @throws Exception exception
             * @see java.lang.AutoCloseable#close()
             */
            @Override
            public void close() throws Exception {
                m_iterator.close();
            }

            /**
             * Gets the primary unique key - always the first column.
             *
             * @param row Object[]
             * @return String
             */
            private String getKey(Object[] row) {
                String uniqueKey = (String) row[0];
                if (StringUtils.isBlank(uniqueKey)) {
                    throw new RuntimeException("Unique keys must not be blank");
                }
                return uniqueKey;
            }

            /**
             * Gets the next bean.
             *
             * @return Tool bean
             */
            private ToolBean getNextBean() {
                ToolBean nextBean = null;
                while (m_iterator.hasNext()) {
                    Object[] row = getRow();
                    String uniqueKey = getKey(row);
                    nextBean = ToolBean.getCachedToolBean(m_clazzToolBean, uniqueKey);
                    if (nextBean == null) {
                        try {
                            nextBean = (ToolBean) constructor.newInstance(definition, row);
                        } catch (Exception e) {
                            throw new X2RuntimeException(e);
                        }
                        if (definition.filtersOK(nextBean)) {
                            m_isCached = false;
                            return nextBean;
                        }
                        nextBean = null;
                    } else {
                        // existing bean so no need to check filters
                        m_isCached = true;
                        return nextBean;
                    }
                }
                return nextBean;
            }

            /**
             * Gets the row.
             *
             * @return Object[]
             */
            private Object[] getRow() {
                Object[] row = (Object[]) m_iterator.next();
                int index = 0;
                for (ToolBeanColumn column : definition.getResolvedColumns()) {
                    if (column.getConverter() != null) {
                        row[index] = column.getConverter().convertedValue(row[index]);
                    }
                    ++index;
                }
                return row;
            }

        }

        private Class m_clazzToolBean;
        private Constructor constructor;
        private ToolBeanDefinition definition;
        ColumnQuery query;

        /**
         * Instantiates a new tool bean query.
         *
         * @param broker X2Broker
         * @param dictionaryExtractor DictionaryExtractor
         * @param clazzToolBean Class<R>
         * @param criteria X2Criteria
         * @param sortColumns ToolBeanColumns
         */
        public ToolBeanQuery(final X2Broker broker,
                final DictionaryExtractor dictionaryExtractor,
                final Class<R> clazzToolBean,
                final X2Criteria criteria,
                List<ToolBeanColumn> sortColumns) {
            try {
                m_clazzToolBean = clazzToolBean;
                constructor = ToolBean.getConstructorForClass(clazzToolBean);
                definition = ToolBean.getDefinitionForClass(clazzToolBean);
                List<String> resolvedColumns = definition.resolve(dictionaryExtractor);
                List<String> additionalColumns = definition.addExtraColumns(broker, criteria);
                String[] queryColumns = Stream.concat(resolvedColumns.stream(), additionalColumns.stream())
                        .toArray(size -> new String[size]);

                Class<? extends X2BaseBean> clazzX2Bean = ToolBean.getX2BaseClassForClass(clazzToolBean);
                query = new ColumnQuery(clazzX2Bean, queryColumns, criteria);
                if (!definition.getJoinAdjusters().isEmpty()) {
                    for (JoinAdjusterPattern adjuster : definition.getJoinAdjusters()) {
                        query.addQueryAdjuster(adjuster.getJoinAdjuster(broker.getPersistenceKey()));
                    }
                }
                if (sortColumns == null) {
                    sortColumns = definition.getSortColumns();
                }
                if (sortColumns != null) {
                    sortColumns.stream().forEach(
                            column -> query.addOrderBy(column.resolve(dictionaryExtractor),
                                    column.sortAscending()));
                }
            } catch (Exception e) {
                throw new X2RuntimeException(e);
            }
        }

        /**
         * Iterator.
         *
         * @param broker X2Broker
         * @param groupKey String
         * @return ToolBeanQueryIterator
         */
        public ToolBeanQueryIterator iterator(X2Broker broker, String groupKey) {
            QueryIterator iterator = broker.getReportQueryIteratorByQuery(query);
            return new ToolBeanQueryIterator(iterator, groupKey);
        }
    }

    /**
     * The Class ToolConductAction.
     */
    public static class ToolConductAction extends ToolBean {

        public static final ToolBeanColumn FIELD_ACTION_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION.actionCode());
        public static final ToolBeanColumn FIELD_ACTION_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION.actionEndDate());
        public static final ToolBeanColumn FIELD_ACTION_PENALTY_TIME =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION.actionPenaltyTime());
        public static final ToolBeanColumn FIELD_ACTION_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION.actionStartDate());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION.studentOid());

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_CONDUCT_ACTION.student().getBeanType(),
                        SisBeanPaths.STUDENT_CONDUCT_ACTION.student().getValueType(),
                        SisBeanPaths.STUDENT_CONDUCT_ACTION.studentOid().getPath(),
                        SisBeanPaths.STUDENT.conductActions().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_ACTION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ACTION_CODE,
                        FIELD_ACTION_END_DATE,
                        FIELD_ACTION_PENALTY_TIME,
                        FIELD_ACTION_START_DATE,
                        FIELD_STUDENT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_CONDUCT_ACTION.getBeanType();
        }

        /**
         * Instantiates a new conduct incident.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolConductAction(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the action code.
         *
         * @return String
         */
        public String getActionCode() {
            return this.getValueString(FIELD_ACTION_CODE);
        }

        /**
         * Returns the action code state reference.
         *
         * @return String
         */
        public String getActionCodeState() {
            return this.getValueReferenceState(FIELD_ACTION_CODE);
        }

        /**
         * Gets the action end date.
         *
         * @return Plain date
         */
        public PlainDate getActionEndDate() {
            return getValueDate(FIELD_ACTION_END_DATE);
        }

        /**
         * Gets the action penalty time.
         *
         * @return BigDecimal
         */
        public BigDecimal getActionPenaltyTime() {
            return getValueBigDecimal(FIELD_ACTION_PENALTY_TIME);
        }

        /**
         * Gets the action start date.
         *
         * @return Plain date
         */
        public PlainDate getActionStartDate() {
            return getValueDate(FIELD_ACTION_START_DATE);
        }

        /**
         * Gets the student.
         *
         * @param broker the broker
         * @return the student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }
    }

    /**
     * The Class ToolConductIncident.
     */
    public static class ToolConductIncident extends ToolBean {

        public static final ToolBeanColumn FIELD_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.description());
        public static final ToolBeanColumn FIELD_INCIDENT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.incidentCode());
        public static final ToolBeanColumn FIELD_INCIDENT_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.incidentDate());
        public static final ToolBeanColumn FIELD_INCIDENT_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.incidentId());
        public static final ToolBeanColumn FIELD_INCIDENT_LOCATION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.incidentLocation());
        public static final ToolBeanColumn FIELD_INCIDENT_TIME =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.incidentTime());
        public static final ToolBeanColumn FIELD_REFERRAL_STAFF_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.referralStaffOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.schoolOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.studentOid());
        public static final ToolBeanColumn FIELD_USER_DEFINED_TABLE_E_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.userDefinedTableEOid());

        public static ToolBeanRelationship CHILD_CONDUCT_ACTIONS =
                new ToolBeanRelationship(
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductActions().getBeanType(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductActions().getValueType(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductActions().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_ACTION.incidentOid().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductActions().getRelationshipType());

        public static ToolBeanRelationship CHILD_CONDUCT_OFFENSES =
                new ToolBeanRelationship(
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductOffenses().getBeanType(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductOffenses().getValueType(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductOffenses().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_OFFENSE.incidentOid().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.conductOffenses().getRelationshipType());

        public static ToolBeanRelationship PARENT_UDE =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.userDefinedTableE().getBeanType(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.userDefinedTableE().getValueType(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.userDefinedTableEOid().getPath(),
                        SisBeanPaths.USER_DEFINED_TABLE_E.conductIncidents().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.userDefinedTableE().getRelationshipType());


        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DESCRIPTION,
                        FIELD_INCIDENT_CODE,
                        FIELD_INCIDENT_DATE,
                        FIELD_INCIDENT_ID,
                        FIELD_INCIDENT_LOCATION,
                        FIELD_INCIDENT_TIME,
                        FIELD_REFERRAL_STAFF_OID,
                        FIELD_SCHOOL_OID,
                        FIELD_STUDENT_OID,
                        FIELD_USER_DEFINED_TABLE_E_OID)
                .expandRelationships(CHILD_CONDUCT_ACTIONS,
                        CHILD_CONDUCT_OFFENSES,
                        PARENT_UDE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_CONDUCT_INCIDENT.getBeanType();
        }

        /**
         * Instantiates a new conduct incident.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolConductIncident(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the conduct actions.
         *
         * @param broker the broker
         * @return the conduct action
         */
        public List<ToolConductAction> getConductActions(X2Broker broker) {
            return (List<ToolConductAction>) getChildren(broker, CHILD_CONDUCT_ACTIONS);
        }

        /**
         * Gets the conduct offenses.
         *
         * @param broker the broker
         * @return the conduct offense
         */
        public List<ToolConductOffense> getConductOffenses(X2Broker broker) {
            return (List<ToolConductOffense>) getChildren(broker, CHILD_CONDUCT_OFFENSES);
        }

        /**
         * Returns the description.
         *
         * @return String
         */
        public String getDescription() {
            return this.getValueString(FIELD_DESCRIPTION);
        }

        /**
         * Returns the incident code.
         *
         * @return String
         */
        public String getIncidentCode() {
            return this.getValueString(FIELD_INCIDENT_CODE);
        }


        /**
         * Returns the incident date.
         *
         * @return PlainDate
         */
        public PlainDate getIncidentDate() {
            return this.getValueDate(FIELD_INCIDENT_DATE);
        }


        /**
         * Returns the incident id.
         *
         * @return String
         */
        public String getIncidentId() {
            return this.getValueString(FIELD_INCIDENT_ID);
        }

        /**
         * Returns the incident location code.
         *
         * @return String
         */
        public String getIncidentLocation() {
            return this.getValueString(FIELD_INCIDENT_LOCATION);
        }

        /**
         * Returns the incident location state code.
         *
         * @return String
         */
        public String getIncidentLocationState() {
            return this.getValueReferenceState(FIELD_INCIDENT_LOCATION);
        }

        /**
         * Returns the incident time.
         *
         * @return PlainTime
         */
        public PlainTime getIncidentTime() {
            return this.getValueTime(FIELD_INCIDENT_TIME);
        }

        /**
         * Returns the referal staff oid.
         *
         * @return PlainTime
         */
        public String getReferralStaffOid() {
            return this.getValueString(FIELD_REFERRAL_STAFF_OID);
        }

        /**
         * Returns the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return this.getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Returns the student.
         *
         * @param broker X2Broker
         * @return ToolStudent
         */
        public ToolStudent getStudent(X2Broker broker) {
            return getBeanByOid(broker, ToolStudent.class, getStudentOid(), true);
        }

        /**
         * Returns the student oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return this.getValueString(FIELD_STUDENT_OID);
        }

        /**
         * Returns the UDE oid.
         *
         * @return String
         */
        public String getUserDefinedTableEOid() {
            return this.getValueString(FIELD_USER_DEFINED_TABLE_E_OID);
        }
    }

    /**
     * The Class ToolConductIncident.
     */
    public static class ToolConductOffense extends ToolBean {

        public static final ToolBeanColumn FIELD_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_OFFENSE.description());
        public static final ToolBeanColumn FIELD_INCIDENT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_OFFENSE.incidentCode());
        public static final ToolBeanColumn FIELD_INCIDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_OFFENSE.incidentOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DESCRIPTION,
                        FIELD_INCIDENT_CODE,
                        FIELD_INCIDENT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_CONDUCT_OFFENSE.getBeanType();
        }

        /**
         * Instantiates a new conduct incident.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolConductOffense(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the description.
         *
         * @return String
         */
        public String getDescription() {
            return this.getValueString(FIELD_DESCRIPTION);
        }

        /**
         * Returns the incident code.
         *
         * @return String
         */
        public String getIncidentCode() {
            return getValueString(FIELD_INCIDENT_CODE);
        }

        /**
         * Returns the incident state code.
         *
         * @return String
         */
        public String getIncidentCodeState() {
            return this.getValueReferenceState(FIELD_INCIDENT_CODE);
        }

        /**
         * Returns the incident code.
         *
         * @return ReferenceCode
         */
        public ReferenceCode getIncidentRefCode() {
            ReferenceCode code = null;
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            DataDictionaryField field = FIELD_INCIDENT_CODE.getField(extractor);
            Map<String, ReferenceCode> refCodes = extractor.getReferenceCodes(field.getReferenceTableOid());
            code = refCodes.get(getIncidentCode());
            return code;
        }

    }


    /**
     * The Class ToolBeanRelationship.
     */
    public static class ToolBeanRelationship {
        /**
         * The Enum RenderGranularity.
         */
        public enum LoaderMode {
            // No operation on children
            DEFAULT,
            // Clear existing children and reload
            CLEAR,
            // process only parent's without children
            AUGMENT
        }


        /**
         * The Class Loader.
         */
        public class Loader {
            Set<String> m_oidsToLoad;
            Set<String> m_oidsToSort;
            private String m_parentPath;
            private String m_childPath;

            /**
             * Instantiates a new loader.
             *
             * @param relationship ToolBeanRelationship
             * @param loaderMode LoaderMode
             */
            private Loader(ToolBeanRelationship relationship, LoaderMode loaderMode) {
                m_parentPath = relationship.getParentPath();
                m_childPath = relationship.getChildPath();
                if (LoaderMode.CLEAR.equals(loaderMode)) {
                    ToolBean.getCachedToolBeans(getParentClass()).forEach(bean -> bean.clearChild(m_parentPath));
                }
                if (LoaderMode.AUGMENT.equals(loaderMode)) {
                    m_oidsToLoad = ToolBean.getCachedToolBeans(getParentClass()).stream()
                            .filter(bean -> !bean.isChildLoaded(m_parentPath))
                            .map(bean -> m_parentKeyFunction == null ? bean.getOid() : m_parentKeyFunction.apply(bean))
                            .collect(Collectors.toSet());
                } else if (m_parentKeyFunction == null) {
                    m_oidsToLoad = new HashSet(ToolBean.getCachedToolBeanOids(getParentClass()));
                } else {
                    m_oidsToLoad = ToolBean.getCachedToolBeans(getParentClass()).stream()
                            .map(bean -> m_parentKeyFunction.apply(bean))
                            .collect(Collectors.toSet());
                }
                if (m_comparator != null) {
                    m_oidsToSort = new HashSet();
                }
            }

            /**
             * Gets the loaded oids.
             *
             * @return Sets the
             */
            public Set<String> getLoadedOids() {
                return m_oidsToLoad;
            }

            /**
             * Gets the child parent oid field.
             *
             * @return String
             */
            public String getChildPath() {
                return m_childPath;
            }

            /**
             * Gets the parent path.
             *
             * @return String
             */
            public String getParentPath() {
                return m_parentPath;
            }

            /**
             * Load.
             *
             * @param bean ToolBean
             */
            public void load(ToolBean bean) {
                String parentBeanOid = (String) bean.getFieldValueByColumnName(m_childPath);
                ToolBean parentBean = getBeanByOid(m_parentClass, parentBeanOid, m_resolveUsingX2Class);
                parentBean.addChild(m_parentPath, bean);
                m_oidsToLoad.remove(parentBeanOid);
                if (m_comparator != null) {
                    m_oidsToSort.add(parentBeanOid);
                }
            }

            /**
             * Load empty.
             */
            public void loadEmpty() {
                m_oidsToLoad.stream().forEach(oid -> {
                    ToolBean parentBean = getBeanByOid(m_parentClass, oid, m_resolveUsingX2Class);
                    parentBean.addEmptyChildrenAndSort(m_parentPath, m_comparator);
                });
                if (m_oidsToSort != null) {
                    m_oidsToSort.stream().forEach(oid -> {
                        ToolBean parentBean = getBeanByOid(m_parentClass, oid, m_resolveUsingX2Class);
                        parentBean.addEmptyChildrenAndSort(m_parentPath, m_comparator);
                    });
                }
            }

            /**
             * Parent exists.
             *
             * @param bean ToolBean
             * @return true, if successful
             */
            public boolean parentExists(ToolBean bean) {
                String parentBeanOid = (String) bean.getFieldValueByColumnName(m_childPath);
                return isCached(m_parentClass, parentBeanOid, true);

            }
        }

        private Class<? extends ToolBean> m_childClass;
        private Comparator<ToolBean> m_comparator;
        private Class<?> m_fromClass;
        private String m_fromPath;
        private LoaderMode m_loaderMode = LoaderMode.DEFAULT;
        private Class<? extends ToolBean> m_parentClass;
        private Function<ToolBean, String> m_parentKeyFunction;
        private RelationshipType m_relationshipType;
        private boolean m_resolveUsingX2Class;
        private Class<?> m_toClass;
        private String m_toPath;

        /**
         * Instantiates a new tool bean relationship.
         *
         * @param fromClass Class<?>
         * @param toClass Class<?>
         * @param fromPath String
         * @param toPath String
         * @param relationshipType RelationshipType
         */
        public ToolBeanRelationship(Class<?> fromClass, Class<?> toClass,
                String fromPath, String toPath, RelationshipType relationshipType) {
            this(fromClass, toClass, fromPath, toPath, relationshipType, true);
        }

        /**
         * Instantiates a new tool bean relationship.
         *
         * @param fromClass Class<? extends X2BaseBean>
         * @param toClass Class<? extends X2BaseBean>
         * @param fromPath String
         * @param toPath String
         * @param relationshipType RelationshipType
         * @param resolveUsingX2Class boolean
         */
        public ToolBeanRelationship(Class<?> fromClass, Class<?> toClass, String fromPath, String toPath,
                RelationshipType relationshipType, boolean resolveUsingX2Class) {
            m_fromPath = fromPath;
            m_toPath = toPath;
            m_fromClass = fromClass;
            m_toClass = toClass;
            m_relationshipType = relationshipType;
            m_resolveUsingX2Class = resolveUsingX2Class;
        }

        /**
         * Adds the comparator.
         *
         * @param comparator Comparator<ToolBean>
         * @return ToolBeanRelationship
         */
        public ToolBeanRelationship addComparator(Comparator<ToolBean> comparator) {
            m_comparator = comparator;
            return this;
        }

        /**
         * Adds the parent key function.
         *
         * @param function Function<ToolBean,String>
         * @return ToolBeanRelationship
         */
        public ToolBeanRelationship addParentKeyFunction(Function<ToolBean, String> function) {
            m_parentKeyFunction = function;
            return this;
        }

        /**
         * Creates the loader.
         *
         * @return Loader
         */
        public Loader createLoader() {
            Loader loader = new Loader(this, m_loaderMode);
            m_loaderMode = LoaderMode.DEFAULT;
            return loader;
        }

        /**
         * Gets the comparator.
         *
         * @return Comparator
         */
        public Comparator<ToolBean> getComparator() {
            return m_comparator;
        }

        /**
         * Gets the child class.
         *
         * @return the child class
         */
        public Class<? extends ToolBean> getChildClass() {
            // Defer lookup until first use
            if (m_childClass == null) {
                if (RelationshipType.PARENT.equals(m_relationshipType)) {
                    if (X2BaseBean.class.isAssignableFrom(getFromClass())) {
                        m_childClass = ToolBean.getRegisteredClass((Class<? extends X2BaseBean>) getFromClass());
                    } else if (ToolBean.class.isAssignableFrom(getFromClass())) {
                        m_childClass = (Class<? extends ToolBean>) getFromClass();
                    } else {
                        throw new IllegalStateException("Cannot use " + getFromClass().getName() + "in a relationship");
                    }
                } else {
                    if (X2BaseBean.class.isAssignableFrom(getToClass())) {
                        m_childClass = ToolBean.getRegisteredClass((Class<? extends X2BaseBean>) getToClass());
                    } else if (ToolBean.class.isAssignableFrom(getToClass())) {
                        m_childClass = (Class<? extends ToolBean>) getToClass();
                    } else {
                        throw new IllegalStateException("Cannot use " + getToClass().getName() + "in a relationship");
                    }
                }
            }
            return m_childClass;
        }

        /**
         * Gets the child path.
         *
         * @return String
         */
        public String getChildPath() {
            if (RelationshipType.PARENT.equals(m_relationshipType)) {
                return m_fromPath;
            }
            return m_toPath;
        }

        /**
         * Gets the parent class.
         *
         * @return the parent class
         */
        public Class<? extends ToolBean> getParentClass() {
            // Defer lookup until first use
            if (m_parentClass == null) {
                if (RelationshipType.PARENT.equals(m_relationshipType)) {
                    if (X2BaseBean.class.isAssignableFrom(getToClass())) {
                        m_parentClass = ToolBean.getRegisteredClass((Class<? extends X2BaseBean>) getToClass());
                    } else if (ToolBean.class.isAssignableFrom(getToClass())) {
                        m_parentClass = (Class<? extends ToolBean>) getToClass();
                    } else {
                        throw new IllegalStateException("Cannot use " + getToClass().getName() + "in a relationship");
                    }
                } else {
                    if (X2BaseBean.class.isAssignableFrom(getFromClass())) {
                        m_parentClass = ToolBean.getRegisteredClass((Class<? extends X2BaseBean>) getFromClass());
                    } else if (ToolBean.class.isAssignableFrom(getFromClass())) {
                        m_parentClass = (Class<? extends ToolBean>) getFromClass();
                    } else {
                        throw new IllegalStateException("Cannot use " + getFromClass().getName() + "in a relationship");
                    }
                }
            }
            return m_parentClass;
        }

        /**
         * Gets the parent key function.
         *
         * @return Function
         */
        public Function<ToolBean, String> getParentKeyFunction() {
            return m_parentKeyFunction;
        }

        /**
         * Gets the parent path.
         *
         * @return String
         */
        public String getParentPath() {
            if (RelationshipType.PARENT.equals(m_relationshipType)) {
                return m_toPath;
            }
            return m_fromPath;
        }

        /**
         * Sets the loader mode. The loader mode is transient and will be reset to default after the
         * next loader is created.
         *
         * @param mode void
         * @return ToolBeanRelationship
         */
        public ToolBeanRelationship setLoaderMode(LoaderMode mode) {
            m_loaderMode = mode;
            return this;
        }

        /**
         * Gets the from class.
         *
         * @return the from class
         */
        private Class<?> getFromClass() {
            return m_fromClass;
        }

        /**
         * Gets the to class.
         *
         * @return the to class
         */
        private Class<?> getToClass() {
            return m_toClass;
        }
    }

    /**
     * The Class ToolCourse.
     */
    public static class ToolCourse extends ToolBean {

        public static final ToolBeanColumn FIELD_CREDIT =
                new ToolBeanColumn(SisBeanPaths.COURSE.credit());
        public static final ToolBeanColumn FIELD_PARENT_COURSE_OID =
                new ToolBeanColumn(SisBeanPaths.COURSE.parentCourseOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_CREDIT,
                        FIELD_PARENT_COURSE_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.COURSE.getBeanType();
        }

        /**
         * Instantiates a new tool course.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolCourse(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the Credit.
         * <p>
         * The data dictionary ID for this property is <code>crsCredit</code>.
         *
         * @return BigDecimal
         */
        public BigDecimal getCourseCredit() {
            return this.getValueBigDecimal(FIELD_CREDIT);
        }


        /**
         * Returns the parentCourse.
         * <p>
         * The data dictionary ID for this property is <code>relCrsCrsOidpa</code>.
         *
         * @param broker X2Broker
         * @return Course
         */
        public ToolCourse getParentCourse(X2Broker broker) {
            String crsOid = getValueString(FIELD_PARENT_COURSE_OID);
            return getBeanByOid(broker, ToolCourse.class, crsOid, true);
        }

        /**
         * Returns the root course from which this course stems.
         *
         * @param broker X2Broker
         * @return Course
         */
        public ToolCourse getRootCourse(X2Broker broker) {
            ToolCourse course = this;

            if (getParentCourse(broker) != null) {
                course = getParentCourse(broker).getRootCourse(broker);
            }

            return course;
        }
    }

    /**
     * The Class SchoolYearContext.
     */
    public static class ToolDistrictContext extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_CONTEXT_ID =
                new ToolBeanColumn(SisBeanPaths.DISTRICT_SCHOOL_YEAR_CONTEXT.contextId());
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.DISTRICT_SCHOOL_YEAR_CONTEXT.endDate(), PredefinedConverter.PLAINDATE,
                        true);
        public static final ToolBeanColumn FIELD_SCHOOL_YEAR =
                new ToolBeanColumn(SisBeanPaths.DISTRICT_SCHOOL_YEAR_CONTEXT.schoolYear(),
                        PredefinedConverter.INTEGER);
        public static final ToolBeanColumn FIELD_START_DATE_DESC =
                new ToolBeanColumn(SisBeanPaths.DISTRICT_SCHOOL_YEAR_CONTEXT.startDate(), PredefinedConverter.PLAINDATE,
                        false);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_CONTEXT_ID,
                        FIELD_END_DATE,
                        FIELD_SCHOOL_YEAR,
                        FIELD_START_DATE_DESC);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.DISTRICT_SCHOOL_YEAR_CONTEXT.getBeanType();
        }

        /**
         * Instantiates a new school year context.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolDistrictContext(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the context id.
         *
         * @return the context id
         */
        public String getContextId() {
            return this.getValueString(FIELD_CONTEXT_ID);
        }

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Returns the School year.
         * <p>
         * The data dictionary ID for this property is <code>ctxSchoolYear</code>.
         *
         * @return int
         */
        public int getSchoolYear() {
            return getValueInt(FIELD_SCHOOL_YEAR);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE_DESC);
        }

    }


    /**
     * The Class Enrollment.
     */
    public static class ToolEnrollment extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_DATE_DESC =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.enrollmentDate(), PredefinedConverter.PLAINDATE,
                        false);
        public static final ToolBeanColumn FIELD_ENROLLMENT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.enrollmentCode());
        public static final ToolBeanColumn FIELD_ENROLLMENT_REASON =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.reasonCode());
        public static final ToolBeanColumn FIELD_ENROLLMENT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.enrollmentType());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.schoolOid());
        public static final ToolBeanColumn FIELD_STATUS_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.statusCode());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.studentOid());
        public static final ToolBeanColumn FIELD_TIMESTAMP_DESC =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.timestamp(), false);
        public static final ToolBeanColumn FIELD_YOG =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.yog(), PredefinedConverter.INTEGER,
                        false);

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_SCHOOL_ARCHIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.school().archiveIndicator());
        public static final ToolBeanColumn FIELD_SCHOOL_INACTIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.school().inactiveIndicator());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DATE_DESC,
                        FIELD_ENROLLMENT_CODE,
                        FIELD_ENROLLMENT_REASON,
                        FIELD_ENROLLMENT_TYPE,
                        FIELD_SCHOOL_OID,
                        FIELD_STATUS_CODE,
                        FIELD_STUDENT_OID,
                        FIELD_TIMESTAMP_DESC,
                        FIELD_YOG)
                .expandSort(FIELD_STUDENT_OID, FIELD_DATE_DESC, FIELD_TIMESTAMP_DESC);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_ENROLLMENT.getBeanType();
        }

        /**
         * Instantiates a new enrollment.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolEnrollment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the enrollment calendar code.
         *
         * @return String
         */
        public String getEnrollmentCalendarCode() {
            // If an enrollment calendar code is used, this method must be overriden in the class
            // used with the helper

            return null;
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the student oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }

        /**
         * Gets the enrollment code.
         *
         * @return String
         */
        public String getEnrollmentCode() {
            return getValueString(FIELD_ENROLLMENT_CODE);
        }

        /**
         * Gets the reason code.
         *
         * @return String
         */
        public String getReasonCode() {
            return getValueString(FIELD_ENROLLMENT_REASON);
        }

        /**
         * Gets the enrollment type.
         *
         * @return String
         */
        public String getEnrollmentType() {
            return getValueString(FIELD_ENROLLMENT_TYPE);
        }

        /**
         * Gets the enrollment date.
         *
         * @return Plain date
         */
        public PlainDate getEnrollmentDate() {
            return getValueDate(FIELD_DATE_DESC);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return School
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the status code.
         *
         * @return String
         */
        public String getStatusCode() {
            return getValueString(FIELD_STATUS_CODE);
        }

        /**
         * Returns the Time stamp.
         * <p>
         * The data dictionary ID for this property is <code>enrTimestamp</code>.
         *
         * @return long
         */
        public long getTimestamp() {
            return getValueLong(FIELD_TIMESTAMP_DESC);
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        public int getYog() {
            return getValueInt(FIELD_YOG);
        }

        /**
         * Checks if is active or withdrawal.
         *
         * @param broker X2Broker
         * @return true, if is active or withdrawal
         */
        public boolean isActive(X2Broker broker) {
            return ToolBean.DistrictManager.isActiveStatus(broker, getStatusCode());
        }

        /**
         * Checks if is active or withdrawal.
         *
         * @param broker X2Broker
         * @return true, if is active or withdrawal
         */
        public boolean isActiveOrWithdrawal(X2Broker broker) {
            return WITHDRAWAL.equals(getEnrollmentType()) || isActive(broker);
        }

        /**
         * Checks if is active non withdrawal.
         *
         * @param broker X2Broker
         * @return Object
         */
        public boolean isActiveNonWithdrawal(X2Broker broker) {
            return !WITHDRAWAL.equals(getEnrollmentType()) && isActive(broker);
        }

        /**
         * Ignore for spans.
         *
         * @return true, if successful
         */
        public boolean ignoreForSpans() {
            Predicate<ToolEnrollment> tester =
                    (Predicate<ToolEnrollment>) ToolBean.getPreference(ToolBean.PREFERENCE_ENR_IGNORE_FOR_SPANS);
            return tester == null ? false : tester.test(this);
        }
    }

    /**
     * The Class ToolGradeTermDate.
     */
    public static class ToolGradeTermDate extends ToolBean {
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.districtContextOid());
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.endDate(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_GRADE_TERM_DEFINITION_NAME =
                new ToolBeanColumn(
                        SisBeanPaths.GRADE_TERM_DATE.gradeTerm().gradeTermDefinition().gradeTermDefinitionName());
        public static final ToolBeanColumn FIELD_GRADE_TERM_DEFINITION_OID =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.gradeTerm().gradeTermDefinitionOid());
        public static final ToolBeanColumn FIELD_GRADE_TERM_ID =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.gradeTerm().gradeTermId());
        public static final ToolBeanColumn FIELD_GRADE_TERM_NUM =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.gradeTerm().gradeTermNum());
        public static final ToolBeanColumn FIELD_GRADE_TERM_OID =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.gradeTermOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.GRADE_TERM_DATE.startDate(), PredefinedConverter.PLAINDATE, true);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_END_DATE,
                        FIELD_GRADE_TERM_DEFINITION_NAME,
                        FIELD_GRADE_TERM_DEFINITION_OID,
                        FIELD_GRADE_TERM_NUM,
                        FIELD_GRADE_TERM_OID,
                        FIELD_GRADE_TERM_ID,
                        FIELD_START_DATE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADE_TERM_DATE.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolGradeTermDate(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the grade term definition name.
         *
         * @return the grade term definition name
         */
        public String getGradeTermDefinitionName() {
            return getValueString(FIELD_GRADE_TERM_DEFINITION_NAME);
        }

        /**
         * Gets the grade term oid.
         *
         * @return String
         */
        public String getGradeTermDefinitionOid() {
            return getValueString(FIELD_GRADE_TERM_DEFINITION_OID);
        }

        /**
         * Gets the grade term id.
         *
         * @return the grade term id
         */
        public String getGradeTermId() {
            return getValueString(FIELD_GRADE_TERM_ID);
        }

        /**
         * Returns the Term number.
         * <p>
         * The data dictionary ID for this property is <code>gtmGrdTermNum</code>.
         *
         * @return int
         */
        public int getGradeTermNum() {
            return getValueInt(FIELD_GRADE_TERM_NUM);
        }

        /**
         * Gets the grade term oid.
         *
         * @return the grade term oid
         */
        public String getGradeTermOid() {
            return getValueString(FIELD_GRADE_TERM_OID);
        }

        /**
         * Gets the range.
         *
         * @return Range
         */
        public Range<Date> getRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }
    }

    /**
     * The Class ToolMasterTerm.
     */
    public static class ToolMasterTerm extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_SCHEDULE_DAY_OID =
                new ToolBeanColumn(
                        SisBeanPaths.SCHEDULE_MASTER_TERM.masterMatrices().scheduleMatrix().scheduleDayOid());
        public static final ToolBeanColumn FIELD_SCHEDULE_PERIOD_OID =
                new ToolBeanColumn(
                        SisBeanPaths.SCHEDULE_MASTER_TERM.masterMatrices().scheduleMatrix().schedulePeriodOid());
        public static final ToolBeanColumn FIELD_SCHEDULE_TERM_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TERM.scheduleTermOid());
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TERM.masterScheduleOid());

        public static ToolBeanRelationship PARENT_SECTION =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER_TERM.masterSchedule().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER_TERM.masterSchedule().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER_TERM.masterScheduleOid().toString(),
                        SisBeanPaths.SCHEDULE_MASTER.masterTerms().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER_TERM.masterSchedule().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = new ToolBeanDefinition()
                .expandKeys(ToolBean.FIELD_OID.addNonUnique())
                .expand(FIELD_SCHEDULE_DAY_OID,
                        FIELD_SCHEDULE_PERIOD_OID,
                        FIELD_SCHEDULE_TERM_OID,
                        FIELD_SECTION_OID)
                .expandRelationships(PARENT_SECTION);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_MASTER_TERM.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolMasterTerm(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the schedule day oid.
         *
         * @return String
         */
        public String getScheduleDayOid() {
            return this.getValueString(FIELD_SCHEDULE_DAY_OID);
        }

        /**
         * Gets the schedule period oid.
         *
         * @return String
         */
        public String getSchedulePeriodOid() {
            return this.getValueString(FIELD_SCHEDULE_PERIOD_OID);
        }

        /**
         * Gets the schedule term oid.
         *
         * @return String
         */
        public String getScheduleTermOid() {
            return this.getValueString(FIELD_SCHEDULE_TERM_OID);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSectionOid() {
            return this.getValueString(FIELD_SECTION_OID);
        }

        /**
         * Gets the schedule period.
         *
         * @param broker X2Broker
         * @return Tool schedule period
         */
        public ToolSchedulePeriod getSchedulePeriod(X2Broker broker) {
            String trmOid = getValueString(FIELD_SCHEDULE_PERIOD_OID);
            return getBeanByOid(broker, ToolSchedulePeriod.class, trmOid, true);
        }

        /**
         * Gets the schedule term.
         *
         * @param broker X2Broker
         * @return Tool schedule term
         */
        public ToolScheduleTerm getScheduleTerm(X2Broker broker) {
            String trmOid = getValueString(FIELD_SCHEDULE_TERM_OID);
            return getBeanByOid(broker, ToolScheduleTerm.class, trmOid, true);
        }


    }

    /**
     * The Class ToolOrganization.
     */
    public static class ToolOrganization extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_CURRENT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION.currentContextOid());
        public static final ToolBeanColumn FIELD_ID =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION.id());
        public static final ToolBeanColumn FIELD_NAME =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION.name());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_CURRENT_CONTEXT_OID,
                        FIELD_ID,
                        FIELD_NAME);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.ORGANIZATION.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolOrganization(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }


        private List<ToolDistrictContext> m_descendingContexts = null;

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolDistrictContext getCurrentContext(X2Broker broker) {
            String ctxOid = getValueString(FIELD_CURRENT_CONTEXT_OID);
            return getBeanByOid(broker, ToolDistrictContext.class, ctxOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getCurrentContextOid() {
            return getValueString(FIELD_CURRENT_CONTEXT_OID);
        }

        /**
         * Gets the descending contexts.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolDistrictContext> getDescendingContexts(X2Broker broker) {
            if (m_descendingContexts == null) {
                m_descendingContexts = new ArrayList();
                Class<? extends ToolDistrictContext> classCTX =
                        ToolBean.getRegisteredClass(DistrictSchoolYearContext.class.getName(),
                                ToolDistrictContext.class);
                Filterable<? extends ToolDistrictContext> contexts =
                        FilterableFactory.create(broker, ToolBean.getDictionaryExtractor(), classCTX, new X2Criteria(),
                                Arrays.asList(ToolDistrictContext.FIELD_START_DATE_DESC));

                PlainDate historicalCheckDate = DateUtils.add(new PlainDate(), Calendar.YEAR, -13);

                // Populate internal map
                PlainDate nextYearStartDate = null;
                for (ToolDistrictContext districtSchoolYearContext : contexts.extract()) {
                    PlainDate startDate = districtSchoolYearContext.getStartDate();

                    /*
                     * Reasonableness check that context start dates are within 14 months of each
                     * other
                     * (only apply to last 12 years)
                     */
                    if (startDate.after(historicalCheckDate)) {
                        PlainDate startDatePlusYear = DateUtils.add(startDate, Calendar.YEAR, 1);
                        PlainDate startDatePlusYearLeeway = DateUtils.add(startDatePlusYear, Calendar.MONTH, 2);
                        if (nextYearStartDate != null && nextYearStartDate.after(startDatePlusYearLeeway)) {
                            throw new RuntimeException("Context year start dates are too far apart: "
                                    + startDate + ", " + nextYearStartDate);
                        }
                    }
                    m_descendingContexts.add(districtSchoolYearContext);
                    nextYearStartDate = startDate;
                }
            }
            return m_descendingContexts;
        }

        /**
         * Gets the id.
         *
         * @return the id
         */
        public String getId() {
            return getValueString(FIELD_ID);
        }

        /**
         * Gets the name.
         *
         * @return the name
         */
        public String getName() {
            return getValueString(FIELD_NAME);
        }

        /**
         * Gets the school year context.
         *
         * @param broker X2Broker
         * @param whenDate PlainDate
         * @return Tool district context
         */
        public ToolDistrictContext getSchoolYearContext(X2Broker broker, PlainDate whenDate) {
            if (whenDate == null) {
                return null;
            }
            /*
             * Walking backward in time, return the first context with StartDate on/before WhenDate
             */
            for (ToolDistrictContext schoolYearRange : getDescendingContexts(broker)) {
                // Skip future year
                if (schoolYearRange.getStartDate().after(whenDate)) {
                    continue;
                }

                // Reasonableness check
                PlainDate startDate = schoolYearRange.getStartDate();
                PlainDate startDatePlusYear = DateUtils.add(startDate, Calendar.YEAR, 1);
                PlainDate startDatePlusYearLeeway = DateUtils.add(startDatePlusYear, Calendar.MONTH, 2);
                if (whenDate.after(startDatePlusYearLeeway)) {
                    throw new RuntimeException("Unable to determine District Context Year for " + whenDate);
                }

                return schoolYearRange;
            }
            return null;
        }
    }

    /**
     * The Class ToolPersonAddress.
     */
    public static class ToolPersonAddress extends ToolBean {
        public static final String ADDRESS_TYPE_PHYSICAL = "Physical";

        // Query Fields
        public static final ToolBeanColumn FIELD_ADDRESS_OID =
                new ToolBeanColumn(SisBeanPaths.PERSON_TO_ADDRESS.addressOid());
        public static final ToolBeanColumn FIELD_ADDRESS_TYPE =
                new ToolBeanColumn(SisBeanPaths.PERSON_TO_ADDRESS.addressType());
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.PERSON_TO_ADDRESS.endDate(), PredefinedConverter.PLAINDATE, false);
        public static final ToolBeanColumn FIELD_PERSON_OID =
                new ToolBeanColumn(SisBeanPaths.PERSON_TO_ADDRESS.personOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.PERSON_TO_ADDRESS.startDate(), PredefinedConverter.PLAINDATE, false);
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ADDRESS_OID,
                        FIELD_ADDRESS_TYPE,
                        FIELD_END_DATE,
                        FIELD_PERSON_OID,
                        FIELD_START_DATE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.PERSON_TO_ADDRESS.getBeanType();
        }

        /**
         * Instantiates a new person address.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolPersonAddress(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the address.
         *
         * @param broker X2Broker
         * @return String
         */
        public ToolAddress getAddress(X2Broker broker) {
            String adrOid = getValueString(FIELD_ADDRESS_OID);
            return getBeanByOid(broker, ToolAddress.class, adrOid, true);
        }

        /**
         * Gets the address oid.
         *
         * @return String
         */
        public String getAddressOid() {
            return getValueString(FIELD_ADDRESS_OID);
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }
    }

    /**
     * The Class ToolRace.
     */
    public static class ToolRace extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_PERSON_OID =
                new ToolBeanColumn(SisBeanPaths.PERSON_RACE.personOid());
        public static final ToolBeanColumn FIELD_RACE_CODE =
                new ToolBeanColumn(SisBeanPaths.PERSON_RACE.raceCode());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_PERSON_OID,
                        FIELD_RACE_CODE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.PERSON_RACE.getBeanType();
        }

        /**
         * Instantiates a new tool race.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolRace(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
            // TODO Auto-generated constructor stub
        }

        /**
         * Gets the race code.
         *
         * @return String
         */
        public String getRaceCode() {
            return getValueString(FIELD_RACE_CODE);
        }

        /**
         * Gets the person oid.
         *
         * @return the person oid
         */
        public String getPersonOid() {
            return getValueString(FIELD_PERSON_OID);
        }
    }

    /**
     * The Class ToolRubricAssessment.
     */
    public static class ToolRubricAssessment extends ToolBean {

        public static ToolBeanRelationship CHILD_RUBRIC_ASSESSMENT_PERFORMANCES =
                new ToolBeanRelationship(SisBeanPaths.RUBRIC_ASSESSMENT.assessmentPerformances().getBeanType(),
                        SisBeanPaths.RUBRIC_ASSESSMENT.assessmentPerformances().getValueType(),
                        SisBeanPaths.RUBRIC_ASSESSMENT.assessmentPerformances().getPath(),
                        SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.rubricAssessmentOid().getPath(),
                        SisBeanPaths.RUBRIC_ASSESSMENT.assessmentPerformances().getRelationshipType());
        public static ToolBeanRelationship CHILD_TRANSCRIPT_RUBRICS =
                new ToolBeanRelationship(SisBeanPaths.RUBRIC_ASSESSMENT.transcriptRubrics().getBeanType(),
                        SisBeanPaths.RUBRIC_ASSESSMENT.transcriptRubrics().getValueType(),
                        SisBeanPaths.RUBRIC_ASSESSMENT.transcriptRubrics().getPath(),
                        SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.rubricAssessmentOid().getPath(),
                        SisBeanPaths.RUBRIC_ASSESSMENT.transcriptRubrics().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expandRelationships(CHILD_RUBRIC_ASSESSMENT_PERFORMANCES,
                        CHILD_TRANSCRIPT_RUBRICS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.RUBRIC_ASSESSMENT.getBeanType();
        }

        /**
         * Instantiates a new tool rubric assessment.
         *
         * @param columns the columns
         * @param data the data
         */
        public ToolRubricAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the rubric assessment performances.
         *
         * @param broker the broker
         * @return the rubric assessment performances
         */
        public List<ToolRubricAssessmentPerformance> getRubricAssessmentPerformances(X2Broker broker) {
            return (List<ToolRubricAssessmentPerformance>) getChildren(broker, CHILD_RUBRIC_ASSESSMENT_PERFORMANCES);
        }

        /**
         * Gets the transcript rubrics.
         *
         * @param broker the broker
         * @return the transcript rubrics
         */
        public List<ToolTranscriptRubric> getTranscriptRubrics(X2Broker broker) {
            return (List<ToolTranscriptRubric>) getChildren(broker, CHILD_TRANSCRIPT_RUBRICS);
        }


    }

    /**
     * The Class ToolRubricAssessmentPerformance.
     */
    public static class ToolRubricAssessmentPerformance extends ToolBean {
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.rubricAssessment().transcriptRubrics()
                        .transcript().districtContextOid());
        public static final ToolBeanColumn FIELD_ID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.id());
        public static final ToolBeanColumn FIELD_RUBRIC_ASSESSMENT_OID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.rubricAssessmentOid());
        public static final ToolBeanColumn FIELD_RUBRIC_CRITERION_OID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.rubricCriterionOid());
        public static final ToolBeanColumn FIELD_RUBRIC_DEFINITION_OID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_CRITERION.rubricDefinitionOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.rubricAssessment().transcriptRubrics()
                        .transcript().schoolOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_ID,
                        FIELD_RUBRIC_ASSESSMENT_OID,
                        FIELD_RUBRIC_CRITERION_OID,
                        FIELD_RUBRIC_DEFINITION_OID,
                        FIELD_SCHOOL_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.getBeanType();
        }

        /**
         * Instantiates a new tool rubric assessment performance.
         *
         * @param columns the columns
         * @param data the data
         */
        public ToolRubricAssessmentPerformance(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the rubric assessment.
         *
         * @param broker the broker
         * @return the rubric assessment
         */
        public ToolRubricAssessment getRubricAssessment(X2Broker broker) {
            String rbaOid = getValueString(FIELD_RUBRIC_ASSESSMENT_OID);
            return getBeanByOid(broker, ToolRubricAssessment.class, rbaOid, true);
        }

        /**
         * Gets the id.
         *
         * @return the id
         */
        public String getId() {
            return getValueString(FIELD_ID);
        }

        /**
         * Gets the rubric assessment oid.
         *
         * @return the rubric assessment oid
         */
        public String getRubricAssessmentOid() {
            return getValueString(FIELD_RUBRIC_ASSESSMENT_OID);
        }

        /**
         * Gets the rubric criterion.
         *
         * @param broker the broker
         * @return the rubric criterion
         */
        public ToolRubricCriterion getRubricCriterion(X2Broker broker) {
            String rbcOid = getValueString(FIELD_RUBRIC_CRITERION_OID);
            return getBeanByOid(broker, ToolRubricCriterion.class, rbcOid, true);
        }

        /**
         * Gets the rubric criterion oid.
         *
         * @return the rubric criterion oid
         */
        public String getRubricCriterionOid() {
            return getValueString(FIELD_RUBRIC_CRITERION_OID);
        }

        /**
         * Gets the rubric definition oid.
         *
         * @return the rubric definition oid
         */
        public String getRubricDefinitionOid() {
            return getValueString(FIELD_RUBRIC_DEFINITION_OID);
        }

    }

    /**
     * The Class ToolRubricCriterion.
     */
    public static class ToolRubricCriterion extends ToolBean {
        public static final ToolBeanColumn FIELD_COLUMN_HEADER =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_CRITERION.columnHeader());
        public static final ToolBeanColumn FIELD_RUBRIC_CRITERION_OID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_CRITERION.rubricCriterionOid());
        public static final ToolBeanColumn FIELD_RUBRIC_DEFINITION_OID =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_CRITERION.rubricDefinitionOid());

        public static ToolBeanRelationship CHILD_RUBRIC_CRITERIAS =
                new ToolBeanRelationship(SisBeanPaths.RUBRIC_CRITERION.rubricCriteria().getBeanType(),
                        SisBeanPaths.RUBRIC_CRITERION.rubricCriteria().getValueType(),
                        SisBeanPaths.RUBRIC_CRITERION.rubricCriteria().getPath(),
                        SisBeanPaths.RUBRIC_CRITERION.rubricCriterionOid().toString(),
                        SisBeanPaths.RUBRIC_CRITERION.rubricCriteria().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_COLUMN_HEADER,
                        FIELD_RUBRIC_CRITERION_OID,
                        FIELD_RUBRIC_DEFINITION_OID)
                .expandRelationships(CHILD_RUBRIC_CRITERIAS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.RUBRIC_CRITERION.getBeanType();
        }

        /**
         * Instantiates a new tool rubric criterion.
         *
         * @param columns the columns
         * @param data the data
         */
        public ToolRubricCriterion(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the rubric definition.
         *
         * @param broker the broker
         * @return the rubric definition
         */
        public ToolRubricDefinition getRubricDefinition(X2Broker broker) {
            String rbdOid = getValueString(FIELD_RUBRIC_DEFINITION_OID);
            return getBeanByOid(broker, ToolRubricDefinition.class, rbdOid, true);
        }

        /**
         * Gets the rubric definition oid.
         *
         * @return the rubric definition oid
         */
        public String getRubricDefinitionOid() {
            return getValueString(FIELD_RUBRIC_DEFINITION_OID);
        }

        /**
         * Gets the column header.
         *
         * @return the column header
         */
        public String getColumnHeader() {
            return getValueString(FIELD_COLUMN_HEADER);
        }

        /**
         * Returns the root criterion. If this criterion is the root, it is returned.
         *
         * @param broker the broker
         * @return RubricCriterion
         */
        public ToolRubricCriterion getRootCriterion(X2Broker broker) {
            ToolRubricCriterion criterion = this;

            while (criterion.getRubricCriterion(broker) != null) {
                criterion = criterion.getRubricCriterion(broker);
            }

            return criterion;
        }

        /**
         * Returns the rubricCriteria.
         * <p>
         * The data dictionary ID for this property is <code>relRbcRbcOid1</code>.
         *
         * @param broker the broker
         * @return Collection of RubricCriterion objects
         */
        public List<ToolRubricCriterion> getRubricCriteria(X2Broker broker) {
            return (List<ToolRubricCriterion>) getChildren(broker, CHILD_RUBRIC_CRITERIAS);
        }

        /**
         * Returns the rubricCriterion.
         * <p>
         * The data dictionary ID for this property is <code>relRbcRbcOid</code>.
         *
         * @param broker the broker
         * @return RubricCriterion
         */
        public ToolRubricCriterion getRubricCriterion(X2Broker broker) {
            String rbcOid = getValueString(FIELD_RUBRIC_CRITERION_OID);
            return getBeanByOid(broker, ToolRubricCriterion.class, rbcOid, true);
        }

    }

    /**
     * The Class ToolRubricDefinition.
     */
    public static class ToolRubricDefinition extends ToolBean {
        public static final ToolBeanColumn FIELD_SUBJECT_CODE =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_DEFINITION.subjectCode());

        public static ToolBeanRelationship CHILD_RUBRIC_CRITERIA =
                new ToolBeanRelationship(SisBeanPaths.RUBRIC_DEFINITION.rubricCriteria().getBeanType(),
                        SisBeanPaths.RUBRIC_DEFINITION.rubricCriteria().getValueType(),
                        SisBeanPaths.RUBRIC_DEFINITION.rubricCriteria().getPath(),
                        SisBeanPaths.RUBRIC_CRITERION.rubricDefinitionOid().getPath(),
                        SisBeanPaths.RUBRIC_DEFINITION.rubricCriteria().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_SUBJECT_CODE)
                .expandRelationships(CHILD_RUBRIC_CRITERIA);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.RUBRIC_DEFINITION.getBeanType();
        }

        /**
         * Instantiates a new tool rubric definition.
         *
         * @param columns the columns
         * @param data the data
         */
        public ToolRubricDefinition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the rubricCriteria.
         * <p>
         * The data dictionary ID for this property is <code>relRbdRbcOid</code>.
         *
         * @param broker the broker
         * @return Collection of RubricCriterion objects
         */
        public List<ToolRubricCriterion> getRubricCriteria(X2Broker broker) {
            return (List<ToolRubricCriterion>) getChildren(broker, CHILD_RUBRIC_CRITERIA);
        }

        /**
         * Gets the subject code.
         *
         * @return the subject code
         */
        public String getSubjectCode() {
            return getValueReferenceState(FIELD_SUBJECT_CODE);
        }

    }

    /**
     * The Class ToolStudentSchedule.
     */
    public static class ToolSchedule extends ToolBean {
        // Query Fields
        /*
         * The column FIELD_ACTIVE_SCHEDULE_OID generates an inner join and limits
         * the ToolSchedule to active schedules only
         */
        public static final ToolBeanColumn FIELD_ACTIVE_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE.activeSchoolScheduleContexts().oid());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE.districtContextOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE.schoolOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE.startDate(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE.endDate(), PredefinedConverter.PLAINDATE, true);

        public static ToolBeanRelationship CHILD_SCHEDULE_BELL_PERIODS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE.scheduleBells().scheduleBellPeriods().getBeanType(),
                        SisBeanPaths.SCHEDULE.scheduleBells().scheduleBellPeriods().getValueType(),
                        SisBeanPaths.SCHEDULE.scheduleBells().scheduleBellPeriods().getPath(),
                        SisBeanPaths.SCHEDULE_BELL_PERIOD.bellSchedule().scheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE.scheduleBells().scheduleBellPeriods().getRelationshipType());
        public static ToolBeanRelationship CHILD_SCHEDULE_BELLS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE.scheduleBells().getBeanType(),
                        SisBeanPaths.SCHEDULE.scheduleBells().getValueType(),
                        SisBeanPaths.SCHEDULE.scheduleBells().getPath(),
                        SisBeanPaths.SCHEDULE_BELL.scheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE.scheduleBells().getRelationshipType());
        public static ToolBeanRelationship CHILD_SCHEDULE_DAYS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE.scheduleDays().getBeanType(),
                        SisBeanPaths.SCHEDULE.scheduleDays().getValueType(),
                        SisBeanPaths.SCHEDULE.scheduleDays().getPath(),
                        SisBeanPaths.SCHEDULE_DAY.scheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE.scheduleDays().getRelationshipType())
                                .addComparator(new Comparator<ToolBean>() {
                                    @Override
                                    public int compare(ToolBean o1, ToolBean o2) {
                                        ToolScheduleDay day1, day2;
                                        day1 = (ToolScheduleDay) o1;
                                        day2 = (ToolScheduleDay) o2;
                                        return day1.getNumber() - day2.getNumber();
                                    }
                                });
        public static ToolBeanRelationship CHILD_SCHEDULE_TERMS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE.scheduleTerms().getBeanType(),
                        SisBeanPaths.SCHEDULE.scheduleTerms().getValueType(),
                        SisBeanPaths.SCHEDULE.scheduleTerms().getPath(),
                        SisBeanPaths.SCHEDULE_DAY.scheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE.scheduleTerms().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ACTIVE_SCHEDULE_OID,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_SCHOOL_OID,
                        FIELD_START_DATE,
                        FIELD_END_DATE)
                .expandRelationships(CHILD_SCHEDULE_BELL_PERIODS,
                        CHILD_SCHEDULE_BELLS,
                        CHILD_SCHEDULE_DAYS,
                        CHILD_SCHEDULE_TERMS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE.getBeanType();
        }

        private ToolSchoolCalendar m_calendar;

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolSchedule(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the calendar.
         *
         * @param broker X2Broker
         * @return Tool school calendar
         */
        public ToolSchoolCalendar getCalendar(X2Broker broker) {
            if (m_calendar == null) {
                Schedule schedule = broker.getBeanByOid(Schedule.class, getOid());
                String calendarCode = DistrictManager.getScheduleManager(broker).getMostCommonCalendar(schedule, null);

                List<ToolSchool> schoolList = Arrays.asList(getSchool(broker));
                ToolDistrictContext context = getDistrictContext(broker);
                m_calendar = ToolSchoolCalendar.findCalendarByCode(broker, schoolList, context, calendarCode);

                if (m_calendar == null) {
                    m_calendar = ToolSchoolCalendar.findDefaultCalendar(broker, schoolList, context);
                }

                // 4. find any calendar
                if (m_calendar == null) {
                    m_calendar = ToolSchoolCalendar.findAnyCalendar(broker, schoolList, context);
                }
                if (m_calendar == null) {
                    throw new RuntimeException("Calendar cannot be found for schedule " + this.toString());
                }
            }
            return m_calendar;
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolDistrictContext getDistrictContext(X2Broker broker) {
            String ctxOid = getValueString(FIELD_DISTRICT_CONTEXT_OID);
            return getBeanByOid(broker, ToolDistrictContext.class, ctxOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getDistrictContextOid() {
            return getValueString(FIELD_DISTRICT_CONTEXT_OID);
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the schedule bell period matching by period id with the largest time interval among
         * default bell time bell periods.
         *
         * @param broker X2Broker
         * @param perId String
         * @param bellScheduleOids Set<String>
         * @return Optional
         */
        public ToolScheduleBellPeriod getScheduleBellPeriod(X2Broker broker,
                                                            String perId,
                                                            Set<String> bellScheduleOids) {
            return getScheduleBellPeriods(broker).stream().filter(bpe -> perId.equals(bpe.getId()))
                    .filter(bpe -> bellScheduleOids.isEmpty() || bellScheduleOids.contains(bpe.getBellScheduleOid()))
                    .collect(Collectors.maxBy(new Comparator<ToolScheduleBellPeriod>() {

                        @Override
                        public int compare(ToolScheduleBellPeriod bpe1, ToolScheduleBellPeriod bpe2) {
                            return ((int) (bpe1.getEndTime().getTimeInMinutes()
                                    - bpe1.getStartTime().getTimeInMinutes()))
                                    - ((int) (bpe2.getEndTime().getTimeInMinutes()
                                            - bpe2.getStartTime().getTimeInMinutes()));
                        }
                    })).orElse(null);
        }

        /**
         * Gets the schedule bell periods.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolScheduleBellPeriod> getScheduleBellPeriods(X2Broker broker) {
            return (List<ToolScheduleBellPeriod>) getChildren(broker, CHILD_SCHEDULE_BELL_PERIODS);
        }

        /**
         * Gets the schedule bell periods.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolScheduleBell> getScheduleBells(X2Broker broker) {
            return (List<ToolScheduleBell>) getChildren(broker, CHILD_SCHEDULE_BELLS);
        }

        /**
         * Gets the schedule days.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolScheduleDay> getScheduleDays(X2Broker broker) {
            return (List<ToolScheduleDay>) getChildren(broker, CHILD_SCHEDULE_DAYS);
        }

        /**
         * Gets the schedule terms.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolScheduleTerm> getScheduleTerms(X2Broker broker) {
            return (List<ToolScheduleTerm>) getChildren(broker, CHILD_SCHEDULE_TERMS);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }
    }

    /**
     * The Class ToolScheduleBell.
     */
    public static class ToolScheduleBell extends ToolBean {

        public static final ToolBeanColumn FIELD_DAYS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL.days());
        public static final ToolBeanColumn FIELD_ID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL.id());
        public static final ToolBeanColumn FIELD_NAME =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL.name());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DAYS,
                        FIELD_ID,
                        FIELD_NAME);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_BELL.getBeanType();
        }

        /**
         * Instantiates a new tool schedule bell .
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolScheduleBell(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the Number of days.
         * <p>
         * The data dictionary ID for this property is <code>belDays</code>.
         *
         * @return int
         */
        public int getDays() {
            return this.getValueInt(FIELD_DAYS);
        }

        /**
         * Returns the Bell Identifier.
         *
         * @return String
         */
        public String getId() {
            return getValueString(FIELD_ID);
        }

        /**
         * Returns the Bell Name.
         *
         * @return String
         */
        public String getName() {
            return getValueString(FIELD_NAME);
        }

    }

    /**
     * The Class ToolScheduleBellPeriod.
     */
    public static class ToolScheduleBellPeriod extends ToolBean {
        public static final ToolBeanColumn FIELD_BELL_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL_PERIOD.bellScheduleOid());
        public static final ToolBeanColumn FIELD_END_TIME =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL_PERIOD.endTime(), PredefinedConverter.PLAINTIME);
        public static final ToolBeanColumn FIELD_ID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL_PERIOD.id());
        public static final ToolBeanColumn FIELD_START_TIME =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL_PERIOD.startTime(), PredefinedConverter.PLAINTIME);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_BELL_SCHEDULE_OID,
                        FIELD_END_TIME,
                        FIELD_ID,
                        FIELD_START_TIME);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_BELL_PERIOD.getBeanType();
        }

        /**
         * Instantiates a new tool schedule bell period.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolScheduleBellPeriod(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the student.
         * <p>
         * The data dictionary ID for this property is <code>relTrnStdOid</code>.
         *
         * @param broker X2Broker
         * @return SisStudent
         */
        public ToolScheduleBell getBellSchedule(X2Broker broker) {
            String oid = getValueString(FIELD_BELL_SCHEDULE_OID);
            return getBeanByOid(broker, ToolScheduleBell.class, oid, true);
        }

        /**
         * Returns the Related bell schedule bject id.
         * <p>
         * The data dictionary ID for this property is <code>bpeBelOID</code>.
         *
         * @return String
         */
        public String getBellScheduleOid() {
            return getValueString(FIELD_BELL_SCHEDULE_OID);
        }

        /**
         * Returns the End time.
         * <p>
         * The data dictionary ID for this property is <code>bpeEndTime</code>.
         *
         * @return PlainTime
         */
        public PlainTime getEndTime() {
            return getValueTime(FIELD_END_TIME);
        }

        /**
         * Returns the Bell period Identifier.
         * <p>
         * The data dictionary ID for this property is <code>bpePeriodID</code>.
         *
         * @return String
         */
        public String getId() {
            return getValueString(FIELD_ID);
        }

        /**
         * Returns the Start time.
         * <p>
         * The data dictionary ID for this property is <code>bpeStartTime</code>.
         *
         * @return PlainTime
         */
        public PlainTime getStartTime() {
            return getValueTime(FIELD_START_TIME);
        }

    }

    /**
     * The Class ToolScheduleClass.
     */
    public static class ToolScheduleClass extends ToolBean {
        public static final ToolBeanColumn FIELD_ID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_CLASS.id());
        public static final ToolBeanColumn FIELD_PRIMARY_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_CLASS.primarySectionOid());

        public static ToolBeanRelationship CHILD_SECTIONS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_CLASS.sections().getBeanType(),
                        SisBeanPaths.SCHEDULE_CLASS.sections().getValueType(),
                        SisBeanPaths.SCHEDULE_CLASS.sections().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.sectionClassOid().getPath(),
                        SisBeanPaths.SCHEDULE_CLASS.sections().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ID,
                        FIELD_PRIMARY_SECTION_OID)
                .expandRelationships(CHILD_SECTIONS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_CLASS.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolScheduleClass(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the id.
         *
         * @return String
         */
        public String getId() {
            return getValueString(FIELD_ID);
        }

        /**
         * Gets the primary section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSection getPrimarySection(X2Broker broker) {
            String mstOid = getValueString(FIELD_PRIMARY_SECTION_OID);
            return getBeanByOid(broker, ToolSection.class, mstOid, true);
        }

        /**
         * Gets the primary section oid.
         *
         * @return String
         */
        public String getPrimarySectionOid() {
            return getValueString(FIELD_PRIMARY_SECTION_OID);
        }

        /**
         * Gets the sections.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolSection> getSections(X2Broker broker) {
            return (List<ToolSection>) getChildren(broker, CHILD_SECTIONS);
        }
    }


    /**
     * The Class ToolScheduleTerm.
     */
    public static class ToolScheduleDay extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_DAY.number());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_NUMBER);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_DAY.getBeanType();
        }

        /**
         * Instantiates a new tool schedule day.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolScheduleDay(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
            // TODO Auto-generated constructor stub
        }

        /**
         * Returns the Number.
         * <p>
         * The data dictionary ID for this property is <code>dayNumber</code>.
         *
         * @return int
         */
        public int getNumber() {
            return this.getValueInt(FIELD_NUMBER);
        }

    }

    /**
     * The Class ToolScheduleTerm.
     */
    public static class ToolSchedulePeriod extends ToolBean {
        public static final ToolBeanColumn FIELD_ID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_PERIOD.id());
        public static final ToolBeanColumn FIELD_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_PERIOD.number());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ID,
                        FIELD_NUMBER);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_PERIOD.getBeanType();
        }

        /**
         * Instantiates a new tool schedule period.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolSchedulePeriod(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the Identifier.
         * <p>
         * The data dictionary ID for this property is <code>perID</code>.
         *
         * @return String
         */
        public String getId() {
            return getValueString(FIELD_ID);
        }

        /**
         * Returns the Number.
         * <p>
         * The data dictionary ID for this property is <code>perNumber</code>.
         *
         * @return int
         */
        public int getNumber() {
            return getValueInt(FIELD_NUMBER);
        }

    }

    /**
     * The Class ToolScheduleTeacher.
     */
    public static class ToolScheduleTeacher extends ToolBean {
        public static final ToolBeanColumn FIELD_PRIMARY_TEACHER_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.primaryTeacherIndicator());
        public static final ToolBeanColumn FIELD_ROLE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.role());
        public static final ToolBeanColumn FIELD_SCHEDULE_TERM_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.scheduleTermOid());
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.sectionOid());
        public static final ToolBeanColumn FIELD_STAFF_NAME_VIEW =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.staff().nameView());
        public static final ToolBeanColumn FIELD_STAFF_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.staffOid());

        public static ToolBeanRelationship PARENT_SECTION =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER_TEACHER.section().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.section().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.sectionOid().toString(),
                        SisBeanPaths.SCHEDULE_MASTER.teacherSections().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.section().getRelationshipType());
        public static ToolBeanRelationship PARENT_STAFF =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER_TEACHER.staff().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.staff().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.staffOid().toString(),
                        SisBeanPaths.STAFF.scheduleTeachers().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.staff().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_PRIMARY_TEACHER_INDICATOR,
                        FIELD_ROLE,
                        FIELD_SCHEDULE_TERM_OID,
                        FIELD_SECTION_OID,
                        FIELD_STAFF_NAME_VIEW,
                        FIELD_STAFF_OID)
                .expandRelationships(PARENT_SECTION,
                        PARENT_STAFF);


        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_MASTER_TEACHER.getBeanType();
        }

        /**
         * Instantiates a new tool schedule teacher.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolScheduleTeacher(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the primary teacher indicator.
         *
         * @return boolean
         */
        public boolean getPrimaryTeacherIndicator() {
            return getValueLogical(FIELD_PRIMARY_TEACHER_INDICATOR);
        }

        /**
         * Gets the role.
         *
         * @return String
         */
        public String getRole() {
            return getValueString(FIELD_ROLE);
        }

        /**
         * Gets the schedule term.
         *
         * @param broker X2Broker
         * @return Tool schedule term
         */
        public ToolScheduleTerm getScheduleTerm(X2Broker broker) {
            String trmOid = getValueString(FIELD_SCHEDULE_TERM_OID);
            return StringUtils.isEmpty(trmOid) ? null : getBeanByOid(broker, ToolScheduleTerm.class, trmOid, true);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSection getSection(X2Broker broker) {
            String mstOid = getValueString(FIELD_SECTION_OID);
            return StringUtils.isEmpty(mstOid) ? null : getBeanByOid(broker, ToolSection.class, mstOid, true);
        }

        /**
         * Gets the staff.
         *
         * @param broker X2Broker
         * @return Tool staff
         */
        public ToolStaff getStaff(X2Broker broker) {
            String stfOid = getValueString(FIELD_STAFF_OID);
            return getBeanByOid(broker, ToolStaff.class, stfOid, true);
        }

        /**
         * Gets the staff name view.
         *
         * @return String
         */
        public String getStaffNameView() {
            return getValueString(FIELD_STAFF_NAME_VIEW);
        }

        /**
         * Gets the staff oid.
         *
         * @return String
         */
        public String getStaffOid() {
            return getValueString(FIELD_STAFF_OID);
        }

    }

    /**
     * The Class ToolScheduleTerm.
     */
    public static class ToolScheduleTerm extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_BASE_TERM_MAP =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_TERM.baseTermMap());
        public static final ToolBeanColumn FIELD_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_TERM.code());
        public static final ToolBeanColumn FIELD_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_TERM.scheduleOid());

        public static ToolBeanRelationship CHILD_SCHEDULE_TERM_DATES =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_TERM.scheduleTermDates().getBeanType(),
                        SisBeanPaths.SCHEDULE_TERM.scheduleTermDates().getValueType(),
                        SisBeanPaths.SCHEDULE_TERM.scheduleTermDates().getPath(),
                        SisBeanPaths.SCHEDULE_TERM_DATE.scheduleTermOid().toString(),
                        SisBeanPaths.SCHEDULE_TERM.scheduleTermDates().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_BASE_TERM_MAP,
                        FIELD_CODE,
                        FIELD_SCHEDULE_OID)
                .expandRelationships(CHILD_SCHEDULE_TERM_DATES);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_TERM.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolScheduleTerm(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;
        private PlainDate m_termEndDate;
        private PlainDate m_termStartDate;

        /**
         * Contains date.
         *
         * @param broker X2Broker
         * @param date PlainDate
         * @return Object
         */
        public boolean containsDate(X2Broker broker, PlainDate date) {
            return getScheduleTermDates(broker).stream().anyMatch(tmd -> tmd.containsDate(date));
        }

        /**
         * Gets the base term map.
         *
         * @return String
         */
        public String getBaseTermMap() {
            return getValueString(FIELD_BASE_TERM_MAP);
        }

        /**
         * Gets the code.
         *
         * @return String
         */
        public String getCode() {
            return getValueString(FIELD_CODE);
        }

        /**
         * Gets the range.
         *
         * @param broker X2Broker
         * @return Range
         */
        public Range<Date> getDateRange(X2Broker broker) {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(broker), getEndDate(broker));
            }
            return m_dateRange;
        }

        /**
         * Gets the end date.
         *
         * @param broker X2Broker
         * @return Plain date
         */
        public PlainDate getEndDate(X2Broker broker) {
            if (m_termEndDate == null) {
                m_termEndDate = getScheduleTermDates(broker).stream()
                        .map(ToolScheduleTermDate::getEndDate).max(Comparator.naturalOrder()).orElse(null);
            }
            return m_termEndDate;
        }

        /**
         * Gets the schedule term dates.
         *
         * @param broker X2Broker
         * @return Collection
         */
        public Collection<ToolScheduleTermDate> getScheduleTermDates(X2Broker broker) {
            return (List<ToolScheduleTermDate>) getChildren(broker, CHILD_SCHEDULE_TERM_DATES);
        }

        /**
         * Gets the start date.
         *
         * @param broker X2Broker
         * @return the startDate
         */
        public PlainDate getStartDate(X2Broker broker) {
            if (m_termStartDate == null) {
                m_termStartDate = getScheduleTermDates(broker).stream()
                        .map(tmd -> tmd.getStartDate()).min(Comparator.naturalOrder()).orElse(null);
            }
            return m_termStartDate;
        }

    }


    /**
     * The Class ToolScheduleTermDate.
     */
    public static class ToolScheduleTermDate extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_SCHEDULE_TERM_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_TERM_DATE.scheduleTermOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_TERM_DATE.startDate(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_TERM_DATE.endDate(), PredefinedConverter.PLAINDATE, true);

        public static ToolBeanRelationship PARENT_SCHEDULE_TERM =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_TERM_DATE.scheduleTerm().getBeanType(),
                        SisBeanPaths.SCHEDULE_TERM_DATE.scheduleTerm().getValueType(),
                        SisBeanPaths.SCHEDULE_TERM_DATE.scheduleTermOid().getPath(),
                        SisBeanPaths.SCHEDULE_TERM.scheduleTermDates().getPath(),
                        SisBeanPaths.SCHEDULE_TERM_DATE.scheduleTerm().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_SCHEDULE_TERM_OID,
                        FIELD_START_DATE,
                        FIELD_END_DATE)
                .expandRelationships(PARENT_SCHEDULE_TERM);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_TERM_DATE.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolScheduleTermDate(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Contains date.
         *
         * @param date PlainDate
         * @return true, if successful
         */
        public boolean containsDate(PlainDate date) {
            return getRange().contains(date);
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the range.
         *
         * @return Range
         */
        public Range<Date> getRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the schedule term oid.
         *
         * @return String
         */
        public String getScheduleTermOid() {
            return this.getValueString(FIELD_SCHEDULE_TERM_OID);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }
    }


    /**
     * The Class School.
     */
    public static class ToolSchool extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_ACTIVE_SCHEDULE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.activeSchoolSched().activeScheduleOid());
        public static final ToolBeanColumn FIELD_ADDRESS_OID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.addressOid());
        public static final ToolBeanColumn FIELD_ARCHIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.archiveIndicator());
        public static final ToolBeanColumn FIELD_CURRENT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.currentContextOid());
        public static final ToolBeanColumn FIELD_INACTIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.inactiveIndicator());
        public static final ToolBeanColumn FIELD_NAME =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.name());
        public static final ToolBeanColumn FIELD_ORGANIZATION_1_OID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.organization1Oid());
        public static final ToolBeanColumn FIELD_SCHOOL_ID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.schoolId());
        public static final ToolBeanColumn FIELD_START_GRADE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.startGrade());

        public static ToolBeanRelationship CHILD_CALENDARS =
                new ToolBeanRelationship(SisBeanPaths.SCHOOL.schoolCalendars().getBeanType(),
                        SisBeanPaths.SCHOOL.schoolCalendars().getValueType(),
                        SisBeanPaths.SCHOOL.schoolCalendars().getPath(),
                        SisBeanPaths.CALENDAR_SCHOOL.schoolOid().getPath(),
                        SisBeanPaths.SCHOOL.schoolCalendars().getRelationshipType());

        public static ToolBeanRelationship CHILD_GRADE_TERM_DATES =
                new ToolBeanRelationship(SisBeanPaths.SCHOOL.gradeTermDates().getBeanType(),
                        SisBeanPaths.SCHOOL.gradeTermDates().getValueType(),
                        SisBeanPaths.SCHOOL.gradeTermDates().getPath(),
                        SisBeanPaths.GRADE_TERM_DATE.schoolOid().getPath(),
                        SisBeanPaths.SCHOOL.gradeTermDates().getRelationshipType());

        public static ToolBeanRelationship CHILD_SCHEDULES =
                new ToolBeanRelationship(SisBeanPaths.SCHOOL.schedules().getBeanType(),
                        SisBeanPaths.SCHOOL.schedules().getValueType(),
                        SisBeanPaths.SCHOOL.schedules().getPath(),
                        SisBeanPaths.SCHEDULE.schoolOid().getPath(),
                        SisBeanPaths.SCHOOL.schedules().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ARCHIVE_INDICATOR,
                        FIELD_ACTIVE_SCHEDULE,
                        FIELD_ADDRESS_OID,
                        FIELD_CURRENT_CONTEXT_OID,
                        FIELD_INACTIVE_INDICATOR,
                        FIELD_NAME,
                        FIELD_ORGANIZATION_1_OID,
                        FIELD_SCHOOL_ID,
                        FIELD_START_GRADE)
                .expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER,
                                SisBeanPaths.SCHOOL_SCHEDULE_CONTEXT.getDatabaseName()))
                .expandRelationships(
                        CHILD_CALENDARS,
                        CHILD_GRADE_TERM_DATES,
                        CHILD_SCHEDULES);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHOOL.getBeanType();
        }

        public static final String CALENDAR_ANY = "*";

        Map<String, ToolSchoolCalendar> m_mostCommonCalendars;

        /**
         * Instantiates a new school.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Find session date.
         *
         * @param broker X2Broker
         * @param contextOid String
         * @param calendarCode String
         * @param effectiveDate PlainDate
         * @param after boolean
         * @return PlainDate
         */
        public PlainDate findSessionDate(X2Broker broker,
                                         String contextOid,
                                         String calendarCode,
                                         PlainDate effectiveDate,
                                         boolean after) {
            PlainDate nearestDate = null;
            Set<PlainDate> insessionDates = getCalendarDays(broker, contextOid, calendarCode);
            if (insessionDates == null && !DistrictManager.DEFAULT_CALENDAR_ID.equals(calendarCode)) {
                insessionDates = getCalendarDays(broker, contextOid, DistrictManager.DEFAULT_CALENDAR_ID);
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (after && date.after(effectiveDate)) {
                        if (nearestDate == null || nearestDate.after(date)) {
                            nearestDate = date;
                        }
                    } else if (!after && date.before(effectiveDate)) {
                        if (nearestDate == null || nearestDate.before(date)) {
                            nearestDate = date;
                        }
                    }
                }
            }

            if (nearestDate == null) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(effectiveDate);
                cal.add(Calendar.DAY_OF_YEAR, after ? 1 : -1);
                nearestDate = new PlainDate(cal.getTime());
            }

            return nearestDate;
        }

        /**
         * Gets the active schedule.
         *
         * @param broker X2Broker
         * @return Tool schedule
         */
        public ToolSchedule getActiveSchedule(X2Broker broker) {
            String schOid = getValueString(FIELD_ACTIVE_SCHEDULE);
            return getBeanByOid(broker, ToolSchedule.class, schOid, true);
        }

        /**
         * Gets the active schedule oid.
         *
         * @return String
         */
        public String getActiveScheduleOid() {
            return getValueString(FIELD_ACTIVE_SCHEDULE);
        }

        /**
         * Gets the address.
         *
         * @param broker X2Broker
         * @return String
         */
        public ToolAddress getAddress(X2Broker broker) {
            String adrOid = getValueString(FIELD_ADDRESS_OID);
            return getBeanByOid(broker, ToolAddress.class, adrOid, true);
        }

        /**
         * Gets the address oid.
         *
         * @return String
         */
        public String getAddressOid() {
            return getValueString(FIELD_ADDRESS_OID);
        }

        /**
         * Gets archive indicator
         *
         * @return the checks if is archived
         */

        public boolean getArchiveIndicator() {
            return getValueLogical(FIELD_ARCHIVE_INDICATOR);
        }

        /**
         * Gets the calendar by code.
         *
         * @param broker X2Broker
         * @param contextOid String
         * @param calendarCode String
         * @return Tool school calendar
         */
        public ToolSchoolCalendar getCalendarByCode(X2Broker broker, String contextOid, String calendarCode) {
            List<String> values = Arrays.asList(contextOid, calendarCode);
            return getCalendars(broker).getGroup(new ToolBeanDefinition(ToolSchoolCalendar.FIELD_DISTRICT_CONTEXT_OID,
                    ToolSchoolCalendar.FIELD_CALENDAR_ID), values).stream().findFirst().orElse(null);
        }

        /**
         * Gets the calendars by context.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolSchoolCalendar> getCalendarsByContext(X2Broker broker) {
            return getCalendarsByContext(broker, getCurrentContextOid());
        }

        /**
         * Gets the calendars by context.
         *
         * @param broker X2Broker
         * @param contextOid String
         * @return List
         */
        public List<ToolSchoolCalendar> getCalendarsByContext(X2Broker broker, String contextOid) {
            return getCalendars(broker).getGroup(ToolSchoolCalendar.FIELD_DISTRICT_CONTEXT_OID, contextOid);
        }

        /**
         * Gets the calendar days.
         *
         * @param broker X2Broker
         * @param contextOid String
         * @param calendarCode String
         * @return Sets the
         */
        public Set<PlainDate> getCalendarDays(X2Broker broker, String contextOid, String calendarCode) {
            Set<PlainDate> calendarDates = null;

            ToolSchoolCalendar calendar;
            if (CALENDAR_ANY.equals(calendarCode)) {
                calendar = getCalendarsByContext(broker, contextOid).stream()
                        .filter(cas -> cas.hasDays()).findAny().orElse(null);
            } else {
                calendar = getCalendarByCode(broker, contextOid, calendarCode);
            }

            if (calendar != null) {
                calendarDates = calendar.getDatesInSchedule(broker);
            }
            return calendarDates;
        }

        /**
         * Gets the calendars.
         *
         * @param broker X2Broker
         * @return List
         */
        public Filterable<ToolSchoolCalendar> getCalendars(X2Broker broker) {
            return (Filterable<ToolSchoolCalendar>) getChildrenFilterable(broker, CHILD_CALENDARS);
        }

        /**
         * Returns the Related year context object identifier.
         * <p>
         * The data dictionary ID for this property is <code>sklCtxOIDCurr</code>.
         *
         * @return String
         */
        public String getCurrentContextOid() {
            return getValueString(FIELD_CURRENT_CONTEXT_OID);
        }

        /**
         * Gets the grade term dates.
         *
         * @param broker X2Broker
         * @return Filterable
         */
        public Filterable<ToolGradeTermDate> getGradeTermDates(X2Broker broker) {
            return (Filterable<ToolGradeTermDate>) getChildrenFilterable(broker, CHILD_GRADE_TERM_DATES);
        }

        /**
         * Gets inactive indicator
         *
         * @return the checks if is archived
         */
        public boolean getInactiveIndicator() {
            return getValueLogical(FIELD_INACTIVE_INDICATOR);

        }

        /**
         * Gets the most common calendar.
         *
         * @param broker X2Broker
         * @param contextOid String
         * @return Tool school calendar
         */
        public ToolSchoolCalendar getMostCommonCalendar(X2Broker broker, String contextOid) {
            ToolSchoolCalendar value = null;
            if (m_mostCommonCalendars == null) {
                m_mostCommonCalendars = new HashMap();
            }
            if (m_mostCommonCalendars.containsKey(contextOid)) {
                value = m_mostCommonCalendars.get(contextOid);
            } else {
                Collection<ToolSchoolCalendar> calendars = this.getCalendarsByContext(broker, contextOid);
                if (calendars != null && !calendars.isEmpty()) {
                    value = calendars.iterator().next();
                    if (calendars.size() > 1) {
                        List<String> calendarCodes =
                                calendars.stream().map(ToolSchoolCalendar::getCalendarId).collect(Collectors.toList());

                        X2Criteria criteria = new X2Criteria();
                        criteria.addEqualTo(ToolStudent.FIELD_SCHOOL_OID.resolve(null), getOid());
                        criteria.addIn(ToolStudent.FIELD_CALENDAR_CODE.resolve(null), calendarCodes);

                        String[] columns = new String[] {ToolStudent.FIELD_CALENDAR_CODE.resolve(null), "count(*)"};

                        ColumnQuery query = new ColumnQuery(ToolStudent.getX2BaseClass(), columns, criteria);
                        query.addGroupBy(ToolStudent.FIELD_CALENDAR_CODE.resolve(null));
                        query.addOrderByDescending("count(*)");

                        try (QueryIterator iterator = broker.getIteratorByQuery(query)) {
                            while (iterator.hasNext()) {
                                Object[] row = (Object[]) iterator.next();
                                String calendarCode = (String) row[0];

                                if (!StringUtils.isEmpty(calendarCode)) {
                                    for (ToolSchoolCalendar calendar : calendars) {
                                        if (calendarCode.equals(calendar.getCalendarId())) {
                                            value = calendar;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                m_mostCommonCalendars.put(contextOid, value);
            }
            return value;
        }

        /**
         * Gets the name.
         *
         * @return String
         */
        public String getName() {
            return this.getValueString(FIELD_NAME);
        }

        /**
         * Gets the organization 1.
         *
         * @param broker the broker
         * @return the organization 1
         */
        public ToolOrganization getOrganization1(X2Broker broker) {
            String schOid = getValueString(FIELD_ORGANIZATION_1_OID);
            return getBeanByOid(broker, ToolOrganization.class, schOid, true);
        }

        /**
         * Gets the schedules.
         *
         * @param broker X2Broker
         * @return List
         */
        public Filterable<ToolSchedule> getSchedules(X2Broker broker) {
            return (Filterable<ToolSchedule>) getChildrenFilterable(broker, CHILD_SCHEDULES);
        }

        /**
         * Returns the Identifier.
         * <p>
         * The data dictionary ID for this property is <code>sklSchoolID</code>.
         *
         * @return String
         */
        public String getSchoolId() {
            return getValueString(FIELD_SCHOOL_ID);
        }

        /**
         * Returns the Start grade.
         * <p>
         * The data dictionary ID for this property is <code>sklStartGrade</code>.
         *
         * @return int
         */
        public int getStartGrade() {
            return getValueInt(FIELD_START_GRADE);
        }

    }


    /**
     * The Class SchoolCalendar.
     */
    public static class ToolSchoolCalendar extends ToolBean {

        /**
         * Find any calendar.
         *
         * @param broker X2Broker
         * @param schoolList List<ToolSchool>
         * @param context ToolDistrictContext
         * @return ToolSchoolCalendar
         */
        public static ToolSchoolCalendar findAnyCalendar(X2Broker broker,
                                                         List<ToolSchool> schoolList,
                                                         ToolDistrictContext context) {
            return schoolList.stream()
                    .map(skl -> skl.getCalendars(broker)
                            .filter(ToolSchoolCalendar.FIELD_DISTRICT_CONTEXT_OID, context.getOid()).extract())
                    .flatMap(calendars -> calendars.stream())
                    .filter(testCalendar -> testCalendar.getDaysInSession() > 0)
                    .sorted(new Comparator<ToolSchoolCalendar>() {

                        @Override
                        public int compare(ToolSchoolCalendar o1, ToolSchoolCalendar o2) {
                            return o1.getOid().compareTo(o2.getOid());
                        }
                    })
                    .findFirst().orElse(null);
        }

        /**
         * Span Calendar is the School Calendar with the following criteria
         * - Belongs to span context
         * -Belongs to span school
         * -Calendar ID matches:
         * 1. W record Calendar Code if found
         * 2. else Student Calendar Code
         * 3. else Default Calendar Code(s)
         * 4. else Wild Card (*) Calendar Code
         *
         * @param broker X2Broker
         * @param span AnnualSpan
         * @return ToolSchoolCalendar
         */
        public static ToolSchoolCalendar findBestCalendar(X2Broker broker, AnnualSpan span) {
            return findBestCalendar(broker, span.getStudent(), span.getSchool(),
                    span.getDataStorageEnrollment(), span.getContext());
        }

        /**
         * Find best calendar.
         *
         * @param broker X2Broker
         * @param student ToolStudent
         * @param school ToolSchool
         * @param withdrawal ToolEnrollment
         * @param context ToolSchoolYearContext
         * @return ToolSchoolCalendar
         */
        public static ToolSchoolCalendar findBestCalendar(X2Broker broker,
                                                          ToolStudent student,
                                                          ToolSchool school,
                                                          ToolEnrollment withdrawal,
                                                          ToolDistrictContext context) {
            return findBestCalendar(broker, student, Arrays.asList(school), withdrawal, context);
        }

        /**
         * Find best calendar.
         *
         * @param broker X2Broker
         * @param student SisStudent
         * @param schools Collection<SisSchool>
         * @param withdrawal StudentEnrollment
         * @param context DistrictSchoolYearContext
         * @return SchoolCalendar
         */
        public static ToolSchoolCalendar findBestCalendar(X2Broker broker,
                                                          ToolStudent student,
                                                          Collection<? extends ToolSchool> schools,
                                                          ToolEnrollment withdrawal,
                                                          ToolDistrictContext context) {
            List<ToolSchool> schoolList = new ArrayList(schools.size() + 2);
            if (schools.contains(student.getSchool(broker))) {
                schoolList.add(0, student.getSchool(broker));
            }
            if (withdrawal != null && schools.contains(withdrawal.getSchool(broker))) {
                schoolList.add(0, withdrawal.getSchool(broker));
            }
            schoolList.addAll(schools);

            // 1. Attempt to find calendar from W record
            ToolSchoolCalendar schoolCalendar = null;
            String calendarCode = null;
            if (withdrawal != null) {
                calendarCode = withdrawal.getEnrollmentCalendarCode();
                schoolCalendar = findCalendarByCode(broker, schoolList, context, calendarCode);
            }

            // 2. Attempt to find calendar from student
            if (schoolCalendar == null) {
                calendarCode = student.getCalendarCode();
                schoolCalendar = findCalendarByCode(broker, schoolList, context, calendarCode);
            }

            // 3. Find a default calendar
            if (schoolCalendar == null) {
                schoolCalendar = findDefaultCalendar(broker, schoolList, context);
            }

            // 4. find any calendar
            if (schoolCalendar == null) {
                schoolCalendar = findAnyCalendar(broker, schoolList, context);
            }

            return schoolCalendar;
        }

        /**
         * Find calendar by code.
         *
         * @param broker X2Broker
         * @param schoolList List<ToolSchool>
         * @param context ToolDistrictContext
         * @param calendarCode String
         * @return ToolSchoolCalendar
         */
        public static ToolSchoolCalendar findCalendarByCode(X2Broker broker,
                                                            List<ToolSchool> schoolList,
                                                            ToolDistrictContext context,
                                                            String calendarCode) {
            ToolSchoolCalendar schoolCalendar = null;
            if (!StringUtils.isBlank(calendarCode)) {
                String code = calendarCode;
                Optional<? extends ToolSchool> school = schoolList.stream()
                        .filter(skl -> skl.getCalendarByCode(broker, context.getOid(), code) != null)
                        .findFirst();
                if (school.isPresent()) {
                    schoolCalendar = school.get().getCalendarByCode(broker, context.getOid(), code);
                }
            }
            return schoolCalendar;
        }

        /**
         * Find default calendar.
         *
         * @param broker X2Broker
         * @param schoolList List<ToolSchool>
         * @param context ToolDistrictContext
         * @return ToolSchoolCalendar
         */
        public static ToolSchoolCalendar findDefaultCalendar(X2Broker broker,
                                                             List<ToolSchool> schoolList,
                                                             ToolDistrictContext context) {
            ToolSchoolCalendar schoolCalendar = null;
            for (String testCode : DistrictManager.getDefaultCalendarIds()) {
                Optional<? extends ToolSchool> school = schoolList.stream()
                        .filter(skl -> skl.getCalendarByCode(broker, context.getOid(), testCode) != null)
                        .findFirst();
                if (school.isPresent()) {
                    schoolCalendar = school.get().getCalendarByCode(broker, context.getOid(), testCode);
                }
                if (schoolCalendar != null) {
                    break;
                }
            }
            return schoolCalendar;
        }

        // Query Fields
        public static final ToolBeanColumn FIELD_CALENDAR_ID =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL.calendarId());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL.districtContextOid());
        public static final ToolBeanColumn FIELD_DAYS_IN_SESSION =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL.daysInSession(),
                        PredefinedConverter.INTEGER);
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL.schoolOid());

        public static ToolBeanRelationship CHILD_DATES =
                new ToolBeanRelationship(SisBeanPaths.CALENDAR_SCHOOL.schoolCalendarDates().getBeanType(),
                        SisBeanPaths.CALENDAR_SCHOOL.schoolCalendarDates().getValueType(),
                        SisBeanPaths.CALENDAR_SCHOOL.schoolCalendarDates().getPath(),
                        SisBeanPaths.CALENDAR_SCHOOL_DATE.schoolCalendarOid().getPath(),
                        SisBeanPaths.CALENDAR_SCHOOL.schoolCalendarDates().getRelationshipType())
                                .addComparator(new Comparator<ToolBean>() {
                                    @Override
                                    public int compare(ToolBean o1, ToolBean o2) {
                                        ToolSchoolCalendarDate cdate1, cdate2;
                                        cdate1 = (ToolSchoolCalendarDate) o1;
                                        cdate2 = (ToolSchoolCalendarDate) o2;
                                        PlainDate date1 = cdate1.getDate();
                                        PlainDate date2 = cdate2.getDate();
                                        if (date1 == null) {
                                            return -1;
                                        } else if (date2 == null) {
                                            return 1;
                                        }
                                        return date1.compareTo(date2);
                                    }
                                });

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_CALENDAR_ID,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_DAYS_IN_SESSION,
                        FIELD_SCHOOL_OID)
                .expandRelationships(CHILD_DATES);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.CALENDAR_SCHOOL.getBeanType();
        }

        private TreeSet<PlainDate> m_dates;
        private TreeSet<PlainDate> m_datesInSchedule;
        private TreeSet<PlainDate> m_datesInSession;
        private Map<String, TreeSet<ToolSchoolCalendarDate>> m_datesPerRange;
        private Map<String, TreeSet<ToolSchoolCalendarDate>> m_datesPerTerm;
        private Map<String, TreeSet<ToolSchoolCalendarDate>> m_datesPerTermAndRange;

        /**
         * Instantiates a new school calendar.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolSchoolCalendar(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the student schedules.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolSchoolCalendarDate> getCalendarDates(X2Broker broker) {
            return (List<ToolSchoolCalendarDate>) getChildren(broker, CHILD_DATES);
        }

        /**
         * Gets the calendar dates for term.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Tree set
         */
        public TreeSet<ToolSchoolCalendarDate> getCalendarDatesForRange(X2Broker broker, Range<Date> dateRange) {
            if (m_datesPerRange == null) {
                m_datesPerRange = new HashMap();
            }
            TreeSet<ToolSchoolCalendarDate> dateSet = m_datesPerRange.get(dateRange.toString());
            if (dateSet == null) {
                dateSet = new TreeSet(CHILD_DATES.getComparator());
                dateSet.addAll(getCalendarDates(broker).stream().filter(csd -> dateRange.contains(csd.getDate()))
                        .collect(Collectors.toList()));
                m_datesPerRange.put(dateRange.toString(), dateSet);
            }
            return dateSet;
        }

        /**
         * Gets the calendar dates for term.
         *
         * @param broker X2Broker
         * @param term ToolScheduleTerm
         * @return Tree set
         */
        public TreeSet<ToolSchoolCalendarDate> getCalendarDatesForTerm(X2Broker broker, ToolScheduleTerm term) {
            if (m_datesPerTerm == null) {
                m_datesPerTerm = new HashMap();
            }
            TreeSet<ToolSchoolCalendarDate> dateSet = m_datesPerTerm.get(term.getOid());
            if (dateSet == null) {
                dateSet = new TreeSet(CHILD_DATES.getComparator());
                dateSet.addAll(getCalendarDates(broker).stream().filter(csd -> term.containsDate(broker, csd.getDate()))
                        .collect(Collectors.toList()));
                m_datesPerTerm.put(term.getOid(), dateSet);
            }
            return dateSet;
        }

        /**
         * Gets the calendar dates for term.
         *
         * @param broker X2Broker
         * @param list ToolScheduleTerm
         * @param dateRange Range<Date>
         * @return Tree set
         */
        public TreeSet<ToolSchoolCalendarDate> getCalendarDatesForTermAndRange(X2Broker broker,
                                                                               List<ToolScheduleTerm> list,
                                                                               Range<Date> dateRange) {
            if (m_datesPerTermAndRange == null) {
                m_datesPerTermAndRange = new HashMap();
            }

            String key = list.stream().map(ToolBean::getOid).sorted().collect(Collectors.joining(","))
                    + dateRange.toString();


            TreeSet<ToolSchoolCalendarDate> dateSet = m_datesPerTermAndRange.get(key);
            if (dateSet == null) {
                dateSet = new TreeSet(CHILD_DATES.getComparator());
                dateSet.addAll(getCalendarDates(broker).stream()
                        .filter(csd -> list.stream().anyMatch(trm -> trm.containsDate(broker, csd.getDate()))
                                && dateRange.contains(csd.getDate()))
                        .collect(Collectors.toList()));
                m_datesPerTermAndRange.put(key, dateSet);
            }
            return dateSet;
        }

        /**
         * Find first in session date.
         *
         * @param broker X2Broker
         * @param startingDate PlainDate
         * @param forward boolean
         * @return PlainDate
         */
        public PlainDate findFirstInSessionDate(X2Broker broker, PlainDate startingDate, boolean forward) {
            TreeSet<PlainDate> dateSet = getDatesInSession(broker);

            if (startingDate == null) {
                return forward ? (dateSet.isEmpty() ? null : dateSet.first())
                        : (dateSet.isEmpty() ? null : dateSet.last());
            }

            if (dateSet.contains(startingDate)) {
                return startingDate;
            } else if (forward) {
                return dateSet.isEmpty() ? null : dateSet.higher(startingDate);
            } else {
                return dateSet.isEmpty() ? null : dateSet.lower(startingDate);
            }
        }

        /**
         * Find first in session date.
         *
         * @param broker X2Broker
         * @param startingDate PlainDate
         * @param forward boolean
         * @return PlainDate
         */
        public PlainDate findNextInSessionDate(X2Broker broker, PlainDate startingDate, boolean forward) {
            TreeSet<PlainDate> dateSet = getDatesInSession(broker);

            if (startingDate == null) {
                return forward ? (dateSet.isEmpty() ? null : dateSet.first())
                        : (dateSet.isEmpty() ? null : dateSet.last());
            }

            if (forward) {
                return dateSet.isEmpty() ? null : dateSet.higher(startingDate);
            }
            return dateSet.isEmpty() ? null : dateSet.lower(startingDate);
        }

        /**
         * Gets the calendar id.
         *
         * @return String
         */
        public String getCalendarId() {
            return this.getValueString(FIELD_CALENDAR_ID);
        }

        /**
         * Gets the dates.
         *
         * @param broker X2Broker
         * @return Tree set
         */
        public TreeSet<PlainDate> getDates(X2Broker broker) {
            if (m_dates == null) {
                m_dates = new TreeSet();

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SisBeanPaths.CALENDAR_SCHOOL_DATE.schoolCalendarOid().getPath(), getOid());

                String[] queryColumns = new String[] {
                        SisBeanPaths.CALENDAR_SCHOOL_DATE.date().toString()
                };
                ColumnQuery query =
                        new ColumnQuery(SisBeanPaths.CALENDAR_SCHOOL_DATE.getBeanType(), queryColumns, criteria);
                query.addOrderByAscending(SisBeanPaths.CALENDAR_SCHOOL_DATE.date().getPath());

                try (QueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        PlainDate date = (PlainDate) PredefinedConverter.PLAINDATE.convertedValue(row[0]);
                        m_dates.add(date);
                    }
                }
            }
            return m_dates;
        }

        /**
         * Gets the dates in schedule.
         *
         * @param broker X2Broker
         * @return Tree set
         */
        public TreeSet<PlainDate> getDatesInSchedule(X2Broker broker) {
            if (m_datesInSchedule == null) {
                m_datesInSchedule = new TreeSet();
                PlainDate startDate = null;
                PlainDate endDate = null;
                ToolDistrictContext context =
                        getBeanByOid(broker, ToolDistrictContext.class, getDistrictContextOid(), true);
                ToolSchedule schedule = getSchool(broker).getSchedules(broker)
                        .getGroup(FIELD_DISTRICT_CONTEXT_OID, getDistrictContextOid()).stream().findAny().orElse(null);

                if (schedule != null) {
                    startDate = schedule.getStartDate();
                    endDate = schedule.getEndDate();
                } else {
                    startDate = context.getStartDate();
                    endDate = context.getEndDate();
                }



                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SisBeanPaths.CALENDAR_SCHOOL_DATE.schoolCalendarOid().getPath(), getOid());
                criteria.addEqualTo(SisBeanPaths.CALENDAR_SCHOOL_DATE.inSessionIndicator().toString(), Boolean.TRUE);
                criteria.addLessOrEqualThan(SisBeanPaths.CALENDAR_SCHOOL_DATE.date().getPath(), endDate);
                criteria.addGreaterOrEqualThan(SisBeanPaths.CALENDAR_SCHOOL_DATE.date().getPath(), startDate);

                String[] queryColumns = new String[] {
                        SisBeanPaths.CALENDAR_SCHOOL_DATE.date().toString()
                };
                ColumnQuery query =
                        new ColumnQuery(SisBeanPaths.CALENDAR_SCHOOL_DATE.getBeanType(), queryColumns, criteria);
                query.addOrderByAscending(SisBeanPaths.CALENDAR_SCHOOL_DATE.date().getPath());

                try (QueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        PlainDate date = (PlainDate) PredefinedConverter.PLAINDATE.convertedValue(row[0]);
                        m_datesInSchedule.add(date);
                    }
                }
            }
            return m_datesInSchedule;
        }

        /**
         * Gets the dates in session.
         *
         * @param broker X2Broker
         * @return Tree set
         */
        public TreeSet<PlainDate> getDatesInSession(X2Broker broker) {
            if (m_datesInSession == null) {
                m_datesInSession = new TreeSet();
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SisBeanPaths.CALENDAR_SCHOOL_DATE.schoolCalendarOid().getPath(), getOid());
                criteria.addEqualTo(SisBeanPaths.CALENDAR_SCHOOL_DATE.inSessionIndicator().toString(), Boolean.TRUE);

                String[] queryColumns = new String[] {
                        SisBeanPaths.CALENDAR_SCHOOL_DATE.date().toString()
                };
                ColumnQuery query =
                        new ColumnQuery(SisBeanPaths.CALENDAR_SCHOOL_DATE.getBeanType(), queryColumns, criteria);
                query.addOrderByAscending(SisBeanPaths.CALENDAR_SCHOOL_DATE.date().getPath());

                try (QueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        PlainDate date = (PlainDate) PredefinedConverter.PLAINDATE.convertedValue(row[0]);
                        m_datesInSession.add(date);
                    }
                }
            }
            return m_datesInSession;
        }

        /**
         * Gets the days in session.
         *
         * @return int
         */
        public int getDaysInSession() {
            return getValueInt(FIELD_DAYS_IN_SESSION);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolDistrictContext getDistrictContext(X2Broker broker) {
            String ctxOid = getValueString(FIELD_DISTRICT_CONTEXT_OID);
            return getBeanByOid(broker, ToolDistrictContext.class, ctxOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getDistrictContextOid() {
            return getValueString(FIELD_DISTRICT_CONTEXT_OID);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSchool getSchool(X2Broker broker) {
            String stdOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, stdOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Checks for days.
         *
         * @return true, if successful
         */
        public boolean hasDays() {
            return false;
        }
    }


    /**
     * The Class ToolSchoolCalendarDate.
     */
    public static class ToolSchoolCalendarDate extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_BELL_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL_DATE.bellScheduleOid());
        public static final ToolBeanColumn FIELD_DATE =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL_DATE.date(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_IN_SESSION_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL_DATE.inSessionIndicator());
        public static final ToolBeanColumn FIELD_SCHEDULE_DAY_NUMBER =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL_DATE.scheduleDayNumber());
        public static final ToolBeanColumn FIELD_SCHOOL_CALENDAR_OID =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL_DATE.schoolCalendarOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_BELL_SCHEDULE_OID,
                        FIELD_DATE,
                        FIELD_IN_SESSION_INDICATOR,
                        FIELD_SCHEDULE_DAY_NUMBER,
                        FIELD_SCHOOL_CALENDAR_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.CALENDAR_SCHOOL_DATE.getBeanType();
        }

        /**
         * Instantiates a new school calendar.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolSchoolCalendarDate(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the Related bell schedule object identifier.
         * <p>
         * The data dictionary ID for this property is <code>csdBelOID</code>.
         *
         * @return String
         */
        public String getBellScheduleOid() {
            return getValueString(FIELD_BELL_SCHEDULE_OID);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return getValueDate(FIELD_DATE);
        }

        /**
         * Gets the in session indicator.
         *
         * @return boolean
         */
        public boolean getInSessionIndicator() {
            return getValueLogical(FIELD_IN_SESSION_INDICATOR);
        }

        /**
         * Returns the Schedule day number.
         * <p>
         * The data dictionary ID for this property is <code>csdSchdDayNum</code>.
         *
         * @return int
         */
        public int getScheduleDayNumber() {
            return getValueInt(FIELD_SCHEDULE_DAY_NUMBER);
        }

        /**
         * Gets the calendar.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSchoolCalendar getSchoolCalendar(X2Broker broker) {
            String calOid = getValueString(FIELD_SCHOOL_CALENDAR_OID);
            return getBeanByOid(broker, ToolSchoolCalendar.class, calOid, true);
        }

    }

    /**
     * The Class ToolSection.
     */
    public static class ToolSection extends ToolBean {
        /**
         * The Class ClassSession.
         */
        public class ClassSession implements Comparable {

            private ToolSchoolCalendarDate m_date;
            private List<ClassSessionPeriod> m_periods;

            /**
             * Instantiates a new class session.
             *
             * @param date SisSchoolCalendarDate
             * @param periods List<ClassSessionPeriod>
             */
            public ClassSession(ToolSchoolCalendarDate date, List<ClassSessionPeriod> periods) {
                this.m_date = date;
                this.m_periods = periods;
            }

            /**
             * Compare to.
             *
             * @param o Object
             * @return int
             * @see java.lang.Comparable#compareTo(java.lang.Object)
             */
            @Override
            public int compareTo(Object o) {
                ClassSession session = (ClassSession) o;
                return getPlainDate().compareTo(session.getPlainDate());
            }

            /**
             * Gets the date.
             *
             * @return the m_date
             */
            public ToolSchoolCalendarDate getDate() {
                return m_date;
            }

            /**
             * Gets the periods.
             *
             * @return the m_periods
             */
            public List<ClassSessionPeriod> getPeriods() {
                return m_periods;
            }

            /**
             * Gets the plain date.
             *
             * @return Plain date
             */
            public PlainDate getPlainDate() {
                return getDate().getDate();
            }

            /**
             * To string.
             *
             * @return String
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                return "date: " + m_date.getDate() + ", dayNumber: " + m_date.getScheduleDayNumber() + ", bellOid: "
                        + m_date.getBellScheduleOid() + ", periods: " + m_periods.toString();
            }

        }

        /**
         * The Class ClassSessionsList.
         */
        public class ClassSessionsList extends ArrayList<ClassSession> {
            private Integer m_duration;
            private PlainTime m_endTime;
            private PlainTime m_startTime;

            /**
             * Gets the duration.
             *
             * @return the duration
             */
            public Integer getDuration() {
                return m_duration;
            }

            /**
             * Gets the end time.
             *
             * @return the end time
             */
            public PlainTime getEndTime() {
                return m_endTime;
            }

            /**
             * Gets the start time.
             *
             * @return the start time
             */
            public PlainTime getStartTime() {
                return m_startTime;
            }

            /**
             * Sets the duration.
             *
             * @param duration the new duration
             */
            public void setDuration(int duration) {
                if ((m_duration == null || duration > m_duration.intValue())) {
                    m_duration = Integer.valueOf(duration);
                }
            }

            /**
             * Sets the end time.
             *
             * @param endTime the new end time
             */
            public void setEndTime(PlainTime endTime) {
                if (endTime != null && (m_endTime == null || endTime.after(m_endTime))) {
                    m_endTime = endTime;
                }
            }

            /**
             * Sets the start time.
             *
             * @param startTime the new start time
             */
            public void setStartTime(PlainTime startTime) {
                if (startTime != null && (m_startTime == null || startTime.before(m_startTime))) {
                    m_startTime = startTime;
                }
            }

        }

        /**
         * The Class ClassSessionPeriod.
         */
        public class ClassSessionPeriod {
            private String m_periodId;
            private int m_duration;

            /**
             * Instantiates a new class session period.
             *
             * @param periodId String
             * @param minutes int
             */
            public ClassSessionPeriod(String periodId, int minutes) {
                m_periodId = periodId;
                m_duration = minutes;
            }

            /**
             * Gets the period id.
             *
             * @return the periodId
             */
            public String getPeriodId() {
                return m_periodId;
            }

            /**
             * Gets the duration.
             *
             * @return the duration
             */
            public int getDuration() {
                return m_duration;
            }

            /**
             * To string.
             *
             * @return String
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                return "periodId; " + m_periodId + ", duration: " + m_duration;
            }
        }

        public static final String CHILD_PATH_PERIOD_ATTENDANCE = "studentPeriodAttendance";

        // Nonquery Fields

        public static final ToolBeanColumn FIELD_CSK_MASTER_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().masterType());

        // Query Fields
        public static final ToolBeanColumn FIELD_ACTIVE_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schedule().activeSchoolScheduleContexts().oid());
        public static final ToolBeanColumn FIELD_COURSE_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().description());
        public static final ToolBeanColumn FIELD_CRS_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course().number());
        public static final ToolBeanColumn FIELD_COURSE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().courseOid());
        public static final ToolBeanColumn FIELD_COURSE_VIEW =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.courseView());
        public static final ToolBeanColumn FIELD_CSK_CREDIT =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().credit());
        public static final ToolBeanColumn FIELD_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.description());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schedule().districtContextOid());
        public static final ToolBeanColumn FIELD_PRIMARY_STAFF_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.primaryStaffOid());
        public static final ToolBeanColumn FIELD_RUBRIC_DEFINITION_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().rubricDefinitionOid());
        public static final ToolBeanColumn FIELD_SECTION_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.sectionNumber());
        public static final ToolBeanColumn FIELD_SCHEDULE_DISPLAY =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.scheduleDisplay());
        public static final ToolBeanColumn FIELD_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.scheduleOid());
        public static final ToolBeanColumn FIELD_SCHOOL_COURSE_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourseOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().schoolOid());
        public static final ToolBeanColumn FIELD_SCHEDULE_TERM_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.scheduleTermOid());
        public static final ToolBeanColumn FIELD_SECTION_CLASS_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.sectionClassOid());
        public static final ToolBeanColumn FIELD_STAFF_VIEW =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.staffView());
        public static final ToolBeanColumn FIELD_TERM_VIEW =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.termView());

        public static ToolBeanRelationship CHILD_MASTER_TERMS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER.masterTerms().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.masterTerms().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.masterTerms().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER_TERM.masterScheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.masterTerms().getRelationshipType());

        public static ToolBeanRelationship CHILD_STUDENT_PERIOD_ATTENDANCES = // Backward
                // relationship
                new ToolBeanRelationship(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getBeanType(),
                        SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getValueType(),
                        SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getPath(),
                        CHILD_PATH_PERIOD_ATTENDANCE,
                        SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getRelationshipType());

        public static ToolBeanRelationship CHILD_STUDENT_SCHEDULES =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER.studentSections().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.studentSections().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.studentSections().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE.sectionOid().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.studentSections().getRelationshipType());
        public static ToolBeanRelationship CHILD_STUDENT_SCHEDULE_CHANGES =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER.studentScheduleChanges().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.studentScheduleChanges().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.studentScheduleChanges().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterScheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.studentScheduleChanges().getRelationshipType());

        public static ToolBeanRelationship CHILD_TEACHER_SCHEDULES =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER.teacherSections().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.teacherSections().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.teacherSections().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.sectionOid().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.teacherSections().getRelationshipType());

        public static ToolBeanRelationship PARENT_SCHEDULE_CLASS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER.sectionClass().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.sectionClass().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.sectionClassOid().toString(),
                        SisBeanPaths.SCHEDULE_CLASS.sections().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.sectionClass().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ACTIVE_SCHEDULE_OID,
                        FIELD_COURSE_DESCRIPTION,
                        FIELD_CRS_NUMBER,
                        FIELD_COURSE_OID,
                        FIELD_COURSE_VIEW,
                        FIELD_CSK_CREDIT,
                        FIELD_DESCRIPTION,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_PRIMARY_STAFF_OID,
                        FIELD_RUBRIC_DEFINITION_OID,
                        FIELD_SECTION_NUMBER,
                        FIELD_SCHEDULE_DISPLAY,
                        FIELD_SCHEDULE_OID,
                        FIELD_SCHEDULE_TERM_OID,
                        FIELD_SCHOOL_COURSE_OID,
                        FIELD_SCHOOL_OID,
                        FIELD_SECTION_CLASS_OID,
                        FIELD_STAFF_VIEW,
                        FIELD_TERM_VIEW)
                .expandRelationships(CHILD_MASTER_TERMS,
                        CHILD_STUDENT_PERIOD_ATTENDANCES,
                        CHILD_STUDENT_SCHEDULES,
                        CHILD_STUDENT_SCHEDULE_CHANGES,
                        CHILD_TEACHER_SCHEDULES,
                        PARENT_SCHEDULE_CLASS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_MASTER.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolSection(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;
        private Map<String, ClassSessionsList> m_sessionsMap;
        private List<Pair<ToolStudent, List<StudentScheduleSpan>>> m_scheduleSpans;
        private Map<String, Boolean> m_isStudentEnrolledMap;

        /**
         * Contains date is used if there are date overrides to restrict class sessions
         *
         * @param date the date
         * @param broker the broker
         * @return true, if successful
         */
        public boolean containsDate(PlainDate date, X2Broker broker) {
            return true;
        }

        /**
         * Gets the calendar dates.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param broker X2Broker
         * @return Tree set
         */
        public TreeSet<ToolSchoolCalendarDate> getCalendarDates(PlainDate startDate,
                                                                PlainDate endDate,
                                                                X2Broker broker) {
            String calendarId = getSchedule(broker).getCalendar(broker).getCalendarId();
            return getCalendarDates(calendarId, startDate, endDate, broker);
        }

        /**
         * Gets the calendar dates.
         *
         * @param calendarId String
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param broker X2Broker
         * @return Tree set
         */
        public TreeSet<ToolSchoolCalendarDate> getCalendarDates(String calendarId,
                                                                PlainDate startDate,
                                                                PlainDate endDate,
                                                                X2Broker broker) {
            Range<Date> dateRange = Range.of(startDate, endDate);
            ToolSchedule schedule = getSchedule(broker);
            ToolSchoolCalendar calendar =
                    schedule.getSchool(broker).getCalendarByCode(broker, schedule.getDistrictContextOid(), calendarId);
            return calendar == null ? new TreeSet(ToolSchoolCalendar.CHILD_DATES.getComparator())
                    : calendar.getCalendarDatesForTermAndRange(broker, getScheduleTerms(broker), dateRange);
        }

        /**
         * Gets the class sessions.
         *
         * @param calendarId String
         * @param broker X2Broker
         * @return List
         */
        public ClassSessionsList getClassSessions(String calendarId, X2Broker broker) {
            if (m_sessionsMap == null) {
                m_sessionsMap = new HashMap();
            }
            if (!m_sessionsMap.containsKey(calendarId)) {
                ToolSchedule schedule = getSchedule(broker);
                List<ToolScheduleTerm> terms = getScheduleTerms(broker);
                ClassSessionsList sessions = new ClassSessionsList();
                for (ToolSchoolCalendarDate date : getCalendarDates(calendarId, null, null, broker)) {
                    if (date.getInSessionIndicator()
                            && terms.stream().anyMatch(trm -> trm.containsDate(broker, date.getDate()))
                            && containsDate(date.getDate(), broker)) {
                        Collection<ToolSchedulePeriod> periods = getSectionPeriods(broker, date);
                        List<ClassSessionPeriod> sessionPeriods = new LinkedList();
                        if (periods != null && !periods.isEmpty()) {
                            for (ToolSchedulePeriod period : periods) {

                                ToolScheduleBellPeriod bpe =
                                        schedule.getScheduleBellPeriod(broker, period.getId(), Collections.EMPTY_SET);
                                int minutes = 0;
                                if (bpe != null) {
                                    sessions.setStartTime(bpe.getStartTime());
                                    sessions.setEndTime(bpe.getEndTime());
                                    minutes = (int) (bpe.getEndTime().getTimeInMinutes()
                                            - bpe.getStartTime().getTimeInMinutes());
                                }
                                sessionPeriods.add(new ClassSessionPeriod(period.getId(), minutes));
                            }
                        }
                        if (!sessionPeriods.isEmpty()) {
                            int duration = sessionPeriods.stream()
                                    .collect(Collectors.summingInt(period -> period.getDuration()));
                            sessions.setDuration(duration);
                            sessions.add(new ClassSession(date, sessionPeriods));
                        }
                    }
                }
                Collections.sort(sessions);
                m_sessionsMap.put(calendarId, sessions);
            }
            return m_sessionsMap.get(calendarId);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return Tool school
         */
        public ToolCourse getCourse(X2Broker broker) {
            String crsOid = getValueString(FIELD_COURSE_OID);
            return getBeanByOid(broker, ToolCourse.class, crsOid, true);
        }

        /**
         * Gets the course description.
         *
         * @return String course description
         */
        public String getCourseDescription() {
            return getValueString(FIELD_COURSE_DESCRIPTION);
        }

        /**
         * Gets the course number.
         *
         * @return String course number
         */
        public String getCourseNumber() {
            return getValueString(FIELD_CRS_NUMBER);
        }

        /**
         * Gets the course oid.
         *
         * @return String oid
         */
        public String getCourseOid() {
            return getValueString(FIELD_COURSE_OID);
        }

        /**
         * Gets the course view.
         *
         * @return String
         */
        public String getCourseView() {
            return getValueString(FIELD_COURSE_VIEW);
        }

        /**
         * Returns the Credit.
         * <p>
         * The data dictionary ID for this property is <code>cskCredit</code>.
         *
         * @return BigDecimal
         */
        public BigDecimal getCredit() {
            return this.getValueBigDecimal(FIELD_CSK_CREDIT);
        }

        /**
         * Gets the range.
         *
         * @param broker X2Broker
         * @return Range
         */
        public Range<Date> getDateRange(X2Broker broker) {
            if (m_dateRange == null) {
                PlainDate startDate = getTermStartDate(broker);
                PlainDate endDate = getTermEndDate(broker);
                if (startDate != null && endDate != null && startDate.after(endDate)) {
                    throw new IllegalStateException("Start Date " + startDate + " after " + endDate + " for " + this);
                }
                m_dateRange = Range.of(startDate, endDate);
            }
            return m_dateRange;
        }

        /**
         * Gets the description.
         *
         * @return String
         */
        public String getDescription() {
            return getValueString(FIELD_DESCRIPTION);
        }

        /**
         * Returns the districtContext.
         *
         * @param broker the broker
         * @return ToolDistrictContext
         */
        public ToolDistrictContext getDistrictContext(X2Broker broker) {
            String ctxOid = getValueString(FIELD_DISTRICT_CONTEXT_OID);
            return getBeanByOid(broker, ToolDistrictContext.class, ctxOid, true);
        }

        /**
         * Gets the first term date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Tool schedule term date
         */
        public ToolScheduleTermDate getFirstTermDate(X2Broker broker, Range<Date> dateRange) {
            Optional<ToolScheduleTermDate> termDate = getScheduleTermDates(broker).stream()
                    .filter(scheduleTermDate -> dateRange.isOverlap(
                            Range.of(scheduleTermDate.getStartDate(),
                                    scheduleTermDate.getEndDate())))
                    .min(Comparator.nullsLast(Comparator.comparing(ToolScheduleTermDate::getStartDate)));
            return termDate.orElse(null);
        }

        /**
         * Gets the last term date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Tool schedule term date
         */
        public ToolScheduleTermDate getLastTermDate(X2Broker broker, Range<Date> dateRange) {
            Optional<ToolScheduleTermDate> termDate = getScheduleTermDates(broker).stream()
                    .filter(scheduleTermDate -> dateRange.isOverlap(
                            Range.of(scheduleTermDate.getStartDate(),
                                    scheduleTermDate.getEndDate())))
                    .max(Comparator.nullsFirst(Comparator.comparing(ToolScheduleTermDate::getEndDate)));
            return termDate.orElse(null);
        }

        /**
         * Gets the master terms.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolMasterTerm> getMasterTerms(X2Broker broker) {
            return (List<ToolMasterTerm>) getChildren(broker, CHILD_MASTER_TERMS);
        }

        /**
         * Gets the primary staff.
         *
         * @param broker X2Broker
         * @return Tool staff
         */
        public ToolStaff getPrimaryStaff(X2Broker broker) {
            String stfOid = getValueString(FIELD_PRIMARY_STAFF_OID);
            return getBeanByOid(broker, ToolStaff.class, stfOid, true);
        }

        /**
         * Gets the rubric definition.
         *
         * @param broker the broker
         * @return the rubric definition
         */
        public ToolRubricDefinition getRubricDefinition(X2Broker broker) {
            String rbdOid = getValueString(FIELD_RUBRIC_DEFINITION_OID);
            return getBeanByOid(broker, ToolRubricDefinition.class, rbdOid, true);
        }

        /**
         * Gets the section number.
         *
         * @return String
         */
        public String getSectionNumber() {
            return getValueString(FIELD_SECTION_NUMBER);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSchedule getSchedule(X2Broker broker) {
            String schOid = getValueString(FIELD_SCHEDULE_OID);
            return getBeanByOid(broker, ToolSchedule.class, schOid, true);
        }

        /**
         * Gets the section schedule display.
         *
         * @return String
         */
        public String getScheduleDisplay() {
            return getValueString(FIELD_SCHEDULE_DISPLAY);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getScheduleOid() {
            return getValueString(FIELD_SCHEDULE_OID);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolScheduleTerm getScheduleTerm(X2Broker broker) {
            String trmOid = getValueString(FIELD_SCHEDULE_TERM_OID);
            return getBeanByOid(broker, ToolScheduleTerm.class, trmOid, true);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public List<ToolScheduleTerm> getScheduleTerms(X2Broker broker) {
            List<ToolScheduleTerm> terms = getMasterTerms(broker).stream()
                    .map(mtm -> mtm.getScheduleTerm(broker))
                    .distinct()
                    .collect(Collectors.toList());
            if (terms.isEmpty()) {
                ToolScheduleTerm term = getScheduleTerm(broker);
                if (term != null) {
                    terms = Arrays.asList(term);
                }
            }
            return terms;
        }

        /**
         * Gets the schedule term dates.
         *
         * @param broker X2Broker
         * @return Collection
         */
        public Collection<ToolScheduleTermDate> getScheduleTermDates(X2Broker broker) {
            return getScheduleTerms(broker).stream().flatMap(trm -> trm.getScheduleTermDates(broker).stream())
                    .collect(Collectors.toList());
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getScheduleTermOid() {
            return getValueString(FIELD_SCHEDULE_TERM_OID);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return Tool school
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the school course oid.
         *
         * @return String
         */
        public String getSchoolCourseOid() {
            return getValueString(FIELD_SCHOOL_COURSE_OID);
        }

        /**
         * Gets the section class.
         *
         * @param broker X2Broker
         * @return Tool schedule class
         */
        public ToolScheduleClass getSectionClass(X2Broker broker) {
            String clsOid = getValueString(FIELD_SECTION_CLASS_OID);
            return getBeanByOid(broker, ToolScheduleClass.class, clsOid, true);
        }

        /**
         * Gets the section class oid.
         *
         * @return String
         */
        public String getSectionClassOid() {
            return getValueString(FIELD_SECTION_CLASS_OID);
        }

        /**
         * Gets the section date range.
         *
         * @param broker X2Broker
         * @return Date range
         */
        public Range<Date> getSectionDateRange(X2Broker broker) {
            PlainDate termStart = null;
            PlainDate termEnd = null;
            Collection<ToolScheduleTermDate> termDates = getScheduleTermDates(broker);
            for (ToolScheduleTermDate termDate : termDates) {
                if (termStart == null || termStart.after(termDate.getStartDate())) {
                    termStart = termDate.getStartDate();
                }
                if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                    termEnd = termDate.getEndDate();
                }
            }
            // If a term is missing any dates, use school schedule dates or district calendar
            // dates.
            if (termStart == null) {
                if (getSchedule(broker) != null) {
                    termStart = getSchedule(broker).getStartDate();
                }
                if (termStart == null) {
                    termStart = getSchedule(broker).getDistrictContext(broker).getStartDate();
                }
            }
            if (termEnd == null) {
                if (getSchedule(broker) != null) {
                    termEnd = getSchedule(broker).getEndDate();
                }
                if (termEnd == null) {
                    termEnd = getSchedule(broker).getDistrictContext(broker).getEndDate();
                }
            }
            return Range.of(termStart, termEnd);
        }

        /**
         * Gets the section periods.
         *
         * @param broker X2Broker
         * @param tmd ToolScheduleTermDate
         * @param day ToolScheduleDay
         * @return Sets the
         */
        public Set<ToolSchedulePeriod> getSectionPeriods(X2Broker broker,
                                                         ToolScheduleTermDate tmd,
                                                         ToolScheduleDay day) {
            return getMasterTerms(broker).stream()
                    .filter(mtm -> tmd.getScheduleTermOid().equals(mtm.getScheduleTermOid())
                            && day.getOid().equals(mtm.getScheduleDayOid()))
                    .map(mtm -> mtm.getSchedulePeriod(broker)).filter(Objects::nonNull).distinct()
                    .collect(Collectors.toSet());
        }

        /**
         * Gets the section periods.
         *
         * @param broker X2Broker
         * @param csd ToolSchoolCalendarDate
         * @return Sets the
         */
        public Set<ToolSchedulePeriod> getSectionPeriods(X2Broker broker, ToolSchoolCalendarDate csd) {
            return getSectionPeriods(broker, csd, null);
        }

        /**
         * Gets the section periods.
         *
         * @param broker X2Broker
         * @param csd ToolSchoolCalendarDate
         * @param debugOutput
         * @return Sets the
         */
        public Set<ToolSchedulePeriod> getSectionPeriods(X2Broker broker,
                                                         ToolSchoolCalendarDate csd,
                                                         StringBuilder debugOutput) {
            if (debugOutput != null) {
                debugOutput.append("getSectionPeriods: " + this + "\n");
            }
            Set<ToolSchedulePeriod> periods = null;
            Collection<ToolScheduleTerm> terms = getMasterTerms(broker).stream().map(mtm -> mtm.getScheduleTerm(broker))
                    .filter(Objects::nonNull).distinct().collect(Collectors.toList());
            if (terms != null) {
                for (ToolScheduleTerm trm : terms) {
                    for (ToolScheduleTermDate tmd : trm.getScheduleTermDates(broker)) {
                        if (!csd.getDate().before(tmd.getStartDate()) && !csd.getDate().after(tmd.getEndDate())) {
                            ToolScheduleDay day = getSchedule(broker).getScheduleDays(broker).stream()
                                    .filter(sday -> sday.getNumber() == csd.getScheduleDayNumber()).findFirst()
                                    .orElse(null);
                            if (day == null) {
                                throw new X2RuntimeException(new UnsupportedOperationException(
                                        "No schedule day found for ["
                                                + csd.getSchoolCalendar(broker).getSchool(broker).getName()
                                                + "] calendar: [" +
                                                csd.getSchoolCalendar(broker).getCalendarId() + "] for date "
                                                + csd.getDate() + " Day Number [" + csd.getScheduleDayNumber()
                                                + "] from section " + getCourseView()));
                            }
                            periods = getSectionPeriods(broker, tmd, day);
                            if (debugOutput != null) {
                                debugOutput.append(
                                        "getSectionPeriods: " + csd.getDate() + " - Periods: " + periods + "\n");
                            }
                            break;
                        }
                    }
                    if (periods != null) {
                        break;
                    }
                }
            }
            return periods;
        }

        /**
         * Gets the start date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Plain date
         */
        public PlainDate getStartDate(X2Broker broker, Range<Date> dateRange) {
            final PlainDate startDate = getTermStartDate(broker, dateRange);
            if (startDate != null) {
                ToolSchoolCalendar calendar = getSchedule(broker).getCalendar(broker);
                List<ToolScheduleTerm> terms = getScheduleTerms(broker);
                if (calendar != null) {
                    Optional<ToolSchoolCalendarDate> inSessionDate =
                            terms.stream()
                                    .sorted(new Comparator<ToolScheduleTerm>() {
                                        @Override
                                        public int compare(ToolScheduleTerm term1, ToolScheduleTerm term2) {
                                            PlainDate date1 = term1.getStartDate(broker);
                                            PlainDate date2 = term2.getStartDate(broker);
                                            if (date1 == null) {
                                                return 1;
                                            }
                                            if (date2 == null) {
                                                return -1;
                                            }
                                            return date1.compareTo(date2);
                                        }

                                    })
                                    .flatMap(term -> calendar.getCalendarDatesForTerm(broker, term).stream())
                                    .filter(csd -> csd.getInSessionIndicator() &&
                                            !csd.getDate().before(startDate))
                                    .findFirst();
                    if (inSessionDate.isPresent()) {
                        return inSessionDate.get().getDate();
                    }
                }
            }
            return startDate;

        }

        /**
         * Gets the student period attendances.
         *
         * @param broker X2Broker
         * @return Filterable
         */
        public Filterable<ToolStudentPeriodAttendance> getStudentPeriodAttendances(X2Broker broker) {
            return (Filterable<ToolStudentPeriodAttendance>) getChildrenFilterable(broker,
                    CHILD_STUDENT_PERIOD_ATTENDANCES);
        }

        /**
         * Gets the student schedules.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentSchedule> getStudentSchedules(X2Broker broker) {
            return (List<ToolStudentSchedule>) getChildren(broker, CHILD_STUDENT_SCHEDULES);
        }

        /**
         * Gets the staff view.
         *
         * @return String
         */
        public String getStaffView() {
            return getValueString(FIELD_STAFF_VIEW);
        }

        /**
         * Gets the student schedule changes.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentScheduleChange> getStudentScheduleChanges(X2Broker broker) {
            return (List<ToolStudentScheduleChange>) getChildren(broker, CHILD_STUDENT_SCHEDULE_CHANGES);
        }

        /**
         * Returns the teacherSections.
         * <p>
         * The data dictionary ID for this property is <code>relMstMtcOid</code>.
         *
         * @param broker X2Broker
         * @return Collection of ScheduleTeacher objects
         */
        public Collection<ToolScheduleTeacher> getTeacherSections(X2Broker broker) {
            return (List<ToolScheduleTeacher>) getChildren(broker, CHILD_TEACHER_SCHEDULES);
        }

        /**
         * Gets the term end date.
         *
         * @param broker the broker
         * @return the term end date
         */
        public PlainDate getTermEndDate(X2Broker broker) {
            return getScheduleTerms(broker).stream()
                    .map(trm -> trm.getEndDate(broker))
                    .max(Comparator.nullsFirst(Comparator.naturalOrder()))
                    .orElse(null);
        }

        /**
         * Gets the term end date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Plain date
         */
        public PlainDate getTermEndDate(X2Broker broker, Range<Date> dateRange) {
            PlainDate endDate = null;
            ToolScheduleTermDate tmd = getLastTermDate(broker, dateRange);
            if (tmd != null) {
                endDate = tmd.getEndDate();
            }
            return endDate;
        }

        /**
         * Gets the term start date.
         *
         * @param broker the broker
         * @return the term start date
         */
        public PlainDate getTermStartDate(X2Broker broker) {
            return getScheduleTerms(broker).stream()
                    .map(trm -> trm.getStartDate(broker))
                    .min(Comparator.nullsFirst(Comparator.naturalOrder()))
                    .orElse(null);
        }

        /**
         * Gets the term start date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Plain date
         */
        public PlainDate getTermStartDate(X2Broker broker, Range<Date> dateRange) {
            PlainDate startDate = null;
            ToolScheduleTermDate tmd = getFirstTermDate(broker, dateRange);
            if (tmd != null) {
                startDate = tmd.getStartDate();
            }
            return startDate;
        }

        /**
         * Gets the term view.
         *
         * @return String
         */
        public String getTermView() {
            return getValueString(FIELD_TERM_VIEW);
        }

        /**
         * Gets the total number periods.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param broker X2Broker
         * @return int
         */
        public int getTotalNumberPeriods(PlainDate startDate, PlainDate endDate, X2Broker broker) {
            return getTotalNumberPeriods(startDate, endDate, broker, null);
        }

        /**
         * Gets the total number periods.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param broker X2Broker
         * @param debugOutput
         * @return int
         */
        public int getTotalNumberPeriods(PlainDate startDate,
                                         PlainDate endDate,
                                         X2Broker broker,
                                         StringBuilder debugOutput) {
            return getCalendarDates(startDate, endDate, broker).stream()
                    .filter(csd -> csd.getInSessionIndicator())
                    .map(csd -> getSectionPeriods(broker, csd, debugOutput))
                    .mapToInt(patSet -> patSet == null ? 0 : patSet.size()).sum();
        }

        /**
         * Gets the student schedule spans.
         *
         * @param broker X2Broker
         * @param range the range
         * @param checkEnrollment the check enrollment
         * @return List
         */
        public List<Pair<ToolStudent, List<StudentScheduleSpan>>> getStudentsWithScheduleSpans(X2Broker broker,
                                                                                               Range<Date> range,
                                                                                               boolean checkEnrollment) {
            if (m_scheduleSpans == null) {
                m_scheduleSpans = new ArrayList();
                Map<String, List<ToolStudentSchedule>> schedulesByStudent =
                        getStudentSchedules(broker).stream().collect(Collectors.groupingBy(ssc -> ssc.getStudentOid()));
                Map<String, List<ToolStudentScheduleChange>> changesByStudent =
                        getStudentScheduleChanges(broker).stream()
                                .collect(Collectors.groupingBy(ssc -> ssc.getStudentOid()));
                String schoolOid = getSchedule(broker).getSchoolOid();
                List<String> studentOids = Stream
                        .concat(schedulesByStudent.keySet().stream(), changesByStudent.keySet().stream()).distinct()
                        .filter(stdOid -> ToolBean.isCached(ToolStudent.class, stdOid, true))
                        .collect(Collectors.toList());

                PlainDate queryAsOfDate =
                        (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);
                for (String studentOid : studentOids) {
                    List<ToolStudentSchedule> sscList = schedulesByStudent.get(studentOid);
                    ToolStudentSchedule ssc = sscList == null || sscList.isEmpty() ? null : sscList.get(0);
                    ToolStudent student = ToolBean.getBeanByOid(ToolStudent.class, studentOid, true);
                    List<StudentScheduleSpan> spans =
                            student.getStudentScheduleSpans(broker, ssc, changesByStudent.get(studentOid))
                                    .stream()
                                    .filter(span -> {
                                        Range<Date> spanDateRange = Range.of(span.getEntryDate(), span.getExitDate());
                                        if (spanDateRange.isOverlap(range)) {
                                            if (checkEnrollment) {
                                                if (student.getParentSpans(broker, false, false).stream()
                                                        .map(parent -> parent.getAnnualSpans().stream())
                                                        .flatMap(spanStream -> spanStream
                                                                .filter(testSpan -> queryAsOfDate == null
                                                                        || !testSpan.getSpanStartDate()
                                                                                .after(queryAsOfDate))
                                                                .filter(testSpan -> {
                                                                    if (!schoolOid
                                                                            .equals(testSpan.getSchool().getOid())) {
                                                                        return false;
                                                                    }
                                                                    Range<Date> enrollDateRange =
                                                                            Range.of(testSpan.getSpanStartDate(),
                                                                                    testSpan.getSpanEndDate());
                                                                    return enrollDateRange.isOverlap(range)
                                                                            && enrollDateRange.isOverlap(spanDateRange);
                                                                }))
                                                        .findAny().isPresent()) {
                                                    return true;
                                                }
                                            } else {
                                                return true;
                                            }
                                        }
                                        return false;
                                    })
                                    .collect(Collectors.toList());
                    if (!spans.isEmpty()) {
                        m_scheduleSpans.add(Pair.of(student, spans));
                    }
                }
            }
            return m_scheduleSpans;
        }

        /**
         * Are students enrolled.
         *
         * @param broker X2Broker
         * @param range Range<Date>
         * @param checkEnrollment boolean
         * @return true, if successful
         */
        public boolean isStudentEnrolled(X2Broker broker, Range<Date> range, boolean checkEnrollment) {
            if (m_isStudentEnrolledMap == null) {
                m_isStudentEnrolledMap = new HashMap();
            }
            String key = range.toString() + checkEnrollment;
            if (!m_isStudentEnrolledMap.containsKey(key)) {

                Range<Date> combinedRange = getDateRange(broker).intersection(range);

                boolean returnValue = false;
                Map<String, List<ToolStudentSchedule>> schedulesByStudent =
                        getStudentSchedules(broker).stream().collect(Collectors.groupingBy(ssc -> ssc.getStudentOid()));
                Map<String, List<ToolStudentScheduleChange>> changesByStudent =
                        getStudentScheduleChanges(broker).stream()
                                .collect(Collectors.groupingBy(ssc -> ssc.getStudentOid()));
                String schoolOid = getSchedule(broker).getSchoolOid();
                List<String> studentOids = Stream
                        .concat(schedulesByStudent.keySet().stream(), changesByStudent.keySet().stream()).distinct()
                        .filter(stdOid -> ToolBean.isCached(ToolStudent.class, stdOid, true))
                        .collect(Collectors.toList());

                PlainDate queryAsOfDate =
                        (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);
                for (String studentOid : studentOids) {
                    List<ToolStudentSchedule> sscList = schedulesByStudent.get(studentOid);
                    ToolStudentSchedule ssc = sscList == null || sscList.isEmpty() ? null : sscList.get(0);
                    ToolStudent student = ToolBean.getBeanByOid(ToolStudent.class, studentOid, true);
                    List<StudentScheduleSpan> spans =
                            student.getStudentScheduleSpans(broker, ssc, changesByStudent.get(studentOid));
                    for (StudentScheduleSpan span : spans) {
                        Range<Date> spanDateRange = Range.of(span.getEntryDate(), span.getExitDate());
                        if (combinedRange != null && spanDateRange.isOverlap(combinedRange)) {
                            if (checkEnrollment) {
                                if (student.getParentSpans(broker, false, false).stream()
                                        .map(parent -> parent.getAnnualSpans().stream())
                                        .flatMap(spanStream -> spanStream
                                                .filter(testSpan -> queryAsOfDate == null
                                                        || !testSpan.getSpanStartDate().after(queryAsOfDate))
                                                .filter(testSpan -> {
                                                    if (!schoolOid.equals(testSpan.getSchool().getOid())) {
                                                        return false;
                                                    }
                                                    Range<Date> enrollDateRange =
                                                            Range.of(testSpan.getSpanStartDate(),
                                                                    testSpan.getSpanEndDate());
                                                    return enrollDateRange.isOverlap(combinedRange)
                                                            && enrollDateRange.isOverlap(spanDateRange);
                                                }))
                                        .findAny().isPresent()) {
                                    returnValue = true;
                                    break;
                                }
                            } else {
                                returnValue = true;
                                break;
                            }
                        }
                    }
                    if (returnValue) {
                        break;
                    }
                }

                m_isStudentEnrolledMap.put(key, Boolean.valueOf(returnValue));
            }
            return m_isStudentEnrolledMap.get(key);
        }
    }

    /**
     * The Class ToolStaff.
     */
    public static class ToolStaff extends ToolBean {
        public static final ToolBeanColumn FIELD_EMAIL_01 =
                new ToolBeanColumn(SisBeanPaths.STAFF.person().email01());
        public static final ToolBeanColumn FIELD_FIRST_NAME =
                new ToolBeanColumn(SisBeanPaths.STAFF.person().firstName());
        public static final ToolBeanColumn FIELD_LAST_NAME =
                new ToolBeanColumn(SisBeanPaths.STAFF.person().lastName());
        public static final ToolBeanColumn FIELD_LOCAL_ID =
                new ToolBeanColumn(SisBeanPaths.STAFF.localId());
        public static final ToolBeanColumn FIELD_NAME_VIEW =
                new ToolBeanColumn(SisBeanPaths.STAFF.nameView());
        public static final ToolBeanColumn FIELD_STAFF_TYPE =
                new ToolBeanColumn(SisBeanPaths.STAFF.staffType());

        public static ToolBeanRelationship CHILD_SCHEDULE_TEACHERS =
                new ToolBeanRelationship(SisBeanPaths.STAFF.scheduleTeachers().getBeanType(),
                        SisBeanPaths.STAFF.scheduleTeachers().getValueType(),
                        SisBeanPaths.STAFF.scheduleTeachers().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER_TEACHER.staffOid().toString(),
                        SisBeanPaths.STAFF.scheduleTeachers().getRelationshipType());

        public static ToolBeanRelationship CHILD_STAFF_POSITIONS =
                new ToolBeanRelationship(SisBeanPaths.STAFF.staffPositions().getBeanType(),
                        SisBeanPaths.STAFF.staffPositions().getValueType(),
                        SisBeanPaths.STAFF.staffPositions().getPath(),
                        SisBeanPaths.STAFF_POSITION.staffOid().toString(),
                        SisBeanPaths.STAFF.staffPositions().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_EMAIL_01,
                        FIELD_FIRST_NAME,
                        FIELD_LAST_NAME,
                        FIELD_LOCAL_ID,
                        FIELD_NAME_VIEW,
                        FIELD_STAFF_TYPE)
                .expandRelationships(CHILD_SCHEDULE_TEACHERS,
                        CHILD_STAFF_POSITIONS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STAFF.getBeanType();
        }

        /**
         * Instantiates a new tool staff.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolStaff(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the email.
         *
         * @return String
         */
        public String getEmail01() {
            return getValueString(FIELD_EMAIL_01);
        }

        /**
         * Gets the first name.
         *
         * @return String
         */
        public String getFirstName() {
            return getValueString(FIELD_FIRST_NAME);
        }

        /**
         * Gets the last name.
         *
         * @return String
         */
        public String getLastName() {
            return getValueString(FIELD_LAST_NAME);
        }

        /**
         * Gets the local id.
         *
         * @return String
         */
        public String getLocalId() {
            return getValueString(FIELD_LOCAL_ID);
        }

        /**
         * Gets the name view.
         *
         * @return String
         */
        public String getNameView() {
            return getValueString(FIELD_NAME_VIEW);
        }

        /**
         * Gets the staff positions.
         *
         * @param broker X2Broker
         * @return Collection
         */
        public Collection<ToolScheduleTeacher> getScheduleTeachers(X2Broker broker) {
            return (List<ToolScheduleTeacher>) getChildren(broker, CHILD_SCHEDULE_TEACHERS);
        }

        /**
         * Gets the staff positions.
         *
         * @param broker X2Broker
         * @return Collection
         */
        public Collection<ToolStaffPosition> getStaffPositions(X2Broker broker) {
            return (List<ToolStaffPosition>) getChildren(broker, CHILD_STAFF_POSITIONS);
        }

        /**
         * Returns the Type.
         * <p>
         * The data dictionary ID for this property is <code>stfStaffType</code>.
         *
         * @return String
         */
        public String getStaffType() {
            return getValueString(FIELD_STAFF_TYPE);
        }

    }


    /**
     * The Class ToolStaffPosition.
     */
    public static class ToolStaffPosition extends ToolBean {
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.endDate());
        public static final ToolBeanColumn FIELD_FTE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.fte());
        public static final ToolBeanColumn FIELD_JOB_CODE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.jobCode());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.schoolOid());
        public static final ToolBeanColumn FIELD_STAFF_OID =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.staffOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.startDate());

        public static ToolBeanRelationship PARENT_STAFF =
                new ToolBeanRelationship(SisBeanPaths.STAFF_POSITION.staff().getBeanType(),
                        SisBeanPaths.STAFF_POSITION.staff().getValueType(),
                        SisBeanPaths.STAFF_POSITION.staffOid().toString(),
                        SisBeanPaths.STAFF.staffPositions().getPath(),
                        SisBeanPaths.STAFF_POSITION.staff().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_END_DATE,
                        FIELD_FTE,
                        FIELD_JOB_CODE,
                        FIELD_SCHOOL_OID,
                        FIELD_STAFF_OID,
                        FIELD_START_DATE)
                .expandRelationships(PARENT_STAFF);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STAFF_POSITION.getBeanType();
        }

        /**
         * Instantiates a new tool staff position.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolStaffPosition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the End date.
         * <p>
         * The data dictionary ID for this property is <code>sfpEndDate</code>.
         *
         * @return PlainDate
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Returns the Full time equivalent.
         * <p>
         * The data dictionary ID for this property is <code>sfpFTE</code>.
         *
         * @return BigDecimal
         */
        public BigDecimal getFte() {
            return getValueBigDecimal(FIELD_FTE);
        }

        /**
         * Gets the job code.
         *
         * @return String
         */
        public String getJobCode() {
            return getValueString(FIELD_JOB_CODE);
        }

        /**
         * Gets the staff.
         *
         * @param broker X2Broker
         * @return Staff
         */
        public ToolStaff getStaff(X2Broker broker) {
            String stfOid = getValueString(FIELD_STAFF_OID);
            return getBeanByOid(broker, ToolStaff.class, stfOid, true);
        }

        /**
         * Gets the staff oid.
         *
         * @return String
         */
        public String getStaffOid() {
            return getValueString(FIELD_STAFF_OID);
        }

        /**
         * Returns the Start date.
         * <p>
         * The data dictionary ID for this property is <code>sfpStartDate</code>.
         *
         * @return PlainDate
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

    }

    /**
     * The Class Student.
     */
    public static class ToolStudent extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_CALENDAR_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.calendarCode());
        public static final ToolBeanColumn FIELD_DOB =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().dob());
        public static final ToolBeanColumn FIELD_HOMEROOM =
                new ToolBeanColumn(SisBeanPaths.STUDENT.homeroom());
        public static final ToolBeanColumn FIELD_ENROLLMENT_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT.enrollmentStatus());
        public static final ToolBeanColumn FIELD_GRADE_LEVEL =
                new ToolBeanColumn(SisBeanPaths.STUDENT.gradeLevel());
        public static final ToolBeanColumn FIELD_LOCAL_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.localId());
        public static final ToolBeanColumn FIELD_MAILING_ADDRESS_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().mailingAddressOid());
        public static final ToolBeanColumn FIELD_NAME_VIEW =
                new ToolBeanColumn(SisBeanPaths.STUDENT.nameView());
        public static final ToolBeanColumn FIELD_PERSON_FIRST_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().firstName());
        public static final ToolBeanColumn FIELD_PERSON_LAST_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().lastName());
        public static final ToolBeanColumn FIELD_PERSON_MIDDLE_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().middleName());
        public static final ToolBeanColumn FIELD_PERSON_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.personOid());
        public static final ToolBeanColumn FIELD_PHONE_1 =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().phone01());
        public static final ToolBeanColumn FIELD_PHYSICAL_ADDRESS_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().physicalAddressOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.schoolOid());
        public static final ToolBeanColumn FIELD_SPED_LAST_EVALUATION_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.spedLastEvaluationDate());
        public static final ToolBeanColumn FIELD_STATE_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.stateId());
        public static final ToolBeanColumn FIELD_YOG =
                new ToolBeanColumn(SisBeanPaths.STUDENT.yog());

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_SCHOOL_ARCHIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT.school().archiveIndicator());
        public static final ToolBeanColumn FIELD_SCHOOL_INACTIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT.school().inactiveIndicator());

        public static ToolBeanRelationship CHILD_PERSON_ADDRESSES =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.person().addresses().getBeanType(),
                        SisBeanPaths.STUDENT.person().addresses().getValueType(),
                        SisBeanPaths.STUDENT.person().addresses().getPath(),
                        SisBeanPaths.PERSON_TO_ADDRESS.personOid().getPath(),
                        SisBeanPaths.STUDENT.person().addresses().getRelationshipType())
                                .addParentKeyFunction(new Function<ToolBean, String>() {
                                    @Override
                                    public String apply(ToolBean t) {
                                        ToolStudent student = (ToolStudent) t;
                                        return student.getPersonOid();
                                    }

                                }).addComparator(new Comparator<ToolBean>() {
                                    @Override
                                    public int compare(ToolBean o1, ToolBean o2) {
                                        ToolPersonAddress addr1, addr2;
                                        addr1 = (ToolPersonAddress) o1;
                                        addr2 = (ToolPersonAddress) o2;
                                        // sort descending
                                        int result = nullComparator.compare(addr2.getStartDate(), addr1.getStartDate());
                                        if (result == 0) {
                                            result = nullComparator.compare(addr2.getEndDate(), addr1.getEndDate());
                                        }
                                        return result;
                                    }
                                });
        public static ToolBeanRelationship CHILD_PERSON_RACES =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.person().races().getBeanType(),
                        SisBeanPaths.STUDENT.person().races().getValueType(),
                        SisBeanPaths.STUDENT.person().races().getPath(),
                        SisBeanPaths.PERSON_TO_ADDRESS.personOid().getPath(),
                        SisBeanPaths.STUDENT.person().races().getRelationshipType())
                                .addParentKeyFunction(new Function<ToolBean, String>() {
                                    @Override
                                    public String apply(ToolBean t) {
                                        ToolStudent student = (ToolStudent) t;
                                        return student.getPersonOid();
                                    }

                                });
        public static ToolBeanRelationship CHILD_STUDENT_ATTENDANCE =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.studentAttendance().getBeanType(),
                        SisBeanPaths.STUDENT.studentAttendance().getValueType(),
                        SisBeanPaths.STUDENT.studentAttendance().getPath(),
                        SisBeanPaths.STUDENT_ATTENDANCE.studentOid().getPath(),
                        SisBeanPaths.STUDENT.studentAttendance().getRelationshipType())
                                .addComparator(new Comparator<ToolBean>() {
                                    @Override
                                    public int compare(ToolBean o1, ToolBean o2) {
                                        ToolStudentAttendance cdate1, cdate2;
                                        cdate1 = (ToolStudentAttendance) o1;
                                        cdate2 = (ToolStudentAttendance) o2;
                                        PlainDate date1 = cdate1.getDate();
                                        PlainDate date2 = cdate2.getDate();
                                        if (date1 == null) {
                                            return -1;
                                        } else if (date2 == null) {
                                            return 1;
                                        }
                                        return date1.compareTo(date2);
                                    }
                                });
        public static ToolBeanRelationship CHILD_STUDENT_CONDUCT_ACTIONS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.conductActions().getBeanType(),
                        SisBeanPaths.STUDENT.conductActions().getValueType(),
                        SisBeanPaths.STUDENT.conductActions().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_ACTION.studentOid().getPath(),
                        SisBeanPaths.STUDENT.conductActions().getRelationshipType())
                                .addComparator(new Comparator<ToolBean>() {
                                    @Override
                                    public int compare(ToolBean o1, ToolBean o2) {
                                        ToolConductAction ca1, ca2;
                                        ca1 = (ToolConductAction) o1;
                                        ca2 = (ToolConductAction) o2;
                                        PlainDate date1 = ca1.getActionStartDate();
                                        PlainDate date2 = ca2.getActionStartDate();
                                        if (date1 == null) {
                                            return -1;
                                        } else if (date2 == null) {
                                            return 1;
                                        }
                                        return date1.compareTo(date2);
                                    }
                                });
        public static ToolBeanRelationship CHILD_STUDENT_CONTEXT_ATTRIBUTES =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.contextAttributes().getBeanType(),
                        SisBeanPaths.STUDENT.contextAttributes().getValueType(),
                        SisBeanPaths.STUDENT.contextAttributes().getPath(),
                        SisBeanPaths.STUDENT_CONTEXT_ATTRIBUTES.studentOid().getPath(),
                        SisBeanPaths.STUDENT.contextAttributes().getRelationshipType());
        public static ToolBeanRelationship CHILD_STUDENT_ENROLLMENTS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.enrollments().getBeanType(),
                        SisBeanPaths.STUDENT.enrollments().getValueType(),
                        SisBeanPaths.STUDENT.enrollments().getPath(),
                        SisBeanPaths.STUDENT_ENROLLMENT.studentOid().getPath(),
                        SisBeanPaths.STUDENT.enrollments().getRelationshipType());
        public static ToolBeanRelationship CHILD_STUDENT_SCHEDULES =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.studentSchedules().getBeanType(),
                        SisBeanPaths.STUDENT.studentSchedules().getValueType(),
                        SisBeanPaths.STUDENT.studentSchedules().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE.studentOid().getPath(),
                        SisBeanPaths.STUDENT.studentSchedules().getRelationshipType());
        public static ToolBeanRelationship CHILD_STUDENT_SCHEDULE_CHANGES =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.studentScheduleChanges().getBeanType(),
                        SisBeanPaths.STUDENT.studentScheduleChanges().getValueType(),
                        SisBeanPaths.STUDENT.studentScheduleChanges().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.studentOid().getPath(),
                        SisBeanPaths.STUDENT.studentScheduleChanges().getRelationshipType());
        public static ToolBeanRelationship CHILD_STUDENT_SCHOOLS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.studentSchools().getBeanType(),
                        SisBeanPaths.STUDENT.studentSchools().getValueType(),
                        SisBeanPaths.STUDENT.studentSchools().getPath(),
                        SisBeanPaths.STUDENT_SCHOOL.studentOid().getPath(),
                        SisBeanPaths.STUDENT.studentSchools().getRelationshipType());
        public static ToolBeanRelationship CHILD_TRANSCRIPT_RUBRICS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.transcripts().transcriptRubrics().getBeanType(),
                        SisBeanPaths.STUDENT.transcripts().transcriptRubrics().getValueType(),
                        SisBeanPaths.STUDENT.transcripts().transcriptRubrics().getPath(),
                        SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().studentOid().getPath(),
                        SisBeanPaths.STUDENT.transcripts().transcriptRubrics().getRelationshipType());
        public static ToolBeanRelationship CHILD_TRANSCRIPTS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.transcripts().getBeanType(),
                        SisBeanPaths.STUDENT.transcripts().getValueType(),
                        SisBeanPaths.STUDENT.transcripts().getPath(),
                        SisBeanPaths.STUDENT_TRANSCRIPT.studentOid().getPath(),
                        SisBeanPaths.STUDENT.transcripts().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expandKeys(FIELD_PERSON_OID)
                .expand(FIELD_CALENDAR_CODE,
                        FIELD_DOB,
                        FIELD_HOMEROOM,
                        FIELD_ENROLLMENT_STATUS,
                        FIELD_GRADE_LEVEL,
                        FIELD_LOCAL_ID,
                        FIELD_MAILING_ADDRESS_OID,
                        FIELD_NAME_VIEW,
                        FIELD_PERSON_FIRST_NAME,
                        FIELD_PERSON_LAST_NAME,
                        FIELD_PERSON_MIDDLE_NAME,
                        FIELD_PHONE_1,
                        FIELD_PHYSICAL_ADDRESS_OID,
                        FIELD_SCHOOL_OID,
                        FIELD_SPED_LAST_EVALUATION_DATE,
                        FIELD_STATE_ID,
                        FIELD_YOG)
                .expandRelationships(
                        CHILD_PERSON_ADDRESSES,
                        CHILD_PERSON_RACES,
                        CHILD_STUDENT_ATTENDANCE,
                        CHILD_STUDENT_CONDUCT_ACTIONS,
                        CHILD_STUDENT_CONTEXT_ATTRIBUTES,
                        CHILD_STUDENT_ENROLLMENTS,
                        CHILD_STUDENT_SCHEDULES,
                        CHILD_STUDENT_SCHEDULE_CHANGES,
                        CHILD_STUDENT_SCHOOLS,
                        CHILD_TRANSCRIPT_RUBRICS,
                        CHILD_TRANSCRIPTS);


        private static final NullComparator nullComparator = new NullComparator();

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        protected Map<String, List<AnnualSpan>> m_enrollmentSpans;
        private Map<String, List<AspenSpan>> m_aspenSpans;
        private Map<String, List<ParentSpan>> m_parentSpans;
        private List<StudentScheduleSpan> m_scheduleSpans;
        private List<AnnualSpan> m_secondarySpans;
        private List<ValidationError> m_validationErrors;

        /**
         * Gets the aspen spans.
         *
         * @param broker X2Broker
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         */
        public List<AspenSpan> getAspenSpans(X2Broker broker, boolean isBreakOnYog, boolean isBreakOnStatus) {
            String key = Boolean.toString(isBreakOnYog) + Boolean.toString(isBreakOnStatus);
            if (m_aspenSpans == null) {
                m_aspenSpans = new HashMap();
            }

            List<AspenSpan> spans = m_aspenSpans.get(key);

            if (spans == null) {
                spans = new ArrayList<>();
                List<ToolEnrollment> allEnrollments = getEnrollments(broker);
                if (allEnrollments != null) {
                    List<? extends ToolEnrollment> swappedEnrollments = swapEnrollments(allEnrollments);

                    List<List<? extends ToolEnrollment>> schoolRanges = getSchoolRanges(broker, swappedEnrollments);
                    for (List<? extends ToolEnrollment> schoolRange : schoolRanges) {
                        spans.addAll(
                                getAspenSpansOneSchoolRange(broker, schoolRange, isBreakOnYog, isBreakOnStatus));
                    }

                    Collections.sort(spans);

                    updateTerminatingEnrollments(spans, swappedEnrollments);
                }
                m_aspenSpans.put(key, spans);
            }
            return spans;
        }

        /**
         * Gets the attendances.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentAttendance> getAttendance(X2Broker broker) {
            return (List<ToolStudentAttendance>) getChildren(broker, CHILD_STUDENT_ATTENDANCE);
        }

        /**
         * Gets the conduct actions.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolConductAction> getConductActions(X2Broker broker) {
            return (List<ToolConductAction>) getChildren(broker, CHILD_STUDENT_CONDUCT_ACTIONS);
        }

        /**
         * Gets the calendar code.
         *
         * @return String
         */
        public String getCalendarCode() {
            return getValueString(FIELD_CALENDAR_CODE);
        }

        /**
         * Gets the date of birth.
         *
         * @return PlainDate
         */
        public PlainDate getDob() {
            return getValueDate(FIELD_DOB);
        }

        /**
         * Gets the student schedules.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolEnrollment> getEnrollments(X2Broker broker) {
            return (List<ToolEnrollment>) getChildren(broker, CHILD_STUDENT_ENROLLMENTS);
        }

        /**
         * Gets the enrollment spans.
         *
         * @param broker X2Broker
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         */
        public List<AnnualSpan> getEnrollmentSpans(X2Broker broker, boolean isBreakOnYog, boolean isBreakOnStatus) {
            PlainDate queryAsOfDate =
                    (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);
            return getEnrollmentSpans(broker, queryAsOfDate, isBreakOnYog, isBreakOnStatus);
        }

        /**
         * Gets the enrollment spans.
         *
         * @param broker X2Broker
         * @param queryAsOfDate PlainDate
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         */
        public List<AnnualSpan> getEnrollmentSpans(X2Broker broker,
                                                   PlainDate queryAsOfDate,
                                                   boolean isBreakOnYog,
                                                   boolean isBreakOnStatus) {
            String key = Boolean.toString(isBreakOnYog) + Boolean.toString(isBreakOnStatus) + queryAsOfDate;
            if (m_enrollmentSpans == null) {
                m_enrollmentSpans = new HashMap();
            }

            List<AnnualSpan> spans = m_enrollmentSpans.get(key);

            if (spans == null) {
                List<AspenSpan> aspenSpans = getAspenSpans(broker, isBreakOnYog, isBreakOnStatus);
                spans = splitSpansIntoContexts(broker, aspenSpans, queryAsOfDate, isBreakOnYog);
                spans.addAll(getSecondarySpans(broker));

                Collections.sort(spans);

                if (queryAsOfDate != null) {
                    spans = spans.stream()
                            .filter(span -> !span.getSpanStartDate().after(queryAsOfDate))
                            .collect(Collectors.toList());
                }
                m_enrollmentSpans.put(key, spans);
            }
            return spans;
        }

        /**
         * Gets the enrollment status.
         *
         * @return String
         */
        public String getEnrollmentStatus() {
            return getValueString(FIELD_ENROLLMENT_STATUS);
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return getValueString(FIELD_GRADE_LEVEL);
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevelState() {
            return getValueReferenceState(FIELD_GRADE_LEVEL);
        }

        /**
         * Gets the homeroom.
         *
         * @return String
         */
        public String getHomeroom() {
            return getValueString(FIELD_HOMEROOM);
        }

        /**
         * Gets the local id.
         *
         * @return String
         */
        public String getLocalId() {
            return getValueString(FIELD_LOCAL_ID);
        }

        /**
         * Gets the mailing address.
         *
         * @param broker X2Broker
         * @return String
         */
        public ToolAddress getMailingAddress(X2Broker broker) {
            String adrOid = getValueString(FIELD_MAILING_ADDRESS_OID);
            return getBeanByOid(broker, ToolAddress.class, adrOid, true);
        }

        /**
         * Returns the total number of membership days for the given student from the start date to
         * the
         * end date, inclusive. Entry and withdrawal days are counted towards the total according to
         * the
         * values of the EntryIsMemberDay and WithdrawalIsMemberDay properties.
         *
         * @param broker X2Broker
         * @param sessionDays the set of PlainDate objects that defines the in-session days
         *        for the current student's school and calendar
         * @param isInitiallyMember if true then the student is initially considered a member
         * @param dateRange the date range, inclusive
         * @param school if not null then membership will be considered for just the given
         *        school, otherwise membership will be considered for the entire
         *        district
         * @return int
         */
        public int getMembershipTotal(X2Broker broker,
                                      Set sessionDays,
                                      boolean isInitiallyMember,
                                      Range<Date> dateRange,
                                      ToolSchool school) {
            int totalMembership = 0;

            if (sessionDays != null && dateRange.getStart() != null && dateRange.getEnd() != null) {

                List<ToolEnrollment> enrollments = getEnrollments(broker).stream()
                        .filter(enr -> (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType()) ||
                                StudentEnrollment.WITHDRAWAL.equals(enr.getEnrollmentType())) &&
                                dateRange.contains(enr.getEnrollmentDate()) &&
                                school.getOid().equals(enr.getSchoolOid()))
                        .sorted(new Comparator<ToolEnrollment>() {

                            @Override
                            public int compare(ToolEnrollment enr1, ToolEnrollment enr2) {
                                int value = enr1.getEnrollmentDate().compareTo(enr2.getEnrollmentDate());
                                if (value == 0) {
                                    value = Long.valueOf(enr1.getTimestamp())
                                            .compareTo(Long.valueOf(enr2.getTimestamp()));
                                }
                                return value;
                            }
                        }).collect(Collectors.toList());

                Calendar calendar = Calendar.getInstance(Locale.getDefault());
                calendar.setTime(dateRange.getStart());
                boolean isMemberToday = isInitiallyMember;

                while (!calendar.getTime().after(dateRange.getEnd())) {
                    boolean isMemberTomorrow = isMemberToday;

                    if (!enrollments.isEmpty()) {
                        ToolEnrollment topEnrollmentRecord = enrollments.get(0);
                        while (topEnrollmentRecord.getEnrollmentDate().equals(calendar.getTime())) {
                            if (topEnrollmentRecord.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                                isMemberToday = DistrictManager.isMemberOnWithdrawal(broker);
                                isMemberTomorrow = false;
                            } else {
                                isMemberToday = DistrictManager.isMemberOnEntry(broker);
                                isMemberTomorrow = true;
                            }

                            enrollments.remove(0);

                            if (enrollments.isEmpty()) {
                                break;
                            }

                            topEnrollmentRecord = enrollments.get(0);
                        }
                    }

                    if (sessionDays.contains(calendar.getTime())) {
                        if (isMemberToday) {
                            totalMembership++;
                        }
                    }

                    calendar.add(Calendar.DATE, 1);
                    isMemberToday = isMemberTomorrow;
                }
            }

            return totalMembership;
        }

        /**
         * Gets the name view.
         *
         * @return String
         */
        public String getNameView() {
            return getValueString(FIELD_NAME_VIEW);
        }

        /**
         * Gets the parent spans.
         *
         * @param broker X2Broker
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         */
        public List<ParentSpan> getParentSpans(X2Broker broker, boolean isBreakOnYog, boolean isBreakOnStatus) {
            String key = Boolean.toString(isBreakOnYog) + Boolean.toString(isBreakOnStatus);
            if (m_parentSpans == null) {
                m_parentSpans = new HashMap();
            }

            List<ParentSpan> spans = m_parentSpans.get(key);

            if (spans == null) {
                spans = new ArrayList<>();
                // when splitting spans into parent spans, always set yogBreak and statusBreak to
                // false
                List<AspenSpan> aspenSpans = getAspenSpans(broker, false, false);

                for (AspenSpan aspenSpan : aspenSpans) {
                    ParentSpan parentSpan = new ParentSpan();
                    parentSpan.setStudent(this);
                    parentSpan.setSchool(aspenSpan.getSchool(broker));
                    parentSpan.setEnrollments(aspenSpan.getAscendingEnrollments());
                    parentSpan.setAnnualSpans(
                            splitSpansIntoContexts(broker, Arrays.asList(aspenSpan), null, isBreakOnYog));

                    // Same-day E/W results in zero annual spans - don't keep the parent span
                    if (parentSpan.getAnnualSpans() == null || parentSpan.getAnnualSpans().isEmpty()) {
                        continue;
                    }

                    // set m_parentSpan in each child annual span
                    if (parentSpan.getAnnualSpans() != null) {
                        for (AnnualSpan annualSpan : parentSpan.getAnnualSpans()) {
                            annualSpan.setParentSpan(parentSpan);
                        }
                        spans.add(parentSpan);
                    }
                }

                Collection<AnnualSpan> secondarySpans = getSecondarySpans(broker);
                for (AnnualSpan secondarySpan : secondarySpans) {
                    ParentSpan parentSpan = new ParentSpan();
                    parentSpan.setStudent(secondarySpan.getStudent());
                    parentSpan.setSchool(secondarySpan.getSchool());
                    parentSpan.setEnrollments(null);
                    parentSpan.setAnnualSpans(Arrays.asList(secondarySpan));
                    if (parentSpan.getAnnualSpans() != null && !parentSpan.getAnnualSpans().isEmpty()) {
                        for (AnnualSpan annualSpan : parentSpan.getAnnualSpans()) {
                            annualSpan.setParentSpan(parentSpan);
                        }
                        spans.add(parentSpan);
                    }
                }

                Collections.sort(spans);
            }

            return spans;
        }

        /**
         * Gets the person oid.
         *
         * @return String
         */
        public String getPersonOid() {
            return getValueString(FIELD_PERSON_OID);
        }

        /**
         * Gets the person addresses.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolPersonAddress> getPersonAddresses(X2Broker broker) {
            return (List<ToolPersonAddress>) getChildren(broker, CHILD_PERSON_ADDRESSES);
        }

        /**
         * Gets the person addresses.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolRace> getPersonRaces(X2Broker broker) {
            return (List<ToolRace>) getChildren(broker, CHILD_PERSON_RACES);
        }

        /**
         * Returns the Phone 1.
         * <p>
         * The data dictionary ID for this property is <code>psnPhone01</code>.
         *
         * @return String
         */
        public String getPhone01() {
            return getValueString(FIELD_PHONE_1);
        }

        /**
         * Gets the physical address oid.
         *
         * @param broker X2Broker
         * @return String
         */
        public ToolAddress getPhysicalAddress(X2Broker broker) {
            String adrOid = getValueString(FIELD_PHYSICAL_ADDRESS_OID);
            return getBeanByOid(broker, ToolAddress.class, adrOid, true);
        }

        /**
         * Gets the physical address.
         *
         * @return String
         */
        public String getPhysicalAddressOid() {
            return getValueString(FIELD_PHYSICAL_ADDRESS_OID);
        }

        /**
         * Returns the MailingAddress if valid, otherwise it returns the PhysicalAddress. The
         * MailingAddress is considered valid if it is not null and address line 1 is not empty.
         *
         * @param broker the broker
         * @return Address
         */
        public ToolAddress getResolvedMailingAddress(X2Broker broker) {
            ToolAddress address = getMailingAddress(broker);

            if (address == null || StringUtils.isEmpty(address.getLine1())) {
                address = getPhysicalAddress(broker);
            }

            return address;
        }

        /**
         * Gets the rubric assessment performance.
         *
         * @param broker the broker
         * @return the rubric assessment performance
         */
        public List<ToolRubricAssessmentPerformance> getRubricAssessmentPerformance(X2Broker broker) {
            return getTranscriptRubrics(broker).stream()
                    .map(trr -> trr.getRubricAssessment(broker))
                    .distinct()
                    .map(rba -> rba.getRubricAssessmentPerformances(broker))
                    .flatMap(List::stream)
                    .distinct()
                    .collect(Collectors.toList());
        }

        /**
         * Gets the rubric assessment performance.
         *
         * @param broker the broker
         * @param gradeTermId the grade term id
         * @return the rubric assessment performance
         */
        public List<ToolRubricAssessmentPerformance> getRubricAssessmentPerformance(X2Broker broker,
                                                                                    String gradeTermId) {
            return getTranscriptRubrics(broker).stream()
                    .filter(trr -> gradeTermId.equals(trr.getGradeTermId()))
                    .map(trr -> trr.getRubricAssessment(broker))
                    .distinct()
                    .map(rba -> rba.getRubricAssessmentPerformances(broker))
                    .flatMap(List::stream)
                    .distinct()
                    .collect(Collectors.toList());
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return School
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Gets the secondary spans.
         *
         * @param broker X2Broker
         * @return Collection
         */
        public Collection<AnnualSpan> getSecondarySpans(X2Broker broker) {
            if (m_secondarySpans == null) {
                m_secondarySpans = new ArrayList<>();

                List<? extends ToolStudentSchool> secondarySchoolEnrollments = getStudentSchools(broker);

                for (ToolStudentSchool studentSchool : secondarySchoolEnrollments) {
                    AnnualSpan secondarySpan =
                            DistrictManager.getAnnualSpanFactory().instantiateAnnualSpan(broker, studentSchool);

                    ToolSchoolCalendar schoolCalendar = ToolSchoolCalendar.findBestCalendar(broker, secondarySpan);
                    secondarySpan.setSchoolCalendar(schoolCalendar);

                    ToolDistrictContext context = studentSchool.getDistrictContext(broker);

                    boolean forward = true;
                    PlainDate firstActiveInSessionDate = schoolCalendar == null ? null
                            : schoolCalendar.findFirstInSessionDate(broker, studentSchool.getStartDate(), forward);
                    secondarySpan.setFirstActiveInSessionDate(firstActiveInSessionDate);

                    PlainDate scanFrom = (studentSchool.getEndDate() == null)
                            ? context.getEndDate()
                            : studentSchool.getEndDate();
                    if (!DistrictManager.isMemberOnWithdrawal(broker)) {
                        scanFrom = DateUtils.add(scanFrom, -1);
                    }
                    PlainDate lastActiveInSessionDate = schoolCalendar == null ? null
                            : schoolCalendar.findFirstInSessionDate(broker, scanFrom, false);

                    // Skip invalid span
                    if (firstActiveInSessionDate == null) {
                        continue;
                    }
                    if (lastActiveInSessionDate == null) {
                        continue;
                    }
                    if (firstActiveInSessionDate.after(lastActiveInSessionDate)) {
                        continue;
                    }

                    secondarySpan.setLastActiveInSessionDate(lastActiveInSessionDate);


                    PlainDate dayAfterSpanEnds = DateUtils.add(scanFrom, 1);
                    forward = true;
                    PlainDate firstInactiveInSessionDate = schoolCalendar == null ? null
                            : schoolCalendar.findFirstInSessionDate(broker, dayAfterSpanEnds, forward);
                    /*
                     * If there are no in-session dates in this year after the span,
                     * set firstInactiveInSessionDate to day after span ends
                     */
                    if (firstInactiveInSessionDate == null) {
                        firstInactiveInSessionDate = dayAfterSpanEnds;
                    }

                    secondarySpan.setFirstInactiveInSessionDate(firstInactiveInSessionDate);
                    secondarySpan.setDateAfterLastActiveInSessionDate(firstInactiveInSessionDate);

                    // If queryAsOf date, null out a future lastActive date
                    PlainDate queryAsOfDate =
                            (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);
                    if (queryAsOfDate != null) {
                        if (lastActiveInSessionDate.after(queryAsOfDate)) {
                            secondarySpan.setLastActiveInSessionDate(null);
                        }
                        if (firstInactiveInSessionDate.after(queryAsOfDate)) {
                            secondarySpan.setFirstInactiveInSessionDate(null);
                        }
                    }

                    m_secondarySpans.add(secondarySpan);
                }
            }
            return m_secondarySpans;
        }

        /**
         * Returns the Last IEP evaluation date.
         * <p>
         * The data dictionary ID for this property is <code>stdSpedLastEv</code>.
         *
         * @return PlainDate
         */
        public PlainDate getSpedLastEvaluationDate() {
            return getValueDate(FIELD_SPED_LAST_EVALUATION_DATE);
        }

        /**
         * Gets the state id.
         *
         * @return String
         */
        public String getStateId() {
            return getValueString(FIELD_STATE_ID);
        }

        /**
         * Gets the student context attributes.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentContextAttributes> getStudentContextAttributes(X2Broker broker) {
            return (List<ToolStudentContextAttributes>) getChildren(broker, CHILD_STUDENT_CONTEXT_ATTRIBUTES);
        }

        /**
         * Gets the student schedules.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentSchedule> getStudentSchedules(X2Broker broker) {
            return (List<ToolStudentSchedule>) getChildren(broker, CHILD_STUDENT_SCHEDULES);
        }

        /**
         * Gets the student schedule changes.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentScheduleChange> getStudentScheduleChanges(X2Broker broker) {
            return (List<ToolStudentScheduleChange>) getChildren(broker, CHILD_STUDENT_SCHEDULE_CHANGES);
        }

        /**
         * Gets the student schools.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolStudentSchool> getStudentSchools(X2Broker broker) {
            return (List<ToolStudentSchool>) getChildren(broker, CHILD_STUDENT_SCHOOLS);
        }

        /**
         * Gets the student schedule spans.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<StudentScheduleSpan> getStudentScheduleSpans(X2Broker broker) {
            if (m_scheduleSpans == null) {
                Map<String, StudentScheduleSpan> scheduleSpanMap = new HashMap<String, StudentScheduleSpan>();

                List<ToolStudentSchedule> schedules = getStudentSchedules(broker);
                if (schedules != null) {
                    for (ToolStudentSchedule schedule : schedules) {
                        StudentScheduleSpan info = DistrictManager.getStudentScheduleSpanFactory()
                                .instantiateSpan(schedule.getSection(broker));
                        info.setSchedule(schedule);
                        scheduleSpanMap.put(schedule.getSectionOid(), info);
                    }
                }

                List<ToolStudentScheduleChange> studentScheduleChanges = getStudentScheduleChanges(broker);
                if (studentScheduleChanges != null) {
                    checkScheduleChanges(broker, studentScheduleChanges, scheduleSpanMap);
                }

                // Fill in any empty entry/exit dates with term dates for the section term and
                // remove spans with invalid date ranges
                fillTermDates(broker, scheduleSpanMap);

                List<ToolTranscript> transcripts = getStudentTranscripts(broker);
                if (transcripts != null) {
                    for (ToolTranscript transcript : transcripts) {
                        /*
                         * For each transcript, find a scheduleMap entry with the same section.
                         * If there are more than one, pick the latest one that does not have
                         * a transcript on it already.
                         * Latest is determined by exit date, with null being the latest.
                         */
                        StudentScheduleSpan lastInfo = null;
                        for (StudentScheduleSpan span : scheduleSpanMap.values()) {
                            if (span.getSection().getOid().equals(transcript.getSectionOid()) &&
                                    span.getTranscript() == null) {
                                if (lastInfo == null) {
                                    lastInfo = span;
                                } else if (span.getLastMembershipDate() == null
                                        && lastInfo.getLastMembershipDate() != null) {
                                    lastInfo = span;
                                } else if (span.getLastMembershipDate() != null
                                        && lastInfo.getLastMembershipDate() != null &&
                                        span.getLastMembershipDate().after(lastInfo.getLastMembershipDate())) {
                                    lastInfo = span;
                                }
                            }
                        }
                        if (lastInfo != null) {
                            lastInfo.setTranscript(transcript);
                        }
                    }
                }

                m_scheduleSpans = new ArrayList<StudentScheduleSpan>(scheduleSpanMap.values());
            }
            return m_scheduleSpans;
        }

        /**
         * Gets the student schedule spans.
         *
         * @param broker X2Broker
         * @param schedule ToolStudentSchedule
         * @param changes List<ToolStudentScheduleChange>
         * @return List
         */
        public List<StudentScheduleSpan> getStudentScheduleSpans(X2Broker broker,
                                                                 ToolStudentSchedule schedule,
                                                                 List<ToolStudentScheduleChange> changes) {
            Map<String, StudentScheduleSpan> scheduleSpanMap = new HashMap<String, StudentScheduleSpan>();
            if (schedule != null) {
                StudentScheduleSpan info =
                        DistrictManager.getStudentScheduleSpanFactory().instantiateSpan(schedule.getSection(broker));
                info.setSchedule(schedule);
                scheduleSpanMap.put(schedule.getSectionOid(), info);
            }
            if (changes != null && !changes.isEmpty()) {
                checkScheduleChanges(broker, changes, scheduleSpanMap);
            }
            fillTermDates(broker, scheduleSpanMap);
            return new ArrayList<StudentScheduleSpan>(scheduleSpanMap.values());
        }

        /**
         * Gets the student transcripts.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolTranscript> getStudentTranscripts(X2Broker broker) {
            return (List<ToolTranscript>) getChildren(broker, CHILD_TRANSCRIPTS);
        }

        /**
         * Gets the transcript rubrics.
         *
         * @param broker the broker
         * @return the transcript rubrics
         */
        public List<ToolTranscriptRubric> getTranscriptRubrics(X2Broker broker) {
            return (List<ToolTranscriptRubric>) getChildren(broker, CHILD_TRANSCRIPT_RUBRICS);
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        public int getYog() {
            return getValueInt(FIELD_YOG);
        }

        /**
         * For each original span, generate one or more single-year span.
         *
         * @param broker X2Broker
         * @param aspenSpans List<AspenSpan>
         * @param queryAsOfDate PlainDate
         * @param isBreakOnYog boolean
         * @return List
         */
        protected List<AnnualSpan> splitSpansIntoContexts(X2Broker broker,
                                                          List<AspenSpan> aspenSpans,
                                                          PlainDate queryAsOfDate,
                                                          boolean isBreakOnYog) {
            List<AnnualSpan> contextSpans = new ArrayList<>();

            for (AspenSpan aspenSpan : aspenSpans) {
                List<AnnualSpan> splitSpanIntoContexts =
                        splitSpanIntoContexts(broker, aspenSpan, queryAsOfDate, isBreakOnYog);
                contextSpans.addAll(splitSpanIntoContexts);
            }

            return contextSpans;
        }

        /**
         * Core won't show a message that contains a newline character.
         *
         * @param messages String[]
         */
        private void addValidationError(String... messages) {
            if (m_validationErrors == null) {
                m_validationErrors = new LinkedList();
            }
            m_validationErrors.add(new ValidationError(ValidationConstants.CUSTOM_ERROR, null, null, messages));
        }

        /**
         * Calculate first active enrollment.
         *
         * @param broker X2Broker
         * @param generatedSpan AnnualSpan
         * @param isBreakOnYog boolean
         * @return ToolEnrollment
         */
        private ToolEnrollment calculateFirstActiveEnrollment(X2Broker broker,
                                                              AnnualSpan generatedSpan,
                                                              boolean isBreakOnYog) {
            AnnualSpanFactory factory = DistrictManager.getAnnualSpanFactory();
            List<? extends ToolEnrollment> enrollments = generatedSpan.getAllEnrollmentsAscend()
                    .stream().filter(enr -> !factory.getAnnualSpanIgnoreEnrollmentFn().test(enr))
                    .collect(Collectors.toList());
            ToolEnrollment firstActiveEnrollment =
                    getActiveStudentEnrollment(broker, enrollments, StudentEnrollment.ENTRY);
            if (firstActiveEnrollment == null) {
                firstActiveEnrollment =
                        getActiveStudentEnrollment(broker, enrollments, StudentEnrollment.STATUS_CHANGE);
            }
            if (firstActiveEnrollment == null) {
                firstActiveEnrollment =
                        getActiveStudentEnrollment(broker, enrollments, StudentEnrollment.YOG_CHANGE);
            }

            return firstActiveEnrollment;
        }

        /**
         * Calculate first active in session date.
         *
         * @param broker X2Broker
         * @param generatedSpan AnnualSpan
         * @return PlainDate
         */
        private PlainDate calculateFirstActiveInSessionDate(X2Broker broker, AnnualSpan generatedSpan) {
            /*
             * Gets the first active in session date.
             * This the First Active Date
             * - Adjusted by member on entry preference
             * - Advanced to first session date of the span context
             */
            ToolEnrollment firstActiveEnrollment = generatedSpan.getFirstActiveEnrollment();
            if (firstActiveEnrollment == null) {
                return null;
            }

            PlainDate firstActiveDate = firstActiveEnrollment.getEnrollmentDate();
            if (StudentEnrollment.ENTRY.equals(firstActiveEnrollment.getEnrollmentType())
                    && !DistrictManager.isMemberOnEntry(broker)) {
                firstActiveDate = DateUtils.add(firstActiveDate, 1);
            }

            // First Active date must be in context for findFirstInSessionDate to work correctly
            PlainDate contextStartDate = generatedSpan.getContext().getStartDate();
            if (firstActiveDate.before(contextStartDate)) {
                firstActiveDate = contextStartDate;
            }

            ToolSchoolCalendar schoolCalendar = ToolSchoolCalendar.findBestCalendar(broker, generatedSpan);
            PlainDate firstActiveInSessionDate = null;
            if (schoolCalendar == null || schoolCalendar.getDatesInSession(broker).isEmpty()) {
                // Rare case for historical data where calendar can't be found
                firstActiveInSessionDate = generatedSpan.getContext().getStartDate();

                // Assume user only needs to see validation error for recent data
                long contextStartTime = generatedSpan.getContext().getStartDate().getTime();
                if (new PlainDate().getTime() - contextStartTime > 3 * DURATION_YEAR_MS) {
                    addValidationError("Unable to determine school calendar: " + generatedSpan);
                }
            } else {
                boolean forward = true;
                firstActiveInSessionDate = schoolCalendar.findFirstInSessionDate(broker, firstActiveDate, forward);
            }
            if (firstActiveInSessionDate == null) {
                addValidationError("Unable to determine firstActiveInSessionDate: " + generatedSpan);
            }

            generatedSpan.setFirstActiveInSessionDate(firstActiveInSessionDate);
            generatedSpan.setSchoolCalendar(schoolCalendar);

            return firstActiveInSessionDate;
        }

        /**
         * Determine the first in-session day after the span's withdrawMemberDate
         * i.e first in-session date after W or W-1
         * depending on MemberOnWithdraw preference.
         *
         * If no more in-session days in school calendar, return withdrawMemberDate + 1.
         *
         * @param broker X2Broker
         * @param generatedSpan AnnualSpan
         * @return PlainDate
         */
        private PlainDate calculateFirstInactiveInSessionDate_FromWithdrawMemberDateOrTerminator(X2Broker broker,
                                                                                                 AnnualSpan generatedSpan) {
            debug("calculateFirstInactiveInSessionDate_FromWithdrawMemberDate for " + generatedSpan);

            /*
             * Requires withdrawMemberDate calculated first
             */
            PlainDate withdrawMemberDate = generatedSpan.getWithdrawalMemberDate();
            debug("calculateFirstInactiveInSessionDate withdrawMemberDate: " + withdrawMemberDate);
            if (withdrawMemberDate == null) {
                // Span could be terminated by an enrollment in a different school
                if (generatedSpan.getTerminatingEnrollment() == null) {
                    return null;
                }

                withdrawMemberDate = DateUtils.add(generatedSpan.getTerminatingEnrollment().getEnrollmentDate(), -1);
            }

            ToolSchoolCalendar schoolCalendar = generatedSpan.getSchoolCalendar();
            if (schoolCalendar == null) {
                addValidationError("Unable to determine school calendar: " + generatedSpan);
                return null;
            }


            PlainDate dayAfterWithdrawMemberDate = DateUtils.add(withdrawMemberDate, 1);
            boolean forward = true;
            PlainDate firstInactiveInSessionDate =
                    schoolCalendar.findFirstInSessionDate(broker, dayAfterWithdrawMemberDate, forward);

            /*
             * If there are no more in-session days in the calendar,
             * this will return the span's last active date + 1.
             */
            if (firstInactiveInSessionDate == null) {
                PlainDate lastActiveInSessionDate = generatedSpan.getLastActiveInSessionDate();
                if (lastActiveInSessionDate == null) {
                    firstInactiveInSessionDate = DateUtils.add(withdrawMemberDate, 1);
                } else {
                    firstInactiveInSessionDate = DateUtils.add(lastActiveInSessionDate, 1);
                }
            }

            generatedSpan.setFirstInactiveInSessionDate(firstInactiveInSessionDate);
            generatedSpan.setDateAfterLastActiveInSessionDate(firstInactiveInSessionDate);

            return firstInactiveInSessionDate;
        }

        /**
         * Calculates the last active in session date.
         *
         * Case LastActive (W/S) record exists inside the span context:
         * This is the lastActive record date
         * - Adjusted by member on withdrawal preference only if it's a W
         * - Advanced to last session date on/before adjusted date
         *
         * Case no LastActive record inside the span context:
         * This is the last session date for the school calendar
         *
         * @param broker X2Broker
         * @param generatedSpan AnnualSpan
         * @param aspenSpanTerminatingEnrollment ToolEnrollment
         * @return PlainDate
         */
        private PlainDate calculateLastActiveInSessionDate(X2Broker broker,
                                                           AnnualSpan generatedSpan,
                                                           ToolEnrollment aspenSpanTerminatingEnrollment) {
            /*
             * TODO Beware aspenSpanTerminatingEnrollment only applies if
             * getContext( aspenSpanTerminatingEnrollment) == generatedSpan.getContext
             *
             * Otherwise, generatedSpan is terminated by end of year
             * THEREFORE --> lastActiveInSessionDate =
             * school.getLastInSession(generatedSpan.getContext)
             */

            boolean isLastActiveInContext = false;
            PlainDate lastActiveDate = null;
            if (aspenSpanTerminatingEnrollment != null) {
                lastActiveDate = aspenSpanTerminatingEnrollment.getEnrollmentDate();
                if (lastActiveDate != null) {

                    if (WITHDRAWAL.equals(aspenSpanTerminatingEnrollment.getEnrollmentType())
                            && StringUtils.isEqual(generatedSpan.getSchool().getOid(),
                                    aspenSpanTerminatingEnrollment.getSchoolOid())) {
                        if (!DistrictManager.isMemberOnWithdrawal(broker)) {
                            lastActiveDate = DateUtils.add(lastActiveDate, -1);
                        }
                    } else {
                        // Terminator starts the next span (S/Y) - (rare E with different school)
                        // So this span must end before it
                        lastActiveDate = DateUtils.add(lastActiveDate, -1);
                    }

                    ToolDistrictContext lastActiveContext =
                            DistrictManager.getDistrictSchoolYearContext(broker, lastActiveDate);
                    if (lastActiveContext != null
                            && generatedSpan.getContext().getOid().equals(lastActiveContext.getOid())) {
                        isLastActiveInContext = true;
                    }
                }
            }


            PlainDate lastActiveInSessionDate = null;

            ToolSchoolCalendar schoolCalendar = ToolSchoolCalendar.findBestCalendar(broker, generatedSpan);
            if (schoolCalendar == null || schoolCalendar.getDatesInSession(broker).isEmpty()) {
                // Rare case for historical data where calendar can't be found
                lastActiveInSessionDate = generatedSpan.getContext().getEndDate();
            } else {
                if (isLastActiveInContext) {
                    boolean forward = false;
                    lastActiveInSessionDate = schoolCalendar.findFirstInSessionDate(broker, lastActiveDate, forward);
                } else {
                    PlainDate startingDate = null;
                    boolean forward = false;
                    lastActiveInSessionDate = schoolCalendar.findFirstInSessionDate(broker, startingDate, forward);
                }
            }

            generatedSpan.setLastActiveInSessionDate(lastActiveInSessionDate);

            /*
             * It's normal to start the year with an S record.
             * In this case the lastActiveInSessionDate will be null
             * for the fragmented span at the beginning
             * so it really isn't a span but doesn't warrant a validation error.
             */
            // if (lastActiveInSessionDate == null) {
            // addValidationError(errors, "Unable to determine lastActiveInSessionDate: " +
            // generatedSpan);
            // }

            /*
             * Carryover fragment span:
             *
             * If a span carries over from prior-year enrollment records,
             * and has a last active date
             * that is within a week of the first in-session date for the school,
             * flag a warning.
             *
             * This is likely due to an S record after the first in-session date
             * that should have been on/before the first in-session date
             */
            boolean isSpanAtLeastOneDay =
                    (lastActiveInSessionDate != null && generatedSpan.getFirstActiveInSessionDate() != null
                            && !lastActiveInSessionDate.before(generatedSpan.getFirstActiveInSessionDate()));
            boolean isSpanEndedDuringFirstWeek = false;
            PlainDate firstSchoolInSessionDate = null;
            if (isSpanAtLeastOneDay) {
                PlainDate startingDate = null;
                boolean forward = true;

                firstSchoolInSessionDate = schoolCalendar == null ? null
                        : schoolCalendar.findFirstInSessionDate(broker, startingDate, forward);
                if (firstSchoolInSessionDate != null && !lastActiveInSessionDate.before(firstSchoolInSessionDate)) {
                    long diffMs = lastActiveInSessionDate.getTime() - firstSchoolInSessionDate.getTime();
                    if (diffMs <= DURATION_WEEK_MS) {
                        isSpanEndedDuringFirstWeek = true;
                    }
                }
            }

            if (isSpanAtLeastOneDay && isSpanEndedDuringFirstWeek) {
                boolean isCarryover = (generatedSpan.getAllEnrollmentsAscend().size() == 0);
                if (!isCarryover) {
                    boolean hasEnrThisYearBeforeLastActive = false;
                    for (ToolEnrollment testEnr : generatedSpan.getAllEnrollmentsAscend()) {
                        if (testEnr.getEnrollmentDate().after(lastActiveInSessionDate)) {
                            break;
                        }
                        if (testEnr.getEnrollmentDate().before(generatedSpan.getContext().getStartDate())) {
                            continue;
                        }
                        hasEnrThisYearBeforeLastActive = true;
                        break;
                    }
                    isCarryover = !hasEnrThisYearBeforeLastActive;
                }

                if (isCarryover) {
                    String message = "Possible fragmented carryover span."
                            + " FirstSchoolInSession: " + firstSchoolInSessionDate
                            + " span.FirstActiveInSessionDate: " + generatedSpan.getFirstActiveInSessionDate()
                            + " span.lastActiveInSessionDate: " + lastActiveInSessionDate
                            + " Span: " + generatedSpan;
                    addValidationError(message);
                }
            }

            return lastActiveInSessionDate;
        }

        /**
         * Search through schedule change records for alternate start dates and other
         * sections to report.
         *
         * Loop through schedule change records for each section, in date order. Add section
         * spans based on change drop or add record.s
         *
         * Some will have been dropped after the end of a term but should still be counted.
         *
         * @param broker X2Broker
         * @param scheduleChanges List<StudentScheduleChange>
         * @param scheduleSpanMap Map<String,StudentScheduleSpan>
         */
        private void checkScheduleChanges(X2Broker broker,
                                          List<ToolStudentScheduleChange> scheduleChanges,
                                          Map<String, StudentScheduleSpan> scheduleSpanMap) {
            ToolSection lastSection = null;
            ToolStudentScheduleChange lastChange = null;
            PlainDate termStart = null;
            PlainDate termEnd = null;

            /*
             * Work backward in time through schedule changes.
             * DROP will open a new section and the ADD before it will finish that section.
             * A DROP without a following ADD will be considered open at start of term.
             * Any activity entirely before start of term will be ignored.
             */
            for (ToolStudentScheduleChange change : scheduleChanges) {
                // Check for a new section.
                if (lastSection == null || !lastSection.getOid().equals(change.getSectionOid())) {
                    // Save the working section if necessary.
                    if (lastChange != null) {
                        // The last change record for this section (in reverse chronological order)
                        // was a drop. Assume the section was scheduled from the beginning of the
                        // term/year.
                        StudentScheduleSpan info =
                                DistrictManager.getStudentScheduleSpanFactory().instantiateSpan(lastSection);
                        info.setEntryDate(termStart);
                        PlainDate lastChangeDate = lastChange.getScheduleChangeDate(broker);
                        if (lastChangeDate.after(termEnd)) {
                            info.setLastMembershipDate(termEnd);
                        } else {
                            info.setLastMembershipDate(lastChangeDate);
                        }
                        info.setExitChange(lastChange);
                        // Avoid recording sections scheduled out entirely
                        // before the start of it's term. This is just scheduling activity.
                        if (!info.getLastMembershipDate().before(termStart)) {
                            scheduleSpanMap.put(lastChange.getOid(), info);
                        }
                    }

                    // Initialize the new section
                    lastChange = null;
                    lastSection = change.getSection(broker);
                    termStart = null;
                    termEnd = null;
                    Range<Date> sectionDateRange = lastSection.getSectionDateRange(broker);
                    termStart = (PlainDate) sectionDateRange.getStart();
                    termEnd = (PlainDate) sectionDateRange.getEnd();
                }

                // For a section, see if its dates compare with report dates or term dates.
                if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                    lastChange = change;
                } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                    PlainDate changeDate = change.getScheduleChangeDate(broker);
                    if (lastChange == null) {
                        // No previous record, assume current student schedule. Find based on master
                        // OID.
                        StudentScheduleSpan info = scheduleSpanMap.get(change.getSectionOid());
                        if (info != null) {
                            info.setEntryDate(changeDate);
                            info.setEntryChange(change);
                            if (info.getEntryDate().before(termStart)) {
                                info.setEntryDate(termStart);
                            }
                        }
                    } else {
                        StudentScheduleSpan info = DistrictManager.getStudentScheduleSpanFactory()
                                .instantiateSpan(change.getSection(broker));
                        info.setEntryDate(changeDate);
                        info.setEntryChange(change);
                        if (info.getEntryDate().before(termStart)) {
                            info.setEntryDate(termStart);
                        }
                        PlainDate lastChangeDate = lastChange.getScheduleChangeDate(broker);
                        info.setLastMembershipDate(lastChangeDate);
                        // Avoid entering a change date that is after the term end date
                        if (info.getLastMembershipDate().after(termEnd)) {
                            info.setLastMembershipDate(termEnd);
                        }
                        info.setExitChange(lastChange);
                        // Avoid recording sections scheduled out entirely
                        // before the start of it's term. This is just scheduling activity.
                        if (!info.getLastMembershipDate().before(termStart)) {
                            scheduleSpanMap.put(change.getOid(), info);
                        }
                    }
                    lastChange = null;
                }
            }
            if (lastChange != null) {
                // The last change record for this section (in reverse chronological order)
                // was a drop. Assume the section was scheduled from the beginning of the term/year.
                StudentScheduleSpan info = DistrictManager.getStudentScheduleSpanFactory().instantiateSpan(lastSection);
                info.setEntryDate(termStart);
                PlainDate lastChangeDate = lastChange.getScheduleChangeDate(broker);
                if (lastChangeDate.after(termEnd)) {
                    info.setLastMembershipDate(termEnd);
                } else {
                    info.setLastMembershipDate(lastChangeDate);
                }
                info.setExitChange(lastChange);
                // Avoid recording sections scheduled out entirely
                // before the start of it's term. This is just scheduling activity.
                if (!info.getLastMembershipDate().before(termStart)) {
                    scheduleSpanMap.put(lastChange.getOid(), info);
                }
            }
        }

        /**
         * For all populated sections in the schedule span map, if the entry date or exit date is
         * missing, populate with term dates.
         * After missing dates are populated, remove spans which have out of order dates.
         *
         * @param broker X2Broker
         * @param scheduleSpanMap Map<String,StudentScheduleSpan>
         */
        private void fillTermDates(X2Broker broker, Map<String, StudentScheduleSpan> scheduleSpanMap) {
            Iterator<StudentScheduleSpan> iterator = scheduleSpanMap.values().iterator();
            while (iterator.hasNext()) {
                StudentScheduleSpan info = iterator.next();
                fillSpanTermDates(broker, info);

                /*
                 * If the entry/exit dates are out of order, remove the info.
                 * This can be caused by drop/re-add after the end of term.
                 * The original entry will exist before the drop, so this record is extra.
                 */
                if (info.getLastMembershipDate() != null &&
                        info.getEntryDate() != null &&
                        info.getLastMembershipDate().before(info.getEntryDate())) {
                    iterator.remove();
                }
            }
        }

        /**
         * For the passed StudentScheduleSpan, if the entry date or exit date is missing, populate
         * with
         * term dates.
         *
         * @param broker X2Broker
         * @param info StudentScheduleSpan
         */
        private void fillSpanTermDates(X2Broker broker, StudentScheduleSpan info) {
            if (info.getEntryDate() == null || info.getLastMembershipDate() == null) {
                Range<Date> sectionDateRange = info.getSection().getSectionDateRange(broker);
                if (info.getEntryDate() == null) {
                    info.setEntryDate((PlainDate) sectionDateRange.getStart());
                }
                if (info.getLastMembershipDate() == null) {
                    info.setLastMembershipDate((PlainDate) sectionDateRange.getEnd());
                }
            }
        }

        /**
         * Gets the active student enrollment.
         *
         * @param broker X2Broker
         * @param ascendingEnrollments List<? extends ToolEnrollment>
         * @param type String
         * @return enr
         */
        private ToolEnrollment getActiveStudentEnrollment(X2Broker broker,
                                                          List<? extends ToolEnrollment> ascendingEnrollments,
                                                          String type) {
            Optional<? extends ToolEnrollment> findFirst =
                    ascendingEnrollments.stream().filter(
                            enr -> enr.isActive(broker) && type.equals(enr.getEnrollmentType()))
                            .findFirst();

            return findFirst.isPresent() ? findFirst.get() : null;
        }

        /**
         * Gets the aspen spans one school range.
         *
         * @param broker X2Broker
         * @param enrollments List<? extends ToolEnrollment>
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         */
        private List<AspenSpan> getAspenSpansOneSchoolRange(X2Broker broker,
                                                            List<? extends ToolEnrollment> enrollments,
                                                            boolean isBreakOnYog,
                                                            boolean isBreakOnStatus) {
            // Determine starting status (current status). This should be based on the latest
            // enrollment record if possible, or the student otherwise.
            // isOpen indicates the current enrollment status is active.
            String enrollStatus = null;
            if (enrollments != null && enrollments.size() > 0) {
                ToolEnrollment currentEnrollment = enrollments.iterator().next();
                enrollStatus = currentEnrollment.getStatusCode();
                if (StringUtils.isEmpty(enrollStatus)) {
                    enrollStatus = getEnrollmentStatus();
                }
            } else {
                enrollStatus = getEnrollmentStatus();
            }

            // Current status of student.
            // TODO boolean isActive = StudentManager.isActiveStudent(getOrganization(),
            // enrollStatus);

            // If the enrollment span has a non-withdrawal record in it so far.
            // Used to determine if another withdrawal signifies a break in the span.
            // boolean hasActiveNonWithdrawals = isActive;

            List<AspenSpan> aspenSpans = new ArrayList<>();
            if (enrollments == null) {
                return new ArrayList<>();
            }

            // Work through enrollment records going backward in time and build spans.
            // This will build all spans, regardless of the setting of limit.
            List<ToolEnrollment> currentEnrollments = new ArrayList<>();


            /*
             * SIMPLE CASE
             * ============
             * E Active
             * E PreReg
             * W
             * E
             *
             * currentEnrollments: [E,W]
             * currentTerminator: W
             *
             * Emitted:
             * [E Active, E PreReg], null
             * [E, W], W
             *
             *
             * BREAK ON S (no PreReg)
             * ============
             * S Active
             * E Active
             *
             * currentEnrollments: []
             * currentTerminator: S
             *
             * Emitted:
             * [S], null
             */

            // schoolOid of the current span that has non-withdrawals
            String currentSpanSchoolOid = null;
            ToolEnrollment currentTerminator = null;
            ToolEnrollment dataStorageEnrollment = null;
            AspenSpan followingSpanForTrailingPreReg = null;
            PlainDate historicalCutoffDate =
                    (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE);

            for (ToolEnrollment enrollment : enrollments) {
                /*
                 * If m_historicalCutoffDate is specified,
                 * stop generating AspenSpans once a valid span has been created
                 * that starts before m_historicalCutoffDate
                 */
                if (historicalCutoffDate != null && !aspenSpans.isEmpty()) {
                    AspenSpan oldestAspenSpan = aspenSpans.get(0);
                    PlainDate oldestSpanDate = oldestAspenSpan.getAscendingEnrollments().get(0).getEnrollmentDate();
                    if (oldestSpanDate != null && oldestSpanDate.before(historicalCutoffDate)) {
                        // We've gone past the cutoff date - don't add the in-progress span
                        currentEnrollments.clear();
                        break;
                    }
                }

                if (enrollment.isActiveOrWithdrawal(broker)) {
                    followingSpanForTrailingPreReg = null;
                } else {
                    if (followingSpanForTrailingPreReg != null) {
                        followingSpanForTrailingPreReg.addAscendingEnrollment(enrollment);
                        continue;
                    }
                }

                // initialize currentSpanSchoolOid on the first record
                if (currentSpanSchoolOid == null) {
                    currentSpanSchoolOid = enrollment.getSchoolOid();
                }

                if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                    currentEnrollments.add(0, enrollment);

                    // Only report a YOG as a break if the student is active or there are other
                    // records in the span already. Not for inactive students between spans.
                    if (isBreakOnYog && hasActiveNonWithdrawal(broker, currentEnrollments)) {
                        // Emit the current span
                        followingSpanForTrailingPreReg =
                                new AspenSpan(currentEnrollments, currentTerminator, dataStorageEnrollment);
                        aspenSpans.add(0, followingSpanForTrailingPreReg);

                        // Start a new span, saving this Y as its terminating record
                        currentEnrollments = new ArrayList<>();
                        currentTerminator = enrollment;
                    }
                    currentSpanSchoolOid = enrollment.getSchoolOid();
                } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                    currentEnrollments.add(0, enrollment);

                    if (enrollment.ignoreForSpans()) {
                        continue;
                    }

                    // Only report a STATUS as a break if the student is active or there are other
                    // records in the span already. Not for inactive students between spans.
                    if (isBreakOnStatus && hasActiveNonWithdrawal(broker, currentEnrollments)) {
                        // Emit the current span
                        followingSpanForTrailingPreReg =
                                new AspenSpan(currentEnrollments, currentTerminator, dataStorageEnrollment);
                        aspenSpans.add(0, followingSpanForTrailingPreReg);

                        // Start a new span, saving this S as its terminating record
                        currentEnrollments = new ArrayList<>();
                        currentTerminator = enrollment;
                    }
                    currentSpanSchoolOid = enrollment.getSchoolOid();
                } else if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    // Emit the currently-accumulated span
                    if (hasActiveNonWithdrawal(broker, currentEnrollments)) {
                        followingSpanForTrailingPreReg =
                                new AspenSpan(currentEnrollments, currentTerminator, dataStorageEnrollment);
                        aspenSpans.add(0, followingSpanForTrailingPreReg);
                        currentEnrollments = new ArrayList<>();
                    }

                    // if we have more than just a W e.g. an Inactive E, clear but don't emit
                    if (hasNonWithdrawal(currentEnrollments)) {
                        followingSpanForTrailingPreReg = null;
                        currentEnrollments.clear();
                    }


                    // Don't accumulate W's for different schools
                    if (!StringUtils.isEqual(enrollment.getSchoolOid(), currentSpanSchoolOid)) {
                        followingSpanForTrailingPreReg = null;
                        currentEnrollments.clear();
                    }

                    // Encountered the W record for the next older span
                    if (historicalCutoffDate != null
                            && enrollment.getEnrollmentDate().before(historicalCutoffDate)) {
                        // We've gone past the cutoff date - don't add the in-progress span
                        currentEnrollments.clear();
                        break;
                    }
                    currentSpanSchoolOid = enrollment.getSchoolOid();
                    currentEnrollments.add(0, enrollment);
                    currentTerminator = enrollment;
                    dataStorageEnrollment = enrollment;
                } else if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    // If already have non-withdrawals in this span,
                    // and encounter a new E record for a different school,
                    // the new E record is not part of the current span. Create a span break.
                    if (hasActiveNonWithdrawal(broker, currentEnrollments)) {
                        if (!StringUtils.isEqual(currentSpanSchoolOid, enrollment.getSchoolOid())) {
                            // The Entry record is for a different school so close the prior span
                            followingSpanForTrailingPreReg =
                                    new AspenSpan(currentEnrollments, currentTerminator, dataStorageEnrollment);
                            aspenSpans.add(0, followingSpanForTrailingPreReg);
                            currentEnrollments = new ArrayList<>();
                            currentTerminator = enrollment;
                            dataStorageEnrollment = null;
                            String message =
                                    "Entry record found for different school before preceding span is terminated: "
                                            + followingSpanForTrailingPreReg;
                            addValidationError(message);
                        }
                    }

                    currentEnrollments.add(0, enrollment);
                    currentSpanSchoolOid = enrollment.getSchoolOid();
                }
            }

            if (hasActiveNonWithdrawal(broker, currentEnrollments)) {
                followingSpanForTrailingPreReg =
                        new AspenSpan(currentEnrollments, currentTerminator, dataStorageEnrollment);
                aspenSpans.add(0, followingSpanForTrailingPreReg);
                currentEnrollments = new ArrayList<>();
                currentTerminator = null;
            }

            return aspenSpans;
        }

        /**
         * Gets the enrollments in this year.
         *
         * @param broker X2Broker
         * @param enrollments List<? extends ToolEnrollment>
         * @param loopYear int
         * @return List
         */
        private List<ToolEnrollment> getEnrollmentsInThisYear(X2Broker broker,
                                                              List<? extends ToolEnrollment> enrollments,
                                                              int loopYear) {
            return enrollments.stream().filter(
                    enr -> DistrictManager.getDistrictSchoolYearContext(broker, enr.getEnrollmentDate())
                            .getSchoolYear() == loopYear)
                    .collect(Collectors.toList());
        }


        /**
         * Gets the enrollments up to year.
         *
         * @param broker X2Broker
         * @param enrollments List<? extends ToolEnrollment>
         * @param loopYear int
         * @return List
         */
        private List<ToolEnrollment> getEnrollmentsUpToYear(X2Broker broker,
                                                            List<? extends ToolEnrollment> enrollments,
                                                            int loopYear) {
            return enrollments.stream().filter(
                    enr -> DistrictManager.getDistrictSchoolYearContext(broker, enr.getEnrollmentDate())
                            .getSchoolYear() <= loopYear)
                    .collect(Collectors.toList());
        }

        /**
         * Gets the next enrollment.
         *
         * @param span AspenSpan
         * @param allDescendingEnrollments List<? extends ToolEnrollment>
         * @return enr
         */
        private ToolEnrollment getNextEnrollment(AspenSpan span,
                                                 List<? extends ToolEnrollment> allDescendingEnrollments) {
            ToolEnrollment spanLastEnrollment = (span.getAscendingEnrollments().isEmpty())
                    ? null
                    : span.getAscendingEnrollments().get(span.getAscendingEnrollments().size() - 1);

            if (spanLastEnrollment == null) {
                return null;
            }

            ToolEnrollment followingEnrollment = null;
            for (int i = 0; i < allDescendingEnrollments.size(); i++) {
                ToolEnrollment testEnrollment = allDescendingEnrollments.get(i);
                if (followingEnrollment == null) {
                    followingEnrollment = testEnrollment;
                    continue;
                }

                if (spanLastEnrollment.equals(testEnrollment)) {
                    return followingEnrollment;
                }
                followingEnrollment = testEnrollment;
            }

            return null;
        }

        /**
         * Gets the school ranges.
         *
         * @param broker X2Broker
         * @param enrollments List<? extends ToolEnrollment>
         * @return List
         */
        private List<List<? extends ToolEnrollment>> getSchoolRanges(X2Broker broker,
                                                                     List<? extends ToolEnrollment> enrollments) {
            /*
             * Partition enrollments into contiguous by School
             */
            List<List<? extends ToolEnrollment>> enrollmentSchoolRanges = new ArrayList<>();

            String currentSchoolOid = null;
            ArrayList<ToolEnrollment> enrollmentRange = null;
            for (ToolEnrollment enrollment : enrollments) {
                if (StringUtils.isBlank(enrollment.getSchoolOid())) {
                    ToolStudent student = enrollment.getStudent(null);
                    String stateId = student.getStateId();
                    String localId = student.getLocalId();
                    String nameView = student.getNameView();
                    String message = "School missing from [" + enrollment + "] for student: " + nameView +
                            " [stateId=" + stateId + "]" + ",[localId=" + localId + "]";
                    throw new RuntimeException(message);
                }

                if (!StringUtils.isEqual(currentSchoolOid, enrollment.getSchoolOid())) {
                    enrollmentRange = new ArrayList<>();
                    enrollmentSchoolRanges.add(enrollmentRange);
                    currentSchoolOid = enrollment.getSchool(broker).getOid(); // preload school
                }
                enrollmentRange.add(enrollment);
            }

            return enrollmentSchoolRanges;
        }

        /**
         * Checks for active non withdrawal.
         *
         * @param broker X2Broker
         * @param enrollments List<? extends ToolEnrollment>
         * @return true, if successful
         */
        private boolean hasActiveNonWithdrawal(X2Broker broker, List<? extends ToolEnrollment> enrollments) {
            return enrollments.stream().filter(enr -> enr.isActiveNonWithdrawal(broker)).findFirst().isPresent();
        }

        /**
         * Checks for non withdrawal.
         *
         * @param enrollments List<? extends ToolEnrollment>
         * @return true, if successful
         */
        private boolean hasNonWithdrawal(List<? extends ToolEnrollment> enrollments) {
            return enrollments.stream().filter(enr -> !WITHDRAWAL.equals(enr.getEnrollmentType())).findFirst()
                    .isPresent();
        }

        /**
         * For each original multi-year AspenSpan, generate one or more single-year span.
         *
         * @param broker X2Broker
         * @param aspenSpan AspenSpan
         * @param queryAsOfDate PlainDate
         * @param isBreakOnYog boolean
         * @return List
         */
        private List<AnnualSpan> splitSpanIntoContexts(X2Broker broker,
                                                       AspenSpan aspenSpan,
                                                       PlainDate queryAsOfDate,
                                                       boolean isBreakOnYog) {
            List<AnnualSpan> splitSpans = new ArrayList<>();
            ToolOrganization organization = ToolBean.DistrictManager.getOrganizationToolBean(broker);

            ToolDistrictContext currentContext = organization.getCurrentContext(broker);
            int firstSchoolYear = 0;
            int lastSchoolYear = 0;

            ToolEnrollment firstEnrollment = aspenSpan.getAscendingEnrollments().get(0);
            if (firstEnrollment == null) {
                return splitSpans;
            }

            ToolDistrictContext firstContext =
                    organization.getSchoolYearContext(broker, firstEnrollment.getEnrollmentDate());
            if (firstContext == null) {
                String message = "Unable to determine district context year for enrolment date ["
                        + firstEnrollment.getEnrollmentDate()
                        + "] on " + firstEnrollment + " " + firstEnrollment.getStudent(broker).getNameView();
                throw new RuntimeException(message);
            }

            firstSchoolYear = firstContext.getSchoolYear();

            ToolEnrollment dataStorageEnrollment = aspenSpan.getDataStorageEnrollment();


            // TODO adjust by member pref before determining context if terminating Enrollment is W
            ToolEnrollment terminatingEnrollment = aspenSpan.getTerminatingEnrollment();

            /*
             * If the back-scanning start date for a span termination
             * is on/after the district start date
             * but before the school's first in-session date,
             * the scanning start date should be downgraded to the last day of the prior district
             * context,
             * and getLastActiveInSessionDate should return the last in-session date of that
             * context.
             */
            PlainDate effectiveTerminatingDate =
                    (terminatingEnrollment == null) ? null : terminatingEnrollment.getEnrollmentDate();
            if (effectiveTerminatingDate != null) {
                // Adjust for Member Pref
                if (WITHDRAWAL.equals(terminatingEnrollment.getEnrollmentType())
                        && !ToolBean.DistrictManager.isMemberOnWithdrawal(broker)) {
                    effectiveTerminatingDate = DateUtils.add(effectiveTerminatingDate, -1);
                }

                // If it falls before school's first in-session date, backdate to end of last year
                ToolDistrictContext tryContext = organization.getSchoolYearContext(broker, effectiveTerminatingDate);
                if (tryContext == null) {
                    throw new RuntimeException(
                            "Unable to locate DistrictSchoolYearContext for effectiveTerminatingDate of ["
                                    + effectiveTerminatingDate + "] for span " + aspenSpan);
                }

                ToolSchoolCalendar schoolCalendar = null;
                if (aspenSpan.getAscendingEnrollments() != null && !aspenSpan.getAscendingEnrollments().isEmpty()) {
                    ToolStudent student = aspenSpan.getAscendingEnrollments().get(0).getStudent(broker);
                    ToolSchool school = aspenSpan.getAscendingEnrollments().get(0).getSchool(broker);
                    ToolEnrollment withdrawal = aspenSpan.getDataStorageEnrollment();

                    if (student == null) {
                        throw new RuntimeException("Enrollment span missing Student: " + aspenSpan);
                    }

                    if (school == null) {
                        throw new RuntimeException("Enrollment span missing School: " + aspenSpan);
                    }

                    schoolCalendar =
                            ToolSchoolCalendar.findBestCalendar(broker, student, school, withdrawal, tryContext);
                }

                boolean forward = true;
                PlainDate firstActiveInSessionDate = schoolCalendar == null ? null
                        : schoolCalendar.findFirstInSessionDate(broker, tryContext.getStartDate(), forward);
                if (effectiveTerminatingDate != null && firstActiveInSessionDate != null
                        && effectiveTerminatingDate.before(firstActiveInSessionDate)) {
                    ToolDistrictContext prevContext = DistrictManager.getDistrictSchoolYearContexts(broker)
                            .getGroup(ToolDistrictContext.FIELD_SCHOOL_YEAR,
                                    Integer.valueOf(tryContext.getSchoolYear() - 1))
                            .stream().findAny().orElse(null);
                    effectiveTerminatingDate = prevContext.getEndDate();
                }
            }

            ToolDistrictContext lastContext = (effectiveTerminatingDate == null) ? currentContext
                    : DistrictManager.getDistrictSchoolYearContext(broker, effectiveTerminatingDate);

            if (lastContext == null) {
                return splitSpans;
            }

            lastSchoolYear = lastContext.getSchoolYear();


            for (int loopYear = firstSchoolYear; loopYear <= lastSchoolYear; loopYear++) {
                ToolDistrictContext spanContext = DistrictManager.getDistrictSchoolYearContexts(broker)
                        .getGroup(ToolDistrictContext.FIELD_SCHOOL_YEAR,
                                Integer.valueOf(loopYear))
                        .stream().findAny().orElse(null);
                if (spanContext == null) {
                    continue;
                }

                List<ToolEnrollment> enrollmentsUpToYear = // originalSpan.getEnrollments();
                        getEnrollmentsUpToYear(broker, aspenSpan.getAscendingEnrollments(), loopYear);

                List<ToolEnrollment> enrollmentsInThisYear =
                        getEnrollmentsInThisYear(broker, enrollmentsUpToYear, loopYear);

                /*
                 * If the terminating enrollment is a W, it belongs with the last span.
                 * It might not have been included if the W was adjusted to the prior year.
                 */
                if (terminatingEnrollment != null && WITHDRAWAL.equals(terminatingEnrollment.getEnrollmentType())) {
                    if (loopYear == lastSchoolYear && !enrollmentsUpToYear.contains(terminatingEnrollment)) {
                        enrollmentsUpToYear.add(terminatingEnrollment);
                        enrollmentsInThisYear.add(terminatingEnrollment);
                    }
                }

                AnnualSpan generatedSpan =
                        DistrictManager.getAnnualSpanFactory().instantiateAnnualSpan(enrollmentsUpToYear,
                                enrollmentsInThisYear,
                                spanContext);

                ToolEnrollment firstActiveEnrollment =
                        calculateFirstActiveEnrollment(broker, generatedSpan, isBreakOnYog);

                // This must be done first before the date calculations
                generatedSpan.setFirstActiveEnrollment(firstActiveEnrollment);

                // Now we can calculate the dates
                PlainDate firstActiveInSessionDate = calculateFirstActiveInSessionDate(broker, generatedSpan);
                PlainDate lastActiveInSessionDate =
                        calculateLastActiveInSessionDate(broker, generatedSpan, aspenSpan.getTerminatingEnrollment());

                generatedSpan.setFirstActiveInSessionDate(firstActiveInSessionDate);
                generatedSpan.setLastActiveInSessionDate(lastActiveInSessionDate);
                generatedSpan.setDataStorageEnrollment(dataStorageEnrollment);

                /*
                 * Set the terminating enrollment on the span.
                 * This will be null if the span is auto-terminated by a context change
                 */
                if (loopYear == lastSchoolYear && terminatingEnrollment != null) {
                    generatedSpan.setTerminatingEnrollment(terminatingEnrollment);
                }

                /*
                 * withdrawalMemberDate is the W record date:
                 * It IS adjusted by memberOnWithdrawal preference.
                 * It is NOT adjusted to last in-session date.
                 * It is NOT set when the span is terminated by an S record.
                 * It is NOT set when the span is auto-terminated by a context change.
                 * This means that the withdrawalMemberDate can overlap into the next context year.
                 */
                PlainDate withdrawalMemberDate = null;
                if (generatedSpan.getTerminatingEnrollment() != null
                        && WITHDRAWAL.equals(generatedSpan.getTerminatingEnrollment().getEnrollmentType())
                        && generatedSpan.getSchool().getOid()
                                .equals(generatedSpan.getTerminatingEnrollment().getSchoolOid())) {
                    withdrawalMemberDate = generatedSpan.getTerminatingEnrollment().getEnrollmentDate();
                    if (!DistrictManager.isMemberOnWithdrawal(broker)) {
                        withdrawalMemberDate = DateUtils.add(withdrawalMemberDate, -1);
                    }
                    generatedSpan.setWithdrawalMemberDate(withdrawalMemberDate);
                }

                /*
                 * Determine the first Inactive In-session date,
                 * which is the first day of classes the student misses due to withdrawal
                 */
                PlainDate firstInactiveInSessionDate =
                        calculateFirstInactiveInSessionDate_FromWithdrawMemberDateOrTerminator(broker, generatedSpan);
                generatedSpan.setFirstInactiveInSessionDate(firstInactiveInSessionDate);
                generatedSpan.setDateAfterLastActiveInSessionDate(firstInactiveInSessionDate);

                if (firstActiveInSessionDate == null) {
                    continue;
                }

                if (lastActiveInSessionDate == null) {
                    continue;
                }

                boolean isSpanAtLeastOneDay = generatedSpan.getFirstActiveInSessionDate() != null
                        && (lastActiveInSessionDate == null ||
                                !lastActiveInSessionDate.before(generatedSpan.getFirstActiveInSessionDate()));
                if (!isSpanAtLeastOneDay) {
                    continue;
                }

                /*
                 * Un-terminate the span if it ends after queryAsOfDate
                 * - Set lastActiveInSessionDate = null
                 * - Remove Enrollment records after the queryAsOfDate date
                 */
                if (queryAsOfDate != null) {
                    if (lastActiveInSessionDate.after(queryAsOfDate)) {
                        generatedSpan.setLastActiveInSessionDate(null);
                        generatedSpan.m_enrollments = generatedSpan.m_enrollments.stream()
                                .filter(enr -> !enr.getEnrollmentDate().after(queryAsOfDate))
                                .collect(Collectors.toList());
                        generatedSpan.m_spanEnrollments = generatedSpan.m_spanEnrollments.stream()
                                .filter(enr -> !enr.getEnrollmentDate().after(queryAsOfDate))
                                .collect(Collectors.toList());
                    }
                }

                PlainDate historicalCutoffDate =
                        (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE);
                if (historicalCutoffDate != null) {
                    PlainDate spanLastDate = generatedSpan.getWithdrawalMemberOrLastActiveDate();
                    if (spanLastDate != null && spanLastDate.before(historicalCutoffDate)) {
                        continue;
                    }
                }

                splitSpans.add(generatedSpan);
            }

            return splitSpans;
        }

        /**
         * On-the-fly swap E/W records that are wrong based on just timestamp.
         *
         * @param enrollments List<? extends ToolEnrollment>
         * @return List
         */
        private List<? extends ToolEnrollment> swapEnrollments(List<? extends ToolEnrollment> enrollments) {
            List<ToolEnrollment> swappedEnrollments = new ArrayList<>();

            /*
             * On-the-fly swap E/W records that are reversed based on just timestamp.
             *
             * Condition:
             * 1. W (sklA) and E (sklB) record on same date, with W timestamp > E timestamp, but
             * different schoolOids
             *
             * Further validation:
             * 1. Future record after W is for sklB
             * 2. Past record before E is for sklA
             *
             * Then swap so E timestamp > W timestamp
             */
            ToolEnrollment futureRecord = null;
            ToolEnrollment pastRecord = null;
            ToolEnrollment wRecord = null;
            ToolEnrollment eRecord = null;

            // Walk down from newest -> oldest
            for (int i = 0; i < enrollments.size(); i++) {
                /*
                 * Condition:
                 * 1. W (sklA) and E (sklB) record on same date, with W timestamp > E timestamp, but
                 * different schoolOids
                 */
                // find w record
                ToolEnrollment testRecord = enrollments.get(i);
                if (!WITHDRAWAL.equals(testRecord.getEnrollmentType())) {
                    swappedEnrollments.add(testRecord);
                    continue;
                }
                wRecord = testRecord;

                // if no more records -> done
                if (i + 1 >= enrollments.size()) {
                    swappedEnrollments.add(wRecord);
                    break;
                }

                // is the next oldest record below the W an E record?
                eRecord = enrollments.get(i + 1);
                if (!ENTRY.equals(eRecord.getEnrollmentType())) {
                    swappedEnrollments.add(wRecord);
                    continue;
                }

                // are they on the same date?
                if (!wRecord.getEnrollmentDate().equals(eRecord.getEnrollmentDate())) {
                    swappedEnrollments.add(wRecord);
                    continue;
                }

                // are they for different schools?
                if (StringUtils.isEqual(wRecord.getSchoolOid(), eRecord.getSchoolOid())) {
                    swappedEnrollments.add(wRecord);
                    continue;
                }

                /*
                 * Further validation - make sure adjacent records will match schools after the swap
                 * 1. Future record after W is for sklB
                 * 2. Past record before E is for sklA
                 */
                // skip if the newer record above the W is the same school as the W
                if (i > 0) {
                    futureRecord = enrollments.get(i - 1);
                }
                if (futureRecord != null && !StringUtils.isEqual(eRecord.getSchoolOid(), futureRecord.getSchoolOid())) {
                    swappedEnrollments.add(wRecord);
                    continue;
                }

                // skip if the older record below the E is the same school as the E
                if (i + 2 < enrollments.size()) {
                    pastRecord = enrollments.get(i + 2);
                }
                if (pastRecord != null && !StringUtils.isEqual(wRecord.getSchoolOid(), pastRecord.getSchoolOid())) {
                    swappedEnrollments.add(wRecord);
                    continue;
                }

                /*
                 * All conditions met. Swap so E timestamp > W timestamp
                 */
                swappedEnrollments.add(eRecord);
                swappedEnrollments.add(wRecord);
                i++;
            }

            return swappedEnrollments;
        }

        /**
         * Update terminating enrollments.
         *
         * @param ascendingAspenSpans List<AspenSpan>
         * @param allDescendingEnrollments List<? extends ToolEnrollment>
         */
        private void updateTerminatingEnrollments(List<AspenSpan> ascendingAspenSpans,
                                                  List<? extends ToolEnrollment> allDescendingEnrollments) {
            /*
             * If a span doesn't have a terminatingEnrolment,
             * and there is a following enrollment record for a different school,
             * set the first span's terminating enrolment
             * to the following enrollment
             */
            for (int i = 0; i < ascendingAspenSpans.size(); i++) {
                AspenSpan span = ascendingAspenSpans.get(i);
                if (span.getTerminatingEnrollment() != null) {
                    continue;
                }

                ToolEnrollment nextEnrollment = getNextEnrollment(span, allDescendingEnrollments);
                span.setTerminatingEnrollment(nextEnrollment);
            }
        }


    }

    /**
     * The Class ToolStudentAttendance.
     */
    public static class ToolStudentAttendance extends ToolBean {

        public static final ToolBeanColumn FIELD_ABSENT_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.absentIndicator());
        public static final ToolBeanColumn FIELD_ABSENT_INDICATOR_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.absentIndicator02());
        public static final ToolBeanColumn FIELD_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.date(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_DISMISSED_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.dismissedIndicator());
        public static final ToolBeanColumn FIELD_EXCUSED_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.excusedIndicator());
        public static final ToolBeanColumn FIELD_OTHER_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.otherCode());
        public static final ToolBeanColumn FIELD_OTHER_CODE_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.otherCode02());
        public static final ToolBeanColumn FIELD_PORTION_ABSENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.portionAbsent());
        public static final ToolBeanColumn FIELD_REASON_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.reasonCode());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.schoolOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.studentOid());
        public static final ToolBeanColumn FIELD_TARDY_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.tardyIndicator());
        public static final ToolBeanColumn FIELD_TARDY_INDICATOR_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.tardyIndicator02());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ABSENT_INDICATOR,
                        FIELD_ABSENT_INDICATOR_02,
                        FIELD_DATE,
                        FIELD_DISMISSED_INDICATOR,
                        FIELD_EXCUSED_INDICATOR,
                        FIELD_OTHER_CODE,
                        FIELD_OTHER_CODE_02,
                        FIELD_PORTION_ABSENT,
                        FIELD_REASON_CODE,
                        FIELD_SCHOOL_OID,
                        FIELD_STUDENT_OID,
                        FIELD_TARDY_INDICATOR,
                        FIELD_TARDY_INDICATOR_02);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_ATTENDANCE.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolStudentAttendance(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the absent indicator.
         *
         * @return boolean
         */
        public boolean getAbsentIndicator() {
            return getValueLogical(FIELD_ABSENT_INDICATOR);
        }

        /**
         * Gets the absent indicator.
         *
         * @return boolean
         */
        public boolean getAbsentIndicator02() {
            return getValueLogical(FIELD_ABSENT_INDICATOR_02);
        }

        /**
         * Gets the date.
         *
         * @return PlainDate
         */
        public PlainDate getDate() {
            return getValueDate(FIELD_DATE);
        }

        /**
         * Gets the dismissed indicator.
         *
         * @return boolean
         */
        public boolean getDismissedIndicator() {
            return getValueLogical(FIELD_DISMISSED_INDICATOR);
        }

        /**
         * Gets the excused indicator.
         *
         * @return boolean
         */
        public boolean getExcusedIndicator() {
            return getValueLogical(FIELD_EXCUSED_INDICATOR);
        }

        /**
         * Returns the Other code.
         * <p>
         * The data dictionary ID for this property is <code>attOtherCode01</code>.
         *
         * @return String
         */
        public String getOtherCode() {
            return getValueString(FIELD_OTHER_CODE);
        }

        /**
         * Returns the Other code 2.
         * <p>
         * The data dictionary ID for this property is <code>attOtherCode02</code>.
         *
         * @return String
         */
        public String getOtherCode02() {
            return getValueString(FIELD_OTHER_CODE_02);
        }

        /**
         * Returns the Portion absent.
         * <p>
         * The data dictionary ID for this property is <code>attPortionAbs</code>.
         *
         * @return BigDecimal
         */
        public BigDecimal getPortionAbsent() {
            return getValueBigDecimal(FIELD_PORTION_ABSENT);
        }

        /**
         * Returns the Reason.
         * <p>
         * The data dictionary ID for this property is <code>attReason</code>.
         *
         * @return String
         */
        public String getReasonCode() {
            return this.getValueString(FIELD_REASON_CODE);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return ToolSchool
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return ToolStudent
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the tardy indicator.
         *
         * @return boolean
         */
        public boolean getTardyIndicator() {
            return getValueLogical(FIELD_TARDY_INDICATOR);
        }

        /**
         * Gets the tardy indicator 02.
         *
         * @return boolean
         */
        public boolean getTardyIndicator02() {
            return getValueLogical(FIELD_TARDY_INDICATOR_02);
        }

    }

    /**
     * The Class ToolStudentContextAttributes.
     */
    public static class ToolStudentContextAttributes extends ToolBean {
        public static final ToolBeanColumn FIELD_BLOB_INFORMATION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONTEXT_ATTRIBUTES.blobInformation());
        public static final ToolBeanColumn FIELD_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONTEXT_ATTRIBUTES.contextOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_BLOB_INFORMATION,
                        FIELD_CONTEXT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_CONTEXT_ATTRIBUTES.getBeanType();
        }

        /**
         * Instantiates a new tool student context attributes.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolStudentContextAttributes(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the attribute value.
         *
         * @param broker X2Broker
         * @param extractor DictionaryExtractor
         * @param column ToolBeanColumn
         * @param stateCode boolean
         * @return Object
         */
        public Object getAttributeValue(X2Broker broker,
                                        DictionaryExtractor extractor,
                                        ToolBeanColumn column,
                                        boolean stateCode) {
            Object foundObject = null;
            ModelProperty property = column.getModelProperty(extractor);
            Map<String, String> blobContent =
                    ContextAttributesManager.parseBlobContents(getBlobInformation());

            if (blobContent.containsKey(property.getDictionaryPath())) {
                String valueAsString = blobContent.get(property.getDictionaryPath());

                // Translate to state code if requested
                if (stateCode) {
                    // find the ref table from the original Student field
                    DataDictionaryField field = column.getField(extractor);
                    if (field == null) {
                        return null;
                    }

                    foundObject = valueAsString;
                    String refTableOid = field.getReferenceTableOid();
                    if (!StringUtils.isBlank(refTableOid)) {
                        foundObject = extractor.lookupReferenceCodeByRefTbl(refTableOid, valueAsString,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else {
                    if (property.getField() != null) {
                        Converter converter =
                                ConverterFactory.getConverterForClass(property.getField().getEffectiveJavaType(),
                                        LocalizationCache.getPrimarySystemLocale(broker.getPersistenceKey()),
                                        property.getField().isString());

                        if (converter != null) {
                            foundObject = converter.stringToJava(valueAsString);
                        } else {
                            foundObject = valueAsString;
                        }
                    }
                }
            }
            return foundObject;
        }

        /**
         * Gets the blob information.
         *
         * @return String
         */
        public String getBlobInformation() {
            return getValueString(FIELD_BLOB_INFORMATION);
        }

        /**
         * Gets the context oid.
         *
         * @return String
         */
        public String getContextOid() {
            return getValueString(FIELD_CONTEXT_OID);
        }
    }

    /**
     * The Class ToolStudentPeriodAttendance.
     */
    public static class ToolStudentPeriodAttendance extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_ABSENT_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.absentIndicator(),
                        PredefinedConverter.LOGICAL, true);
        public static final ToolBeanColumn FIELD_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.codeView());
        public static final ToolBeanColumn FIELD_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.date(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_MASTER_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterScheduleOid());
        public static final ToolBeanColumn FIELD_OTHER_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.otherCode());
        public static final ToolBeanColumn FIELD_OTHER_CODE_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.otherCode02());
        public static final ToolBeanColumn FIELD_PERIOD_VIEW =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.periodView());
        public static final ToolBeanColumn FIELD_REASON_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.reasonCode());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.schoolOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.studentOid());
        public static final ToolBeanColumn FIELD_TARDY_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.tardyIndicator(),
                        PredefinedConverter.LOGICAL, true);

        public static ToolBeanRelationship PARENT_SECTION =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getBeanType(),
                        SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getValueType(),
                        SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterScheduleOid().getPath(),
                        ToolSection.CHILD_PATH_PERIOD_ATTENDANCE,
                        SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.masterSchedule().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ABSENT_INDICATOR,
                        FIELD_CODE,
                        FIELD_DATE,
                        FIELD_MASTER_SCHEDULE_OID,
                        FIELD_OTHER_CODE,
                        FIELD_OTHER_CODE_02,
                        FIELD_PERIOD_VIEW,
                        FIELD_REASON_CODE,
                        FIELD_SCHOOL_OID,
                        FIELD_STUDENT_OID,
                        FIELD_TARDY_INDICATOR)
                .expandRelationships(
                        PARENT_SECTION);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.getBeanType();
        }

        /**
         * Instantiates a new tool student period attendance.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ToolStudentPeriodAttendance(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the Is absent?.
         * <p>
         * The data dictionary ID for this property is <code>patAbsentInd</code>.
         *
         * @return boolean
         */
        public boolean getAbsentIndicator() {
            return getValueLogical(FIELD_ABSENT_INDICATOR);
        }

        /**
         * Returns the Code View.
         * <p>
         * The data dictionary ID for this property is <code>patCodeView</code>.
         *
         * @return String
         */
        public String getCodeView() {
            return getValueString(FIELD_CODE);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return getValueDate(FIELD_DATE);
        }

        /**
         * Returns the Other code.
         * <p>
         * The data dictionary ID for this property is <code>patOtherCode01</code>.
         *
         * @return String
         */
        public String getOtherCode() {
            return getValueString(FIELD_OTHER_CODE);
        }

        /**
         * Returns the Other code 2.
         * <p>
         * The data dictionary ID for this property is <code>patOtherCode02</code>.
         *
         * @return String
         */
        public String getOtherCode02() {
            return getValueString(FIELD_OTHER_CODE_02);
        }

        /**
         * Returns the Period.
         * <p>
         * The data dictionary ID for this property is <code>patPeriodView</code>.
         *
         * @return String
         */
        public String getPeriodView() {
            return getValueString(FIELD_PERIOD_VIEW);
        }

        /**
         * Returns the Reason.
         * <p>
         * The data dictionary ID for this property is <code>patReason</code>.
         *
         * @return String
         */
        public String getReasonCode() {
            return getValueString(FIELD_REASON_CODE);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSection getSection(X2Broker broker) {
            String mstOid = getValueString(FIELD_MASTER_SCHEDULE_OID);
            return getBeanByOid(broker, ToolSection.class, mstOid, true);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return Tool school
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return ToolBean.getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Onsis student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return ToolBean.getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Returns the Is tardy?.
         * <p>
         * The data dictionary ID for this property is <code>patTardyInd</code>.
         *
         * @return boolean
         */
        public boolean getTardyIndicator() {
            return getValueLogical(FIELD_TARDY_INDICATOR);
        }

    }

    /**
     * The Class ToolStudentProgramParticipation.
     */
    public static class ToolStudentProgramParticipation extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate(),
                        PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_PROGRAM_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programCode());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.startDate(),
                        PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_END_DATE,
                        FIELD_PROGRAM_CODE,
                        FIELD_START_DATE,
                        FIELD_STUDENT_OID);

        /**
         * Gets the date range criteria.
         *
         * @param broker X2Broker
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return X 2 criteria
         */
        public static final X2Criteria getDateRangeCriteria(X2Broker broker, PlainDate startDate, PlainDate endDate) {
            X2Criteria pgmCriteria = new X2Criteria();
            X2Criteria endDateEmpty = new X2Criteria();
            endDateEmpty.addEmpty(FIELD_END_DATE.resolve(null), broker.getPersistenceKey());
            X2Criteria notBeforeSubmissionStart = new X2Criteria();
            notBeforeSubmissionStart.addGreaterOrEqualThan(FIELD_END_DATE.resolve(null),
                    startDate);
            notBeforeSubmissionStart.addGreaterOrEqualThanField(FIELD_END_DATE.resolve(null),
                    FIELD_START_DATE.resolve(null));
            endDateEmpty.addOrCriteria(notBeforeSubmissionStart);
            pgmCriteria.addAndCriteria(endDateEmpty);
            pgmCriteria.addLessOrEqualThan(FIELD_START_DATE.resolve(null), endDate);
            return pgmCriteria;
        }

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolStudentProgramParticipation(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the program code.
         *
         * @return String
         */
        public String getProgramCode() {
            return getValueString(FIELD_PROGRAM_CODE);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Onsis student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return ToolBean.getBeanByOid(broker, ToolStudent.class, stdOid, true);

        }
    }

    /**
     * The Class ToolStudentSchedule.
     */
    public static class ToolStudentSchedule extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE.scheduleOid());
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE.sectionOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE.studentOid());

        public static ToolBeanRelationship PARENT_SECTION =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_SCHEDULE.section().getBeanType(),
                        SisBeanPaths.STUDENT_SCHEDULE.section().getValueType(),
                        SisBeanPaths.STUDENT_SCHEDULE.sectionOid().toString(),
                        SisBeanPaths.SCHEDULE_MASTER.studentSections().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE.section().getRelationshipType());
        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_SCHEDULE.student().getBeanType(),
                        SisBeanPaths.STUDENT_SCHEDULE.student().getValueType(),
                        SisBeanPaths.STUDENT_SCHEDULE.studentOid().getPath(),
                        SisBeanPaths.STUDENT.studentSchedules().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_SCHEDULE_OID,
                        FIELD_SECTION_OID,
                        FIELD_STUDENT_OID)
                .expandRelationships(PARENT_STUDENT,
                        PARENT_SECTION);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_SCHEDULE.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolStudentSchedule(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSection getSection(X2Broker broker) {
            String mstOid = getValueString(FIELD_SECTION_OID);
            return getBeanByOid(broker, ToolSection.class, mstOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSectionOid() {
            return getValueString(FIELD_SECTION_OID);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }

    }


    /**
     * The Class ToolStudentScheduleChange.
     */
    public static class ToolStudentScheduleChange extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_SCHEDULE_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.scheduleOid());
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterScheduleOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.studentOid());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().districtContextOid());
        public static final ToolBeanColumn FIELD_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.date(), PredefinedConverter.PLAINDATE, true);
        // sort in descending order
        public static final ToolBeanColumn FIELD_EFFECTIVE_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.effectiveDate(), PredefinedConverter.PLAINDATE,
                        false);
        // If Add/Drop on same date/time, sort Drop then Add in descending order
        public static final ToolBeanColumn FIELD_CHANGE_TYPE_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.changeTypeCode(), false);


        // Nonquery Fields
        // sort in descending order
        public static final ToolBeanColumn FIELD_TIMESTAMP =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.timestamp(), false);

        public static ToolBeanRelationship PARENT_SECTION =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().getBeanType(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().getValueType(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterScheduleOid().toString(),
                        SisBeanPaths.SCHEDULE_MASTER.studentScheduleChanges().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().getRelationshipType());
        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.student().getBeanType(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.student().getValueType(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.studentOid().getPath(),
                        SisBeanPaths.STUDENT.studentScheduleChanges().getPath(),
                        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_SCHEDULE_OID,
                        FIELD_SECTION_OID,
                        FIELD_STUDENT_OID,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_DATE,
                        FIELD_EFFECTIVE_DATE,
                        FIELD_CHANGE_TYPE_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        X2Criteria actionDateCriteria = new X2Criteria();
                        actionDateCriteria.addNotNull(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.actionDate().getPath());

                        X2Criteria effectiveDateCriteria = new X2Criteria();
                        effectiveDateCriteria
                                .addNotNull(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.effectiveDate().getPath());

                        actionDateCriteria.addOrCriteria(effectiveDateCriteria);
                        criteria.addAndCriteria(actionDateCriteria);
                        return criteria;
                    }
                })
                .expandRelationships(PARENT_STUDENT,
                        PARENT_SECTION)
                .expandSort(FIELD_STUDENT_OID,
                        FIELD_SECTION_OID,
                        FIELD_EFFECTIVE_DATE,
                        FIELD_TIMESTAMP,
                        FIELD_CHANGE_TYPE_CODE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_SCHEDULE_CHANGE.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolStudentScheduleChange(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSection getSection(X2Broker broker) {
            String mstOid = getValueString(FIELD_SECTION_OID);
            return getBeanByOid(broker, ToolSection.class, mstOid, true);
        }

        /**
         * Gets the change type code.
         *
         * @return String
         */
        public String getChangeTypeCode() {
            return getValueString(FIELD_CHANGE_TYPE_CODE);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return getValueDate(FIELD_DATE);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getDistrictContextOid() {
            return getValueString(FIELD_DISTRICT_CONTEXT_OID);
        }

        /**
         * Gets the effective date.
         *
         * @return Plain date
         */
        public PlainDate getEffectiveDate() {
            return getValueDate(FIELD_EFFECTIVE_DATE);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSchedule getSchedule(X2Broker broker) {
            String schOid = getValueString(FIELD_SCHEDULE_OID);
            return getBeanByOid(broker, ToolSchedule.class, schOid, true);
        }

        /**
         * Gets the schedule change date.
         *
         * @param broker X2Broker
         * @return Plain date
         */
        public PlainDate getScheduleChangeDate(X2Broker broker) {
            PlainDate effectiveDate = getEffectiveDate();
            if (effectiveDate == null) {
                effectiveDate = getDate();
            }


            /*
             * 2020-12-15 New logic for schedule span exit date:
             * Only back-date to last active if the Drop-1 date is < term end.
             * If Drop-1 date is on/after Term End Date, set Span Exit Date = Term End Date.
             */
            if (StudentScheduleChange.CODE_DROP.equals(getChangeTypeCode())
                    && !DistrictManager.isMemberOnWithdrawal(broker)) {
                // adjust -1 for memberOnWithdrawal
                PlainDate dropMinusOne = DateUtils.add(effectiveDate, -1);

                // if before term end date, adjust back to last in-session date
                Range<Date> sectionDateRange = getSection(broker).getSectionDateRange(broker);
                PlainDate termEnd = (PlainDate) sectionDateRange.getEnd();

                // If Drop-1 < Term End Date, back-date to last in-session date before effectiveDate
                if (termEnd != null) {
                    if (dropMinusOne.before(termEnd)) {
                        ToolSchool school = getSchedule(broker).getSchool(broker);
                        ToolStudent student = getStudent(broker);
                        boolean after = false;
                        effectiveDate = school.findSessionDate(broker, getDistrictContextOid(),
                                student.getCalendarCode(), effectiveDate, after);
                    } else {
                        effectiveDate = termEnd;
                    }
                }
            }

            return effectiveDate;
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getScheduleOid() {
            return getValueString(FIELD_SCHEDULE_OID);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSectionOid() {
            return getValueString(FIELD_SECTION_OID);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }

    }


    /**
     * The Class StudentSchool.
     */
    public static class ToolStudentSchool extends ToolBean {
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.endDate(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.startDate(), PredefinedConverter.PLAINDATE, true);
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.studentOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.school().oid());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.districtContextOid());

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_SCHOOL_ARCHIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.school().archiveIndicator());
        public static final ToolBeanColumn FIELD_SCHOOL_INACTIVE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.school().inactiveIndicator());
        public static final ToolBeanColumn FIELD_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.type());
        public static final ToolBeanColumn FIELD_START_DATE_DESC =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL.startDate(), false);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_END_DATE, FIELD_START_DATE, FIELD_STUDENT_OID, FIELD_SCHOOL_OID,
                        FIELD_DISTRICT_CONTEXT_OID)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(ToolStudentSchool.FIELD_TYPE.resolve(null),
                                Integer.valueOf(StudentSchool.SECONDARY));
                        criteria.addNotEqualTo(ToolStudentSchool.FIELD_SCHOOL_INACTIVE_INDICATOR.resolve(null),
                                Boolean.TRUE);
                        criteria.addNotEqualTo(ToolStudentSchool.FIELD_SCHOOL_ARCHIVE_INDICATOR.resolve(null),
                                Boolean.TRUE);
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_SCHOOL.getBeanType();
        }

        /**
         * Instantiates a new student school.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolStudentSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return School
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the district context.
         *
         * @param broker X2Broker
         * @return School year context
         */
        public ToolDistrictContext getDistrictContext(X2Broker broker) {
            String ctxOid = getValueString(FIELD_DISTRICT_CONTEXT_OID);
            return getBeanByOid(broker, ToolDistrictContext.class, ctxOid, true);
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }
    }


    /**
     * The Class ToolTranscript.
     */
    public static class ToolTranscript extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_COURSE_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.courseDescription());
        public static final ToolBeanColumn FIELD_COURSE_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course().oid());
        public static final ToolBeanColumn FIELD_CSK_CREDIT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().credit());
        public static final ToolBeanColumn FIELD_CSK_NUMBER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().number());
        public static final ToolBeanColumn FIELD_CSK_EQUIV_NUMBER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.equivalentSchoolCourse().number());
        public static final ToolBeanColumn FIELD_CSK_EQUIV_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.equivalentSchoolCourseOid());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.districtContextOid());
        public static final ToolBeanColumn FIELD_FINAL_GRADE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.finalGrade());
        public static final ToolBeanColumn FIELD_GRADE_LEVEL =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.gradeLevel());
        public static final ToolBeanColumn FIELD_GRADE_TERM_DEFINITION_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.transcriptDefinition().gradeTermDefinitionOid());
        public static final ToolBeanColumn FIELD_POTENTIAL_CREDIT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.potentialCredit());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolOid());
        public static final ToolBeanColumn FIELD_SECTION_COURSE_VIEW =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.masterSchedule().courseView());
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.masterScheduleOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.studentOid());
        public static final ToolBeanColumn FIELD_TERM_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.termCode());
        public static final ToolBeanColumn FIELD_TOTAL_CREDIT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.totalCredit(), PredefinedConverter.BIG_DECIMAL,
                        true);
        public static final ToolBeanColumn FIELD_TRANSCRIPT_DEFINITION_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.transcriptDefinitionOid());
        public static final ToolBeanColumn FIELD_USER_DESCRIPTION_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.userDescriptionIndicator());

        public static ToolBeanRelationship PARENT_SECTION =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_TRANSCRIPT.masterSchedule().getBeanType(),
                        SisBeanPaths.STUDENT_TRANSCRIPT.masterSchedule().getValueType(),
                        SisBeanPaths.STUDENT_TRANSCRIPT.masterScheduleOid().toString(),
                        null,
                        SisBeanPaths.STUDENT_TRANSCRIPT.masterSchedule().getRelationshipType());
        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(SisBeanPaths.STUDENT_TRANSCRIPT.student().getBeanType(),
                        SisBeanPaths.STUDENT_TRANSCRIPT.student().getValueType(),
                        SisBeanPaths.STUDENT_TRANSCRIPT.studentOid().getPath(),
                        SisBeanPaths.STUDENT.transcripts().getPath(),
                        SisBeanPaths.STUDENT_TRANSCRIPT.student().getRelationshipType());

        /*
         * IMPORTANT NOTE: FIELD_CSK_EQUIV_NUMBER must be first field so that the join adjuster
         * applies to the correct table
         */
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_CSK_EQUIV_NUMBER,
                        FIELD_CSK_EQUIV_OID,
                        FIELD_COURSE_DESCRIPTION,
                        FIELD_COURSE_OID,
                        FIELD_CSK_CREDIT,
                        FIELD_CSK_NUMBER,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_FINAL_GRADE,
                        FIELD_GRADE_LEVEL,
                        FIELD_GRADE_TERM_DEFINITION_OID,
                        FIELD_POTENTIAL_CREDIT,
                        FIELD_SCHOOL_OID,
                        FIELD_SECTION_COURSE_VIEW,
                        FIELD_SECTION_OID,
                        FIELD_STUDENT_OID,
                        FIELD_TERM_CODE,
                        FIELD_TOTAL_CREDIT,
                        FIELD_TRANSCRIPT_DEFINITION_OID,
                        FIELD_USER_DESCRIPTION_INDICATOR,
                        FIELD_TERM_CODE)
                .expandRelationships(PARENT_STUDENT,
                        PARENT_SECTION)
                .expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.COURSE_SCHOOL.getDatabaseName(),
                                SisBeanPaths.STUDENT_TRANSCRIPT.equivalentSchoolCourseOid().getDatabaseName()),
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.SCHEDULE_MASTER.getDatabaseName()));

        private static final ToolBeanDefinition LOOKUP_GRADE_TERM_DEFINITION =
                new ToolBeanDefinition(ToolGradeTermDate.FIELD_DISTRICT_CONTEXT_OID,
                        ToolGradeTermDate.FIELD_GRADE_TERM_DEFINITION_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_TRANSCRIPT.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolTranscript(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return Tool school
         */
        public ToolCourse getCourse(X2Broker broker) {
            String crsOid = getValueString(FIELD_COURSE_OID);
            return getBeanByOid(broker, ToolCourse.class, crsOid, true);
        }

        /**
         * Gets the course description.
         *
         * @return String
         */
        public String getCourseDescription() {
            return getValueString(FIELD_COURSE_DESCRIPTION);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolDistrictContext getDistrictContext(X2Broker broker) {
            String ctxOid = getValueString(FIELD_DISTRICT_CONTEXT_OID);
            return getBeanByOid(broker, ToolDistrictContext.class, ctxOid, true);
        }

        /**
         * Gets the district context oid.
         *
         * @return String
         */
        public String getDistrictContextOid() {
            return getValueString(FIELD_DISTRICT_CONTEXT_OID);
        }

        /**
         * Gets the equivalent school course number.
         *
         * @return String
         */
        public String getEquivalentSchoolCourseNumber() {
            return getValueString(FIELD_CSK_EQUIV_NUMBER);
        }

        /**
         * Gets the equivalent school course oid.
         *
         * @return the equivalent school course oid
         */
        public String getEquivalentSchoolCourseOid() {
            return getValueString(FIELD_CSK_EQUIV_OID);
        }

        /**
         * Returns the Final grade.
         * <p>
         * The data dictionary ID for this property is <code>trnFinalGrade</code>.
         *
         * @return String
         */
        public String getFinalGrade() {
            return getValueString(FIELD_FINAL_GRADE);
        }

        /**
         * Gets the calendars.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolGradeTermDate> getGradeTermDates(X2Broker broker) {
            return getSchool(broker).getGradeTermDates(broker).getGroup(LOOKUP_GRADE_TERM_DEFINITION,
                    Arrays.asList(getDistrictContextOid(), getGradeTermDefinitionOid()));
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return getValueString(FIELD_GRADE_LEVEL);
        }

        /**
         * Gets the grade term definition oid.
         *
         * @return String
         */
        public String getGradeTermDefinitionOid() {
            return getValueString(FIELD_GRADE_TERM_DEFINITION_OID);
        }

        /**
         * Returns the Potential credit override.
         * <p>
         * The data dictionary ID for this property is <code>trnPotCrdOvrd</code>.
         *
         * @return String
         */
        public String getPotentialCredit() {
            return getValueString(FIELD_POTENTIAL_CREDIT);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Returns the Credit.
         * <p>
         * The data dictionary ID for this property is <code>cskCredit</code>.
         *
         * @return BigDecimal
         */
        public BigDecimal getSchoolCourseCredit() {
            return this.getValueBigDecimal(FIELD_CSK_CREDIT);
        }

        /**
         * Gets the school course number.
         *
         * @return String
         */
        public String getSchoolCourseNumber() {
            return getValueString(FIELD_CSK_NUMBER);
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Gets the section.
         *
         * @param broker X2Broker
         * @return Tool section
         */
        public ToolSection getSection(X2Broker broker) {
            String mstOid = getValueString(FIELD_SECTION_OID);
            return getBeanByOid(broker, ToolSection.class, mstOid, true);
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSectionOid() {
            return getValueString(FIELD_SECTION_OID);
        }

        /**
         * Returns the student.
         * <p>
         * The data dictionary ID for this property is <code>relTrnStdOid</code>.
         *
         * @param broker X2Broker
         * @return SisStudent
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Returns the Related student object identifier.
         * <p>
         * The data dictionary ID for this property is <code>trnStdOID</code>.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }

        /**
         * Gets the term code.
         *
         * @return String
         */
        public String getTermCode() {
            return getValueString(FIELD_TERM_CODE);
        }

        /**
         * Gets the total credit.
         *
         * @return BigDecimal
         */
        public BigDecimal getTotalCredit() {
            return this.getValueBigDecimal(FIELD_TOTAL_CREDIT);
        }


        /**
         * Gets the transcript definition.
         *
         * @param broker X2Broker
         * @return Tool transcript definition
         */
        public ToolTranscriptDefinition getTranscriptDefinition(X2Broker broker) {
            String gtdOid = getValueString(FIELD_TRANSCRIPT_DEFINITION_OID);
            return getBeanByOid(broker, ToolTranscriptDefinition.class, gtdOid, true);
        }

        /**
         * Gets the transcript definition oid.
         *
         * @return String
         */
        public String getTranscriptDefinitionOid() {
            return getValueString(FIELD_TRANSCRIPT_DEFINITION_OID);
        }

        /**
         * Gets the user description indicator.
         *
         * @return String
         */
        public Boolean getUserDescriptionIndicator() {
            return getValueLogical(FIELD_USER_DESCRIPTION_INDICATOR);
        }

    }

    /**
     * The Class ToolTranscriptColumnDefinition.
     */
    public static class ToolTranscriptColumnDefinition extends ToolBean {
        public static final ToolBeanColumn FIELD_ALIAS =
                new ToolBeanColumn(SisBeanPaths.GRADE_TRANS_COLUMN_DEFINITION.gradeColumnHeader());
        public static final ToolBeanColumn FIELD_GRADE_TERM_OID =
                new ToolBeanColumn(SisBeanPaths.GRADE_TRANS_COLUMN_DEFINITION.gradeTermOid());
        public static final ToolBeanColumn FIELD_JAVA_NAME =
                new ToolBeanColumn(SisBeanPaths.GRADE_TRANS_COLUMN_DEFINITION.dataFieldConfig().dataField().javaName());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ALIAS,
                        FIELD_GRADE_TERM_OID,
                        FIELD_JAVA_NAME);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADE_TRANS_COLUMN_DEFINITION.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolTranscriptColumnDefinition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the alias.
         *
         * @return String
         */
        public String getAlias() {
            return getValueString(FIELD_ALIAS);
        }

        /**
         * Gets the grade term oid.
         *
         * @return String
         */
        public String getGradeTermOid() {
            return getValueString(FIELD_GRADE_TERM_OID);
        }

        /**
         * Gets the java name.
         *
         * @return String
         */
        public String getJavaName() {
            return getValueString(FIELD_JAVA_NAME);
        }

    }

    /**
     * The Class ToolTranscriptDefinition.
     */
    public static class ToolTranscriptDefinition extends ToolBean {
        public static ToolBeanRelationship CHILD_TRANCRIPT_COLUMN_DEFINITIONS =
                new ToolBeanRelationship(
                        SisBeanPaths.GRADE_TRANSCRIPT_DEFINITION.transcriptColumnDefinitions().getBeanType(),
                        SisBeanPaths.GRADE_TRANSCRIPT_DEFINITION.transcriptColumnDefinitions().getValueType(),
                        SisBeanPaths.GRADE_TRANSCRIPT_DEFINITION.transcriptColumnDefinitions().getPath(),
                        SisBeanPaths.GRADE_TRANS_COLUMN_DEFINITION.transcriptDefinitionOid().getPath(),
                        SisBeanPaths.GRADE_TRANSCRIPT_DEFINITION.transcriptColumnDefinitions().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expandRelationships(CHILD_TRANCRIPT_COLUMN_DEFINITIONS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADE_TRANSCRIPT_DEFINITION.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public ToolTranscriptDefinition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the transcript column definitions.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ToolTranscriptColumnDefinition> getTranscriptColumnDefinitions(X2Broker broker) {
            return (List<ToolTranscriptColumnDefinition>) getChildren(broker, CHILD_TRANCRIPT_COLUMN_DEFINITIONS);
        }
    }

    /**
     * The Class ToolTranscriptRubric.
     */
    public static class ToolTranscriptRubric extends ToolBean {
        public static final ToolBeanColumn FIELD_COURSE_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().schoolCourse().description());
        public static final ToolBeanColumn FIELD_DISTRICT_CONTEXT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().districtContextOid());
        public static final ToolBeanColumn FIELD_GRADE_TERM_ID =
                new ToolBeanColumn(
                        SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcriptColumnDefinition().gradeTerm().gradeTermId());
        public static final ToolBeanColumn FIELD_GTC_REPORT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcriptColumnDefinition().reportType());
        public static final ToolBeanColumn FIELD_RUBRIC_ASSESSMENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.rubricAssessmentOid());
        public static final ToolBeanColumn FIELD_SCHOOL_COURSE_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().schoolCourseOid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().schoolCourse().schoolOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().studentOid());
        public static final ToolBeanColumn FIELD_TRANSCRIPT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcriptOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_COURSE_DESCRIPTION,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_GRADE_TERM_ID,
                        FIELD_GTC_REPORT_TYPE,
                        FIELD_RUBRIC_ASSESSMENT_OID,
                        FIELD_SCHOOL_COURSE_OID,
                        FIELD_SCHOOL_OID,
                        FIELD_STUDENT_OID,
                        FIELD_TRANSCRIPT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.getBeanType();
        }

        /**
         * Instantiates a new tool transcript rubric.
         *
         * @param columns the columns
         * @param data the data
         */
        public ToolTranscriptRubric(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the course description.
         *
         * @return the course description
         */
        public String getCourseDescription() {
            return getValueString(FIELD_COURSE_DESCRIPTION);
        }

        /**
         * Gets the grade term id.
         *
         * @return the grade term id
         */
        public String getGradeTermId() {
            return getValueString(FIELD_GRADE_TERM_ID);
        }

        /**
         * Gets the rubric assessment.
         *
         * @param broker the broker
         * @return the rubric assessment
         */
        public ToolRubricAssessment getRubricAssessment(X2Broker broker) {
            String rbaOid = getValueString(FIELD_RUBRIC_ASSESSMENT_OID);
            return getBeanByOid(broker, ToolRubricAssessment.class, rbaOid, true);
        }

        /**
         * Gets the rubric assessment oid.
         *
         * @return the rubric assessment oid
         */
        public String getRubricAssessmentOid() {
            return getValueString(FIELD_RUBRIC_ASSESSMENT_OID);
        }

        /**
         * Gets the school course oid.
         *
         * @return String
         */
        public String getSchoolCourseOid() {
            return getValueString(FIELD_SCHOOL_COURSE_OID);
        }

        /**
         * Gets the transcript.
         *
         * @param broker the broker
         * @return the transcript
         */
        public ToolTranscript getTranscript(X2Broker broker) {
            String trnOid = getValueString(FIELD_TRANSCRIPT_OID);
            return getBeanByOid(broker, ToolTranscript.class, trnOid, true);
        }

        /**
         * Gets the transcript oid.
         *
         * @return the transcript oid
         */
        public String getTranscriptOid() {
            return getValueString(FIELD_TRANSCRIPT_OID);
        }

    }

    // STATIC member variables
    private static final ToolBeanCache s_beanCache = new ToolBeanCache();
    private static X2Broker s_broker;
    private static final Map<String, X2Criteria> s_criteriaCache = new HashMap();
    private static Predicate<String> s_debugPredicate = null;
    private static final Map<String, Class<? extends ToolBean>> s_defaultClass = new HashMap();
    private static DictionaryExtractor s_dictionaryExtractor;
    private static HashMap<String, Pair<Method, Object[]>> s_getters = null;
    private static final Map<String, PlainDate> s_plainDateValues = new HashMap();
    private static final Map<String, Object> s_preference = new HashMap();
    private static final Map<String, Class<? extends ToolBean>> s_registeredClass = new HashMap();

    // STATIC constants
    public static final String EMPTY_STRING = "";
    public static final int MAX_SAFE_PARAMETERS = 1800;
    public static final String PREFERENCE_AS_OF_DATE = "asOfDate";
    public static final String PREFERENCE_CURRENT_CONTEXT = "currentDistrictContext";
    public static final String PREFERENCE_ENR_IGNORE_FOR_SPANS = "ignoreForSpans";
    public static final String PREFERENCE_EXCLUDE_STUDENT = "excludeStudent";
    public static final String PREFERENCE_EXCLUDE_STUDENT_COLUMN = "excludeStudentAlias";
    public static final String PREFERENCE_HISTORICAL_CUTOFF_DATE = "historicalCutoffDate";
    public static final String PREFERENCE_INCLUDE_SECONDARY_SPANS = "includeSecondarySpans";
    public static final String PREFERENCE_LIMITING_SCHOOL_OIDS = "limitingSchoolOids";
    public static final String PREFERENCE_LIMITING_STUDENT_CRITERIA = "limitingStudentCriteria";
    public static final String PREFERENCE_LIMITING_STUDENT_OIDS = "limitingStudentOids";
    public static final String PREFERENCE_QUERY_AS_OF_DATE = "queryAsOfDate";

    private static final long DURATION_YEAR_MS = 365 * 24 * 60 * 60 * 1000;
    private static final long DURATION_WEEK_MS = 7 * 24 * 60 * 60 * 1000;

    private static final Class<?>[] EMPTY_PARAM_CLASSES = new Class<?>[] {};
    private static final Object[] EMPTY_PARAMS = new Object[] {};

    // ToolBean base definition
    public static final ToolBeanColumn FIELD_OID =
            new ToolBeanColumn(X2BaseBean.COL_OID);

    public static final ToolBeanDefinition FULL_DEFINITION =
            new ToolBeanDefinition().expandKeys(FIELD_OID);

    // STATIC DEFAULT initialization
    static {
        setPreference(PREFERENCE_ENR_IGNORE_FOR_SPANS, new Predicate<ToolEnrollment>() {
            @Override
            public boolean test(ToolEnrollment t) {
                return false;
            }
        });
        setPreference(PREFERENCE_HISTORICAL_CUTOFF_DATE, null);
        setPreference(PREFERENCE_QUERY_AS_OF_DATE, null);
    }

    static {
        registerDefaultClass(ToolAddress.class);
        registerDefaultClass(ToolConductAction.class);
        registerDefaultClass(ToolConductIncident.class);
        registerDefaultClass(ToolConductOffense.class);
        registerDefaultClass(ToolCourse.class);
        registerDefaultClass(ToolDistrictContext.class);
        registerDefaultClass(ToolEnrollment.class);
        registerDefaultClass(ToolGradeTermDate.class);
        registerDefaultClass(ToolMasterTerm.class);
        registerDefaultClass(ToolOrganization.class);
        registerDefaultClass(ToolPersonAddress.class);
        registerDefaultClass(ToolRace.class);
        registerDefaultClass(ToolRubricAssessment.class);
        registerDefaultClass(ToolRubricAssessmentPerformance.class);
        registerDefaultClass(ToolRubricCriterion.class);
        registerDefaultClass(ToolRubricDefinition.class);
        registerDefaultClass(ToolSchedule.class);
        registerDefaultClass(ToolScheduleBell.class);
        registerDefaultClass(ToolScheduleBellPeriod.class);
        registerDefaultClass(ToolScheduleClass.class);
        registerDefaultClass(ToolScheduleDay.class);
        registerDefaultClass(ToolSchedulePeriod.class);
        registerDefaultClass(ToolScheduleTeacher.class);
        registerDefaultClass(ToolScheduleTerm.class);
        registerDefaultClass(ToolScheduleTermDate.class);
        registerDefaultClass(ToolSchool.class);
        registerDefaultClass(ToolSchoolCalendar.class);
        registerDefaultClass(ToolSchoolCalendarDate.class);
        registerDefaultClass(ToolSection.class);
        registerDefaultClass(ToolStaff.class);
        registerDefaultClass(ToolStaffPosition.class);
        registerDefaultClass(ToolStudent.class);
        registerDefaultClass(ToolStudentAttendance.class);
        registerDefaultClass(ToolStudentContextAttributes.class);
        registerDefaultClass(ToolStudentPeriodAttendance.class);
        registerDefaultClass(ToolStudentProgramParticipation.class);
        registerDefaultClass(ToolStudentSchedule.class);
        registerDefaultClass(ToolStudentScheduleChange.class);
        registerDefaultClass(ToolStudentSchool.class);
        registerDefaultClass(ToolTranscript.class);
        registerDefaultClass(ToolTranscriptColumnDefinition.class);
        registerDefaultClass(ToolTranscriptDefinition.class);
        registerDefaultClass(ToolTranscriptRubric.class);
        // X2 direct registers for children
        registerDefaultClass(SchoolCalendarDate.class, ToolSchoolCalendarDate.class);
    }

    // STATIC methods
    /**
     * Adds the and criteria.
     *
     * @param broker X2Broker
     * @param clazz Class<? extends ToolBean>
     * @param criteria X2Criteria
     */
    public static void addAndCriteria(X2Broker broker, Class<? extends ToolBean> clazz, X2Criteria criteria) {
        X2Criteria currentCriteria = getCriteria(broker, clazz);
        currentCriteria.addAndCriteria(criteria);
    }

    /**
     * Cache tool bean.
     *
     * @param definition ToolBeanDefinition
     * @param bean ToolBean
     * @param groupKey String
     */
    public static void cacheToolBean(ToolBeanDefinition definition, ToolBean bean, String groupKey) {
        s_beanCache.cacheToolBean(definition, bean, groupKey);
    }

    /**
     * Debug.
     *
     * @param message String
     */
    public static void debug(String message) {
        if (s_debugPredicate != null) {
            s_debugPredicate.test(message);
        }
    }

    /**
     * Clear all cached tool beans.
     *
     * @param clazzToolBean Class<? extends ToolBean>
     */
    public static void clearAllCachedToolBeans(final Class<? extends ToolBean> clazzToolBean) {
        s_beanCache.clearAllCachedToolBeans(clazzToolBean);
    }

    /**
     * Filter cached tool beans.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @param test Predicate<T>
     * @return Collection
     */
    public static <T extends ToolBean> Collection<T> filterCachedToolBeans(final Class<T> clazzToolBean,
                                                                           Predicate<T> test) {
        List<String> oidsToDelete = new ArrayList();
        s_beanCache.getCachedToolBeans(clazzToolBean).forEach(bean -> {
            if (!test.test(bean)) {
                oidsToDelete.add(bean.getOid());
            }
        });
        removeCachedToolBeans(clazzToolBean, oidsToDelete, null);
        return s_beanCache.getCachedToolBeans(clazzToolBean);
    }

    /**
     * Gets the bean by oid.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @param oid String
     * @param resolveUsingX2Class boolean
     * @return t
     */
    public static <T extends ToolBean> T getBeanByOid(final Class<T> clazzToolBean,
                                                      final String oid,
                                                      final boolean resolveUsingX2Class) {
        return getBeanByOid(null, null, clazzToolBean, oid, resolveUsingX2Class);
    }

    /**
     * Gets the bean by oid.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param clazzToolBean Class<T>
     * @param oid String
     * @return t
     */
    public static <T extends ToolBean> T getBeanByOid(final X2Broker broker,
                                                      final Class<T> clazzToolBean,
                                                      final String oid) {
        return getBeanByOid(broker, null, clazzToolBean, oid, false);
    }

    /**
     * Gets the bean by oid.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param clazzToolBean Class<T>
     * @param oid String
     * @param resolveUsingX2Class boolean
     * @return t
     */
    public static <T extends ToolBean> T getBeanByOid(final X2Broker broker,
                                                      final Class<T> clazzToolBean,
                                                      final String oid,
                                                      final boolean resolveUsingX2Class) {
        return getBeanByOid(broker, null, clazzToolBean, oid, resolveUsingX2Class);
    }

    /**
     * Gets the bean by oid.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param clazzToolBean Class<T>
     * @param oid String
     * @param resolveUsingX2Class boolean
     * @return t
     */
    public static <T extends ToolBean> T getBeanByOid(final X2Broker broker,
                                                      DictionaryExtractor dictionaryExtractor,
                                                      Class<T> clazzToolBean,
                                                      final String oid,
                                                      final boolean resolveUsingX2Class) {
        if (StringUtils.isEmpty(oid)) {
            return null;
        }
        if (resolveUsingX2Class) {
            Class<X2BaseBean> x2Class = getX2BaseClassForClass(clazzToolBean);
            clazzToolBean = getRegisteredClass(x2Class.getName(), clazzToolBean);
        }
        T cachedBean = getCachedToolBean(clazzToolBean, oid);
        if (cachedBean != null) {
            return cachedBean;
        }
        if (broker == null) {
            throw new IllegalStateException(
                    "Cannot create bean without broker - " + clazzToolBean.getName() + "[" + oid + "]");
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, oid);

        ToolBeanQuery query =
                new ToolBeanQuery(broker, dictionaryExtractor, clazzToolBean, criteria, null);
        try (ToolBeanQueryIterator iterator = query.iterator(broker, "beanByOid")) {
            while (iterator.hasNext()) {
                T bean = (T) iterator.next();
                return bean;
            }
        } catch (Exception e) {
            throw new X2RuntimeException(e);
        }
        return null;
    }

    /**
     * Gets the broker.
     *
     * @param isRequired the is required
     * @return the broker
     */
    public static X2Broker getBroker(boolean isRequired) {
        if (isRequired && s_broker == null) {
            throw new IllegalStateException("ToolBean broker must be set.");
        }
        return s_broker;
    }

    /**
     * Gets the cached tool bean.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @param oid String
     * @return t
     */
    public static <T extends ToolBean> T getCachedToolBean(final Class<T> clazzToolBean, final String oid) {
        return s_beanCache.getCachedToolBean(clazzToolBean, oid);
    }

    /**
     * Gets the cached tool beans.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @return Collection
     */
    public static <T extends ToolBean> Collection<T> getCachedToolBeans(final Class<T> clazzToolBean) {
        return s_beanCache.getCachedToolBeans(clazzToolBean);
    }

    /**
     * Gets the cached tool bean oids.
     *
     * @param clazzToolBean Class<T>
     * @return Sets the
     */
    public static Set<String> getCachedToolBeanOids(final Class<? extends ToolBean> clazzToolBean) {
        return s_beanCache.getCachedToolBeanOids(clazzToolBean);
    }

    /**
     * Gets the cached tool bean.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @param oid String
     * @return t
     */
    public static <T extends ToolBean> Set<T> getCachedToolBeanSet(final Class<T> clazzToolBean, final String oid) {
        return s_beanCache.getCachedToolBeanSet(clazzToolBean, oid);
    }

    /**
     * Gets the cached counts.
     *
     * @return String
     */
    public static String getCachedCounts() {
        return s_beanCache.toString();
    }

    /**
     * Gets the criteria.
     *
     * @param broker X2Broker
     * @param clazz Class<? extends ToolBean>
     * @return X 2 criteria
     */
    public static X2Criteria getCriteria(X2Broker broker, Class<? extends ToolBean> clazz) {
        String className = clazz.getName();
        X2Criteria criteria = s_criteriaCache.get(className);
        if (criteria == null) {
            criteria = new X2Criteria();
            getDefinitionForClass(clazz).applyCriteria(broker, criteria);
            s_criteriaCache.put(className, criteria);
        }
        return criteria == null ? new X2Criteria() : criteria;
    }

    /**
     * Gets the dictionary extractor.
     *
     * @return Dictionary extractor
     */
    public static DictionaryExtractor getDictionaryExtractor() {
        return s_dictionaryExtractor;
    }

    /**
     * Gets the dictionary extractor.
     *
     * @param isRequired the is required
     * @return the dictionary extractor
     */
    public static DictionaryExtractor getDictionaryExtractor(boolean isRequired) {
        if (isRequired && s_dictionaryExtractor == null) {
            throw new IllegalStateException("ToolBean dictionary extractor must be set.");
        }
        return s_dictionaryExtractor;
    }

    /**
     * Gets the plain date value.
     *
     * @param year the year
     * @param month the month
     * @param day the day
     * @return the plain date value
     */
    public static PlainDate getPlainDateValue(int year, int month, int day) {
        String key = String.format("%d-%d-%d", year, month, day);
        PlainDate value = s_plainDateValues.get(key);
        if (value == null) {
            Calendar calendar = Calendar.getInstance();
            calendar.set(Calendar.YEAR, year);
            calendar.set(Calendar.MONTH, month);
            calendar.set(Calendar.DAY_OF_MONTH, day);
            value = new PlainDate(calendar.getTime());
            s_plainDateValues.put(key, value);
        }
        return value;
    }

    /**
     * Gets the key columns for class.
     *
     * @param clazz Class<? extends ToolBean>
     * @return Rpt bean columns
     * @throws IllegalArgumentException exception
     * @throws IllegalAccessException exception
     * @throws NoSuchFieldException exception
     * @throws SecurityException exception
     */
    public static List<ToolBeanColumn> getKeyColumnsForClass(Class<? extends ToolBean> clazz)
            throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
        ToolBeanDefinition definition = getDefinitionForClass(clazz);
        return definition == null ? Collections.EMPTY_LIST : definition.getKeys();
    }

    /**
     * Gets the preference.
     *
     * @param key String
     * @return Object
     */
    public static Object getPreference(String key) {
        return s_preference.get(key);
    }

    /**
     * Gets the registered class.
     *
     * @param <T> the generic type
     * @param nameX2BaseClass String
     * @param defaultClass Class<T>
     * @return Class
     */
    public static <T extends ToolBean> Class<T> getRegisteredClass(String nameX2BaseClass, Class<T> defaultClass) {
        if (s_registeredClass.containsKey(nameX2BaseClass)) {
            return (Class<T>) s_registeredClass.get(nameX2BaseClass);
        }
        s_registeredClass.put(nameX2BaseClass, defaultClass);
        return defaultClass;
    }

    /**
     * Gets the registered class.
     *
     * @param <T> the generic type
     * @param x2Class Class<? extends X2BaseBean>
     * @return Class
     */
    public static <T extends ToolBean> Class<T> getRegisteredClass(Class<? extends X2BaseBean> x2Class) {
        Class<T> clazz = (Class<T>) s_registeredClass.get(x2Class.getName());
        if (clazz == null) {
            clazz = (Class<T>) s_defaultClass.get(x2Class.getName());
            if (clazz == null) {
                throw new IllegalStateException(x2Class.getName() + " must be registered.");
            }
            s_registeredClass.put(x2Class.getName(), clazz);
        }
        return clazz;
    }

    /**
     * Gets the X2 base class for class.
     *
     * @param <X> the generic type
     * @param clazz Class<? extends ToolBean>
     * @return Class
     * @throws SecurityException exception
     * @throws IllegalArgumentException exception
     */
    public static <X extends X2BaseBean> Class<X> getX2BaseClassForClass(Class<? extends ToolBean> clazz) {
        try {
            Method m = clazz.getMethod("getX2BaseClass", EMPTY_PARAM_CLASSES);
            return (Class<X>) m.invoke(m, EMPTY_PARAMS);
        } catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException e) {
            throw new X2RuntimeException(e);
        }
    }

    /**
     * Checks for column.
     *
     * @param clazz Class<? extends ToolBean>
     * @param column String
     * @param extractor DictionaryExtractor
     * @return true, if successful
     */
    public static boolean hasColumn(Class<? extends ToolBean> clazz, String column, DictionaryExtractor extractor) {
        boolean result = false;
        result = getDefinitionForClass(clazz).getResolvedColumns(extractor).stream()
                .map(col -> col.resolve(extractor))
                .anyMatch(str -> str.equals(column));
        return result;
    }

    /**
     * Checks if is cached.
     *
     * @param clazzToolBean Class<? extends ToolBean>
     * @param oid String
     * @param resolveUsingX2Class boolean
     * @return true, if is cached
     */
    public static boolean isCached(Class<? extends ToolBean> clazzToolBean,
                                   final String oid,
                                   final boolean resolveUsingX2Class) {
        if (resolveUsingX2Class) {
            Class<X2BaseBean> x2Class = getX2BaseClassForClass(clazzToolBean);
            clazzToolBean = getRegisteredClass(x2Class.getName(), clazzToolBean);
        }
        return getCachedToolBean(clazzToolBean, oid) != null;
    }

    /**
     * Load.
     *
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param clazzToLoad Class<? extends ToolBean>
     */
    public static void load(final X2Broker broker,
                            final DictionaryExtractor dictionaryExtractor,
                            final Class<? extends ToolBean> clazzToLoad) {
        load(broker, dictionaryExtractor, clazzToLoad, null);
    }

    /**
     * Load.
     *
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param clazzToLoad Class<? extends ToolBean>
     * @param sortColumns List<ToolBeanColumn>
     */
    public static void load(final X2Broker broker,
                            final DictionaryExtractor dictionaryExtractor,
                            final Class<? extends ToolBean> clazzToLoad,
                            List<ToolBeanColumn> sortColumns) {
        X2Criteria criteria = ToolBean.getCriteria(broker, clazzToLoad);
        ToolBeanQuery query =
                new ToolBeanQuery(broker, dictionaryExtractor, clazzToLoad, criteria, sortColumns);
        try (ToolBeanQueryIterator iterator = query.iterator(broker, "load")) {
            while (iterator.hasNext()) {
                // iterate through to lead beans
                iterator.next();
            }
        } catch (Exception e) {
            throw new X2RuntimeException(e);
        }
    }

    /**
     * Load.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param clazzToLoad Class<? extends ToolBean>
     * @param oidsToLoad Collection<String>
     * @return Collection
     */
    public static <T extends ToolBean> Collection<T> loadByOid(final X2Broker broker,
                                                               final DictionaryExtractor dictionaryExtractor,
                                                               final Class<T> clazzToLoad,
                                                               final Collection<String> oidsToLoad) {
        return loadByOid(broker, dictionaryExtractor, clazzToLoad, oidsToLoad, null);
    }

    /**
     * Load.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param clazzToLoad Class<? extends ToolBean>
     * @param oidsToLoad Collection<String>
     * @param sortColumns List<ToolBeanColumn>
     * @return Collection
     */
    public static <T extends ToolBean> Collection<T> loadByOid(final X2Broker broker,
                                                               final DictionaryExtractor dictionaryExtractor,
                                                               final Class<T> clazzToLoad,
                                                               final Collection<String> oidsToLoad,
                                                               List<ToolBeanColumn> sortColumns) {
        CollectionCriteriaHelper helper = null;
        List<T> values = new ArrayList(oidsToLoad.size());
        try {
            X2Criteria criteria = ToolBean.getCriteria(broker, clazzToLoad).copy();
            X2Criteria idCriteria = new X2Criteria();
            if (oidsToLoad.size() > MAX_SAFE_PARAMETERS) {
                helper = new CollectionCriteriaHelper(oidsToLoad, broker);
                helper.applyToCriteria(ToolBean.FIELD_OID.resolve(dictionaryExtractor), idCriteria);
            } else {
                idCriteria.addIn(ToolBean.FIELD_OID.resolve(dictionaryExtractor), oidsToLoad);
            }
            criteria.addAndCriteria(idCriteria);

            ToolBeanQuery query =
                    new ToolBeanQuery(broker, dictionaryExtractor, clazzToLoad, criteria, sortColumns);
            try (ToolBeanQueryIterator iterator = query.iterator(broker, "load")) {
                while (iterator.hasNext()) {
                    // iterate through to lead beans
                    values.add((T) iterator.next());
                }
            } catch (Exception e) {
                throw new X2RuntimeException(e);
            }
            return values;
        } finally {
            if (helper != null) {
                helper.cleanup();
            }
        }
    }

    /**
     * Preload loads the children into cache and completes the parent association.
     *
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param sortColumns ToolBeanColumns
     * @param relationships ToolBeanRelationship[]
     */
    public static void preload(final X2Broker broker,
                               final DictionaryExtractor dictionaryExtractor,
                               final List<ToolBeanColumn> sortColumns,
                               final ToolBeanRelationship... relationships) {
        Class<? extends ToolBean> clazzToLoad = relationships[0].getChildClass();
        List<Loader> loaders = new ArrayList();
        X2Criteria criteria = ToolBean.getCriteria(broker, clazzToLoad).copy();
        for (ToolBeanRelationship relationship : relationships) {
            loaders.add(relationship.createLoader());
        }

        CollectionCriteriaHelper helper = null;
        try {
            for (Loader loader : loaders.stream().sorted(new Comparator<Loader>() {

                @Override
                public int compare(Loader arg0, Loader arg1) {
                    // sort in descending order by number of oids
                    return arg1.getLoadedOids().size() - arg0.getLoadedOids().size();
                }
            }).collect(Collectors.toList())) {
                X2Criteria idCriteria = new X2Criteria();
                if (loader.getLoadedOids().size() > MAX_SAFE_PARAMETERS) {
                    if (helper == null) {
                        helper = new CollectionCriteriaHelper(loader.getLoadedOids(), broker);
                        helper.applyToCriteria(loader.getChildPath(), idCriteria);
                    } else {
                        ParameterSelectionHandler.addParameterSafeOIDList(idCriteria, broker, loader.getLoadedOids(),
                                MAX_SAFE_PARAMETERS,
                                loader.getChildPath());
                    }
                } else {
                    idCriteria.addIn(loader.getChildPath(), loader.getLoadedOids());
                }
                criteria.addAndCriteria(idCriteria);
            }

            loaders = loaders.stream().filter(loader -> !StringUtils.isEmpty(loader.getParentPath()))
                    .collect(Collectors.toList());

            ToolBeanQuery query =
                    new ToolBeanQuery(broker, dictionaryExtractor, clazzToLoad, criteria, sortColumns);
            try (ToolBeanQueryIterator iterator = query.iterator(broker, "preload")) {
                while (iterator.hasNext()) {
                    ToolBean bean = iterator.next();
                    loaders.stream().forEach(loader -> loader.load(bean));
                }
                loaders.stream().forEach(loader -> loader.loadEmpty());
            } catch (Exception e) {
                throw new X2RuntimeException(e);
            }

        } finally {
            if (helper != null) {
                helper.cleanup();
            }
        }
    }

    /**
     * Register class.
     *
     * @param clazz Class<? extends ToolBean>
     * @throws IllegalArgumentException exception
     * @throws SecurityException exception
     */
    public static void registerClass(Class<? extends ToolBean> clazz) {
        String nameX2BaseClass = getX2BaseClassForClass(clazz).getName();
        Class currentClass = s_registeredClass.get(nameX2BaseClass);
        if (currentClass == null) {
            s_registeredClass.put(nameX2BaseClass, clazz);
        } else if (!currentClass.equals(clazz)) {
            throw new IllegalStateException(
                    "It is illegal to register two ToolBean classes for a " + nameX2BaseClass
                            + " Current registration is " + currentClass.getName() + " Requested registration is "
                            + clazz.getName());
        }
    }

    /**
     * reload adds the currently cached child beans to a parent.
     *
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param sortColumns ToolBeanColumns
     * @param relationships ToolBeanRelationship[]
     */
    public static void reload(final X2Broker broker,
                              final DictionaryExtractor dictionaryExtractor,
                              final List<ToolBeanColumn> sortColumns,
                              final ToolBeanRelationship... relationships) {
        Class<? extends ToolBean> clazzToLoad = relationships[0].getChildClass();
        List<Loader> loaders = new ArrayList();
        for (ToolBeanRelationship relationship : relationships) {
            loaders.add(relationship.createLoader());
        }

        loaders = loaders.stream().filter(loader -> !StringUtils.isEmpty(loader.getParentPath()))
                .collect(Collectors.toList());

        Collection<? extends ToolBean> loadedBeans = ToolBean.getCachedToolBeans(clazzToLoad);
        for (ToolBean bean : loadedBeans) {
            loaders.stream().forEach(loader -> {
                if (loader.parentExists(bean)) {
                    loader.load(bean);
                }
            });
        }
        loaders.stream().forEach(loader -> loader.loadEmpty());
    }

    /**
     * Removes the cached tool bean.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @param oid String
     * @param groupKey String
     * @return T
     */
    public static <T extends ToolBean> T removeCachedToolBean(final Class<T> clazzToolBean,
                                                              final String oid,
                                                              String groupKey) {
        return s_beanCache.removeCachedToolBean(clazzToolBean, oid, groupKey);
    }

    /**
     * Removes the cached tool beans.
     *
     * @param <T> the generic type
     * @param clazzToolBean Class<T>
     * @param oids List<String>
     * @param groupKey String
     * @return List
     */
    public static <T extends ToolBean> List<T> removeCachedToolBeans(final Class<T> clazzToolBean,
                                                                     final List<String> oids,
                                                                     String groupKey) {
        return s_beanCache.removeCachedToolBeans(clazzToolBean, oids, groupKey);
    }

    /**
     * Reset criteria.
     *
     * @param broker X2Broker
     * @param clazz Class<? extends ToolBean>
     */
    public static void resetCriteria(X2Broker broker, Class<? extends ToolBean> clazz) {
        String className = clazz.getName();
        s_criteriaCache.remove(className);
    }

    /**
     * Resolve method.
     *
     * This generic method resolves linked properties for an object.
     * The property path includes ';' separated list of method names to be invoked
     * in the order of appearance. The result of the final method invocation is returned;
     *
     * It is possible to call methods with arguments. The parameter types of the method are
     * interrogated and compared to available argument types. The currently available argument types
     * are
     * broker and dictionary extractor.
     *
     * TODO: add an interface to populate arguments
     *
     * @param originalSourceObject the original source object
     * @param propertyPath the property path
     * @return the object
     */
    public static Object resolveMethod(Object originalSourceObject, String propertyPath) {
        String currentPathNode = propertyPath.trim();
        String remainingPath = "";
        int index = currentPathNode.indexOf(";");
        if (index >= 0) {
            remainingPath = currentPathNode.substring(index + 1).trim();
            currentPathNode = currentPathNode.substring(0, index).trim();
        }

        Object sourceObject = originalSourceObject;
        Object result = null;

        while (sourceObject != null && !StringUtils.isEmpty(currentPathNode)) {
            if (s_getters == null) {
                s_getters = new HashMap();
            }

            Class clazz = sourceObject.getClass();
            String getterName = currentPathNode;
            try {
                String key = clazz.getName() + "." + getterName;
                Pair<Method, Object[]> pair = null;
                if (s_getters.containsKey(key)) {
                    pair = s_getters.get(key);
                } else {
                    Method method = Arrays.stream(clazz.getMethods())
                            .filter(item -> item.getName().equals(getterName))
                            .sorted(new Comparator<Method>() {
                                @Override
                                public int compare(Method o1, Method o2) {
                                    return o1.getParameterCount() - o2.getParameterCount();
                                }
                            })
                            .findFirst().orElse(null);
                    if (method == null) {
                        throw new IllegalStateException(
                                "The method " + getterName + " cannnot be found for " + sourceObject);
                    }
                    Object[] arglist = new Object[method.getParameterCount()];
                    if (arglist.length > 0) {
                        List<Object> arguments = Arrays.asList(
                                ToolBean.getBroker(false),
                                ToolBean.getDictionaryExtractor());
                        int parameterIndex = 0;
                        for (Class<?> parameterClass : method.getParameterTypes()) {
                            for (Object arg : arguments) {
                                if (parameterClass.isAssignableFrom(arg.getClass())) {
                                    arglist[parameterIndex] = arg;
                                }
                            }
                            ++parameterIndex;
                        }

                    }
                    pair = Pair.of(method, arglist);
                    s_getters.put(key, pair);
                }

                if (pair.getLeft() != null) {
                    result = pair.getLeft().invoke(sourceObject, pair.getRight());
                }
            } catch (Exception e) {
                throw new X2RuntimeException(e);
            }

            // Continue to next node
            currentPathNode = remainingPath;
            remainingPath = "";
            index = currentPathNode.indexOf(";");
            if (index >= 0) {
                remainingPath = currentPathNode.substring(index + 1).trim();
                currentPathNode = currentPathNode.substring(0, index).trim();
            }
            sourceObject = result;
        }
        return result;

    }

    /**
     * Sets the broker.
     *
     * @param broker void
     */
    public static void setBroker(X2Broker broker) {
        s_broker = broker;
    }

    /**
     * Sets the debug predicate.
     *
     * @param debugPredicate void
     */
    public static void setDebugPredicate(Predicate<String> debugPredicate) {
        s_debugPredicate = debugPredicate;
    }

    /**
     * Sets the dictionary extractor.
     *
     * @param dictionaryExtractor void
     */
    public static void setDictionaryExtractor(DictionaryExtractor dictionaryExtractor) {
        s_dictionaryExtractor = dictionaryExtractor;
    }

    /**
     * Sets the preference.
     *
     * @param key String
     * @param value Object
     */
    public static void setPreference(String key, Object value) {
        s_preference.put(key, value);
    }

    /**
     * Gets the constructor for class.
     *
     * @param clazz Class<? extends ToolBean>
     * @return Constructor
     * @throws NoSuchMethodException exception
     * @throws SecurityException exception
     */
    private static Constructor getConstructorForClass(Class<? extends ToolBean> clazz)
            throws NoSuchMethodException, SecurityException {
        Class<?>[] parameterTypes = new Class<?>[2];
        parameterTypes[0] = ToolBeanDefinition.class;
        parameterTypes[1] = Object[].class;
        Constructor constructor = clazz.getDeclaredConstructor(parameterTypes);
        constructor.setAccessible(true);
        return constructor;
    }


    /**
     * Gets the columns for class.
     *
     * @param clazz Class<? extends ToolBean>
     * @return Rpt bean columns
     * @throws SecurityException exception
     * @throws IllegalArgumentException exception
     */
    private static ToolBeanDefinition getDefinitionForClass(Class<? extends ToolBean> clazz) {
        ToolBeanDefinition result = null;
        try {
            Field columns = clazz.getField("FULL_DEFINITION");
            result = (ToolBeanDefinition) columns.get(null);
        } catch (Exception e) {
            throw new X2RuntimeException(e);
        }
        return result;
    }


    /**
     * Register class.
     *
     * @param clazz Class<? extends ToolBean>
     * @throws IllegalArgumentException exception
     * @throws SecurityException exception
     */
    private static void registerDefaultClass(Class<? extends ToolBean> clazz) {
        registerDefaultClass(getX2BaseClassForClass(clazz), clazz);
    }

    /**
     * Register class.
     *
     * @param claxx Class<? extends X2BaseBean>
     * @param clazz Class<? extends ToolBean>
     * @throws IllegalArgumentException exception
     * @throws SecurityException exception
     */
    private static void registerDefaultClass(Class<? extends X2BaseBean> claxx, Class<? extends ToolBean> clazz) {
        String nameX2BaseClass = claxx.getName();
        Class currentClass = s_defaultClass.get(nameX2BaseClass);
        if (currentClass == null) {
            s_defaultClass.put(nameX2BaseClass, clazz);
        } else if (!currentClass.equals(clazz)) {
            throw new IllegalStateException(
                    "It is illegal to register two ToolBean classes for a " + nameX2BaseClass
                            + " Current registration is " + currentClass.getName() + " Requested registration is "
                            + clazz.getName());
        }
    }


    Map<String, List<? extends ToolBean>> m_children;
    Map<String, Filterable<? extends ToolBean>> m_childrenFilterable;
    ToolBeanDefinition m_columns;
    Object[] m_data;


    /**
     * Instantiates a new tool bean.
     *
     * @param columns RptBeanColumns
     * @param data Object[]
     */
    public ToolBean(ToolBeanDefinition columns, Object[] data) {
        m_columns = columns;
        m_data = data;
    }

    /**
     * Adds the child.
     *
     * @param path String
     * @param child ToolBean
     */
    public void addChild(String path, ToolBean child) {
        if (m_children == null) {
            m_children = new HashMap();
        }
        List children = m_children.get(path);
        if (children == null || children.isEmpty()) {
            children = new ArrayList();
            m_children.put(path, children);
        }
        children.add(child);
    }

    /**
     * Adds the empty children.
     *
     * @param path String
     * @param comparator Comparator<ToolBean>
     */
    public void addEmptyChildrenAndSort(String path, Comparator<ToolBean> comparator) {
        if (m_children == null) {
            m_children = new HashMap();
        }
        List<? extends ToolBean> children = m_children.get(path);
        if (children == null || children.isEmpty()) {
            m_children.put(path, Collections.EMPTY_LIST);
        } else if (comparator != null) {
            Collections.sort(children, comparator);
        }
    }

    /**
     * Adds the child.
     *
     * @param path String
     */
    public void clearChild(String path) {
        if (m_children != null) {
            if (m_children.containsKey(path)) {
                m_children.remove(path);
            }
        }
    }

    /**
     * Gets the children.
     *
     * @param broker X2Broker
     * @param relationship ToolBeanRelationship
     * @return the children
     */
    public List<? extends ToolBean> getChildren(X2Broker broker, ToolBeanRelationship relationship) {
        List<? extends ToolBean> list = null;
        String path = relationship.getParentPath();
        if (m_children != null) {
            list = m_children.get(path);
        }
        if (list == null) {
            if (m_childrenFilterable != null && m_childrenFilterable.get(path) != null) {
                m_childrenFilterable.get(path).extract().stream().forEach(bean -> addChild(path, bean));
                addEmptyChildrenAndSort(path, relationship.getComparator());
                list = m_children.get(path);
            } else {
                Class<? extends ToolBean> clazzToLoad = relationship.getChildClass();

                X2Criteria criteria = ToolBean.getCriteria(broker, clazzToLoad).copy();
                X2Criteria idCriteria = new X2Criteria();
                idCriteria.addEqualTo(relationship.getChildPath(),
                        relationship.getParentKeyFunction() == null ? this.getOid()
                                : relationship.getParentKeyFunction());
                criteria.addAndCriteria(idCriteria);

                ToolBeanQuery query =
                        new ToolBeanQuery(broker, null, clazzToLoad, criteria, null);
                try (ToolBeanQueryIterator iterator = query.iterator(broker, "children")) {
                    while (iterator.hasNext()) {
                        ToolBean bean = iterator.next();
                        addChild(path, bean);
                    }
                } catch (Exception e) {
                    throw new X2RuntimeException(e);
                }
                addEmptyChildrenAndSort(path, relationship.getComparator());
                list = m_children.get(path);
            }
        }
        return list == null ? Collections.EMPTY_LIST : list;
    }

    /**
     * Gets the children.
     *
     * @param broker X2Broker
     * @param relationship ToolBeanRelationship
     * @return the children
     */
    public Filterable<? extends ToolBean> getChildrenFilterable(X2Broker broker, ToolBeanRelationship relationship) {
        Filterable<? extends ToolBean> filterable = null;
        String path = relationship.getParentPath();
        if (m_childrenFilterable == null) {
            m_childrenFilterable = new HashMap();
        }
        if (m_childrenFilterable != null) {
            filterable = m_childrenFilterable.get(path);
        }
        if (filterable == null) {
            if (m_children != null && m_children.get(path) != null) {
                List<? extends ToolBean> list = null;
                list = m_children.get(path);
                filterable = FilterableFactory.createFilterableToolBeans(list);
                m_childrenFilterable.put(path, filterable);
            } else {
                Class<? extends ToolBean> clazzToLoad = relationship.getChildClass();

                X2Criteria criteria = ToolBean.getCriteria(broker, clazzToLoad).copy();
                X2Criteria idCriteria = new X2Criteria();
                idCriteria.addEqualTo(relationship.getChildPath(),
                        relationship.getParentKeyFunction() == null ? this.getOid()
                                : relationship.getParentKeyFunction());
                criteria.addAndCriteria(idCriteria);

                filterable = FilterableFactory.create(broker, ToolBean.getDictionaryExtractor(), clazzToLoad, criteria,
                        null);
                m_childrenFilterable.put(path, filterable);
            }
        }
        return filterable;
    }

    /**
     * Gets the field value by column name.
     *
     * @param key String
     * @return Object
     */
    public Object getFieldValueByColumnName(String key) {
        int index = m_columns.getIndex(key);
        if (index < 0) {
            throw new IllegalStateException(
                    "Column field " + key + " in class " + this.getClass().getName() + " is not defined");
        }
        return m_data[index];
    }

    /**
     * Gets the field value by column name.
     *
     * @param key String
     * @param isThrowException boolean
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getFieldValueByColumnName(String key, boolean isThrowException) throws X2BaseException {
        int index = m_columns.getIndex(key);
        if (isThrowException && index < 0) {
            throw new X2BaseException(
                    new IllegalStateException(this.getClass().getSimpleName() + " does not contain column " + key));
        }
        return m_data[index];
    }

    /**
     * Gets the oid.
     *
     * @return String
     */
    public String getOid() {
        return getValueString(FIELD_OID);
    }

    /**
     * Gets the value.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public Object getValue(ToolBeanColumn column) {
        String key = column.resolve(null);
        return StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
    }

    /**
     * Gets the value as java type.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public Object getValueAsJavaType(ToolBeanColumn column) {
        return ToolBean.getDictionaryExtractor().getAliasAsJavaType(this, column);
    }

    /**
     * Gets the value big decimal.
     *
     * @param column ToolBeanColumn
     * @return Big decimal
     */
    public BigDecimal getValueBigDecimal(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        return ((BigDecimal) PredefinedConverter.BIG_DECIMAL.convertedValue(rawValue));
    }

    /**
     * Gets the value.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public PlainDate getValueDate(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        return ((PlainDate) PredefinedConverter.PLAINDATE.convertedValue(rawValue));
    }

    /**
     * Gets the value.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public double getValueDouble(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        Double value = ((Double) PredefinedConverter.DOUBLE.convertedValue(rawValue));
        return value == null ? 0.0 : value.doubleValue();
    }

    /**
     * Gets the value.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public int getValueInt(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        Integer value = ((Integer) PredefinedConverter.INTEGER.convertedValue(rawValue));
        return value == null ? 0 : value.intValue();
    }

    /**
     * Gets the value long.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public long getValueLong(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        Long value = ((Long) PredefinedConverter.LONG.convertedValue(rawValue));
        return value == null ? 0 : value.longValue();
    }

    /**
     * Gets the value logical.
     *
     * @param column ToolBeanColumn
     * @return boolean
     */
    public boolean getValueLogical(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        return rawValue == null ? false
                : ((Boolean) PredefinedConverter.LOGICAL.convertedValue(rawValue)).booleanValue();
    }

    /**
     * Gets the value reference.
     *
     * @param column ToolBeanColumn
     * @param referenceMap int
     * @return String
     */
    public String getValueReference(ToolBeanColumn column, int referenceMap) {
        String finalValue = null;
        String key = column.resolve(null);
        String value = (String) (StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key));
        if (!StringUtils.isEmpty(value)) {
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            DataDictionaryField dictionaryField = column.getField(extractor);
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                finalValue =
                        extractor.lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value,
                                referenceMap);
            }
        }
        return finalValue;
    }

    /**
     * Gets the values reference state.
     *
     * @param column ToolBeanColumn
     * @return List
     */
    public List<String> getValuesReferenceState(ToolBeanColumn column) {
        List<String> values = null;
        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
        if (extractor != null) {
            DataDictionaryField dictionaryField = column.getField(extractor);
            if (dictionaryField.hasReferenceTable()) {
                values = extractor.getStateValues(this, column);
            }
        }
        return values;
    }

    /**
     * Gets the value reference state.
     *
     * @param column ToolBeanColumn
     * @return String
     */
    public String getValueReferenceState(ToolBeanColumn column) {
        return getValueReference(column, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
    }

    /**
     * Gets the value.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public String getValueString(ToolBeanColumn column) {
        String key = column.resolve(null);
        return (String) (StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key));
    }

    /**
     * Gets the value.
     *
     * @param column RptBeanColumn
     * @return Object
     */
    public PlainTime getValueTime(ToolBeanColumn column) {
        String key = column.resolve(null);
        Object rawValue = StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key);
        return ((PlainTime) PredefinedConverter.PLAINTIME.convertedValue(rawValue));
    }

    /**
     * Gets the x 2 bean.
     *
     * @return the x 2 bean
     */
    public X2BaseBean getX2Bean() {
        Class<X2BaseBean> x2Class = getX2BaseClassForClass(getClass());
        return getBroker(true).getBeanByOid(x2Class, getOid());
    }

    /**
     * Checks if is child loaded.
     *
     * @param path String
     * @return true, if is child loaded
     */
    public boolean isChildLoaded(String path) {
        return m_children != null && m_children.containsKey(path);
    }

    /**
     * To string.
     *
     * @return String
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.getClass().getSimpleName() + ":" + Arrays.toString(m_data);
    }

    /**
     * **********************************************************************
     *
     * This is a special purpose Map interface that is used only to easily
     * use reflection to execute methods
     *
     * It is primarily intended as an easy mechanism to use ToolBeans in reports
     *
     * Only the get method is implemented
     *
     * **********************************************************************.
     *
     * @return the int
     */

    /**
     * @see java.util.Map#size()
     */
    @Override
    public int size() {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Checks if is empty.
     *
     * @return true, if is empty
     * @see java.util.Map#isEmpty()
     */
    @Override
    public boolean isEmpty() {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Contains key.
     *
     * @param key the key
     * @return true, if successful
     * @see java.util.Map#containsKey(java.lang.Object)
     */
    @Override
    public boolean containsKey(Object key) {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Contains value.
     *
     * @param value the value
     * @return true, if successful
     * @see java.util.Map#containsValue(java.lang.Object)
     */
    @Override
    public boolean containsValue(Object value) {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Gets the.
     *
     * @param key the key
     * @return the object
     * @see java.util.Map#get(java.lang.Object)
     */
    @Override
    public Object get(Object key) {
        return ToolBean.resolveMethod(this, (String) key);
    }

    /**
     * Put.
     *
     * @param key the key
     * @param value the value
     * @return the object
     * @see java.util.Map#put(java.lang.Object, java.lang.Object)
     */
    @Override
    public Object put(Object key, Object value) {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Removes the.
     *
     * @param key the key
     * @return the object
     * @see java.util.Map#remove(java.lang.Object)
     */
    @Override
    public Object remove(Object key) {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Put all.
     *
     * @param m the m
     * @see java.util.Map#putAll(java.util.Map)
     */
    @Override
    public void putAll(Map m) {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Clear.
     *
     * @see java.util.Map#clear()
     */
    @Override
    public void clear() {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Key set.
     *
     * @return the sets the
     * @see java.util.Map#keySet()
     */
    @Override
    public Set keySet() {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Values.
     *
     * @return the collection
     * @see java.util.Map#values()
     */
    @Override
    public Collection values() {
        throw new IllegalStateException("DO NOT USE");
    }

    /**
     * Entry set.
     *
     * @return the sets the
     * @see java.util.Map#entrySet()
     */
    @Override
    public Set entrySet() {
        throw new IllegalStateException("DO NOT USE");
    }

}
