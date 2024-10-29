/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.common;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.PredefinedResolver;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanQuery;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanQuery.ToolBeanQueryIterator;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.apache.commons.collections.comparators.ComparatorChain;
import org.apache.commons.collections.comparators.NullComparator;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * A factory for creating Filterable objects.
 */
public class FilterableFactory {
    public static final Filterable EMPTY_FILTERABLE =
            FilterableFactory.create(Collections.EMPTY_LIST, Collections.EMPTY_LIST, PredefinedResolver.EMPTY);

    /**
     * The Class Filterable.
     *
     * @param <T> the generic type
     */
    public static abstract class Filterable<T> {

        public interface Folder<T, E> {
            public E fold(T item, E accumulator);
        }

        public interface Mapper<T, E> {
            public Filterable<E> map(T toMap);
        }

        public interface Processor<T> {
            public void process(T item) throws X2BaseException;
        }

        public enum PredefinedResolver implements MultiLevelMap.ValueByKeyResolver {
            EMPTY {
                @Override
                public Object getValue(String key,
                                       Object entity) {
                    return null;
                }
            },
            RPT_BEAN {
                @Override
                public Object getValue(String key,
                                       Object entity) {
                    return ((ToolBean) entity).getFieldValueByColumnName(key);
                }
            },
            X2BASE_BEAN {
                @Override
                public Object getValue(String key,
                                       Object entity) {
                    return ((X2BaseBean) entity).getFieldValueByBeanPath(key);
                }
            };
        }

        private final Map<String, T> m_entities;
        private Map<List<String>, MultiLevelMap> m_multiLevelMapsByKeys;
        private final List<String> m_uniqueKeys;
        private final ValueByKeyResolver m_valueResolver;
        private Map<String, List<Object>> m_valuesByKeys;

        /**
         * Instantiates a new Filterable.
         */
        public Filterable() {
            m_entities = initializeEntities();
            m_uniqueKeys = initializeUniqueKeys();
            m_valueResolver = initializeValueResolver();
        }

        /**
         * Any.
         *
         * @param filters Filter<T>[]
         * @return true, if successful
         */
        public boolean any(Filter<T>... filters) {
            return any(Collections.EMPTY_LIST, Collections.EMPTY_LIST, filters);
        }

        /**
         * Any.
         *
         * @param javaName String
         * @param value Object
         * @param filters Filter<T>[]
         * @return true, if successful
         */
        public boolean any(String javaName, Object value, Filter<T>... filters) {
            return any(getGroup(Arrays.asList(javaName), Arrays.asList(value)), filters);
        }

        /**
         * Any.
         *
         * @param javaNames List<String>
         * @param values List<?>
         * @param filters Filter<T>[]
         * @return true, if successful
         */
        public boolean any(List<String> javaNames, List<?> values, Filter<T>... filters) {
            return any(getGroup(javaNames, values), filters);
        }

        /**
         * @param filterable
         * @return
         */
        public Filterable<T> concat(Filterable<T> filterable) {
            List<T> joinedEntities = new ArrayList<>();
            joinedEntities.addAll(m_entities.values());
            joinedEntities.addAll(filterable.m_entities.values());
            return FilterableFactory.create(joinedEntities, m_uniqueKeys, m_valueResolver);
        }

        /**
         * @param filters
         * @return
         */
        public int count(Filter<T>... filters) {
            return filter(filters).extract().size();
        }

        /**
         * @param javaName
         * @param value
         * @param filters
         * @return
         */
        public int count(String javaName, Object value, Filter<T>... filters) {
            return filter(javaName, value, filters).extract().size();
        }

        /**
         * @param javaNames
         * @param values
         * @param filters
         * @return
         */
        public int count(List<String> javaNames, List<?> values, Filter<T>... filters) {
            return filter(javaNames, values, filters).extract().size();
        }

        public Filterable<T> distinct(String key) {
            return FilterableFactory.create(m_entities.values(), Arrays.asList(key), m_valueResolver);
        }

        /**
         * Gets the all.
         *
         * @return Collection
         */
        public Collection<T> extract() {
            return m_entities.values();
        }

        public <E> List<E> extract(String key) {
            if (m_valuesByKeys == null) {
                m_valuesByKeys = new HashMap<>();
            }
            List<Object> values = m_valuesByKeys.get(key);
            if (values == null) {
                values = new ArrayList<>();
                m_valuesByKeys.put(key, values);
                for (T entity : m_entities.values()) {
                    values.add(m_valueResolver.getValue(key, entity));
                }
            }
            List<E> valuesWithType = new ArrayList<>();
            for (Object value : values) {
                valuesWithType.add((E) value);
            }
            return valuesWithType;
        }

        public List<Object> extract(String key, ValueByKeyResolver<T> resolver) {
            if (m_valuesByKeys == null) {
                m_valuesByKeys = new HashMap<>();
            }
            List<Object> values = m_valuesByKeys.get(key);
            if (values == null) {
                values = new ArrayList<>();
                m_valuesByKeys.put(key, values);
                for (T entity : m_entities.values()) {
                    values.add(resolver.getValue(key, entity));
                }
            }
            return values;
        }

        /**
         * Map.
         *
         * @param beans List<? extends X2BaseBean>
         * @param beanPath String
         * @return List
         */
        public static List<Object> extract(List<? extends X2BaseBean> beans, String beanPath) {
            List<Object> values = new ArrayList<>();
            for (X2BaseBean bean : beans) {
                values.add(bean.getFieldValueByBeanPath(beanPath));
            }
            return values;
        }

        /**
         * Map.
         *
         * @param key
         * @return List
         */
        public Object extractFirst(String key) {
            Iterator iterator = extract(key).iterator();
            return iterator.hasNext() ? iterator.next() : null;
        }

        /**
         * Find entity.
         *
         * @param filters Filter<T>[]
         * @return T
         */
        public T extractFirst(Filter<T>... filters) {
            return extractFirst(Collections.<String>emptyList(), Collections.<T>emptyList(), filters);
        }

        /**
         * Find entity.
         *
         * @param javaName String
         * @param value Object
         * @param filters Filter<T>[]
         * @return T
         */
        public T extractFirst(String javaName, Object value, Filter<T>... filters) {
            return findEntity(getGroup(Arrays.asList(javaName), Arrays.asList(value)), filters);
        }

        /**
         * Find entity.
         *
         * @param column ToolBeanColumn
         * @param value Object
         * @param filters Filter<T>[]
         * @return T
         */
        public T extractFirst(ToolBeanColumn column, Object value) {
            return findEntity(getGroup(column, value));
        }

        /**
         * Find entity.
         *
         * @param javaNames List<String>
         * @param values List<?>
         * @param filters Filter<T>[]
         * @return T
         */
        public T extractFirst(List<String> javaNames, List<?> values, Filter<T>... filters) {
            return findEntity(getGroup(javaNames, values), filters);
        }

        /**
         * @param keys
         * @param isAscending
         * @return
         */
        public List<T> extractSorted(List<String> keys, final boolean isAscending) {
            List<T> items = new ArrayList<>();
            items.addAll(m_entities.values());
            ComparatorChain comparatorChain = new ComparatorChain();
            final NullComparator nullComparator = new NullComparator();
            for (final String key : keys) {
                comparatorChain.addComparator(new Comparator<T>() {

                    @Override
                    public int compare(T o1, T o2) {
                        Object value1 = m_valueResolver.getValue(key, o1);
                        Object value2 = m_valueResolver.getValue(key, o2);
                        int result = nullComparator.compare(value1, value2);
                        return isAscending ? result : -result;
                    }

                });
            }
            Collections.sort(items, comparatorChain);
            return items;
        }

        /**
         * Apply filter.
         *
         * @param filters Filter<T>[]
         * @return Filterable
         */
        public Filterable<T> filter(Filter<T>... filters) {
            return FilterableFactory.create(
                    filterEntities(getGroup(Collections.EMPTY_LIST, Collections.EMPTY_LIST), filters),
                    m_uniqueKeys, m_valueResolver);
        }

        /**
         * Apply filter.
         *
         * @param javaName String
         * @param value Object
         * @param filters Filter<T>[]
         * @return Filterable
         */
        public Filterable<T> filter(String javaName, Object value, Filter<T>... filters) {
            return FilterableFactory.create(
                    filterEntities(getGroup(Arrays.asList(javaName), Arrays.asList(value)), filters), m_uniqueKeys,
                    m_valueResolver);
        }

        /**
         * Apply filter.
         *
         * @param javaNames List<String>
         * @param values List<?>
         * @param filters Filter<T>[]
         * @return Filterable
         */
        public Filterable<T> filter(List<String> javaNames, List<?> values, Filter<T>... filters) {
            return FilterableFactory.create(
                    filterEntities(getGroup(javaNames, values), filters), m_uniqueKeys,
                    m_valueResolver);
        }

        /**
         * Apply filter.
         *
         * @param column RptBeanColumn
         * @param value Object
         * @param filters Filter<T>[]
         * @return Filterable
         */
        public Filterable<T> filter(ToolBeanColumn column, Object value, Filter<T>... filters) {
            return FilterableFactory.create(
                    filterEntities(getGroup(Arrays.asList(column.resolve(null)), Arrays.asList(value)), filters),
                    m_uniqueKeys,
                    m_valueResolver);
        }

        /**
         * Apply filter.
         *
         * @param columns RptBeanColumns
         * @param values List<?>
         * @param filters Filter<T>[]
         * @return Filterable
         */
        public Filterable<T> filter(ToolBeanDefinition columns, List<?> values, Filter<T>... filters) {
            return FilterableFactory.create(
                    filterEntities(
                            getGroup(
                                    columns.getColumns().stream().map(column -> column.resolve(null))
                                            .collect(Collectors.toList()),
                                    values),
                            filters),
                    m_uniqueKeys,
                    m_valueResolver);
        }

        public <E> E fold(E accumulator, Folder<T, E> folder) {
            for (T item : m_entities.values()) {
                accumulator = folder.fold(item, accumulator);
            }
            return accumulator;
        }

        /**
         * Gets the oids.
         *
         * @return Sets the
         */
        public Set<String> getKeySet() {
            return m_entities.keySet();
        }

        public List<List<T>> groupBy(List<String> keys) {
            MultiLevelMap groupedList = getMultiLevelMapByKeys(keys, m_valueResolver);
            return groupedList.getGroups();
        }

        public List<List<T>> groupBy(List<String> keys, ValueByKeyResolver valueByKeyResolver) {
            MultiLevelMap groupedList = getMultiLevelMapByKeys(keys, valueByKeyResolver);
            return groupedList.getGroups();
        }

        public Filterable<T> in(final String key, List<?> values) {
            List<Filterable<T>> innedFilterables = new ArrayList<>();
            for (Object value : values) {
                Filterable<T> filtered = filter(key, value);
                if (filtered != null) {
                    innedFilterables.add(filtered);
                }
            }
            return FilterableFactory.concat(innedFilterables);
        }

        /**
         * Initialize filterables.
         *
         * @return Map
         */
        public abstract Map<String, T> initializeEntities();

        public abstract List<String> initializeUniqueKeys();

        public abstract ValueByKeyResolver initializeValueResolver();

        public <E> Filterable<E> map(Mapper<T, E> mapper) {
            List<Filterable<E>> mapped = new ArrayList<>();
            for (T value : m_entities.values()) {
                Filterable<E> currentMapped = mapper.map(value);
                if (currentMapped != null) {
                    mapped.add(currentMapped);
                }
            }
            return FilterableFactory.concat(mapped);
        }

        /**
         * Use for some side effects.
         *
         * @param processor
         * @throws X2BaseException
         */
        public void process(Processor<T> processor) throws X2BaseException {
            for (T value : m_entities.values()) {
                processor.process(value);
            }
        }

        /**
         * Any.
         *
         * @param entities List<T>
         * @param filters Filter<T>[]
         * @return true, if successful
         */
        private boolean any(List<T> entities, Filter<T>... filters) {
            for (T toFilter : entities) {
                if (isFiltered(toFilter, filters)) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Filter entities.
         *
         * @param entities List<T>
         * @param filters Filter<T>[]
         * @return List
         */
        private Collection<T> filterEntities(List<T> entities, Filter<T>... filters) {
            Collection<T> filtered = new ArrayList<T>();
            for (T toFilter : entities) {
                if (isFiltered(toFilter, filters)) {
                    filtered.add(toFilter);
                }
            }
            return filtered;
        }

        /**
         * Find entity.
         *
         * @param entities List<T>
         * @param filters Filter<T>[]
         * @return T
         */
        private T findEntity(List<T> entities, Filter<T>... filters) {
            for (T toFilter : entities) {
                if (isFiltered(toFilter, filters)) {
                    return toFilter;
                }
            }
            return null;
        }

        /**
         * Gets the group.
         *
         * @param column RptBeanColumn
         * @param value Object
         * @return List
         */
        public List<T> getGroup(ToolBeanColumn column, Object value) {
            return getGroup(Arrays.asList(column.resolve(null)), Arrays.asList(value));
        }

        /**
         * Gets the group.
         *
         * @param columns RptBeanColumns
         * @param values List<?>
         * @return List
         */
        public List<T> getGroup(ToolBeanDefinition columns, List<?> values) {
            return getGroup(
                    columns.getColumns().stream().map(column -> column.resolve(null)).collect(Collectors.toList()),
                    values);
        }

        /**
         * Gets the group.
         *
         * @param keys List<String>
         * @param values List<?>
         * @return List
         */
        public List<T> getGroup(List<String> keys, List<?> values) {
            if (keys == null || keys.isEmpty()) {
                return new ArrayList(m_entities.values());
            }

            MultiLevelMap groupedList = getMultiLevelMapByKeys(keys, m_valueResolver);

            MultiLevelMap currentGroup = groupedList;
            for (Object value : values) {
                currentGroup = currentGroup.getGroup(value);
                if (currentGroup == null) {
                    break;
                }
                if (!currentGroup.hasInnerGroup()) {
                    return currentGroup.getEntities();
                }
            }
            return Collections.EMPTY_LIST;
        }

        /**
         * Gets the unique keys at one level of the "groupBy" MultiLevelMap.
         * See getGroupKeys(keys, values, resolver)
         *
         * @param keys List<String>
         * @param values List<?> Can be partial list (shorter than keys)
         * @return List keys at the MultiLevelMap level specified by values
         */
        public Set<String> getGroupKeys(List<String> keys, List<?> values) {
            return getGroupKeys(keys, values, m_valueResolver);
        }

        /**
         * Gets the unique keys at one level of the "groupBy" MultiLevelMap.
         *
         * E.g.:
         * keys=("SchoolID", "StudentID", "EnrollmentID")
         * values=("SKL_5", "STD_1")
         *
         * To get List of EnrollmentIDs for a School/Student:
         * getGroupKeys(["SchoolID", "StudentID", "EnrollmentID"], ["SKL_5", "STD_1"])
         *
         * To get List of StudentIDs for a School:
         * getGroupKeys(["SchoolID", "StudentID", "EnrollmentID"], ["SKL_5"])
         *
         * To get List of SchoolIDs (i.e., root level):
         * getGroupKeys(["SchoolID", "StudentID", "EnrollmentID"], [])
         *
         * @param keys List<String>
         * @param values List<?> Can be partial list (shorter than keys)
         * @param resolver
         * @return List keys at the MultiLevelMap level specified by values
         */
        public Set<String> getGroupKeys(List<String> keys, List<?> values, ValueByKeyResolver resolver) {
            if (keys == null || keys.isEmpty()) {
                return Collections.EMPTY_SET;
            }

            // Start at root node
            MultiLevelMap groupedList = getMultiLevelMapByKeys(keys, resolver);
            MultiLevelMap currentGroup = groupedList;

            // Walk down the tree using requested values
            if (values != null) {
                for (Object value : values) {
                    currentGroup = currentGroup.getGroup(value);
                    if (currentGroup == null) {
                        return Collections.EMPTY_SET;
                    }
                }
            }

            if (currentGroup.m_innerGroups == null) {
                return Collections.EMPTY_SET;
            }

            return currentGroup.m_innerGroups.keySet();
        }

        /**
         * Gets the multi level map by keys.
         *
         * @param keys List<String>
         * @return Multi level bean map
         */
        private MultiLevelMap getMultiLevelMapByKeys(List<String> keys, ValueByKeyResolver resolver) {
            if (m_multiLevelMapsByKeys == null) {
                m_multiLevelMapsByKeys = new HashMap<>();
            }
            MultiLevelMap groupedList = m_multiLevelMapsByKeys.get(keys);
            if (groupedList == null) {
                groupedList = MultiLevelMap.groupList(keys.toArray(new String[keys.size()]),
                        new ArrayList(m_entities.values()), resolver);
                m_multiLevelMapsByKeys.put(keys, groupedList);
            }
            return groupedList;
        }

        /**
         * Checks if is filtered.
         *
         * @param entity T
         * @param filters Filter<T>[]
         * @return true, if is filtered
         */
        private boolean isFiltered(T entity, Filter<T>... filters) {
            for (Filter<T> additionalFilter : filters) {
                if (!additionalFilter.isFiltered(entity)) {
                    return false;
                }
            }
            return true;
        }
    }


    /**
     * The Interface Filter.
     *
     * @param <T> the generic type
     */
    public interface Filter<T> {

        /**
         * Checks if is filtered.
         *
         * @param toFilter T
         * @return true, if is filtered
         */
        public boolean isFiltered(T toFilter);
    }


    /**
     * The Class MultiLevelBeanMap.
     *
     * @param <T>
     */
    public static class MultiLevelMap<T> {

        public interface ValueByKeyResolver<T> {
            Object getValue(String key, T entity);
        }

        private final List<T> m_groupedEntities;
        private final Map<Object, MultiLevelMap> m_innerGroups;

        /**
         * Instantiates a new multi level bean map.
         *
         * @param groupedEntities List<T>
         */
        public MultiLevelMap(List<T> groupedEntities) {
            m_groupedEntities = groupedEntities;
            m_innerGroups = null;
        }

        /**
         * Instantiates a new multi level bean map.
         *
         * @param innerGroups Map<Object,MultiLevelBeanMap>
         */
        public MultiLevelMap(Map<Object, MultiLevelMap> innerGroups) {
            m_groupedEntities = null;
            m_innerGroups = innerGroups;
        }

        /**
         * Gets the entities.
         *
         * @return List
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.GroupI#getEntities()
         */
        public List<T> getEntities() {
            return m_groupedEntities;
        }

        /**
         * Gets the group.
         *
         * @param keyValue Object
         * @return Multi level bean map
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.GroupI#getGroup(java.lang.String)
         */
        public MultiLevelMap getGroup(Object keyValue) {
            if (!hasInnerGroup()) {
                throw new X2RuntimeException();
            }
            return m_innerGroups.get(keyValue);
        }

        public List<List<T>> getGroups() {
            List<List<T>> groups = new ArrayList<>();
            if (hasInnerGroup()) {
                for (MultiLevelMap<T> innerGroup : m_innerGroups.values()) {
                    groups.addAll(innerGroup.getGroups());
                }
            } else {
                groups.add(m_groupedEntities);
            }
            return groups;
        }

        /**
         * Group list.
         *
         * @param keys String[]
         * @param nonGroupedEntities List<T>
         * @param resolver
         * @return MultiLevelBeanMap
         */
        public static <T> MultiLevelMap groupList(String[] keys,
                                                  List<T> nonGroupedEntities,
                                                  ValueByKeyResolver resolver) {
            Queue<String> keysQueue = new LinkedList(Arrays.asList(keys));
            String keyToGroup = keysQueue.poll();

            // Map<Object, List<T>> groupedEntities = new HashMap<Object, List<T>>();
            Map<Object, List<T>> groupedEntities = new TreeMap<Object, List<T>>(new Comparator<Object>() {
                @Override
                public int compare(Object o1, Object o2) {
                    if (o1 == null) {
                        return (o2 == null) ? 0 : -1;
                    }

                    if (o2 == null) {
                        return 1;
                    }

                    if (o1 instanceof Comparable && o2 instanceof Comparable) {
                        return ((Comparable) o1).compareTo(o2);
                    }

                    if (o1 instanceof X2BaseBean && o2 instanceof X2BaseBean) {
                        return compare(((X2BaseBean) o1).getOid(), (((X2BaseBean) o2).getOid()));
                    }

                    return (o1.hashCode() - o2.hashCode());
                }
            });

            for (T entityToGroup : nonGroupedEntities) {
                Object value = resolver.getValue(keyToGroup, entityToGroup);
                List<T> entities = groupedEntities.get(value);
                if (entities == null) {
                    entities = new ArrayList<T>();
                    groupedEntities.put(value, entities);
                }
                entities.add(entityToGroup);
            }
            Map<Object, MultiLevelMap> innerGroups = new HashMap<>();
            if (keysQueue.isEmpty()) {
                for (Entry<Object, List<T>> groupStructure : groupedEntities.entrySet()) {
                    List<T> entities = groupStructure.getValue();
                    MultiLevelMap groupWithEntities = new MultiLevelMap(entities);
                    innerGroups.put(groupStructure.getKey(), groupWithEntities);
                }
                return new MultiLevelMap(innerGroups);
            }
            for (Entry<Object, List<T>> groupStructure : groupedEntities.entrySet()) {
                List<T> entities = groupStructure.getValue();
                MultiLevelMap subGroup =
                        MultiLevelMap.groupList(keysQueue.toArray(new String[keysQueue.size()]), entities,
                                resolver);
                innerGroups.put(groupStructure.getKey(), subGroup);
            }
            return new MultiLevelMap(innerGroups);
        }

        /**
         * Checks for inner group.
         *
         * @return true, if successful
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.GroupI#hasInnerGroup()
         */
        public boolean hasInnerGroup() {
            return m_innerGroups != null;
        }

    }

    public static <T> Filterable<T> concat(List<Filterable<T>> filterables) {
        List<T> entities = new ArrayList<>();
        for (Filterable<T> currentFilterable : filterables) {
            entities.addAll(currentFilterable.extract());
        }
        if (filterables.isEmpty()) {
            return FilterableFactory.create(Collections.EMPTY_LIST, Collections.EMPTY_LIST, PredefinedResolver.EMPTY);
        }
        Filterable<T> filterable = filterables.get(0);
        return FilterableFactory.create(entities, filterable.m_uniqueKeys, filterable.m_valueResolver);
    }

    /**
     * Creates a new Filterable object.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param clazz Class<T>
     * @return the Filterable< t>
     */
    public static <T extends X2BaseBean> Filterable<T> create(final X2Broker broker,
                                                              final Class<T> clazz) {
        return new Filterable<T>() {
            @Override
            public Map<String, T> initializeEntities() {
                X2Criteria criteria = new X2Criteria();
                QueryByCriteria query = new QueryByCriteria(clazz, criteria);
                query.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, true);
                return broker.getMapByQuery(query, X2BaseBean.COL_OID, 100);
            }

            @Override
            public PredefinedResolver initializeValueResolver() {
                return PredefinedResolver.X2BASE_BEAN;
            }

            @Override
            public List<String> initializeUniqueKeys() {
                return Arrays.asList(X2BaseBean.COL_OID);
            }
        };
    }

    /**
     * Creates a new Filterable object.
     *
     * @param <T> the generic type
     * @param broker X2Broker
     * @param clazz Class<T>
     * @param criteria X2Criteria
     * @return the Filterable< t>
     */
    public static <T extends X2BaseBean> Filterable<T> create(final X2Broker broker,
                                                              final Class<T> clazz,
                                                              final X2Criteria criteria) {
        return new Filterable<T>() {
            @Override
            public Map<String, T> initializeEntities() {
                QueryByCriteria query = new QueryByCriteria(clazz, criteria);
                // query.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, true);
                query.addOrderByAscending(X2BaseBean.COL_OID);
                return broker.getMapByQuery(query, X2BaseBean.COL_OID, 100);
            }

            @Override
            public PredefinedResolver initializeValueResolver() {
                return PredefinedResolver.X2BASE_BEAN;
            }

            @Override
            public List<String> initializeUniqueKeys() {
                return Arrays.asList(X2BaseBean.COL_OID);
            }
        };
    }

    public static <R extends ToolBean> Filterable<R> create(final X2Broker broker,
                                                            final Class<R> clazzRptBean,
                                                            final X2Criteria criteria,
                                                            final List<ToolBeanColumn> sortColumns) {
        return create(broker, new DictionaryExtractor(broker), clazzRptBean, criteria, sortColumns);
    }

    /**
     * Creates a new Filterable object.
     *
     * @param <R> the generic type
     * @param <T> the generic type
     * @param broker X2Broker
     * @param dictionaryExtractor DictionaryExtractor
     * @param clazzToolBean Class<R>
     * @param criteria X2Criteria
     * @param sortColumns List<RptBeanColumn>
     * @return the Filterable< t>
     */
    public static <R extends ToolBean> Filterable<R> create(final X2Broker broker,
                                                            final DictionaryExtractor dictionaryExtractor,
                                                            final Class<R> clazzToolBean,
                                                            final X2Criteria criteria,
                                                            final List<ToolBeanColumn> sortColumns) {
        return new Filterable<R>() {

            @Override
            public Map<String, R> initializeEntities() {
                try {
                    Map<String, R> filterables = new LinkedHashMap<>();
                    ToolBeanQuery<R> query =
                            new ToolBeanQuery(broker, dictionaryExtractor, clazzToolBean,
                                    criteria, sortColumns);
                    try (ToolBeanQueryIterator iterator = query.iterator(broker, "filterable")) {
                        while (iterator.hasNext()) {
                            R bean = (R) iterator.next();
                            filterables.put(bean.getOid(), bean);
                        }
                    }
                    return filterables;
                } catch (Exception e) {
                    throw new X2RuntimeException(e);
                }
            }

            @Override
            public List<String> initializeUniqueKeys() {
                try {
                    return ToolBean.getKeyColumnsForClass(clazzToolBean).stream()
                            .map(column -> column.resolve(dictionaryExtractor)).collect(Collectors.toList());
                } catch (Exception e) {
                    throw new X2RuntimeException(e);
                }
            }

            @Override
            public ValueByKeyResolver initializeValueResolver() {
                return PredefinedResolver.RPT_BEAN;
            }
        };
    }


    /**
     * Creates a new Filterable object.
     *
     * @param <T> the generic type
     * @param entities List<T>
     * @param uniqueKeys
     * @param resolver
     * @return the Filterable< t>
     */
    public static <T> Filterable<T> create(final Collection<T> entities,
                                           final List<String> uniqueKeys,
                                           final ValueByKeyResolver resolver) {
        return new Filterable<T>() {
            @Override
            public Map<String, T> initializeEntities() {
                Map<String, T> filterables = new LinkedHashMap<>();
                if (!entities.isEmpty()) {
                    if (uniqueKeys == null || uniqueKeys.isEmpty()) {
                        throw new RuntimeException("Unique keys must be provided");
                    }
                    for (T entity : entities) {
                        StringBuilder uniqueKey = new StringBuilder();
                        for (String key : uniqueKeys) {
                            uniqueKey.append(resolver.getValue(key, entity));
                        }
                        if (StringUtils.isBlank(uniqueKey.toString())) {
                            throw new RuntimeException("Unique keys must not be blank");
                        }
                        filterables.put(uniqueKey.toString(), entity);
                    }
                }
                return filterables;
            }

            @Override
            public ValueByKeyResolver initializeValueResolver() {
                return resolver;
            }

            @Override
            public List<String> initializeUniqueKeys() {
                return uniqueKeys;
            }
        };
    }


    /**
     * Creates the Filterable.
     *
     * @param <T> the generic type
     * @param beans List<T>
     * @return Filterable
     */
    public static <T extends X2BaseBean> Filterable createFilterable(Collection<T> beans) {
        return create(beans, Arrays.asList(X2BaseBean.COL_OID), PredefinedResolver.X2BASE_BEAN);
    }

    /**
     * Creates the Filterable.
     *
     * @param <T> the generic type
     * @param beans List<T>
     * @return Filterable
     */
    public static <T extends ToolBean> Filterable createFilterableToolBeans(Collection<T> beans) {
        return create(beans, Arrays.asList(ToolBean.FIELD_OID.resolve(null)), PredefinedResolver.RPT_BEAN);
    }


}
