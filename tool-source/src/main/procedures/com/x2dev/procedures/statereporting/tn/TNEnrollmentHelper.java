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

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DictionaryHelper;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.x2dev.procedures.statereporting.tn.TNStateReportData.Pair;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.ContextAttributesManager;
import com.x2dev.sis.model.business.StaffContextAttributesManager;
import com.x2dev.sis.model.business.StudentContextAttributesManager;
import com.x2dev.sis.tools.reports.ContextReportHelper;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.Method;
import java.sql.Date;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The TN State reports must calculate enrollment spans based on only enrollment records with state
 * reference code values.
 * This class provides an extended StudentHistoryHelper that will exclude enrollment records that do
 * not match this criteria.
 *
 */
public class TNEnrollmentHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class TNMultiYearHelper.
     */
    public static abstract class TNMultiYearHelper extends ContextReportHelper {
        /**
         * Enum contains implementations of CriteriaStrategy interface.
         *
         * @author Follett Software Company
         */
        public enum Strategy {
            BETWEEN(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    boolean isEligible = false;
                    PlainDate startSpan = new PlainDate((Date) args[0]);
                    PlainDate endSpan = new PlainDate((Date) args[1]);
                    PlainDate inPutDate = null;
                    if (valueAsObject instanceof String) {
                        try {
                            inPutDate = new PlainDate(new SimpleDateFormat("yyyy-MM-dd").parse((String) valueAsObject));
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                    } else if (valueAsObject instanceof Date) {
                        inPutDate = new PlainDate((Date) valueAsObject);
                    }
                    if (startSpan != null && endSpan != null && inPutDate != null &&
                            (startSpan.before(inPutDate) || startSpan.equals(inPutDate)) &&
                            (endSpan.after(inPutDate) || endSpan.equals(inPutDate))) {
                        isEligible = true;
                    }

                    return isEligible;
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addBetween(adjustedPath, args[0], args[1]);
                    return criteria;
                }

            }), EQUAL_TO(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    return args[0].equals(valueAsObject);
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(adjustedPath, args[0]);
                    return criteria;
                }
            }),

            GREATER_OR_EQUAL_THAN(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    boolean eligibleValue = false;

                    PlainDate comparedDate = new PlainDate((Date) args[0]);
                    PlainDate blobDate = null;

                    if (valueAsObject instanceof String) {
                        try {
                            blobDate = new PlainDate(new SimpleDateFormat("yyyy-MM-dd").parse((String) valueAsObject));
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                    } else if (valueAsObject instanceof Date) {
                        blobDate = new PlainDate((Date) valueAsObject);
                    }

                    if (blobDate != null && comparedDate != null) {
                        eligibleValue = !blobDate.before(comparedDate);
                    }

                    return eligibleValue;
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addGreaterOrEqualThan(adjustedPath, args[0]);
                    return criteria;
                }
            }),

            IN(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    return ((Collection) args[0]).contains(valueAsObject);
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    if (args[0] instanceof Collection) {
                        criteria.addIn(adjustedPath, (Collection) args[0]);
                    } else if (args[0] instanceof ReportQueryByCriteria) {
                        criteria.addIn(adjustedPath, (ReportQueryByCriteria) args[0]);
                    }
                    return criteria;
                }
            }),

            LESS_OR_EQUAL_THAN(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    boolean eligibleValue = false;

                    PlainDate comparedDate = new PlainDate((Date) args[0]);
                    PlainDate blobDate = null;

                    if (valueAsObject instanceof String) {
                        try {
                            blobDate = new PlainDate(new SimpleDateFormat("yyyy-MM-dd").parse((String) valueAsObject));
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                    } else if (valueAsObject instanceof Date) {
                        blobDate = new PlainDate((Date) valueAsObject);
                    }

                    if (blobDate != null && comparedDate != null) {
                        eligibleValue = !blobDate.after(comparedDate);
                    }

                    return eligibleValue;
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addLessOrEqualThan(adjustedPath, args[0]);
                    return criteria;
                }
            }),

            NOT_EMPTY(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    boolean eligibleValue = false;

                    if (valueAsObject != null && !valueAsObject.equals("null")) {
                        if (valueAsObject instanceof String) {
                            eligibleValue = !((String) valueAsObject).isEmpty();
                        } else {
                            eligibleValue = true;
                        }
                    }

                    return eligibleValue;
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addNotEmpty(adjustedPath, (PersistenceKey) args[0]);
                    return criteria;
                }
            }),

            NOT_EQUAL_TO(new CriteriaStrategy() {
                @Override
                public boolean eligibleValue(Object[] args, Object valueAsObject) {
                    return !args[0].equals(valueAsObject);
                }

                @Override
                public X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addNotEqualTo(adjustedPath, args[0]);
                    return criteria;
                }
            });

            /**
             * Instantiates a new strategy.
             *
             * @param strategy CriteriaStrategy
             */
            private Strategy(CriteriaStrategy strategy) {
                m_strategy = strategy;
            }

            CriteriaStrategy m_strategy = null;

            /**
             * Eligible value.
             *
             * @param args Object[]
             * @param valueAsObject Object
             * @return true, if successful
             */
            boolean eligibleValue(Object[] args, Object valueAsObject) {
                return m_strategy.eligibleValue(args, valueAsObject);
            }

            /**
             * Gets the usual criteria.
             *
             * @param adjustedPath String
             * @param args Object[]
             * @return X 2 criteria
             */
            X2Criteria getUsualCriteria(String adjustedPath, Object[] args) {
                return m_strategy.getUsualCriteria(adjustedPath, args);
            }
        }

        /**
         * Interface contains methods that should be implemented by Criteria Strategies to be used
         * in algorithm of
         * criteria adjustment to consider overridden school year context and Blob Information of
         * Context Attributes.
         *
         * @author Follett Software Company
         */
        protected interface CriteriaStrategy {

            /**
             * Determine based on passed arguments if valueAsObject is eligible.
             *
             * @param args Object[]
             * @param valueAsObject Object
             * @return boolean
             */
            boolean eligibleValue(Object[] args, Object valueAsObject);

            /**
             * Get appropriate criteria based on passed path and arguments.
             *
             * @param path String
             * @param args Object[]
             * @return X2Criteria
             */
            X2Criteria getUsualCriteria(String path, Object[] args);
        }

        /**
         * Criteria handler adjusting criteria considering selected context.
         *
         * @author Follett Software Company
         */
        protected abstract class CriteriaHandler {
            protected ContextAttributesManager m_attributesManager;
            protected Map<Integer, Map<String, Collection<ContextAttributes>>> m_attributesMap;
            protected X2Broker m_broker;
            protected DataDictionary m_dictionary;

            /**
             * Instantiates a new criteria handler.
             *
             * @param dictionary DataDictionary
             * @param broker X2Broker
             * @param manager ContextAttributesManager
             */
            CriteriaHandler(DataDictionary dictionary, X2Broker broker, ContextAttributesManager manager) {
                m_broker = broker;
                m_dictionary = dictionary;
                m_attributesManager = manager;
            }

            /**
             * Return common criteria for bean path considering selected context.
             * Common criteria is formed from criteria for each school year.
             *
             * @param strategy Strategy
             * @param beanPath String
             * @param args Object[]
             * @return String
             */
            public X2Criteria getCriteria(Strategy strategy, String beanPath, Object... args) {
                int currentYear = getCurrentContext().getSchoolYear();
                ArrayList<Integer> previousYears = new ArrayList<Integer>();
                ArrayList<X2Criteria> criteriaCollection = new ArrayList<X2Criteria>();
                while (!yearAfterOrgContext(currentYear)) {
                    if (m_attributesMap == null) {
                        initializeAllAttributes();
                    }

                    X2Criteria currentYearCriteria = new X2Criteria();

                    // each next criteria should include only students that have no attributes for
                    // previous school years
                    // because if student has attributes for previous years his criteria already
                    // exists
                    if (!previousYears.isEmpty()) {
                        // we need find beans that have attributes for previous years and exclude
                        // them
                        ArrayList<String> beanOids = new ArrayList<String>();
                        for (Integer year : previousYears) {
                            Map<String, Collection<ContextAttributes>> yearMap = m_attributesMap.get(year);
                            if (yearMap != null) {
                                beanOids.addAll(yearMap.keySet());
                            }
                        }
                        X2Criteria previousBeansCriteria = new X2Criteria();
                        previousBeansCriteria.addIn(getAttributeRelationship() + PATH_DELIMITER +
                                "districtContext" + PATH_DELIMITER + "schoolYear", previousYears);
                        SubQuery previousSubQuery =
                                new SubQuery(getSourceClass(), X2BaseBean.COL_OID, previousBeansCriteria);
                        String relToBeanOid = getRelationToBeanOids(beanPath);
                        currentYearCriteria.addNotIn(relToBeanOid, previousSubQuery);
                    }
                    currentYearCriteria.addAndCriteria(getCriteriaForYear(strategy, beanPath, args, currentYear));

                    criteriaCollection.add(currentYearCriteria);
                    previousYears.add(Integer.valueOf(currentYear));
                    currentYear++;
                }

                X2Criteria commonCriteria = new X2Criteria();
                if (isContextOverride()) {
                    X2Criteria.addNoMatchCriteria(commonCriteria);
                }

                for (X2Criteria currentCriteria : criteriaCollection) {
                    commonCriteria.addOrCriteria(currentCriteria);
                }

                return commonCriteria;
            }

            /**
             * Return adjusted arguments. Needed to avoid repeating calculation of arguments. e.g.,
             * IN values when
             * SubQuery is passed as argument.
             *
             * @param strategy Strategy
             * @param args Object[]
             * @return Object[]
             */
            protected Object[] getAdjustedArgs(Strategy strategy, Object[] args) {
                // Adjustments for IN strategy.
                if (Strategy.IN.equals(strategy)) {
                    Object inObject = args[0];
                    if (inObject instanceof SubQuery) {
                        ArrayList<Object> inValues = new ArrayList<Object>();

                        SubQuery subQuery = (SubQuery) inObject;

                        String[] atts = subQuery.getAttributes();
                        Class baseClass = subQuery.getBaseClass();
                        Criteria criteria = subQuery.getCriteria();
                        ReportQueryByCriteria query = new ReportQueryByCriteria(baseClass, atts, criteria);
                        ReportQueryIterator iterator = m_broker.getReportQueryIteratorByQuery(query);
                        try {
                            while (iterator.hasNext()) {
                                Object[] items = (Object[]) iterator.next();
                                Object inValue = items[0];
                                inValues.add(inValue);
                            }
                        } finally {
                            iterator.close();
                        }

                        args[0] = inValues;
                    }
                }

                return args;
            }

            /**
             * Initialize all attributes.
             */
            protected abstract void initializeAllAttributes();

            /**
             * Returns criteria based on blob information for passed year.
             *
             * @param strategy Strategy
             * @param adjustedPath String
             * @param args Object[]
             * @param year int
             * @return X2Criteria
             */
            private X2Criteria getBlobCriteriaForYear(Strategy strategy, String adjustedPath, Object[] args, int year) {
                X2Criteria criteria = null;

                ArrayList<String> relCols =
                        new ArrayList<String>(Arrays.asList(adjustedPath.split(REGEX_PATH_DELIMITER)));

                // check if beanPath relates to context attributes
                if (relCols.size() > 1 && relCols.get(relCols.size() - 2).equals(getAttributeRelationship())) {
                    String columnBeanPath = relCols.get(relCols.size() - 1);

                    String contextBeanPath = m_attributesManager.getAttributeColumn(columnBeanPath);
                    if (contextBeanPath == null) {
                        // if attributes columns has no the column, we need handle blob
                        Collection<String> queriedOids = new ArrayList<String>();

                        String id = DictionaryHelper.translateAlias(columnBeanPath, m_dictionary, true);
                        ModelProperty property = new ModelProperty(getSourceClass(), id, m_dictionary);

                        args = getAdjustedArgs(strategy, args);

                        Map<String, Collection<ContextAttributes>> mapOfYear =
                                m_attributesMap.get(Integer.valueOf(year));
                        if (mapOfYear != null) {
                            for (Entry<String, Collection<ContextAttributes>> beanAttributes : mapOfYear.entrySet()) {
                                String beanOid = beanAttributes.getKey();
                                ContextAttributes attributes = beanAttributes.getValue().iterator().next();

                                // Check if bean path is in blob data
                                Map<String, String> blobContent =
                                        ContextAttributesManager.parseBlobContents(attributes.getBlobInformation());
                                if (blobContent.containsKey(property.getDictionaryPath())) {
                                    String valueAsString = blobContent.get(property.getDictionaryPath());
                                    DictionaryHelper helper = new DictionaryHelper(m_broker,
                                            LocalizationCache.getPrimarySystemLocale(m_broker.getPersistenceKey()));
                                    Object valueAsObject = helper.getValueAsObject(property.getField(), valueAsString);

                                    if (strategy.eligibleValue(args, valueAsObject)) {
                                        queriedOids.add(beanOid);
                                    }
                                }
                            }
                        }

                        // remove attributes from path as it is not needed after we have bean oids
                        relCols.remove(relCols.size() - 2);
                        // set column to bean oid
                        relCols.set(relCols.size() - 1, X2BaseBean.COL_OID);

                        StringBuilder newBeanPathBuilder = new StringBuilder();
                        for (String relCol : relCols) {
                            if (newBeanPathBuilder.length() > 0) {
                                newBeanPathBuilder.append(ModelProperty.PATH_DELIMITER);
                            }
                            newBeanPathBuilder.append(relCol);
                        }

                        criteria = new X2Criteria();
                        criteria.addIn(newBeanPathBuilder.toString(), queriedOids);
                    }
                }

                return criteria;
            }

            /**
             * Return criteria for bean path considering passed context.
             * <p>
             * 1) If context is not overridden, create and return usual criteria.
             * <p>
             * 2) If context is overridden, create and return criteria with adjusted beanPath. In
             * this case we need add
             * relation to context attributes of passed year with correct bean path of the column.
             * <p>
             * 3) If context is overridden and field value is stored in blob, iterate through loaded
             * attributes
             * for passed year to determine attributes with blob containing needed field and value
             * in the field.
             * Then return criteria filtering by beans' oids.
             *
             * @param strategy Strategy
             * @param beanPath String
             * @param args Object[]
             * @param year int
             * @return X2Criteria
             */
            private X2Criteria getCriteriaForYear(Strategy strategy, String beanPath, Object[] args, int year) {
                X2Criteria criteria = null;
                String adjustedPath = getAdjustedPath(beanPath, year);
                if (isContextOverride(year)) {
                    criteria = getBlobCriteriaForYear(strategy, adjustedPath, args, year);
                }

                if (criteria == null) {
                    if (isContextOverride(year)) {
                        String[] adjColumns = adjustedPath.split(REGEX_PATH_DELIMITER);

                        // convert criteria to avoid join on relation, because we need to get
                        // information from beans
                        // that have no attributes also.

                        int relationIndex = 0;
                        for (int i = 0; i < adjColumns.length; i++) {
                            if (adjColumns[i].equals(getAttributeRelationship())) {
                                relationIndex = i;
                            }
                        }
                        ArrayList<String> fromBeanColumns = new ArrayList<String>(Arrays.asList(adjColumns));
                        for (int i = 0; i < relationIndex; i++) {
                            fromBeanColumns.remove(i);
                        }

                        String pathFromBean = getPathFromCols(fromBeanColumns);
                        X2Criteria beanCriteria = strategy.getUsualCriteria(pathFromBean, args);
                        beanCriteria.addEqualTo(getAttributeRelationship() + PATH_DELIMITER + "districtContext" +
                                PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(year));

                        SubQuery beanSubQuery = new SubQuery(getSourceClass(), X2BaseBean.COL_OID, beanCriteria);

                        criteria = new X2Criteria();
                        criteria.addIn(getRelationToBeanOids(beanPath), beanSubQuery);
                    } else {
                        criteria = strategy.getUsualCriteria(adjustedPath, args);
                    }
                }

                return criteria;
            }

            /**
             * Gets the relation to bean oids.
             *
             * @param beanPath String
             * @return String
             */
            private String getRelationToBeanOids(String beanPath) {
                String relationToBeanOid = null;

                ArrayList<String> relCols = new ArrayList(Arrays.asList(beanPath.split(REGEX_PATH_DELIMITER)));
                if (!relCols.isEmpty()) {
                    if (relCols.contains(getSourceClassRelation())) {
                        int relationIndex = relCols.indexOf(getSourceClassRelation());
                        relCols.set(relationIndex + 1, "oid");
                        relCols = new ArrayList<String>(relCols.subList(0, relationIndex + 2));
                    } else {
                        relCols.set(0, "oid");
                        relCols = new ArrayList<String>(relCols.subList(0, 1));
                    }

                    StringBuilder newPathBuilder = new StringBuilder();
                    for (String relCol : relCols) {
                        if (newPathBuilder.length() != 0) {
                            newPathBuilder.append(ModelProperty.PATH_DELIMITER);
                        }
                        newPathBuilder.append(relCol);
                    }
                    relationToBeanOid = newPathBuilder.toString();
                }

                return relationToBeanOid;
            }
        }

        /**
         * Constructor.
         *
         * @param organization Organization
         * @param context DistrictSchoolYearContext
         * @param broker X2Broker
         */
        public TNMultiYearHelper(Organization organization, DistrictSchoolYearContext context, X2Broker broker) {
            super(organization, context, broker);
            m_dataContext =
                    yearAfterOrgContext(context.getSchoolYear()) ? getOrganization().getCurrentContext() : context;
        }

        private static final String REGEX_PATH_DELIMITER = "\\.";

        protected Map<String, Collection<ContextAttributes>> m_contextsAttributesMap;
        protected Criteria m_criteria;
        protected ContextAttributesManager m_manager;

        CriteriaHandler m_criteriaHandler = null;

        private DistrictSchoolYearContext m_dataContext;
        private Boolean m_isContextOverride;

        /**
         * Adjust passed criteria considering selected context using the strategy.
         *
         * @param criteria X2Criteria
         * @param strategy Strategy
         * @param beanPath String
         * @param args Object[]
         */
        public void adjustCriteria(X2Criteria criteria, Strategy strategy, String beanPath, Object... args) {
            if (m_criteriaHandler == null) {
                m_criteriaHandler = getCriteriaHandler();
            }
            criteria.addAndCriteria(m_criteriaHandler.getCriteria(strategy, beanPath, args));
        }

        /**
         * Returns adjusted path with inserted attributes relation if context is overridden.
         *
         * @param originalPath String
         * @return String
         */
        public String getAdjustedPath(String originalPath) {
            String adjustedPath = originalPath;

            if (isContextOverride()) {
                ArrayList<String> relCols = new ArrayList(Arrays.asList(originalPath.split(REGEX_PATH_DELIMITER)));
                if (!relCols.isEmpty()) {
                    if (relCols.contains(getSourceClassRelation())) {
                        int relationIndex = relCols.indexOf(getSourceClassRelation());
                        String relColFromBean = relCols.get(relationIndex + 1);
                        String relColFromAttributes = getRelColFromAttributes(relColFromBean);
                        relCols.set(relationIndex + 1, relColFromAttributes);
                    } else {
                        String relColFromBean = relCols.get(0);
                        String relColFromAttributes = getRelColFromAttributes(relColFromBean);
                        relCols.set(0, relColFromAttributes);
                    }

                    adjustedPath = getPathFromCols(relCols);
                }
            }

            return adjustedPath;
        }

        /**
         * Returns adjusted path with inserted attributes relation if context is overridden for
         * passed school year.
         *
         * @param originalPath String
         * @param year int
         * @return String
         */
        public String getAdjustedPath(String originalPath, int year) {
            String adjustedPath = originalPath;

            if (isContextOverride(year)) {
                ArrayList<String> relCols = new ArrayList(Arrays.asList(originalPath.split(REGEX_PATH_DELIMITER)));
                if (!relCols.isEmpty()) {
                    if (relCols.contains(getSourceClassRelation())) {
                        int relationIndex = relCols.indexOf(getSourceClassRelation());
                        String relColFromBean = relCols.get(relationIndex + 1);
                        String relColFromAttributes = getRelColFromAttributes(relColFromBean);
                        relCols.set(relationIndex + 1, relColFromAttributes);
                    } else {
                        String relColFromBean = relCols.get(0);
                        String relColFromAttributes = getRelColFromAttributes(relColFromBean);
                        relCols.set(0, relColFromAttributes);
                    }

                    adjustedPath = getPathFromCols(relCols);
                }
            }

            return adjustedPath;
        }

        /**
         * Return most recent attributes after current context.
         *
         * @param oid String
         * @return Context attributes
         */
        @Override
        public ContextAttributes getContextAttributes(String oid) {
            // Create contextAttributesMap if not created yet.
            initializeAttributes();

            ContextAttributes attributes = null;

            Collection<ContextAttributes> contextsAttributes = m_contextsAttributesMap.get(oid);

            // Check and add if not found.
            contextsAttributes = addBeansAttributes(oid, contextsAttributes);

            if (!contextsAttributes.isEmpty()) {
                attributes = contextsAttributes.iterator().next();
            }
            return attributes;
        }

        /**
         * Return source bean criteria.
         *
         * @return Criteria
         */
        public Criteria getCriteria() {
            return m_criteria;
        }

        /**
         * Returns selected context.
         *
         * @return DistrictSchoolYearContext
         */
        public DistrictSchoolYearContext getCurrentContext() {
            return m_dataContext;
        }

        /**
         * Returns the field value for the passed bean path. For the current school year, the value
         * is retrieved directly
         * from the passed bean. For other school years, the value is retrieved from the context
         * attribute record.
         *
         * If no attributes record is found, the method will return the value from the bean.
         *
         * If beanPath contain contains - logic try change first relation depend on context<br>
         * Example <code>school.fieldA001</code>, <code>school</code> depend from context and will
         * take from bean or from <code>StudentContextAttributes</code><br>
         * other path (<code>.fieldA001</code>) will without changes.
         *
         * @param bean X2BaseBean
         * @param beanPath String
         * @return Object
         */
        @Override
        public Object getFieldValueByBeanPath(X2BaseBean bean, String beanPath) {
            // 1) Set value to base value from bean
            Object value = bean.getFieldValueByBeanPath(beanPath);
            if (isContextOverride()) {
                ContextAttributes attribute = getContextAttributes(bean.getOid());
                if (attribute != null) {
                    // 2) Check if the bean path is in attribute data
                    String contextBeanPath = getManager().getAttributeColumn(beanPath);
                    if (contextBeanPath != null) {
                        value = attribute.getFieldValueByBeanPath(contextBeanPath);
                    } else {
                        String id = DictionaryHelper.translateAlias(beanPath, getDictionary(), true);
                        ModelProperty property = new ModelProperty(getSourceClass(), id, getDictionary());

                        // 3) Check if bean path is in blob data
                        Map<String, String> blobContent =
                                ContextAttributesManager.parseBlobContents(attribute.getBlobInformation());
                        if (blobContent.containsKey(property.getDictionaryPath())) {
                            String valueAsString = blobContent.get(property.getDictionaryPath());
                            DictionaryHelper helper = new DictionaryHelper(getBroker(),
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
                            value = helper.getValueAsObject(property.getField(), valueAsString);
                        }
                    }
                }
            }

            return value;
        }

        /**
         * Return value from reference code by aliasHitory considering selected context. If value is
         * not found,just get
         * value by aliasGeneral.
         *
         * @param refCode ReferenceCode
         * @param aliasHistory String
         * @param aliasGeneral String
         * @return String
         */
        public String getHistoryValueByAlias(ReferenceCode refCode, String aliasHistory, String aliasGeneral) {
            String value = (String) refCode.getFieldValueByAlias(aliasGeneral);

            final int YEAR = 0;
            final int VALUE = 1;

            String yearsValuesString = (String) refCode.getFieldValueByAlias(aliasHistory);

            if (yearsValuesString != null) {
                String[] allYearsValuesArray = yearsValuesString.split(",");

                Map<String, String> yearValueMap = new HashMap<String, String>();

                for (String yearValue : allYearsValuesArray) {
                    String[] yearValueArray = yearValue.split("=");

                    if (yearValueArray.length > 1) {
                        yearValueMap.put(yearValueArray[YEAR], yearValueArray[VALUE]);
                    }
                }

                String schoolYear = String.valueOf(getCurrentContext().getSchoolYear());
                if (yearValueMap.containsKey(schoolYear)) {
                    value = yearValueMap.get(schoolYear);
                }
            }

            return value;
        }

        /**
         * Return the related bean based on the bean path using this context helper.
         *
         * @param bean X2BaseBean
         * @param beanPath String
         * @return X 2 base bean
         */
        public X2BaseBean getRelationByBeanPath(X2BaseBean bean, String beanPath) {
            X2BaseBean relatedBean = null;

            relatedBean = TNStateReportData.getRelatedBean(bean, beanPath);
            if (isContextOverride()) {
                String methodName = "get" + beanPath.substring(0, 1).toUpperCase() + beanPath.substring(1);
                Method method;
                try {
                    method = getClass().getDeclaredMethod(methodName, new Class[] {bean.getClass()});
                    if (method != null) {
                        relatedBean = (X2BaseBean) method.invoke(this, bean);
                    }
                } catch (Exception e) {
                    // use no context relation
                }
            }

            return relatedBean;
        }

        /**
         * Return criteria to select beans that have Context Attributes for current context if
         * context is overridden.
         *
         * @return X2Criteria
         */
        public abstract X2Criteria getWithAttributesCriteria();

        /**
         * Return criteria to select beans that have Context Attributes for current context if
         * context is overridden.
         *
         * @param prefix String
         * @return X2Criteria
         */
        public abstract X2Criteria getWithAttributesCriteria(String prefix);

        /**
         * Returns true if contexts of TNStateReportData and Organization are different and
         * historical.
         *
         * @return boolean
         */
        @Override
        public boolean isContextOverride() {
            if (m_isContextOverride == null) {

                m_isContextOverride =
                        Boolean.valueOf(!getOrganization().getCurrentContextOid().equals(m_dataContext.getOid()) &&
                                isContextOverride(m_dataContext.getSchoolYear()));
            }
            return m_isContextOverride.booleanValue();
        }

        /**
         * Set source bean criteria.
         *
         * @param criteria void
         */
        public void setCriteria(Criteria criteria) {
            m_criteria = criteria;
        }

        /**
         * Whenever we use the map of attributes records with a bean that is not found in the
         * lookup, we need
         * to get the attributes records and add the appropriate collection to the map. This will
         * insure that
         * we are only looking up the attributes records once and will provide a fallback if the
         * bean's attributes record.
         *
         * @param oid String
         * @param contextsAttributes Collection<ContextAttributes>
         * @return Collection<ContextAttributes>
         */
        protected abstract Collection<ContextAttributes> addBeansAttributes(String oid,
                                                                            Collection<ContextAttributes> contextsAttributes);

        /**
         * Gets the attribute relationship.
         *
         * @return String
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getAttributeRelationship()
         */
        @Override
        protected abstract String getAttributeRelationship();

        /**
         * Gets the criteria handler.
         *
         * @return CriteriaHandler
         */
        protected abstract CriteriaHandler getCriteriaHandler();

        /**
         * Gets the manager.
         *
         * @return ContextAttributesManager
         */
        protected abstract ContextAttributesManager getManager();

        /**
         * Gets the source class.
         *
         * @return Class
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getSourceClass()
         */
        @Override
        protected abstract Class getSourceClass();

        /**
         * Gets the source class relation.
         *
         * @return relation to current source class
         */
        protected abstract String getSourceClassRelation();

        /**
         * Initializes context attributes for the passed criteria.
         */
        protected abstract void initializeAttributesByCriteria();

        /**
         * Returns bean path from passed columns.
         *
         * @param relCols ArrayList<String>
         * @return String
         */
        String getPathFromCols(ArrayList<String> relCols) {
            String adjustedPath;
            StringBuilder newPathBuilder = new StringBuilder();
            for (String relCol : relCols) {
                if (newPathBuilder.length() != 0) {
                    newPathBuilder.append(ModelProperty.PATH_DELIMITER);
                }
                newPathBuilder.append(relCol);
            }
            adjustedPath = newPathBuilder.toString();
            return adjustedPath;
        }

        /**
         * Returns true if passed year is less than or equal to organization context year, otherwise
         * false.
         *
         * @param year int
         * @return boolean
         */
        boolean isContextOverride(int year) {
            boolean override = false;
            if (year < getOrganization().getCurrentContext().getSchoolYear()) {
                override = true;
            }
            return override;
        }

        /**
         * Returns true if passed year is less than or equal to organization context year, otherwise
         * false.
         *
         * @param year int
         * @return boolean
         */
        boolean yearAfterOrgContext(int year) {
            boolean after = false;
            if (year > getOrganization().getCurrentContext().getSchoolYear()) {
                after = true;
            }
            return after;
        }

        /**
         * Return columns/relation adjusted from attributes context.
         *
         * @param relColFromBean String
         * @return String
         */
        private String getRelColFromAttributes(String relColFromBean) {
            String addedRelCol = getField(relColFromBean);
            if (addedRelCol.equals(relColFromBean)) {
                addedRelCol = getAttributeRelationship() + ModelProperty.PATH_DELIMITER + relColFromBean;
            }
            return addedRelCol;
        }

        /**
         * Initialize context attributes.
         */
        private void initializeAttributes() {
            if (m_contextsAttributesMap == null) {
                if (m_criteria != null) {
                    initializeAttributesByCriteria();
                } else {
                    m_contextsAttributesMap = new HashMap<String, Collection<ContextAttributes>>();
                }
            }
        }
    }

    /**
     * Helper class for staff to get correct data that dependent from school year.
     *
     * @author Follett Software Company
     */
    public static class TNStaffMultiYearHelper extends TNMultiYearHelper {

        /**
         * Handler of passed criteria, can return Criteria that should be used as alternative of
         * passed criteria to
         * consider overridden school year context and Blob Information of Context Attributes etc.
         *
         * @author Follett Software Company
         */
        private class StaffCriteriaHandler extends CriteriaHandler {

            /**
             * Instantiates a new staff criteria handler.
             *
             * @param dictionary DataDictionary
             * @param broker X2Broker
             * @param manager ContextAttributesManager
             */
            StaffCriteriaHandler(DataDictionary dictionary, X2Broker broker, ContextAttributesManager manager) {
                super(dictionary, broker, manager);
            }

            /**
             * Initializes context attributes for all staff for current context.
             */
            @Override
            protected void initializeAllAttributes() {
                X2Criteria contextsAttributesCriteria = new X2Criteria();
                contextsAttributesCriteria
                        .addGreaterOrEqualThan(StaffContextAttributes.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                                DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                                Integer.valueOf(getCurrentContext().getSchoolYear()));
                QueryByCriteria contextAttributesQuery =
                        new QueryByCriteria(StaffContextAttributes.class, contextsAttributesCriteria, false);
                String[] columns = {
                        StaffContextAttributes.REL_DISTRICT_CONTEXT + PATH_DELIMITER
                                + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        StaffContextAttributes.COL_STAFF_OID
                };
                int[] sizes = {
                        1,
                        1
                };
                m_attributesMap = m_broker.getGroupedCollectionByQuery(contextAttributesQuery, columns, sizes);
            }
        }

        private static final String RELATION_STAFF = "staff";

        /**
         * Constructs a new TNStaffMultiYearHelper object.
         *
         * @param organization Organization
         * @param context DistrictSchoolYearContext
         * @param broker X2Broker
         */
        public TNStaffMultiYearHelper(Organization organization, DistrictSchoolYearContext context, X2Broker broker) {
            super(organization, context, broker);
        }

        /**
         * Returns the school.
         *
         * @param staff SisStaff
         * @return SisSchool
         */
        public SisSchool getSchool(SisStaff staff) {
            SisSchool school = null;

            school = staff.getSchool();

            if (isContextOverride()) {
                StaffContextAttributes attribute = (StaffContextAttributes) getContextAttributes(staff.getOid());
                if (attribute != null) {
                    school = attribute.getSchool();
                }
            }

            return school;
        }

        /**
         * Return criteria to select staff that have <code>StaffContextAttributes</code> for current
         * context if
         * context is overridden.
         *
         * @return X2Criteria
         */
        @Override
        public X2Criteria getWithAttributesCriteria() {
            return getWithAttributesCriteria(null);
        }

        /**
         * Return criteria to select staff that have <code>StaffContextAttributes</code> for current
         * context if
         * context is overridden.
         *
         * @param prefix String
         * @return X2Criteria
         */
        @Override
        public X2Criteria getWithAttributesCriteria(String prefix) {
            X2Criteria criteria = new X2Criteria();
            if (isContextOverride()) {
                criteria.addEqualTo(BeanManager.getFullPrefix(prefix) + getAttributeRelationship() +
                        ModelProperty.PATH_DELIMITER + StaffContextAttributes.COL_CONTEXT_OID,
                        getCurrentContext().getOid());
            }
            return criteria;
        }

        /**
         * Adds the beans attributes.
         *
         * @param oid String
         * @param contextsAttributes Collection<ContextAttributes>
         * @return Collection
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#addBeansAttributes(java.lang.String,
         *      java.util.Collection)
         */
        @Override
        protected Collection<ContextAttributes> addBeansAttributes(String oid,
                                                                   Collection<ContextAttributes> contextsAttributes) {
            if (contextsAttributes == null) {
                X2Criteria attributesCriteria = new X2Criteria();
                attributesCriteria.addEqualTo(StaffContextAttributes.COL_STAFF_OID, oid);
                attributesCriteria.addGreaterOrEqualThan(
                        StaffContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                                DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(getCurrentContext().getSchoolYear()));
                QueryByCriteria attributesQuery =
                        new QueryByCriteria(StaffContextAttributes.class, attributesCriteria, false);
                attributesQuery.addOrderBy(StaffContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                        DistrictSchoolYearContext.COL_SCHOOL_YEAR, true);
                contextsAttributes = getBroker().getCollectionByQuery(attributesQuery);
                m_contextsAttributesMap.put(oid, contextsAttributes);
            }
            return contextsAttributes;
        }

        /**
         * Gets the attribute relationship.
         *
         * @return String
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getAttributeRelationship()
         */
        @Override
        protected String getAttributeRelationship() {
            return SisStaff.REL_STAFF_CONTEXT_ATTRIBUTES;
        }

        /**
         * Gets the criteria handler.
         *
         * @return Criteria handler
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#getCriteriaHandler()
         */
        @Override
        protected CriteriaHandler getCriteriaHandler() {
            return new StaffCriteriaHandler(getDictionary(), getBroker(), m_manager);
        }

        /**
         * Gets the manager.
         *
         * @return Context attributes manager
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#getManager()
         */
        @Override
        protected ContextAttributesManager getManager() {
            if (m_manager == null) {
                m_manager = new StaffContextAttributesManager(getOrganization(), getBroker());
            }
            return m_manager;
        }

        /**
         * Gets the manager.
         *
         * @param organization Organization
         * @param broker X2Broker
         * @return Context attributes manager
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getManager(com.follett.fsc.core.k12.beans.Organization,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        protected ContextAttributesManager getManager(Organization organization, X2Broker broker) {
            return getManager();
        }

        /**
         * Gets the source class.
         *
         * @return Class
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getSourceClass()
         */
        @Override
        protected Class getSourceClass() {
            return SisStaff.class;
        }

        /**
         * Gets the source class relation.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#getSourceClassRelation()
         */
        @Override
        protected String getSourceClassRelation() {
            return RELATION_STAFF;
        }

        /**
         * Initializes the staff context attributes for the passed criteria.
         */
        @Override
        protected void initializeAttributesByCriteria() {
            SubQuery subQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_criteria);
            Collection<String> staffOids = getBroker().getSubQueryCollectionByQuery(subQuery);

            X2Criteria contextsAttributesCriteria = new X2Criteria();
            contextsAttributesCriteria.addIn(StaffContextAttributes.COL_STAFF_OID, staffOids);
            contextsAttributesCriteria
                    .addGreaterOrEqualThan(StaffContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                            Integer.valueOf(getCurrentContext().getSchoolYear()));
            QueryByCriteria contextAttributesQuery =
                    new QueryByCriteria(StaffContextAttributes.class, contextsAttributesCriteria, false);
            contextAttributesQuery
                    .addOrderBy(StaffContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR, true);
            m_contextsAttributesMap = getBroker().getGroupedCollectionByQuery(contextAttributesQuery,
                    StaffContextAttributes.COL_STAFF_OID, 5);

            for (String staffOid : staffOids) {
                if (!m_contextsAttributesMap.keySet().contains(staffOid)) {
                    m_contextsAttributesMap.put(staffOid, new ArrayList<ContextAttributes>());
                }
            }
        }
    }

    /**
     * This class is copied from StudentEnrollmentSpan, primarily because the constructor in
     * StudentEnrollmentSpan is protected.
     * There are no logic changes other than the use of a limited set of enrollment records.
     *
     * @author Follett Software Company
     */
    public class TNStudentEnrollmentSpan {
        /*
         * Constants
         */
        private static final String DEFAULT_CALENDAR_ID = "Standard";

        /*
         * Instance variables
         */
        List<StudentEnrollment> m_enrollments;
        PlainDate m_firstActiveDate;
        StudentEnrollment m_firstActiveEnrollment;
        StudentEnrollment m_firstEntry;
        StudentEnrollment m_firstInactiveEnrollment;
        StudentEnrollment m_firstWithdrawal;
        TNStudentHistoryHelper m_helper;
        PlainDate m_lastActiveDate;
        Integer m_membershipDays;
        boolean m_preferenceMemberOnEntry;
        boolean m_preferenceMemberOnWithdrawal;
        SisSchool m_school;
        SisStudent m_student;
        Collection<String> m_studentActiveStatuses;
        List<StudentAttendance> m_studentAttendance;

        /**
         * Constructor:
         * Find other values from the enrollment list.
         *
         * @param enrollments List<StudentEnrollment>
         * @param helper TNStudentHistoryHelper
         */
        TNStudentEnrollmentSpan(List<StudentEnrollment> enrollments, TNStudentHistoryHelper helper) {
            m_helper = helper;
            Organization organization = helper.getOrganization();

            PlainDate districtBeginDate = m_studentHistoryHelper.m_data.getCurrentContext().getStartDate();

            m_studentActiveStatuses = StudentManager.getActiveStudentCodeList(m_helperData.getOrganization());
            m_preferenceMemberOnEntry = Boolean.valueOf(PreferenceManager.getPreferenceValue(organization,
                    SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
            m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(organization,
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

            boolean splitYog =
                    ((Boolean) helper.getTNStudentSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG,
                            Boolean.class, Boolean.FALSE)).booleanValue();
            boolean splitStatus =
                    ((Boolean) helper.getTNStudentSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS,
                            Boolean.class, Boolean.FALSE)).booleanValue();


            boolean isActive = false;
            boolean isInactiveAgain = false;
            m_enrollments = enrollments;
            for (StudentEnrollment enrollment : enrollments) {
                // Get student and school from the enrollment record.
                if (m_student == null) {
                    m_student = enrollment.getStudent();
                }
                if (m_school == null) {
                    m_school = enrollment.getSchool();
                }

                if (!isActive) {
                    // Active code, or sometimes empty status and a non-withdrawal record.
                    String statusCode = enrollment.getStatusCode();
                    if (m_studentActiveStatuses.contains(statusCode) ||
                            (StringUtils.isEmpty(statusCode) &&
                                    !StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()))) {
                        isActive = true;
                        m_firstActiveEnrollment = enrollment;
                        if (m_preferenceMemberOnEntry) {
                            m_firstActiveDate = enrollment.getEnrollmentDate();
                            if (m_firstActiveDate.before(districtBeginDate)) {
                                m_firstActiveDate = districtBeginDate;
                            }
                        } else {
                            // Lookup next in-session date for the school.
                            m_firstActiveDate = findSessionDate(enrollment.getEnrollmentDate(), true);
                        }
                    }
                } else if (!isInactiveAgain) {
                    String statusCode = enrollment.getStatusCode();
                    if (!m_studentActiveStatuses.contains(statusCode)) {
                        isInactiveAgain = true;
                        m_firstInactiveEnrollment = enrollment;
                        if (m_preferenceMemberOnWithdrawal) {
                            m_lastActiveDate = enrollment.getEnrollmentDate();
                        } else {
                            // Lookup previous in-session date for the school.
                            m_lastActiveDate = findSessionDate(enrollment.getEnrollmentDate(), false);
                        }
                    }
                }

                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                        m_firstEntry == null) {
                    m_firstEntry = enrollment;
                }
                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                        m_firstWithdrawal == null) {
                    m_firstWithdrawal = enrollment;
                }
            }

            // If no end-of-enrollment records was found (in case of YOG, STATUS) determine if
            // the last record should be treated as the exit record.
            if ((m_lastActiveDate == null || m_firstInactiveEnrollment == null) &&
                    m_enrollments.size() > 1) {
                StudentEnrollment enrollment = m_enrollments.get(m_enrollments.size() - 1);

                if ((splitYog && StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())
                        && !yogEliminate(enrollment, m_enrollments.get(m_enrollments.size() - 2))) ||
                        (splitStatus && StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) ||
                        StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    m_firstInactiveEnrollment = enrollment;
                    if (m_preferenceMemberOnWithdrawal) {
                        m_lastActiveDate = enrollment.getEnrollmentDate();
                    } else {
                        m_lastActiveDate = findSessionDate(enrollment.getEnrollmentDate(), false);
                    }
                }
            }
        }

        /**
         * Constructor to create copy of span.
         *
         * @param span TNStudentEnrollmentSpan
         */
        private TNStudentEnrollmentSpan(TNStudentEnrollmentSpan span) {
            this.m_enrollments = span.m_enrollments;
            this.m_firstActiveDate = span.m_firstActiveDate;
            this.m_firstActiveEnrollment = span.m_firstActiveEnrollment;
            this.m_firstEntry = span.m_firstEntry;
            this.m_firstInactiveEnrollment = span.m_firstInactiveEnrollment;
            this.m_firstWithdrawal = span.m_firstWithdrawal;
            this.m_helper = span.m_helper;
            this.m_lastActiveDate = span.m_lastActiveDate;
            this.m_membershipDays = span.m_membershipDays;
            this.m_preferenceMemberOnEntry = span.m_preferenceMemberOnEntry;
            this.m_preferenceMemberOnWithdrawal = span.m_preferenceMemberOnWithdrawal;
            this.m_studentActiveStatuses = span.m_studentActiveStatuses;
            this.m_school = span.m_school;
            this.m_student = span.m_student;
            this.m_studentAttendance = span.m_studentAttendance;
        }

        /**
         * Find the nearest in session date to the date provided.
         * <br>
         * parameter "after" indicates if the nearest date should be
         * after the date provided. Otherwise, the nearest in session date
         * before the date provided is returned.
         *
         * @param enrollmentDate PlainDate
         * @param after boolean
         * @return PlainDate
         */
        public PlainDate findSessionDate(PlainDate enrollmentDate, boolean after) {
            PlainDate nearestDate = null;
            String studentCalendar =
                    (String) getStudentValueByBeanPath(m_student, SisStudent.COL_CALENDAR_CODE);
            Set<PlainDate> insessionDates = m_helper.getCalendarDays(m_school, studentCalendar);
            if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(studentCalendar)) {
                insessionDates = m_helper.getCalendarDays(m_school, DEFAULT_CALENDAR_ID);
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (after && date.after(enrollmentDate)) {
                        if (nearestDate == null || nearestDate.after(date)) {
                            nearestDate = date;
                        }
                    } else if (!after && date.before(enrollmentDate)) {
                        if (nearestDate == null || nearestDate.before(date)) {
                            nearestDate = date;
                        }
                    }
                }
            }
            if (nearestDate == null) {
                if (after) {
                    nearestDate = enrollmentDate;
                } else {
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(enrollmentDate);
                    cal.add(Calendar.DATE, -1);
                    nearestDate = new PlainDate(cal.getTime());
                }
            }
            return nearestDate;
        }

        /**
         * Returns copy of this span.
         *
         * @return TNStudentEnrollmentSpan
         */
        public TNStudentEnrollmentSpan getCopy() {
            return new TNStudentEnrollmentSpan(this);
        }

        /**
         * Return the first active date for the student in the enrollment span.
         * <p>
         * This finds the first enrollment record in the span that indicates
         * the student is active. It then checks the system preference for membership
         * on entry date to determine if this is the first active date or looks up
         * the next in session date as the first active date.
         * <p>
         * This value will be adjusted to fit the current school year. It is not
         * representative of reportable enrollment dates. First and last active dates
         * are most useful for counting membership and attendance days for an
         * enrollment span.
         *
         * @return PlainDate
         */
        public PlainDate getFirstActiveDate() {
            return m_firstActiveDate;
        }

        /**
         * Return the first enrollment record to indicate active status.
         * This is usually considered to be the entry record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstActiveEnrollment() {
            return m_firstActiveEnrollment;
        }

        /**
         * Return the first entry enrollment record to indicate active status.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstEntryEnrollment() {
            return m_firstEntry;
        }

        /**
         * Return the first enrollment record to indicate inactive status after having been active
         * in this span.
         * This is usually considered to be the exit record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstInactiveEnrollment() {
            return m_firstInactiveEnrollment;
        }

        /**
         * Return the most recent enrollment record of the specified types that exists on or before
         * the specified date.
         *
         * @param date as of date to find enrollment records for.
         * @param types a String that includes a combination of the four StudentEnrollment type
         *        constants ('E','W','S','Y').
         *        <br>
         *        EX: "ES" to search for only Entry or Status Change records.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEnrollmentForDate(PlainDate date, String types) {
            StudentEnrollment lastEnrollment = null;
            for (StudentEnrollment enrollment : m_enrollments) {
                if (!enrollment.getEnrollmentDate().after(date)) {
                    if (types.contains(enrollment.getEnrollmentType())) {
                        lastEnrollment = enrollment;
                    }
                }
            }
            return lastEnrollment;
        }

        /**
         * Return the list of enrollments in this span.
         *
         * @return List<StudentEnrollment>
         */
        public List<StudentEnrollment> getEnrollments() {
            return m_enrollments;
        }

        /**
         * Return the last active date for the student in the enrollment span.
         * <p>
         * This finds the first enrollment record in the span that indicates
         * the student is inactive after having been active previously. It then
         * checks the system preference for membership on withdrawal date to
         * determine if this is the last active date or looks up the previous
         * in session date as the last active date.
         * <p>
         * This value will be adjusted to fit the current school year. It is not
         * representative of reportable enrollment dates. First and last active dates
         * are most useful for counting membership and attendance days for an
         * enrollment span.
         *
         * @return PlainDate
         */
        public PlainDate getLastActiveDate() {
            return m_lastActiveDate;
        }

        /**
         * Return the school for the enrollment span.
         *
         * @return School
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Returns a list of student attendance records for the student within the
         * specified date range first active to last active.
         *
         * @return List<StudentAttendance>
         */
        public List<StudentAttendance> getStudentAttendance() {
            if (m_studentAttendance == null) {
                m_studentAttendance = new ArrayList<StudentAttendance>(1);
                List<StudentAttendance> parentList = m_helper.getStudentAttendances(m_student.getOid());

                if (parentList != null) {
                    PlainDate endDate = m_lastActiveDate;
                    if (endDate == null) {
                        endDate = (PlainDate) m_helper.getTNStudentSelectionProperty(
                                StudentHistoryHelper.PROPERTY_END_DATE,
                                PlainDate.class,
                                new PlainDate(OrganizationManager.getTimeZone(m_helper.getOrganization())));
                    }

                    for (StudentAttendance attendance : parentList) {
                        if (m_firstActiveDate != null && !attendance.getDate().before(m_firstActiveDate)
                                && !attendance.getDate().after(endDate)) {
                            m_studentAttendance.add(attendance);
                        }
                    }
                }
            }
            return m_studentAttendance;
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        /*
         * get the yog from the first active enrollment, unless there exists a YOG_CHANGE record
         * immediately following the first active enrollment on the same date for the same school.
         * In this case, use the yog from the YOG_CHANGE record.
         */
        public int getYog() {
            int value = 0;
            if (m_firstActiveEnrollment != null) {
                value = m_firstActiveEnrollment.getYog();
                Iterator<StudentEnrollment> enrollments = m_enrollments.iterator();
                while (enrollments.hasNext()) {
                    StudentEnrollment enrollment = enrollments.next();
                    if (enrollment == m_firstActiveEnrollment) {
                        if (enrollments.hasNext()) {
                            StudentEnrollment nextEnrollment = enrollments.next();
                            if (yogEliminate(nextEnrollment, enrollment)) {
                                value = nextEnrollment.getYog();
                            }
                        }
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder value = new StringBuilder();

            value.append("School:");
            value.append(m_school == null ? m_school : m_school.getName());
            value.append(";");

            value.append("First Active Date:");
            value.append(m_firstActiveDate);
            value.append(";");

            value.append("Last Active Date:");
            value.append(m_lastActiveDate);
            value.append(";");

            value.append("First Active:{");
            if (m_firstActiveEnrollment == null) {
                value.append("null");
            } else {
                value.append("Date:");
                value.append(m_firstActiveEnrollment.getEnrollmentDate());
                value.append(";");
                value.append("Type:");
                value.append(m_firstActiveEnrollment.getEnrollmentType());
                value.append(";");
                value.append("Code:");
                value.append(m_firstActiveEnrollment.getEnrollmentCode());
                value.append(";");
                value.append("Yog:");
                value.append(m_firstActiveEnrollment.getYog());
                value.append(";");
            }
            value.append("}");

            value.append("First Inactive:{");
            if (m_firstInactiveEnrollment == null) {
                value.append("null");
            } else {
                value.append("Date:");
                value.append(m_firstInactiveEnrollment.getEnrollmentDate());
                value.append(";");
                value.append("Type:");
                value.append(m_firstInactiveEnrollment.getEnrollmentType());
                value.append(";");
                value.append("Code:");
                value.append(m_firstInactiveEnrollment.getEnrollmentCode());
                value.append(";");
                value.append("Yog:");
                value.append(m_firstInactiveEnrollment.getYog());
                value.append(";");
            }
            value.append("}");

            value.append("Withdrawal:{");
            if (m_firstWithdrawal == null) {
                value.append("null");
            } else {
                value.append("Date:");
                value.append(m_firstWithdrawal.getEnrollmentDate());
                value.append(";");
                value.append("Type:");
                value.append(m_firstWithdrawal.getEnrollmentType());
                value.append(";");
                value.append("Code:");
                value.append(m_firstWithdrawal.getEnrollmentCode());
                value.append(";");
                value.append("Yog:");
                value.append(m_firstActiveEnrollment.getYog());
                value.append(";");
            }
            value.append("}");
            return value.toString();
        }

        /**
         * Sets the first active enrollment.
         *
         * @param enrollment void
         */
        public void setFirstActiveEnrollment(StudentEnrollment enrollment) {
            m_firstActiveEnrollment = enrollment;
        }
    }

    /**
     * The Class TNStudentHistoryHelper.
     */
    public class TNStudentHistoryHelper extends StudentHistoryHelper {
        TNStateReportData m_data;
        /*
         * Standard aliases
         */
        private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
        private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
        private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
        private static final String ALIAS_EXCLUDE_SCC = "all-scc-ExcludeFromStateReporting";

        private String m_fieldExcludeCrs;
        private String m_fieldExcludeMst;
        private String m_fieldExcludeScc;
        private String m_fieldExcludeStd;
        private PlainDate m_firstDate;
        private PlainDate m_firstAttendDate;
        private PlainDate m_lastDate;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars =
                new HashMap<String, Map<String, Set<PlainDate>>>();
        private Map<String, List<StudentEnrollment>> m_studentEnrollmentMap;
        private X2Criteria m_studentScheduleChangeCriteria;
        private Map<String, List<StudentScheduleChange>> m_studentScheduleChangeMap;
        private X2Criteria m_studentScheduleCriteria;
        private Map<String, List<StudentSchedule>> m_studentScheduleMap;
        private Map<String, Collection<ScheduleTermDate>> m_termDateMap;

        /**
         * Instantiates a new TN student history helper.
         *
         * @param data TNStateReportData
         * @param firstDate PlainDate
         * @param lastDate PlainDate
         * @param firstAttendDate
         */
        TNStudentHistoryHelper(TNStateReportData data, PlainDate firstDate, PlainDate lastDate,
                PlainDate firstAttendDate) {
            super(data);
            m_data = data;
            m_firstDate = firstDate;
            m_firstAttendDate = firstAttendDate;
            m_lastDate = lastDate;

            m_fieldExcludeStd = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
            m_fieldExcludeCrs = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
            m_fieldExcludeMst = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_MST, false);
            m_fieldExcludeScc = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_SCC, false);
        }

        /**
         * Returns a set of days-in-session for the given school and calendar ID combination.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set of PlainDate objects
         */
        @Override
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            Map<String, Set<PlainDate>> calendarData = null;
            Set<PlainDate> calendarDates = null;
            Schedule schedule = null;
            if (school != null && !m_schoolsToCalendars.containsKey(school.getOid())) {
                PlainDate startDate = null;
                PlainDate endDate = null;
                if (school.getActiveSchedule() != null) {
                    schedule = school.getActiveSchedule();
                }
                if (schedule == null
                        || !schedule.getDistrictContextOid().equals(getData().getCurrentContext().getOid())) {
                    Collection<SchoolScheduleContext> schoolContexts =
                            school.getSchoolScheduleContexts(getData().getBroker());
                    for (SchoolScheduleContext schoolContext : schoolContexts) {
                        if (schoolContext.getDistrictContextOid().equals(getData().getCurrentContext().getOid())) {
                            schedule = schoolContext.getActiveSchedule();
                            break;
                        }
                    }
                }
                if (schedule != null) {
                    startDate = schedule.getStartDate();
                    endDate = schedule.getEndDate();
                } else {
                    startDate = getData().getCurrentContext().getStartDate();
                    endDate = getData().getCurrentContext().getEndDate();
                }
                calendarData = getEnrollmentManager().getCalendarLookup(school, startDate, endDate,
                        getData().getCurrentContext().getOid());
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            }

            if (school != null) {
                calendarData = m_schoolsToCalendars.get(school.getOid());
                // Get any calendar after checking the calendars map is not empty
                if (CALENDAR_ANY.equals(calendar) && !calendarData.isEmpty()) {
                    calendarDates = calendarData.values().iterator().next();
                } else {
                    calendarDates = calendarData.get(calendar);
                }
            }
            return calendarDates;
        }

        /**
         * Gets the first date.
         *
         * @return Plain date
         */
        public PlainDate getFirstAttendDate() {
            return m_firstAttendDate;
        }

        public PlainDate getFirstDate() {
            return m_firstDate;
        }

        /**
         * Returns reference code of grade based on dates.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return Reference code
         */
        public ReferenceCode getGradeLevelByDates(SisStudent student, PlainDate startDate, PlainDate endDate) {
            if (m_referenceGradeCodeMap == null) {
                m_referenceGradeCodeMap = loadRefCodeMapByField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
            }

            int yog = getYog(student, startDate, endDate);

            ReferenceCode gradeCode = null;

            ModelBroker broker = new ModelBroker(m_data.getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(m_data.getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    m_data.getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
            return gradeCode;
        }

        /**
         * Gets the organization.
         *
         * @return Organization
         */
        public Organization getOrganization() {
            return m_data.getOrganization();
        }

        /**
         * Gets the last date.
         *
         * @return Plain date
         */
        public PlainDate getLastDate() {
            return m_lastDate;
        }

        /**
         * Gets the student schedule change criteria.
         *
         * @return X 2 criteria
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentScheduleChangeCriteria()
         */
        @Override
        public X2Criteria getStudentScheduleChangeCriteria() {
            // This override is necessary to remove the MASTER_TYPE_CLASS criteria
            if (m_studentScheduleChangeCriteria == null) {
                m_studentScheduleChangeCriteria = buildTNStudentScheduleChangeCriteria();
            }
            return m_studentScheduleChangeCriteria;
        }

        /**
         * Gets the student schedule criteria.
         *
         * @return X 2 criteria
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentScheduleCriteria()
         */
        @Override
        public X2Criteria getStudentScheduleCriteria() {
            // This override is necessary to remove the MASTER_TYPE_CLASS criteria
            if (m_studentScheduleCriteria == null) {
                m_studentScheduleCriteria = buildTNStudentScheduleCriteria();
            }
            return m_studentScheduleCriteria;
        }

        /**
         * Create a list of schedule spans that account for the override dates.
         *
         * @param student SisStudent
         * @return List
         */
        public List<TNStudentScheduleSpan> getTNStudentScheduleSpans(SisStudent student) {
            Map<String, TNStudentScheduleSpan> scheduleSpanMap = new HashMap<String, TNStudentScheduleSpan>();

            List<StudentSchedule> schedules = getTNStudentSchedules(student.getOid());
            List<StudentScheduleChange> changes = null;

            changes = getStudentScheduleChanges(student.getOid());

            if (schedules != null) {
                for (StudentSchedule schedule : schedules) {
                    TNStudentScheduleSpan info = new TNStudentScheduleSpan(schedule.getSection());
                    info.setSchedule(schedule);
                    scheduleSpanMap.put(schedule.getSectionOid(), info);
                }
            }

            if (changes != null) {
                checkScheduleChanges(changes, scheduleSpanMap);
            }

            // Fill in any empty entry/exit dates with term dates for the section term.
            fillTermDates(scheduleSpanMap);

            List<TNStudentScheduleSpan> scheduleSpanList =
                    new ArrayList<TNStudentScheduleSpan>(scheduleSpanMap.values());
            return scheduleSpanList;
        }

        /**
         * Returns a list of student enrollment spans that represent all of the
         * students enrollment activity and segments.
         * <p>
         * Spans are identified by one or more E enrollment records followed
         * by one or more consecutive W enrollment records and any records in between.
         * There will typically be only one E and one W record, however messy data may provide more.
         * <p>
         * If the properties PROPERTY_SPAN_BREAK_ON_YOG or PROPERTY_SPAN_BREAK_ON_STATUS
         * are set to TRUE, the presence of these enrollment records will also cause a break.
         * The STATUS or YOG record that caused the break will be included in BOTH enrollment spans
         * as the appropriate end or begin enrollment record. This may make looking up entry
         * enrollment
         * record values such as entry code or override school difficult if those values are not
         * also
         * in the YOG or STATUS record.
         *
         * @param student Student
         * @param limit - Limit spans to only those that overlap the reporting
         *        date range.
         * @return List<StudentEnrollmentSpan>
         */
        public List<TNStudentEnrollmentSpan> getTNStudentEnrollmentSpans(Student student, boolean limit) {
            PlainDate beginDate = (PlainDate) getTNStudentSelectionProperty(PROPERTY_BEGIN_DATE, PlainDate.class,
                    m_data.getCurrentContext().getStartDate());
            PlainDate endDate =
                    (PlainDate) getTNStudentSelectionProperty(PROPERTY_END_DATE, PlainDate.class, m_currentDate);
            if (endDate.before(m_data.getCurrentContext().getStartDate())) {
                endDate = m_data.getCurrentContext().getEndDate();
            }
            PlainDate waDate =
                    (PlainDate) getTNStudentSelectionProperty(PROPERTY_WITHDRAWN_AFTER_DATE, PlainDate.class, null);
            boolean yogBreak =
                    ((Boolean) getTNStudentSelectionProperty(PROPERTY_SPAN_BREAK_ON_YOG, Boolean.class, Boolean.FALSE))
                            .booleanValue();
            boolean statusBreak = ((Boolean) getTNStudentSelectionProperty(PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.class,
                    Boolean.FALSE)).booleanValue();
            if (waDate != null) {
                beginDate = waDate;
            }

            // Get the list of student enrollment records.
            List<StudentEnrollment> enrollments = getStudentEnrollments(student.getOid());

            // Determine starting status (current status). This should be based on the latest
            // enrollment record if possible, or the student otherwise.
            // isOpen indicates the current enrollment status is active.
            String enrollStatus = null;
            if (enrollments != null && enrollments.size() > 0) {
                StudentEnrollment currentEnrollment = enrollments.iterator().next();
                enrollStatus = currentEnrollment.getStatusCode();
            }
            if (StringUtils.isEmpty(enrollStatus)) {
                enrollStatus = (String) getStudentValueByBeanPath(student, SisStudent.COL_ENROLLMENT_STATUS);
            }
            // Current status of student.
            boolean isActive = m_preferenceStudentActiveStatuses.contains(enrollStatus);
            // If the enrollment span has a non-withdrawal record in it so far.
            // Used to determine if another withdrawal signifies a break in the span.
            boolean hasNonWithdrawals = isActive;

            // Work through enrollment records going backward in time and build spans.
            // This will build all spans, regardless of the setting of limit.
            List<TNStudentEnrollmentSpan> enrollmentSpans = new ArrayList<TNStudentEnrollmentSpan>();

            List<StudentEnrollment> currentEnrollments = new ArrayList<StudentEnrollment>();
            if (enrollments != null) {
                /*
                 * Since we need to look ahead in the list of enrollments to combine Y and E records
                 * on the same date,
                 * we need to create an array list and navigate using an index.
                 */
                enrollments = new ArrayList(enrollments);
                for (int index = 0; index < enrollments.size(); ++index) {
                    StudentEnrollment enrollment = enrollments.get(index);
                    if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                        // Only report a YOG as a break if the student is active or there are other
                        // records in the span already. Not for inactive students between spans.
                        isActive = m_preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                        if (yogBreak && (isActive || currentEnrollments.size() > 0)) {
                            // Complete the previous span. Start a new one.
                            currentEnrollments.add(0, enrollment);
                            enrollmentSpans.add(0, new TNStudentEnrollmentSpan(currentEnrollments, this));
                            /*
                             * check to see if next span should be combined. This will occur if the
                             * next span is an E
                             * record on the same date at the same school.
                             */
                            if (index + 1 < enrollments.size()) {
                                StudentEnrollment nextEnrollment = enrollments.get(index + 1);
                                if (yogEliminate(enrollment, nextEnrollment)) {
                                    // remove this span - the YOG_CHANGE will be included in the
                                    // next span
                                    enrollmentSpans.remove(0);
                                    currentEnrollments.remove(0);
                                } else {
                                    currentEnrollments = new ArrayList<StudentEnrollment>();
                                }
                            } else {
                                currentEnrollments = new ArrayList<StudentEnrollment>();
                            }
                        }
                        currentEnrollments.add(0, enrollment);
                        hasNonWithdrawals = true;
                    } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                        // Only report a STATUS as a break if the student is active or there are
                        // other
                        // records in the span already. Not for inactive students between spans.
                        isActive = m_preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                        if (statusBreak && (isActive || currentEnrollments.size() > 0)) {
                            // Complete the previous span. Start a new one.
                            currentEnrollments.add(0, enrollment);
                            enrollmentSpans.add(0, new TNStudentEnrollmentSpan(currentEnrollments, this));
                            currentEnrollments = new ArrayList<StudentEnrollment>();
                        }
                        currentEnrollments.add(0, enrollment);
                        hasNonWithdrawals = true;
                    } else if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                        if (hasNonWithdrawals && !currentEnrollments.isEmpty()) {
                            enrollmentSpans.add(0, new TNStudentEnrollmentSpan(currentEnrollments, this));
                            currentEnrollments = new ArrayList<StudentEnrollment>();
                        }

                        isActive = m_preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                        currentEnrollments.add(0, enrollment);
                        hasNonWithdrawals = false;
                    } else if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                        currentEnrollments.add(0, enrollment);
                        isActive = m_preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                        hasNonWithdrawals = true;
                    }
                }
                if (hasNonWithdrawals && !currentEnrollments.isEmpty()) {
                    enrollmentSpans.add(0, new TNStudentEnrollmentSpan(currentEnrollments, this));
                    currentEnrollments = new ArrayList<StudentEnrollment>();
                }
            }

            // remove spans without an active interval
            Iterator<TNStudentEnrollmentSpan> iterator = enrollmentSpans.iterator();
            while (iterator.hasNext()) {
                TNStudentEnrollmentSpan span = iterator.next();
                if (span.getFirstActiveEnrollment() == null) {
                    iterator.remove();
                }
            }

            // If limit is set, go back through spans and remove any that are out of date range.
            if (limit) {
                iterator = enrollmentSpans.iterator();
                while (iterator.hasNext()) {
                    TNStudentEnrollmentSpan span = iterator.next();
                    if (span.getFirstActiveDate() == null) {
                        iterator.remove();
                        continue;
                    }
                    if (span.getFirstActiveDate() != null
                            && m_data.getCurrentContext().getEndDate().before(span.getFirstActiveDate())) {
                        // Include any enrollment span that starts before context end date
                        iterator.remove();
                        continue;
                    }
                    if (span.getLastActiveDate() != null && beginDate.after(span.getLastActiveDate())) {
                        iterator.remove();
                        continue;
                    }
                    if (MODE_STUDENT_ACTIVE_SNAPSHOT.equals(getStudentSelectionMode())) {
                        if (span.getLastActiveDate() != null && endDate.after(span.getLastActiveDate())) {
                            iterator.remove();
                            continue;
                        }
                    }
                }
            }

            return enrollmentSpans;
        }

        /**
         * Sets the selection property.
         *
         * @param key String
         * @param property Object
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#setSelectionProperty(java.lang.String,
         *      java.lang.Object)
         */
        @Override
        public void setSelectionProperty(String key, Object property) {
            if (key == StudentHistoryHelper.PROPERTY_END_DATE && property != null && property instanceof Date) {
                Date date = (Date) property;
                if (m_firstDate != null && date.before(m_firstDate)) {
                    property = m_firstDate;
                }
            }
            super.setSelectionProperty(key, property);
        }

        /**
         * Gets the schedule change date.
         *
         * @param change StudentScheduleChange
         * @return Plain date
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getScheduleChangeDate(com.x2dev.sis.model.beans.StudentScheduleChange)
         */
        @Override
        protected PlainDate getScheduleChangeDate(StudentScheduleChange change) {
            PlainDate date = change.getDate();
            PlainDate overrideDate = m_data.getOverrideDate(change);

            if (overrideDate != null) {
                date = overrideDate;
            }

            return date;
        }

        /**
         * Return the user value entered for a parameter. Verify that the value matches a specified
         * type.
         *
         * @param selectKey The key of the property to retrieve.
         * @param expectedClass The class type expected for the value.
         * @param defaultValue The value to return if the property is not present or is null.
         *
         * @return Object
         */
        Object getTNStudentSelectionProperty(String selectKey, Class expectedClass, Object defaultValue) {
            Object value = getSelectionProperty(selectKey);
            if (value != null) {
                if (!expectedClass.isInstance(value)) {
                    throw new ClassCastException("getTNStudentSelectionProperty(" + selectKey + "): Expected "
                            + expectedClass.getName() + ", found " + value.getClass().getName());
                }
            } else {
                value = defaultValue;
            }
            return value;
        }

        /**
         * Construct an initial criteria for loading student schedule change records.
         * This should match the student criteria used for the student query as closely as possible.
         *
         * @return X2Criteria
         */
        private X2Criteria buildTNStudentScheduleChangeCriteria() {
            // Verify that we have enough information to start.
            if (getStudentSelectionMode() == null) {
                throw new X2RuntimeException();
            }

            // Identify parameters for student schedule selection.
            PlainDate endDate =
                    (PlainDate) getTNStudentSelectionProperty(PROPERTY_END_DATE, PlainDate.class, m_currentDate);
            Boolean applyExclude =
                    (Boolean) getTNStudentSelectionProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
            Boolean excludeFutureSched = (Boolean) getTNStudentSelectionProperty(PROPERTY_EXCLUDE_FUTURE_SCHEDULES,
                    Boolean.class, Boolean.FALSE);
            Boolean applyInput =
                    (Boolean) getTNStudentSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
            Boolean applySchool =
                    (Boolean) getTNStudentSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);

            X2Criteria studentScheduleChangeCriteria = new X2Criteria();

            // Must include state course code
            studentScheduleChangeCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER + m_data.m_fieldStateCourseCode,
                    m_data.getBroker().getPersistenceKey());
            if (m_fieldExcludeScc != null) {
                studentScheduleChangeCriteria.addNotEqualTo(m_fieldExcludeScc, BooleanAsStringConverter.TRUE);
            }
            // From active Schedule
            // All queries using REL_ACTIVE_SCHOOL_SCHED can only be used when the selected context
            // is the current context.
            // If this is not the case, Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS must be used.
            if (m_data.m_contextOid.equals(getOrganization().getCurrentContextOid())) {
                studentScheduleChangeCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        StudentScheduleChange.COL_SCHEDULE_OID);
            } else {
                studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        m_data.m_contextOid);
            }


            // section term started before report date.
            // Require section term to start before end/report date.
            if (excludeFutureSched.booleanValue()) {
                studentScheduleChangeCriteria
                        .addLessOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                                ScheduleTermDate.COL_START_DATE, endDate);
            }

            // check school or organization selection.
            if (applySchool.booleanValue() && m_data.isSchoolContext()) {
                studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, m_data.getSchool().getOid());
            } else {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            // Check exclusion flags for student, section and student schedule.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldExcludeStd)) {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        m_fieldExcludeStd,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the section exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldExcludeMst)) {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        m_fieldExcludeMst,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the course exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldExcludeCrs)) {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        m_fieldExcludeCrs,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the user criteria needs to be applied.
            if (applyInput.booleanValue()) {
                m_data.applyInputCriteria(studentScheduleChangeCriteria, false, StudentScheduleChange.REL_STUDENT);
            }

            return studentScheduleChangeCriteria;
        }

        /**
         * Construct an initial criteria for loading student schedule records.
         * This should match the student criteria used for the student query as closely as possible.
         *
         * @return X2Criteria
         */
        private X2Criteria buildTNStudentScheduleCriteria() {
            // Verify that we have enough information to start.
            if (getStudentSelectionMode() == null) {
                throw new X2RuntimeException();
            }

            // Identify parameters for student schedule selection.
            PlainDate endDate =
                    (PlainDate) getTNStudentSelectionProperty(PROPERTY_END_DATE, PlainDate.class, m_currentDate);
            Boolean applyExclude =
                    (Boolean) getTNStudentSelectionProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
            Boolean excludeFutureSched = (Boolean) getTNStudentSelectionProperty(PROPERTY_EXCLUDE_FUTURE_SCHEDULES,
                    Boolean.class, Boolean.FALSE);
            Boolean applyInput =
                    (Boolean) getTNStudentSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
            Boolean applySchool =
                    (Boolean) getTNStudentSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);

            X2Criteria studentScheduleCriteria = new X2Criteria();

            // Must include state course code
            studentScheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER + m_data.m_fieldStateCourseCode,
                    m_data.getBroker().getPersistenceKey());
            // From active Schedule
            // All queries using REL_ACTIVE_SCHOOL_SCHED can only be used when the selected context
            // is the current context.
            // If this is not the case, Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS must be used.
            if (m_data.m_contextOid.equals(getOrganization().getCurrentContextOid())) {
                studentScheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        StudentSchedule.COL_SCHEDULE_OID);
            } else {
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        m_data.m_contextOid);
            }


            // Require section term to start before end/report date.
            if (excludeFutureSched.booleanValue()) {
                studentScheduleCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                        ScheduleTermDate.COL_START_DATE, endDate);
            }

            // check school or organization selection.
            if (applySchool.booleanValue() && m_data.isSchoolContext()) {
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, m_data.getSchool().getOid());
            } else {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            // Check exclusion flags for student, section and student schedule.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldExcludeStd)) {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        m_fieldExcludeStd,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the section exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldExcludeMst)) {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        m_fieldExcludeMst,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the course exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldExcludeCrs)) {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        m_fieldExcludeCrs,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the user criteria needs to be applied.
            if (applyInput.booleanValue()) {
                m_data.applyInputCriteria(studentScheduleCriteria, false, StudentSchedule.REL_STUDENT);
            }

            return studentScheduleCriteria;
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
         * @param scheduleChanges List<StudentScheduleChange>
         * @param scheduleSpanMap Map<String,TNStudentScheduleSpan>
         */
        private void checkScheduleChanges(List<StudentScheduleChange> scheduleChanges,
                                          Map<String, TNStudentScheduleSpan> scheduleSpanMap) {
            MasterSchedule lastSection = null;
            StudentScheduleChange lastChange = null;
            PlainDate termStart = null;
            PlainDate termEnd = null;

            /*
             * Work backward in time through schedule changes.
             * DROP will open a new section and the ADD before it will finish that section.
             * A DROP without a following ADD will be considered open at start of term.
             * Any activity entirely before start of term will be ignored.
             */
            for (StudentScheduleChange change : scheduleChanges) {
                if (change.getMasterSchedule() != null) {
                    // Check for a new section.

                    if (lastSection == null || !lastSection.getOid().equals(change.getMasterScheduleOid())) {
                        // Save the working section if necessary.
                        if (lastChange != null) {
                            // The last change record for this section (in reverse chronological
                            // order)
                            // was a drop. Assume the section was scheduled from the beginning of
                            // the term/year.
                            TNStudentScheduleSpan info = new TNStudentScheduleSpan(lastSection);
                            info.setEntryDate(termStart);
                            if (getScheduleChangeDate(lastChange).after(termEnd)) {
                                info.setExitDate(termEnd);
                            } else {
                                info.setExitDate(getScheduleChangeDate(lastChange));
                            }
                            info.setExitChange(lastChange);
                            // Avoid recording sections scheduled out entirely
                            // before the start of it's term. This is just scheduling activity.
                            if (!info.getExitDate().before(termStart)) {
                                scheduleSpanMap.put(lastChange.getOid(), info);
                            }
                        }

                        // Initialize the new section
                        lastChange = null;
                        lastSection = change.getMasterSchedule();
                        termStart = null;
                        termEnd = null;
                        Collection<ScheduleTermDate> termDates = getTermDates(lastSection.getScheduleTermOid());
                        for (ScheduleTermDate termDate : termDates) {
                            if (termStart == null || termStart.after(termDate.getStartDate())) {
                                termStart = termDate.getStartDate();
                            }
                            if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                                termEnd = termDate.getEndDate();
                            }
                        }
                        // If a term is missing any dates, use school schedule dates or district
                        // calendar dates.
                        if (termStart == null) {
                            termStart = lastSection.getSchedule().getStartDate();
                            if (termStart == null) {
                                termStart = m_data.getCurrentContext().getStartDate();
                            }
                        }
                        if (termEnd == null) {
                            termEnd = lastSection.getSchedule().getEndDate();
                            if (termEnd == null) {
                                termEnd = m_data.getCurrentContext().getEndDate();
                            }
                        }
                    }

                    // For a section, see if its dates compare with report dates or term dates.
                    if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                        lastChange = change;
                    } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                        if (lastChange == null) {
                            // No previous record, assume current student schedule. Find based on
                            // master OID.
                            TNStudentScheduleSpan info = scheduleSpanMap.get(change.getMasterScheduleOid());
                            if (info != null) {
                                info.setEntryDate(getScheduleChangeDate(change));
                                info.setEntryChange(change);
                                if (info.getEntryDate().before(termStart)) {
                                    info.setEntryDate(termStart);
                                }
                            }
                        } else {
                            TNStudentScheduleSpan info = new TNStudentScheduleSpan(change.getMasterSchedule());
                            info.setEntryDate(getScheduleChangeDate(change));
                            info.setEntryChange(change);
                            if (info.getEntryDate().before(termStart)) {
                                info.setEntryDate(termStart);
                            }
                            info.setExitDate(getScheduleChangeDate(lastChange));
                            // Avoid entering a change date that is after the term end date
                            if (info.getExitDate().after(termEnd)) {
                                info.setExitDate(termEnd);
                            }
                            info.setExitChange(lastChange);
                            // Avoid recording sections scheduled out entirely
                            // before the start of it's term. This is just scheduling activity.
                            if (!info.getExitDate().before(termStart)) {
                                scheduleSpanMap.put(change.getOid(), info);
                            }
                        }
                        lastChange = null;
                    }
                }
            }
            if (lastChange != null) {
                // The last change record for this section (in reverse chronological order)
                // was a drop. Assume the section was scheduled from the beginning of the term/year.
                TNStudentScheduleSpan info = new TNStudentScheduleSpan(lastSection);
                info.setEntryDate(termStart);
                if (getScheduleChangeDate(lastChange).after(termEnd)) {
                    info.setExitDate(termEnd);
                } else {
                    info.setExitDate(getScheduleChangeDate(lastChange));
                }
                info.setExitChange(lastChange);
                // Avoid recording sections scheduled out entirely
                // before the start of it's term. This is just scheduling activity.
                if (!info.getExitDate().before(termStart)) {
                    scheduleSpanMap.put(lastChange.getOid(), info);
                }
            }
        }

        /**
         * For all populated sections in the schedule span map, if the entry date or exit date is
         * missing, populate with term dates.
         *
         * @param scheduleSpanMap Map<String,TNStudentScheduleSpan>
         */
        private void fillTermDates(Map<String, TNStudentScheduleSpan> scheduleSpanMap) {
            PlainDate termStart = null;
            PlainDate termEnd = null;

            Iterator<TNStudentScheduleSpan> iterator = scheduleSpanMap.values().iterator();
            while (iterator.hasNext()) {
                TNStudentScheduleSpan info = iterator.next();
                if ((info.getEntryDate() == null || info.getExitDate() == null) && info.getSection() != null) {
                    termStart = null;
                    termEnd = null;
                    Collection<ScheduleTermDate> termDates = getTermDates(info.getSection().getScheduleTermOid());
                    for (ScheduleTermDate termDate : termDates) {
                        if (termStart == null || termStart.after(termDate.getStartDate())) {
                            termStart = termDate.getStartDate();
                        }
                        if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                            termEnd = termDate.getEndDate();
                        }
                    }

                    if (info.getEntryDate() == null) {
                        info.setEntryDate(termStart);
                    }
                    if (info.getExitDate() == null) {
                        info.setExitDate(termEnd);
                    }
                }

                /*
                 * If the entry/exit dates are out of order, remove the info.
                 * This can be caused by drop/re-add after the end of term.
                 * The original entry will exist before the drop, so this record is extra.
                 */
                if (info.getExitDate() != null &&
                        info.getEntryDate() != null &&
                        info.getExitDate().before(info.getEntryDate())) {
                    iterator.remove();
                }
            }
        }

        /**
         * Get collection of program codes corresponding to given state reference code.
         *
         * @return Collection of program codes
         */

        // preserve in case need to reverse change in getStudentEnrollments()
        @SuppressWarnings("unused")
        private Collection<String> getEnrollmentCodes() {
            X2Criteria criteria = new X2Criteria();
            DataDictionaryField field = m_data.getDataDictionaryField(StudentEnrollment.class,
                    StudentEnrollment.COL_ENROLLMENT_CODE);

            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, m_data.getBroker().getPersistenceKey());

            String[] columns = new String[] {ReferenceCode.COL_CODE};

            ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

            List<String> result = new ArrayList<String>();
            ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] record = (Object[]) iterator.next();
                    String code = (String) record[0];
                    result.add(code);
                }
            } finally {
                iterator.close();
            }
            return result;
        }

        /**
         * Returns the map of student enrollment records for a student.
         * The order of enrollment records in the list is descending, newest to oldest.
         *
         * @param studentOid String
         * @return List<StudentEnrollment>
         */
        private List<StudentEnrollment> getStudentEnrollments(String studentOid) {
            if (m_studentEnrollmentMap == null) {
                SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, getStudentCriteria());

                X2Criteria studentEnrollmentCriteria = new X2Criteria();
                studentEnrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
                studentEnrollmentCriteria.addNotNull(StudentEnrollment.COL_ENROLLMENT_DATE);

                // include state only enrollment codes plus any YOG change codes and status change
                // codes

                /*
                 * Removed restriction limiting entry and withdrawal records to require state codes
                 * based on discussion with Denise 2015-12-10
                 * X2Criteria criteria1 = new X2Criteria();
                 * criteria1.addIn(StudentEnrollment.COL_ENROLLMENT_CODE, getEnrollmentCodes());
                 *
                 * X2Criteria criteria2 = new X2Criteria();
                 * criteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE,
                 * StudentEnrollment.YOG_CHANGE);
                 *
                 * X2Criteria criteria3 = new X2Criteria();
                 * criteria3.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE,
                 * StudentEnrollment.STATUS_CHANGE);
                 *
                 * X2Criteria criteriaOr = new X2Criteria();
                 * criteriaOr.addOrCriteria(criteria1);
                 * criteriaOr.addOrCriteria(criteria2);
                 * criteriaOr.addOrCriteria(criteria3);
                 *
                 * studentEnrollmentCriteria.addAndCriteria(criteriaOr);
                 */

                QueryByCriteria studentEnrollmentQuery =
                        new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_TYPE, true);
                m_studentEnrollmentMap = m_data.getBroker().getGroupedCollectionByQuery(studentEnrollmentQuery,
                        StudentEnrollment.COL_STUDENT_OID, 500);
            }

            return m_studentEnrollmentMap.get(studentOid);
        }

        /**
         * Return a list of student schedule change records for the requested student.
         * The order of schedule change records in the list is descending, newest to oldest.
         *
         * @param studentOid String
         * @return List<StudentScheduleChange>
         */
        private List<StudentScheduleChange> getStudentScheduleChanges(String studentOid) {
            if (m_studentScheduleChangeMap == null) {
                QueryByCriteria sccQuery;
                Boolean applyStudentCriteria = Boolean.FALSE;
                if (getSelectionProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION) != null
                        && getSelectionProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION) instanceof Boolean) {
                    applyStudentCriteria =
                            (Boolean) getSelectionProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION);
                }
                X2Criteria criteria = getStudentScheduleChangeCriteria();
                if (applyStudentCriteria.booleanValue() && isMaterialized()) {
                    sccQuery = getStudentSelectionQuery(StudentScheduleChange.class, criteria,
                            StudentScheduleChange.COL_STUDENT_OID);
                } else {
                    sccQuery = new QueryByCriteria(StudentScheduleChange.class, criteria);
                }
                sccQuery.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
                sccQuery.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
                sccQuery.addOrderBy(StudentScheduleChange.COL_DATE, false);
                sccQuery.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, false);
                m_studentScheduleChangeMap = getData().getBroker().getGroupedCollectionByQuery(sccQuery,
                        StudentScheduleChange.COL_STUDENT_OID, 500);
            }
            List<StudentScheduleChange> schedules = m_studentScheduleChangeMap.get(studentOid);
            if (schedules != null && !schedules.isEmpty()) {
                Collections.sort(schedules, new Comparator<StudentScheduleChange>() {

                    @Override
                    public int compare(StudentScheduleChange o1, StudentScheduleChange o2) {
                        int result = o1.getStudentOid().compareTo(o2.getStudentOid());
                        if (result == 0) {
                            result = o1.getMasterScheduleOid().compareTo(o2.getMasterScheduleOid());
                        }
                        if (result == 0) {
                            PlainDate date1 = getScheduleChangeDate(o1);
                            PlainDate date2 = getScheduleChangeDate(o2);
                            // Reverse order
                            result = date2.compareTo(date1);
                        }
                        if (result == 0) {
                            PlainDate date1 = o1.getDate();
                            PlainDate date2 = o2.getDate();
                            // Reverse order
                            result = date2.compareTo(date1);
                        }
                        if (result == 0) {
                            // Reverse order
                            result = Long.valueOf(o2.getLastModifiedTime())
                                    .compareTo(Long.valueOf(o1.getLastModifiedTime()));
                        }
                        return result;
                    }
                });

                // We need to remove schedules where an ADD and DROP occur for the same schedule on
                // the same day
                List<StudentScheduleChange> removedSchedules = new LinkedList();
                StudentScheduleChange previousSchedule = null;
                for (StudentScheduleChange schedule : schedules) {
                    if (previousSchedule != null &&
                            previousSchedule.getMasterScheduleOid().equals(schedule.getMasterScheduleOid()) &&
                            getScheduleChangeDate(previousSchedule).equals(getScheduleChangeDate(schedule)) &&
                            ((StudentScheduleChange.CODE_ADD.equals(previousSchedule.getChangeTypeCode()) &&
                                    StudentScheduleChange.CODE_DROP.equals(schedule.getChangeTypeCode())) ||
                                    (StudentScheduleChange.CODE_DROP.equals(previousSchedule.getChangeTypeCode()) &&
                                            StudentScheduleChange.CODE_ADD.equals(schedule.getChangeTypeCode())))) {
                        removedSchedules.add(previousSchedule);
                        removedSchedules.add(schedule);
                        previousSchedule = schedule;
                    } else {
                        previousSchedule = schedule;
                    }
                }
                schedules.removeAll(removedSchedules);
            }

            return schedules;
        }

        /**
         * Load the schedule term dates for a schedule term oid.
         * Keep a map of existing codes for lookup.
         *
         * @param scheduleTermOid String
         * @return Collection<ScheduleTermDate>
         */
        private Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
            Collection<ScheduleTermDate> dates = null;

            if (m_termDateMap == null) {
                m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
            }

            if (!m_termDateMap.containsKey(scheduleTermOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
                QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
                dates = m_data.getBroker().getCollectionByQuery(query);
                m_termDateMap.put(scheduleTermOid, dates);
            }

            return m_termDateMap.get(scheduleTermOid);
        }

        /**
         * Return a list of student schedules for the requrested student.
         *
         * @param studentOid String
         * @return List<StudentSchedule>
         */
        private List<StudentSchedule> getTNStudentSchedules(String studentOid) {
            if (m_studentScheduleMap == null) {
                Boolean applyStudentCriteria = Boolean.FALSE;
                if (getSelectionProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION) != null
                        && getSelectionProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION) instanceof Boolean) {
                    applyStudentCriteria =
                            (Boolean) getSelectionProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION);
                }
                QueryByCriteria scheduleQuery;
                X2Criteria scheduleCriteria = getStudentScheduleCriteria();
                if (applyStudentCriteria.booleanValue() && isMaterialized()) {
                    scheduleQuery = getStudentSelectionQuery(StudentSchedule.class, scheduleCriteria,
                            StudentSchedule.COL_STUDENT_OID);
                } else {
                    scheduleQuery = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
                }
                scheduleQuery.addOrderBy(StudentSchedule.COL_STUDENT_OID, true);
                scheduleQuery.addOrderBy(StudentSchedule.COL_SECTION_OID, true);
                m_studentScheduleMap = getData().getBroker().getGroupedCollectionByQuery(scheduleQuery,
                        StudentSchedule.COL_STUDENT_OID, 500);
            }

            return m_studentScheduleMap.get(studentOid);
        }

        /**
         * Return student's yog considering context and spans.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return int
         */
        private int getYog(SisStudent student, PlainDate startDate, PlainDate endDate) {
            int yog = 0;
            // use student's attributes first (but only for current context)
            StudentContextAttributes attributes =
                    (StudentContextAttributes) getStudentMultiYearHelper().getContextAttributes(student.getOid());
            if (attributes != null && attributes.getContextOid().equals(m_data.getCurrentContext().getOid())) {
                yog = ((Integer) getStudentValueByBeanPath(student, SisStudent.COL_YOG)).intValue();
            }

            // then, if there are no attributes for current context or the yog of it == 0, use
            // span's yog
            if (yog == 0) {
                List<TNStudentEnrollmentSpan> spans = getTNStudentEnrollmentSpans(student, true);

                Collections.sort(spans, new Comparator<TNStudentEnrollmentSpan>() {
                    @Override
                    public int compare(TNStudentEnrollmentSpan o1, TNStudentEnrollmentSpan o2) {
                        return o1.getFirstActiveEnrollment().getEnrollmentDate()
                                .compareTo(o2.getFirstActiveEnrollment().getEnrollmentDate());
                    }
                });

                for (TNStudentEnrollmentSpan span : spans) {
                    if (!span.getFirstActiveDate().after(endDate) &&
                            (span.getLastActiveDate() == null || !span.getLastActiveDate().before(startDate))) {
                        // if span has no withdrawal, and contains Y record, use last Y record to
                        // determine YOG
                        if (span.getFirstInactiveEnrollment() == null) {
                            StudentEnrollment lastYEnrollment = getEnrollmentForDate(student.getOid(), endDate, "Y");

                            if (lastYEnrollment != null &&
                                    lastYEnrollment.getEnrollmentDate()
                                            .after(span.getFirstActiveEnrollment().getEnrollmentDate())) {
                                yog = lastYEnrollment.getYog();
                            }
                        }
                        if (yog == 0) {
                            yog = span.getYog();
                        }
                    }
                }
            }

            // and only then, if yog still is 0, use yog of student
            if (yog == 0) {
                yog = student.getYog();
            }

            return yog;
        }

        /**
         * Load reference code map by field name.
         *
         * @param beanClass Class
         * @param fieldName String
         * @return Map<String, ReferenceCode>
         */
        private Map<String, ReferenceCode> loadRefCodeMapByField(Class beanClass, String fieldName) {
            Map<String, ReferenceCode> refCodeMap = null;
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_data.getBroker().getPersistenceKey());
            ModelProperty prop = new ModelProperty(beanClass, fieldName, m_data.getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                refCodeMap = referenceTable.getCodeMap();
            }
            return refCodeMap;
        }

    }

    /**
     * Helper class for student to get correct data that dependent from school year.
     *
     * @author Follett Software Company
     */
    public static class TNStudentMultiYearHelper extends TNMultiYearHelper {

        /**
         * Handler of passed criteria, can return Criteria that should be used as alternative of
         * passed criteria to
         * consider overridden school year context and Blob Information of Context Attributes etc.
         *
         * @author Follett Software Company
         */
        private class StudentCriteriaHandler extends CriteriaHandler {

            /**
             * Instantiates a new student criteria handler.
             *
             * @param dictionary DataDictionary
             * @param broker X2Broker
             * @param manager ContextAttributesManager
             */
            StudentCriteriaHandler(DataDictionary dictionary, X2Broker broker, ContextAttributesManager manager) {
                super(dictionary, broker, manager);
            }

            /**
             * Initializes student context attributes for all students for current context.
             */
            @Override
            protected void initializeAllAttributes() {
                X2Criteria contextsAttributesCriteria = new X2Criteria();
                contextsAttributesCriteria
                        .addGreaterOrEqualThan(StudentContextAttributes.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                                DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                                Integer.valueOf(getCurrentContext().getSchoolYear()));
                QueryByCriteria contextAttributesQuery =
                        new QueryByCriteria(StudentContextAttributes.class, contextsAttributesCriteria, false);
                String[] columns = {
                        StudentContextAttributes.REL_DISTRICT_CONTEXT + PATH_DELIMITER
                                + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        StudentContextAttributes.COL_STUDENT_OID
                };
                int[] sizes = {
                        1,
                        1
                };
                m_attributesMap = m_broker.getGroupedCollectionByQuery(contextAttributesQuery, columns, sizes);
            }
        }

        private static final String EMPTY = "";
        private static final String RELATION_STUDENT = "student";

        /**
         * Constructs a new TNStudentMultiYearHelper object.
         *
         * @param organization Organization
         * @param context DistrictSchoolYearContext
         * @param broker X2Broker
         */
        public TNStudentMultiYearHelper(Organization organization, DistrictSchoolYearContext context, X2Broker broker) {
            super(organization, context, broker);
        }

        /**
         * Returns the criteria to find active students for current context.
         *
         * @return Criteria
         */
        public Criteria getActiveStudentCriteria() {
            return getActiveStudentCriteria(null);
        }

        /**
         * Returns the criteria to find active students for current context.
         *
         * @param prefix String
         * @return Criteria
         */
        public Criteria getActiveStudentCriteria(String prefix) {
            String field = BeanManager.getFullPrefix(prefix) + getField(SisStudent.COL_ENROLLMENT_STATUS);
            Criteria criteria = StudentManager.getActiveStudentStatusCriteria(getOrganization(), field);
            criteria.addAndCriteria(getWithAttributesCriteria(prefix));
            return criteria;
        }

        /**
         * return criteria addBetween for field <code>propertyPath</code>
         * property path - can be property or relation + property.
         *
         * @param propertyPath String
         * @param value1 Object
         * @param value2 Object
         * @return Criteria
         */
        public Criteria getAddBetween(String propertyPath, Object value1, Object value2) {
            Criteria criteria = new X2Criteria();
            Pair<String, String> relPropPair = splitToRelationAndProperty(propertyPath);
            String prefix = relPropPair.getLeft();
            String property = relPropPair.getRight();
            String field = prefix + getField(property);
            criteria.addBetween(field, value1, value2);
            if (isContextOverride()) {
                criteria.addAndCriteria(getWithAttributesCriteria(prefix));
            }
            return criteria;

        }

        /**
         * Returns the bean path for the <code>stdCalendar</code> field.
         *
         * @return String
         */
        public String getCalendarCodeField() {
            return getField(SisStudent.COL_CALENDAR_CODE);
        }

        /**
         * Return criteria to select students that have calendar code for current context.
         *
         * @return X2Criteria
         */
        public X2Criteria getNotEmptyCalendarCriteria() {
            X2Criteria criteria = new X2Criteria();
            criteria.addAndCriteria(getWithAttributesCriteria(null));
            criteria.addNotEmpty(BeanManager.getFullPrefix(null) + getCalendarCodeField(),
                    getBroker().getPersistenceKey());
            return criteria;
        }

        /**
         * Return criteria to select students that have calendar code for current context.
         *
         * @param prefix String
         * @return X2Criteria
         */
        public X2Criteria getNotEmptyCalendarCriteria(String prefix) {
            X2Criteria criteria = new X2Criteria();
            if (isContextOverride()) {
                criteria.addAndCriteria(getWithAttributesCriteria(prefix));
            }
            criteria.addNotEmpty(BeanManager.getFullPrefix(prefix) + getCalendarCodeField(),
                    getBroker().getPersistenceKey());
            return criteria;
        }

        /**
         * Returns the school.
         *
         * @param student SisStudent
         * @return SisSchool
         */
        public SisSchool getSchool(SisStudent student) {
            SisSchool school = student.getSchool();

            if (isContextOverride()) {
                StudentContextAttributes attribute = (StudentContextAttributes) getContextAttributes(student.getOid());
                if (attribute != null) {
                    school = attribute.getSchool();
                }
            }

            return school;
        }

        /**
         * Returns the bean path for the <code>stdSklOID</code> field.
         *
         * @return String
         */
        public String getSchoolOidField() {
            return getField(SisStudent.COL_SCHOOL_OID);
        }

        /**
         * Return criteria to select students that have <code>StudentContextAttributes</code> for
         * current context if
         * context is overridden.
         *
         * @return X2Criteria
         */
        @Override
        public X2Criteria getWithAttributesCriteria() {
            return getWithAttributesCriteria(null);
        }

        /**
         * Return criteria to select students that have <code>StudentContextAttributes</code> for
         * current context if
         * context is overridden.
         *
         * @param prefix String
         * @return X2Criteria
         */
        @Override
        public X2Criteria getWithAttributesCriteria(String prefix) {
            X2Criteria criteria = new X2Criteria();
            if (isContextOverride()) {
                criteria.addEqualTo(BeanManager.getFullPrefix(prefix) + getAttributeRelationship() +
                        ModelProperty.PATH_DELIMITER + StudentContextAttributes.COL_CONTEXT_OID,
                        getCurrentContext().getOid());
            }
            return criteria;
        }

        /**
         * Adds the beans attributes.
         *
         * @param oid String
         * @param contextsAttributes Collection<ContextAttributes>
         * @return Collection
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#addBeansAttributes(java.lang.String,
         *      java.util.Collection)
         */
        @Override
        protected Collection<ContextAttributes> addBeansAttributes(String oid,
                                                                   Collection<ContextAttributes> contextsAttributes) {
            if (contextsAttributes == null) {
                X2Criteria attributesCriteria = new X2Criteria();
                attributesCriteria.addEqualTo(StudentContextAttributes.COL_STUDENT_OID, oid);
                attributesCriteria.addGreaterOrEqualThan(
                        StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                                DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(getCurrentContext().getSchoolYear()));
                QueryByCriteria attributesQuery =
                        new QueryByCriteria(StudentContextAttributes.class, attributesCriteria, false);
                attributesQuery
                        .addOrderBy(StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                                DistrictSchoolYearContext.COL_SCHOOL_YEAR, true);
                contextsAttributes = getBroker().getCollectionByQuery(attributesQuery);
                m_contextsAttributesMap.put(oid, contextsAttributes);
            }
            return contextsAttributes;
        }

        /**
         * Gets the attribute relationship.
         *
         * @return String
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getAttributeRelationship()
         */
        @Override
        protected String getAttributeRelationship() {
            return SisStudent.REL_CONTEXT_ATTRIBUTES;
        }

        /**
         * Gets the criteria handler.
         *
         * @return Criteria handler
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#getCriteriaHandler()
         */
        @Override
        protected CriteriaHandler getCriteriaHandler() {
            return new StudentCriteriaHandler(getDictionary(), getBroker(), m_manager);
        }

        /**
         * Gets the manager.
         *
         * @return Context attributes manager
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#getManager()
         */
        @Override
        protected ContextAttributesManager getManager() {
            if (m_manager == null) {
                m_manager = new StudentContextAttributesManager(getOrganization(), getBroker());
            }
            return m_manager;
        }

        /**
         * Gets the manager.
         *
         * @param organization Organization
         * @param broker X2Broker
         * @return Context attributes manager
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getManager(com.follett.fsc.core.k12.beans.Organization,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        protected ContextAttributesManager getManager(Organization organization, X2Broker broker) {
            return getManager();
        }

        /**
         * Gets the source class.
         *
         * @return Class
         * @see com.x2dev.sis.tools.reports.ContextReportHelper#getSourceClass()
         */
        @Override
        protected Class getSourceClass() {
            return SisStudent.class;
        }

        /**
         * Gets the source class relation.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper#getSourceClassRelation()
         */
        @Override
        protected String getSourceClassRelation() {
            return RELATION_STUDENT;
        }

        /**
         * Initializes the student context attributes for the passed criteria.
         */
        @Override
        protected void initializeAttributesByCriteria() {
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_criteria);
            Collection<String> studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

            X2Criteria contextsAttributesCriteria = new X2Criteria();
            contextsAttributesCriteria.addIn(StudentContextAttributes.COL_STUDENT_OID, subQuery);
            contextsAttributesCriteria.addGreaterOrEqualThan(
                    StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                    Integer.valueOf(getCurrentContext().getSchoolYear()));
            QueryByCriteria contextAttributesQuery =
                    new QueryByCriteria(StudentContextAttributes.class, contextsAttributesCriteria, false);
            contextAttributesQuery
                    .addOrderBy(StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR, true);
            m_contextsAttributesMap = getBroker().getGroupedCollectionByQuery(contextAttributesQuery,
                    StudentContextAttributes.COL_STUDENT_OID, 5);

            for (String studentOid : studentOids) {
                if (!m_contextsAttributesMap.keySet().contains(studentOid)) {
                    m_contextsAttributesMap.put(studentOid, new ArrayList<ContextAttributes>());
                }
            }
        }

        /**
         * split <code>propertyPath</code> to relation and property.
         *
         * @param propertyPath String
         * @return Pair first - relation, second - path
         */
        private Pair<String, String> splitToRelationAndProperty(String propertyPath) {
            String rel = null;
            String property = null;

            int lastPosition = propertyPath.lastIndexOf(PATH_DELIMITER);
            if (lastPosition != -1) {
                rel = propertyPath.substring(0, lastPosition + 1);
                property = propertyPath.substring(lastPosition + 1);
            } else {
                rel = EMPTY;
                property = propertyPath;
            }

            rel = rel == null ? EMPTY : rel;
            property = property == null ? EMPTY : property;
            return Pair.of(rel, property);

        }
    }

    /**
     * The Student Schedule Span is a container class used by the Student History Helper .
     * This span contains information about one student scheduled in one class.
     * It contains the section. It can also contain begin date, end date and transcript when
     * available.
     *
     * @author X2 Development Corporation
     */
    public class TNStudentScheduleSpan {
        /*
         * Instance variables
         */
        private MasterSchedule m_section;
        private StudentScheduleChange m_entryChange;
        private PlainDate m_entryDate;
        private StudentScheduleChange m_exitChange;
        private PlainDate m_exitDate;
        private StudentSchedule m_schedule;

        /**
         * Constructor.
         *
         * @param section MasterSchedule
         */
        TNStudentScheduleSpan(MasterSchedule section) {
            m_section = section;
        }

        /**
         * Gets the entry change.
         *
         * @return the m_entryChange
         */
        public StudentScheduleChange getEntryChange() {
            return m_entryChange;
        }

        /**
         * Returns the schedule begin date. This may be the term begin date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        public PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * Gets the exit change.
         *
         * @return the m_exitChange
         */
        public StudentScheduleChange getExitChange() {
            return m_exitChange;
        }

        /**
         * Returns the schedule end date. This may be the term end date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        public PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Return the student schedule record if present.
         *
         * @return StudentSchedule
         */
        public StudentSchedule getSchedule() {
            return m_schedule;
        }

        /**
         * Returns the master section for this schedule span.
         *
         * @return MasterSchedule
         */
        public MasterSchedule getSection() {
            return m_section;
        }

        /**
         * Set the student schedule change for add.
         *
         * @param entryChange void
         */
        public void setEntryChange(StudentScheduleChange entryChange) {
            this.m_entryChange = entryChange;
        }

        /**
         * Sets the entry date for this student in this class.
         *
         * @param entryDate void
         */
        public void setEntryDate(PlainDate entryDate) {
            m_entryDate = entryDate;
        }

        /**
         * Set the student schedule change for drop.
         *
         * @param exitChange void
         */
        public void setExitChange(StudentScheduleChange exitChange) {
            this.m_exitChange = exitChange;
        }

        /**
         * Sets the exit date for this student in this class.
         *
         * @param exitDate void
         */
        public void setExitDate(PlainDate exitDate) {
            m_exitDate = exitDate;
        }

        /**
         * Set the student schedule record.
         *
         * @param schedule void
         */
        public void setSchedule(StudentSchedule schedule) {
            this.m_schedule = schedule;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder span = new StringBuilder();
            span.append("[");
            span.append("Section:");
            span.append(m_section == null ? "null" : m_section.getCourseView());
            span.append(" From:");
            span.append(m_entryDate == null ? "null" : m_entryDate.toString());
            span.append(" to: ");
            span.append(m_exitDate == null ? "null" : m_exitDate.toString());
            span.append("]");
            return span.toString();
        }
    }

    /**
     * Yog eliminate.
     *
     * @param yogEnrollment StudentEnrollment
     * @param previousEnrollment StudentEnrollment
     * @return true, if successful
     */
    static boolean yogEliminate(StudentEnrollment yogEnrollment, StudentEnrollment previousEnrollment) {
        return (StudentEnrollment.YOG_CHANGE.equals(yogEnrollment.getEnrollmentType()) &&
                StudentEnrollment.ENTRY.equals(previousEnrollment.getEnrollmentType()) &&
                (yogEnrollment.getEnrollmentDate() != null && previousEnrollment.getEnrollmentDate() != null
                        && yogEnrollment.getEnrollmentDate().equals(previousEnrollment.getEnrollmentDate()))
                &&
                (yogEnrollment.getSchool() != null && previousEnrollment.getSchool() != null
                        && yogEnrollment.getSchool().equals(previousEnrollment.getSchool())));
    }

    protected static final String ALIAS_DAY_EVENT_TYPE_2 = "DOE DAY EVENT TYPE 2";
    protected static final String ALIAS_DAY_EVENT_TYPE_3 = "DOE DAY EVENT TYPE 3";
    protected static final String DAY_EVENT_CALENDAR_END = "CE";
    protected static final String DAY_EVENT_CALENDAR_START = "CS";
    protected static final String DAY_EVENT_ATTENDANCE_START = "AS";
    protected static final String DISTRICT_SCHOOL_CODE = "dist";

    protected HashMap<DataDictionaryField, SystemStringConverter> m_converterMap;
    protected PlainDate m_currentDate;
    protected DataDictionary m_dictionary;
    protected Collection<String> m_preferenceStudentActiveStatuses;

    TNStateReportData m_helperData;
    TNStudentHistoryHelper m_studentHistoryHelper;

    /**
     * Instantiates a new TN enrollment helper.
     *
     * @param data TNStateReportData
     */
    public TNEnrollmentHelper(TNStateReportData data) {
        m_helperData = data;
        PlainDate firstDate = getDistrictDate(DAY_EVENT_CALENDAR_START, true);
        if (firstDate == null) {
            throw new IllegalStateException(
                    "District first date cannot be determined because no calendar event type 'CS' can be found in the calendar for school 'dist'.");
        }
        PlainDate firstAttendDate = getDistrictDate(DAY_EVENT_ATTENDANCE_START, false);
        if (firstAttendDate == null) {
            throw new IllegalStateException(
                    "District first attend date cannot be determined because no calendar event type 'AS' can be found in the calendar for school 'dist'.");
        }
        PlainDate lastDate = getDistrictDate(DAY_EVENT_CALENDAR_END, false);
        if (lastDate == null) {
            throw new IllegalStateException(
                    "District last date cannot be determined because no calendar event type 'CE' can be found in the calendar for school 'dist'.");
        }
        m_studentHistoryHelper = new TNStudentHistoryHelper(data, firstDate, lastDate, firstAttendDate);
        m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, firstDate);
        if (data.getBeanClass() != null && data.getBeanClass().equals(ScheduleTeacher.class)) {
            m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.FALSE);
        }
        m_preferenceStudentActiveStatuses = StudentManager.getActiveStudentCodeList(m_helperData.getOrganization());
        m_currentDate = new PlainDate(OrganizationManager.getTimeZone(m_studentHistoryHelper.getOrganization()));
    }

    /**
     * Gets the student history helper.
     *
     * @return TN student history helper
     */
    public TNStudentHistoryHelper getStudentHistoryHelper() {
        return m_studentHistoryHelper;
    }

    /**
     * Return instance of TNMultiYearContextHelper.
     *
     * @return TNMultiYearContextHelper
     */
    public TNStudentMultiYearHelper getStudentMultiYearHelper() {
        if (m_helperData.getStudentMultiYearHelper().getCriteria() == null) {
            if (getStudentHistoryHelper().getStudentSelectionMode() != null) {
                m_helperData.getStudentMultiYearHelper().setCriteria(getStudentHistoryHelper().getStudentCriteria());
            }
        }
        return m_helperData.getStudentMultiYearHelper();
    }

    /**
     * return value depend on context<br>
     * rule the same like in <code>TNStudentMultiYearHelper.getFieldValueByBeanPath</code><br>
     * method try translate value to Java type which is described in userDataType for this beanPath
     *
     * @param student SisStudent
     * @param beanPath String
     * @return Object
     * @see TNStudentMultiYearHelper#getFieldValueByBeanPath(X2BaseBean, String)
     */
    public Object getStudentPropertyAsJavaType(SisStudent student, String beanPath) {

        Object value = getStudentValueByBeanPath(student, beanPath);

        if (value instanceof String) {
            DataDictionaryField field = getDictionaryField(student.getClass(), beanPath);
            value = getValueAsObject(field, (String) value);
        }

        return value;
    }

    /**
     * If the m_helperData current context is the same as the organization current context, just
     * return the value from
     * the student bean, otherwise use TNMultiYearContextHelper to get value.
     *
     * @param student Student
     * @param beanPath String
     * @return X 2 base bean
     */
    public X2BaseBean getStudentRelationByBeanPath(Student student, String beanPath) {
        X2BaseBean bean = null;

        if (!m_helperData.getStudentMultiYearHelper().isContextOverride()) {
            bean = TNStateReportData.getRelatedBean(student, beanPath);
        } else {
            if (m_helperData.getStudentMultiYearHelper().getCriteria() == null) {
                if (getStudentHistoryHelper().getStudentSelectionMode() != null) {
                    m_helperData.getStudentMultiYearHelper()
                            .setCriteria(getStudentHistoryHelper().getStudentCriteria());
                }
            }

            bean = m_helperData.getStudentMultiYearHelper().getRelationByBeanPath(student, beanPath);
        }

        return bean;
    }

    /**
     * If the m_helperData current context is the same as the organization current context, just
     * return the value from
     * the student bean, otherwise use TNMultiYearContextHelper to get value.
     *
     * @param student Student
     * @param beanPath String
     * @return Object
     */
    public Object getStudentValueByBeanPath(Student student, String beanPath) {
        Object value = null;

        if (!m_helperData.getStudentMultiYearHelper().isContextOverride()) {
            value = student.getFieldValueByBeanPath(beanPath);
        } else {
            if (m_helperData.getStudentMultiYearHelper().getCriteria() == null) {
                if (getStudentHistoryHelper().getStudentSelectionMode() != null) {
                    m_helperData.getStudentMultiYearHelper()
                            .setCriteria(getStudentHistoryHelper().getStudentCriteria());
                }
            }

            value = m_helperData.getStudentMultiYearHelper().getFieldValueByBeanPath(student, beanPath);
        }

        return value;
    }

    /**
     * For a DataDictionaryField, find a SystemStringConverter appropriate for the field.
     * If no converter is appropriate, return null.
     * Use a map to maintain a cache of converters.
     *
     * @param field DataDictionaryField
     * @return SystemStringConverter
     */
    protected SystemStringConverter getConverter(DataDictionaryField field) {
        SystemStringConverter converter = null;
        if (m_converterMap == null) {
            m_converterMap = new HashMap<DataDictionaryField, SystemStringConverter>();
        }
        if (m_converterMap.keySet().contains(field)) {
            converter = m_converterMap.get(field);
        } else {
            if (field.isString()) {
                Converter baseConverter = ConverterFactory.getConverterForClass(
                        field.getEffectiveJavaType(),
                        LocalizationCache.getPrimarySystemLocale(m_helperData.getBroker().getPersistenceKey()),
                        field.isString());
                if (baseConverter instanceof SystemStringConverter) {
                    converter = ((SystemStringConverter) baseConverter);
                }
            }
            m_converterMap.put(field, converter);
        }

        return converter;
    }

    /**
     * Returns the data dictionary field for the passed field ID.
     *
     * @param beanClass Class
     * @param beanPath String
     * @return DataDictionaryField
     */
    protected DataDictionaryField getDictionaryField(Class beanClass, String beanPath) {
        DataDictionary dictionary = getDistrictDictionary();
        return dictionary.findDataDictionaryField(beanClass.getName(), beanPath);
    }

    /**
     * method try cast String into java type describe on <code>userDataType</code>.
     *
     * @param field DataDictionaryField
     * @param fieldValue String
     * @return Object
     */
    protected Object getValueAsObject(DataDictionaryField field, String fieldValue) {
        Object valueAsObject = null;

        if (field != null) {
            SystemStringConverter converter = getConverter(field);

            if (converter != null) {
                valueAsObject = converter.parseSystemString(fieldValue);
            } else {
                valueAsObject = fieldValue;
            }
        }

        return valueAsObject;
    }

    /**
     * Returns the district data dictionary.
     *
     * @return DataDictionary
     */
    private DataDictionary getDistrictDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(m_helperData.getBroker().getPersistenceKey());
        }

        return m_dictionary;
    }

    /**
     * Get the date of the first occurrence of a particular event type in the calendar.
     *
     * @param dayEvent
     *
     * @return Plain date
     */
    private PlainDate getDistrictDate(String dayEvent, boolean sortAscending) {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField field = m_helperData.getDataDictionaryField(SisSchoolCalendarDate.class,
                SisSchoolCalendarDate.COL_SCHEDULE_DAY_TYPE);
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addBeginsWith(ReferenceCode.COL_STATE_CODE, dayEvent);
        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        Collection<String> calendarStartDateCodes = new LinkedList<String>();
        ReportQueryIterator iterator = m_helperData.getBroker().getReportQueryIteratorByQuery(query);
        try {
            if (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                calendarStartDateCodes.add(code);
            }
        } finally {
            iterator.close();
        }
        if (calendarStartDateCodes.isEmpty()) {
            calendarStartDateCodes.add("__*Dummy*__");
        }

        PlainDate startDate = null;

        DataDictionary dictionary = m_helperData.getDataDictionary();
        DataDictionaryField aliasEventType2 = dictionary.findDataDictionaryFieldByAlias(ALIAS_DAY_EVENT_TYPE_2);
        DataDictionaryField aliasEventType3 = dictionary.findDataDictionaryFieldByAlias(ALIAS_DAY_EVENT_TYPE_3);

        X2Criteria eventTypeCriteria = new X2Criteria();
        eventTypeCriteria.addIn(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, calendarStartDateCodes);
        if (aliasEventType2 != null) {
            X2Criteria eventTypeCriteria2 = new X2Criteria();
            eventTypeCriteria2.addIn(aliasEventType2.getJavaName(), calendarStartDateCodes);
            eventTypeCriteria.addOrCriteria(eventTypeCriteria2);
        }
        if (aliasEventType3 != null) {
            X2Criteria eventTypeCriteria3 = new X2Criteria();
            eventTypeCriteria3.addIn(aliasEventType3.getJavaName(), calendarStartDateCodes);
            eventTypeCriteria.addOrCriteria(eventTypeCriteria3);
        }

        criteria = new X2Criteria();
        criteria.addEqualTo(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                m_helperData.getCurrentContext().getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID, DISTRICT_SCHOOL_CODE);
        criteria.addAndCriteria(eventTypeCriteria);

        QueryByCriteria yearStartQuery = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        yearStartQuery.addOrderBy(SchoolCalendarDate.COL_DATE, sortAscending);

        QueryIterator startDates = m_helperData.getBroker().getIteratorByQuery(yearStartQuery);
        try {
            if (startDates.hasNext()) {
                SchoolCalendarDate date = (SchoolCalendarDate) startDates.next();
                startDate = date.getDate();
            }
        } finally {
            if (startDates != null) {
                startDates.close();
            }
        }

        return startDate;
    }
}
