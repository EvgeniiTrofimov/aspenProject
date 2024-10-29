/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class RiSpedHelper.
 */
public class RiSpedHelper {


    /**
     * Class helps filter List<X2BaseBean> using <code>RiSpedHelper.Operator</code> criteria
     *
     * @author Follett Software Company
     * @param <T> the generic type
     */
    public class Filter<T extends X2BaseBean> implements Cloneable {

        private List<Filter> m_andFielters = new ArrayList<RiSpedHelper.Filter>();
        private Class m_beanClass = null;
        private String m_column = null;
        private String m_column_db_type = null;
        private String m_column_user_type = null;
        private DataDictionary m_fddx = null;
        private List<Filter> m_fielters = new ArrayList<RiSpedHelper.Filter>();
        private Operator m_operator = null;
        private List<Filter> m_orFielters = new ArrayList<RiSpedHelper.Filter>();
        private Object m_value = null;


        /**
         * Instantiates a new filter.
         *
         * @param beanClass Class<T>
         */
        public Filter(Class<T> beanClass) {
            m_beanClass = beanClass;
            m_fddx = getDictionary();
        }

        /**
         * Instantiates a new filter.
         *
         * @param beanClass Class<T>
         * @param ddx will used for translate alias
         */
        public Filter(Class<T> beanClass, DataDictionary ddx) {
            m_beanClass = beanClass;
            m_fddx = ddx;
        }

        /**
         * add criteria equal to.
         *
         * @param alias String
         * @param value Object
         */
        public void addAliasEqualTo(String alias, Object value) {
            addEqualTo(translateAliasToJavaName(alias, m_fddx), value);
        }

        /**
         * add and criteria.
         *
         * @param filter Filter<T>
         */
        public void addAndFilter(Filter<T> filter) {
            m_andFielters.add(filter);
        }

        /**
         * add criteria equal to.
         *
         * @param columnName String
         * @param value Object
         */
        public void addEqualTo(String columnName, Object value) {

            m_operator = Operator.EQUALS;
            m_column = columnName;
            m_value = value;
            try {
                Filter filter = (Filter) this.clone();
                m_fielters.add(filter);
                clearFields();

            } catch (CloneNotSupportedException e) {
                e.printStackTrace();
            }
        }

        /**
         * add criteria greater or Equal than.
         *
         * @param columnName String
         * @param value Object
         */
        public void addGreaterOrEqualThan(String columnName, Object value) {
            m_operator = Operator.GREATER_OR_EQUAL_THAN;
            DataDictionaryField ddxField = m_fddx.findDataDictionaryField(m_beanClass.getName(), columnName);
            m_column_user_type = ddxField.getDataFieldConfig().getUserType();
            m_column_db_type = ddxField.getDatabaseType();
            m_column = columnName;
            m_value = value;
            try {
                Filter filter = (Filter) this.clone();
                m_fielters.add(filter);
                clearFields();

            } catch (CloneNotSupportedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        /**
         * add criteria is empty.
         *
         * @param columnName String
         */
        public void addIsEmpty(String columnName) {
            m_operator = Operator.IS_EMPTY;
            DataDictionaryField ddxField = m_fddx.findDataDictionaryField(m_beanClass.getName(), columnName);
            m_column_user_type = ddxField.getDataFieldConfig().getUserType();
            m_column_db_type = ddxField.getDatabaseType();
            m_column = columnName;
            try {
                Filter filter = (Filter) this.clone();
                m_fielters.add(filter);
                clearFields();

            } catch (CloneNotSupportedException e) {
                //
                e.printStackTrace();
            }
        }

        /**
         * add criteria is not empty.
         *
         * @param columnName String
         */
        public void addIsNotEmpty(String columnName) {
            m_operator = Operator.IS_NOT_EMPTY;
            DataDictionaryField ddxField = m_fddx.findDataDictionaryField(m_beanClass.getName(), columnName);
            m_column_user_type = ddxField.getDataFieldConfig().getUserType();
            m_column_db_type = ddxField.getDatabaseType();
            m_column = columnName;
            try {
                Filter filter = (Filter) this.clone();
                m_fielters.add(filter);
                clearFields();

            } catch (CloneNotSupportedException e) {
                //
                e.printStackTrace();
            }
        }

        /**
         * add criteria less or equal than.
         *
         * @param columnName String
         * @param value Object
         */
        public void addLessOrEqualThan(String columnName, Object value) {
            m_operator = Operator.LESS_OR_EQUAL_THAN;
            DataDictionaryField ddxField = m_fddx.findDataDictionaryField(m_beanClass.getName(), columnName);
            m_column_user_type = ddxField.getDataFieldConfig().getUserType();
            m_column_db_type = ddxField.getDatabaseType();
            m_column = columnName;
            m_value = value;
            try {
                Filter filter = (Filter) this.clone();
                m_fielters.add(filter);
                clearFields();

            } catch (CloneNotSupportedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        /**
         * add or Criteria.
         *
         * @param filter Filter<T>
         */
        public void addOrFilter(Filter<T> filter) {
            m_orFielters.add(filter);
        }

        /**
         * trying filter <code>beans</code> by set criteria.
         *
         * @param beans Collection<T>
         * @return List
         */
        public List<T> applyFilter(Collection<T> beans) {
            List<T> resultCollection = new ArrayList<T>();
            for (T x2BaseBean : beans) {
                boolean isSuccess = true;
                for (Filter filter : m_fielters) {
                    isSuccess = filter.applyOperator(x2BaseBean);
                    if (!isSuccess) {
                        for (Filter filterOr : m_orFielters) {
                            Collection<T> oneBean = new ArrayList<T>();
                            oneBean.add(x2BaseBean);
                            isSuccess = filterOr.applyFilter(oneBean).size() == 1 ? true : false;
                            if (isSuccess) {
                                break;
                            }
                        }

                        if (!isSuccess) {
                            break;
                        }
                    }
                }
                if (isSuccess) {
                    for (Filter andFilter : m_andFielters) {
                        Collection<T> oneBean = new ArrayList<T>();
                        oneBean.add(x2BaseBean);
                        isSuccess = andFilter.applyFilter(oneBean).size() == 1 ? true : false;
                        if (!isSuccess) {
                            break;
                        }
                    }

                    if (isSuccess) {
                        resultCollection.add(x2BaseBean);
                    }

                }
            }
            return resultCollection;
        }

        /**
         * clone current filter.
         *
         * @return Object
         * @throws CloneNotSupportedException exception
         * @see java.lang.Object#clone()
         */
        @Override
        protected Object clone() throws CloneNotSupportedException {
            Filter filter = (Filter) super.clone();
            filter.m_value = this.m_value;
            filter.m_column = this.m_column;
            filter.m_operator = this.m_operator;
            filter.m_beanClass = this.m_beanClass;
            return filter;
        }

        /**
         * try check is current bean go through all criteria successful.
         *
         * @param x2BaseBean X2BaseBean
         * @return true, if successful
         */
        private boolean applyOperator(X2BaseBean x2BaseBean) {
            boolean isSuccess = true;
            if (m_operator.equals(Operator.EQUALS)) {
                Object value = x2BaseBean.getFieldValueByBeanPath(m_column);
                if (value == null || !value.equals(m_value)) {
                    isSuccess = false;
                }
            } else if (m_operator.equals(Operator.GREATER_OR_EQUAL_THAN)) {
                if ((!StringUtils.isEmpty(m_column_db_type) && m_column_db_type.equals("D"))
                        ||
                        (!StringUtils.isEmpty(m_column_user_type) && m_column_user_type.equals("date"))) {
                    PlainDate beanDate = getPlainDateByBeanPath(x2BaseBean, m_column);
                    PlainDate valueDate = translateObjectToPlainDate(m_value);
                    if (beanDate == null || valueDate == null || beanDate.before(valueDate)) {
                        isSuccess = false;
                    }
                }
            } else if (m_operator.equals(Operator.LESS_OR_EQUAL_THAN)) {
                if ((!StringUtils.isEmpty(m_column_db_type) && m_column_db_type.equals("D"))
                        ||
                        (!StringUtils.isEmpty(m_column_user_type) && m_column_user_type.equals("date")))

                {
                    PlainDate beanDate = getPlainDateByBeanPath(x2BaseBean, m_column);
                    PlainDate valueDate = translateObjectToPlainDate(m_value);
                    if (beanDate == null || valueDate == null || beanDate.after(valueDate)) {
                        isSuccess = false;
                    }
                }
            } else if (m_operator.equals(Operator.IS_EMPTY)) {
                Object value = x2BaseBean.getFieldValueByBeanPath(m_column);
                if (!(value == null || (value instanceof String && ((String) value).isEmpty()))) {
                    isSuccess = false;
                }
            } else if (m_operator.equals(Operator.IS_NOT_EMPTY)) {
                Object value = x2BaseBean.getFieldValueByBeanPath(m_column);
                if (!((!(value instanceof String) && value != null)
                        || (value instanceof String && !((String) value).isEmpty()))) {
                    isSuccess = false;
                }
            }

            return isSuccess;
        }

        /**
         * clear fields.
         */
        private void clearFields() {
            m_value = null;
            m_column = null;
            m_operator = null;
            m_column_user_type = null;
            m_column_db_type = null;
        }



    }

    /**
     * class help create List map from List<beans> <br>
     * for each bean logic created map<br>
     * map key - optional. java field name/custom name <br>
     * option located on markAlias/markField methods<br>
     * map value - optional. straight value from bean/value as String/value for detail(UI string)
     * <br>
     * option located on createMap method, PropertyValueType parameter
     * you need just mark needed field/s and/or alias/es and call createMap method
     *
     * @author Follett Software Company
     * @param <T> the generic type
     */
    public class MakeDataMapFromBeanHelper<T extends X2BaseBean> {
        private DataDictionary m_dmDdx = null;
        private Map<String, String> m_fields;
        private Map<String, String> m_constants;
        private boolean m_includeBeanKey;
        private static final String KEY_BEAN = "bean";

        /**
         * Instantiates a new make data map from bean helper.
         *
         * @param includeBeanKey - if true - put "bean" key and appropriate X2BaseBean like value.
         * @param ddx - will used for translate alias to java name
         */
        public MakeDataMapFromBeanHelper(boolean includeBeanKey, DataDictionary ddx) {
            m_includeBeanKey = includeBeanKey;
            m_dmDdx = ddx;
            m_fields = new HashMap<String, String>();
            m_constants = new HashMap<String, String>();
        }

        /**
         * create map for each bean based on input fields/aliases.
         *
         * @param beans Collection<T>
         * @param prportyType PropertyValueType
         * @return List
         */
        public List<Map<String, Object>> createMap(Collection<T> beans, PropertyValueType prportyType) {
            List<Map<String, Object>> returnList = new ArrayList<Map<String, Object>>();

            for (X2BaseBean baseBean : beans) {
                Map<String, Object> beanFields = new HashMap<String, Object>();
                if (m_includeBeanKey) {
                    beanFields.put(KEY_BEAN, baseBean);
                }
                for (Entry<String, String> entry : m_constants.entrySet()) {
                    beanFields.put(entry.getKey(), entry.getValue());
                }

                for (Entry<String, String> entry : m_fields.entrySet()) {

                    Object fieldValue = null;
                    ModelProperty prop = new ModelProperty(baseBean.getClass(), entry.getKey(), m_dmDdx);

                    if (prportyType.equals(PropertyValueType.PROPETY_ORIGINAL) ||
                            prportyType.equals(PropertyValueType.PROPERTY_AS_STRING)) {
                        fieldValue = baseBean.getFieldValueByBeanPath(entry.getKey());
                    }
                    if (prportyType.equals(PropertyValueType.PROPERTY_DETAIL)) {
                        try {
                            fieldValue = WebUtils.getPropertyForDetail(baseBean, prop, Locale.getDefault());
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }

                    } else if (prportyType.equals(PropertyValueType.PROPERTY_AS_STRING)) {
                        fieldValue = WebUtils.getPropertyAsString(fieldValue, prop, Locale.getDefault(), null);
                    }

                    beanFields.put(entry.getValue(), fieldValue);
                }
                returnList.add(beanFields);
            }
            return returnList;
        }

        /**
         * mark alias which will put into map .
         *
         * @param alias String
         * @param key - this key will used on map, if key will null - will used javaName field like
         *        key<br>
         *        example alias -"test" key - null; logic try translate alias to java name. for
         *        example
         *        alias -"test" into "fieldA001"
         *        and put "fieldA001" like key
         */
        public void markAlias(String alias, String key) {
            markAlias(alias, key, m_dmDdx);
        }

        /**
         * mark alias which will put into map .
         *
         * @param alias String
         * @param key - this key will used on map, if key will null - will used javaName field like
         *        key<br>
         *        example alias -"test" key - null; logic try translate alias to java name. for
         *        example
         *        "fieldA001"
         *        and put "fieldA001" like key
         * @param ddx DataDictionary
         */
        public void markAlias(String alias, String key, DataDictionary ddx) {
            String javaName = translateAliasToJavaName(alias, ddx);
            markField(javaName, key);
        }

        /**
         * mark field which will put into map.
         *
         * @param field - java name. example "fieldA001"
         * @param key this key will used on map. If key will null - will used <code>field</code>
         *        parameter like key<br>
         *        example field -"id", key - null; logic put "id" like key
         */
        public void markField(String field, String key) {
            if (StringUtils.isEmpty(key)) {
                key = field;
            }
            m_fields.put(field, key);
        }

        /**
         * mark property which will put into map.
         *
         * @param property - field name with data base table prefix. example "gfdFieldA001"<br>
         *        prefix must match <T extends X2BaseBean> which will filtered<br>
         *        example "gfdFieldA001" - T should be GenericFormData
         * @param key this key will used on map. If key will null - will used <code>property</code>
         *        without prefix like key<br>
         *        example field -"gfdId", key - null; logic put "id" like key
         */
        public void markProperty(String property, String key) {
            if (StringUtils.isEmpty(key)) {
                key = property;
            }
            String field = EMPTY;
            if (!StringUtils.isEmpty(property) && property.length() > 3) {
                field = property.substring(3);
                field = field.substring(0, 1).toLowerCase() + field.substring(1);

            }

            m_fields.put(field, key);
        }

        /**
         * add constant which will put into map.
         *
         * @param constant value for map
         * @param key - key for map, key can not be null and empty
         */
        public void markConstant(String constant, String key) {
            if (!StringUtils.isEmpty(key)) {
                m_constants.put(key, constant);
            }
        }



    }

    /**
     * Operator which used on MakeDataMapFromBeanHelper class<br>
     * it is represent state for property value<br>
     * PROPETY_ORIGINAL - mean that it is value get straight form bean<br>
     * PROPERTY_DETAIL - mean that value was converted for show it in UI. Example: <br>
     * PlainDate or String date(yyyy-MM-dd) convert to string (M/d/yyyy)<br>
     * PROPERTY_AS_STRING - mean that value was converted to String, example: <br>
     * PlainDate or String date(yyyy-MM-dd) convert to string (yyyy-MM-dd)<br>
     *
     * @author Follett Software Company
     */
    public enum PropertyValueType {
        PROPERTY_AS_STRING, PROPERTY_DETAIL, PROPETY_ORIGINAL
    }

    private static final String EMPTY = "";
    public static final int REFERENCE_LOOKUP_CODE_DESCRIPTION = 5;
    private X2Broker m_broker = null;
    private Organization m_organization = null;
    private static final SimpleDateFormat FORMATTER_TO = new SimpleDateFormat("M/d/yyyy");
    private static final SimpleDateFormat FORMATTER_FRORM = new SimpleDateFormat("yyyy-MM-dd");
    private DataDictionary m_ddx = null;
    protected Map<String, DataDictionary> m_dictionaryMap = new HashMap<String, DataDictionary>();
    /**
     * A map of maps of reference code.
     * The outer map is indexed by the reference table OID. It contains maps of reference codes.
     * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
     * code.
     */
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;

    /**
     * DataSource based on storage and owner bean. See SimpleFormDataSourceExtends
     * current implementation has additional ability to lookup state code from alias value
     * using a:s:alias_name expression
     *
     * @see SimpleFormDataSourceExtends
     * @author Follett Software Company
     * @copyright 2017
     */
    public class SimpleFormDataSourceExtends extends SimpleFormDataSource {
    
        private Map<String, Object> m_hashedValues = new HashMap<String, Object>();
    
        /**
         * @param formStorage
         * @param formOwner
         * @param dictionary
         * @param locale
         */
        public SimpleFormDataSourceExtends(X2BaseBean formStorage, X2BaseBean formOwner, DataDictionary dictionary,
                                           Locale locale) {
            super(formStorage, formOwner, dictionary, locale);
    
        }
    
        /**
         * @see com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            // TODO Auto-generated method stub
            Object returnValue = null;
    
            if (m_hashedValues.containsKey(fieldName)) {
                returnValue = m_hashedValues.get(fieldName);
            }
            else {
                if (fieldName.startsWith("a:s:")) {
                    String alias = fieldName.substring("a:s:".length());
                    fieldName = "a:" + alias;
                    DataDictionaryField dictionaryField = getDictionary().findDataDictionaryFieldByAlias(alias);
                    if (dictionaryField != null) {
                        String beanPath = dictionaryField.getJavaName();
                        String fieldValue = (String) getCurrentBean().getFieldValueByBeanPath(beanPath);
                        returnValue =
                                lookupReferenceCodeByBeanPath(getCurrentBean().getClass(), beanPath,
                                        fieldValue,
                                        ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
                else
                {
                    returnValue = super.getFieldValue(fieldName);
                }
                m_hashedValues.put(fieldName, returnValue);
            }
            return returnValue;
        }
    }

    /**
     * Operator which used on Filter class<br>
     * it is represent criteria which can be used.
     *
     * @author Follett Software Company
     */
    private enum Operator {
        EQUALS, GREATER_OR_EQUAL_THAN, LESS_OR_EQUAL_THAN, IS_EMPTY, IS_NOT_EMPTY
    }

    /**
     * Instantiates a new ri sped helper.
     *
     * @param broker X2Broker
     * @param organization Organization
     */
    public RiSpedHelper(X2Broker broker, Organization organization) {
        m_broker = broker;
        m_organization = organization;
    }


    /**
     * cast Object (String("yyyy-MM-dd") or Date/PlainDate into String with "M/d/yyyy" format.
     *
     * @param paramDate Object
     * @return String
     */
    public static String formatDate(Object paramDate) {
        String returnValue = null;
        Date date = translateObjectToPlainDate(paramDate);
        if (date != null) {
            returnValue = FORMATTER_TO.format(date);
        }

        return returnValue;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     * @param beanClass Class
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
        ModelProperty prop = new ModelProperty(beanClass, path, m_broker.getPersistenceKey());
        DataDictionaryField dictionaryField = getDictionary().findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * Gets the dictionary.
     *
     * @return DataDictionary
     */
    public DataDictionary getDictionary() {
        if (m_ddx == null) {
            m_ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_ddx;
    }


    /**
     * try find DataDictionary by extendedDataDictionaryID.
     *
     * @param extendedDataDictionaryID String
     * @return Data dictionary
     */
    public DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {

        DataDictionary returnValue = m_dictionaryMap.get(extendedDataDictionaryID);
        if (returnValue == null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
            QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
            ExtendedDataDictionary extendedDataDictionary =
                    (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
            returnValue = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
            m_dictionaryMap.put(extendedDataDictionaryID, returnValue);
        }
        return returnValue;
    }


    /**
     * return criteria for StudentEnrollment.
     * Criteria by student Oid and Enrollment types
     *
     * @param stdOid String
     * @param enrollmentTypes List<String>
     * @return X 2 criteria
     */
    public static X2Criteria getEnrollmentCriteria(String stdOid, List<String> enrollmentTypes) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, stdOid);
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollmentTypes);
        return criteria;
    }


    /**
     * Gets the form detail.
     *
     * @param detail GenericDetail
     * @return Form detail
     */
    public static FormDetail getFormDetail(GenericDetail detail) {
        FormDetail formDetail = null;
        if (detail instanceof SisOutcomeDetail) {
            SisOutcomeDetail outcomeDetail = (SisOutcomeDetail) detail;
            formDetail = outcomeDetail.getCurrentFormDetail();
        }
        if (detail instanceof FormDetail) {
            formDetail = (FormDetail) detail;
        }
        return formDetail;
    }


    /**
     * Calculate and return the student grade level code for the student, YOG and date.
     * The date may be in any school year. The appropriate district school year will be used.
     *
     * @param date PlainDate
     * @param stdEnrollment StudentEnrollment
     * @return String
     */
    public String getGradeLevel(PlainDate date, StudentEnrollment stdEnrollment) {
        int yog = stdEnrollment.getYog();
        String gradeCode = null;
        SisDistrictSchoolYearContext dateContext =
                CalendarManager.getDistrictContext(date, getOrganization().getOid(), getBroker());
        if (dateContext != null) {
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    dateContext.getSchoolYear(),
                    StudentManager.buildGradeLevelMap(getBroker()));
            gradeCode = matchingGradeLevels.isEmpty() ? EMPTY : matchingGradeLevels.get(0);
        }
        return gradeCode != null ? gradeCode : EMPTY;
    }


    /**
     * return near previous enrollment than param date<br>
     * if previous enrollment don't exist - return near next than param date.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @param enrollmentTypes List<String>
     * @return Student enrollment
     */
    public StudentEnrollment getNearLastStudentEnrollment(SisStudent student,
                                                          PlainDate date,
                                                          List<String> enrollmentTypes) {
        StudentEnrollment lastEnrollment = null;
        X2Criteria criteria = getEnrollmentCriteria(student.getOid(), enrollmentTypes);
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, date);
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);

        lastEnrollment = (StudentEnrollment) getBroker().getBeanByQuery(query);

        if (lastEnrollment == null) {
            criteria = getEnrollmentCriteria(student.getOid(), enrollmentTypes);
            query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);
            lastEnrollment = (StudentEnrollment) getBroker().getBeanByQuery(query);
        }
        return lastEnrollment;
    }


    /**
     * try cast value from beanPath to PlainDate<br>
     * value for beanPath can has PlaneDate or String format .
     *
     * @param baseBean X2BaseBean
     * @param beanPath String
     * @return Plain date
     */
    public static PlainDate getPlainDateByBeanPath(X2BaseBean baseBean, String beanPath) {
        PlainDate returnValue = null;
        Object value = baseBean.getFieldValueByBeanPath(beanPath);
        returnValue = translateObjectToPlainDate(value);

        return returnValue;

    }

    /**
     * Lookup a map of reference codes for a reference table oid.
     * Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableMap == null) {
            m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable =
                    (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    codeMap.put(code.getCode(), code);
                }
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - reference map type (ExportFormatField.ReferenceMapTypeCode.*.ordinal())
     *        of the lookup.
     *
     * @return String - state code for input value.
     */
    public String lookupReferenceCodeByBeanPath(Class beanClass, String beanPath, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        } else if (dictionaryField != null && dictionaryField.getExtendedDataField() != null) {
            ReferenceTable refTable = dictionaryField.getExtendedDataField().getReferenceTable();
            if (refTable != null) {
                stateValue = lookupReferenceCodeByRefTbl(refTable.getOid(), value, referenceMap);
            }

        }

        return stateValue;
    }


    /**
     * Returns the lookup code value for field value.
     * Look up based on the reference table.
     *
     * @param referenceTableOid String
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - the reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
     * @return String - reference code lookup value for input value.
     */
    public String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
        String returnValue = null;
        Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
        ReferenceCode code = refCodes.get(value);
        if (code != null) {
            if (referenceMap == ReferenceMapTypeCode.STATE.ordinal()) {
                returnValue = code.getStateCode();
            } else if (referenceMap == ReferenceMapTypeCode.FEDERAL.ordinal()) {
                returnValue = code.getFederalCode();
            } else if (referenceMap == ReferenceMapTypeCode.LOCAL.ordinal()) {
                returnValue = code.getLocalCode();
            } else if (referenceMap == ReferenceMapTypeCode.SYSTEM.ordinal()) {
                returnValue = code.getSystemCode();
            } else if (referenceMap == REFERENCE_LOOKUP_CODE_DESCRIPTION) {
                returnValue = code.getDescription();
            }
        }

        return returnValue;
    }

    public void setDataDictionary(DataDictionary ddx) {
        m_ddx = ddx;
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return String
     */
    public static String translateAliasToJavaName(String alias, DataDictionary dataDictionary) {
        String javaName = null;

        DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }

        return javaName;
    }

    /**
     * cast Object (String("yyyy-MM-dd") or Date/PlainDate into PlainDate.
     *
     * @param value Object
     * @return PlainDate
     */
    public static PlainDate translateObjectToPlainDate(Object value) {
        PlainDate returnValue = null;
        if (value == null) {
            returnValue = null;
        } else if (value instanceof PlainDate) {
            returnValue = (PlainDate) value;
        } else if (value instanceof Date) {
            returnValue = new PlainDate((Date) value);
        } else if (value instanceof String && !((String) value).isEmpty()) {
            try {
                returnValue = new PlainDate(FORMATTER_FRORM.parse((String) value));
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        return returnValue;
    }

    /**
     * Gets the broker.
     *
     * @return X2Broker
     */
    protected X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the organization.
     *
     * @return Organization
     */
    protected Organization getOrganization() {
        return m_organization;
    }
}
