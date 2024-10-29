/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import static com.x2dev.sis.model.beans.StudentEnrollment.*;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.dictionary.ExtendedFieldAttributes;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class IlSpedHelper.
 */
public class IlSpedHelper {

    /**
     * sections for facts form.
     *
     * @author Follett Software Company
     */
    public enum FactsSectionType {
        CURRENT("FACTS TRACKING SHEET", PQSectionType.CURRENT), NEXT("NEXT YEAR FACTS TRACKING SHEET",
                PQSectionType.NEXT);

        private String m_title = null;
        private PQSectionType m_pqSection = null;

        /**
         * Instantiates a new facts section type.
         *
         * @param title String
         * @param type PQSectionType
         */
        private FactsSectionType(String title, PQSectionType type) {
            m_title = title;
            m_pqSection = type;
        }

        /**
         * Gets the PQ section type.
         *
         * @return PQ section type
         */
        public PQSectionType getPQSectionType() {
            return m_pqSection;
        }

        /**
         * Gets the title.
         *
         * @return String
         */
        public String getTitle() {
            return m_title;
        }

    }

    /**
     * class help create map with <br>
     * key - field name/alias <br>
     * value - value from bean by this field name/alias<br>
     * you need just mark needed field/s and/or alias/es and call createMap method.
     *
     * @author Follett Software Company
     */
    public class FieldHelper {
        private DataDictionary m_ddx = null;
        private Map<String, String> m_fields;
        private Map<String, String> m_fieldNameAliasName;
        private boolean m_includeBeanKey;
        private static final String KEY_BEAN = "bean";

        /**
         * Instantiates a new field helper.
         *
         * @param includeBeanKey boolean
         */
        FieldHelper(boolean includeBeanKey) {
            m_includeBeanKey = includeBeanKey;
            m_ddx = getDictionary();
            m_fields = new HashMap<String, String>();
            m_fieldNameAliasName = new HashMap<String, String>();
        }

        /**
         * Instantiates a new field helper.
         *
         * @param includeBeanKey boolean
         * @param ddx DataDictionary
         */
        FieldHelper(boolean includeBeanKey, DataDictionary ddx) {
            m_includeBeanKey = includeBeanKey;
            m_ddx = ddx;
            m_fields = new HashMap<String, String>();
        }

        /**
         * create map for each bean based on input fields/aliases.
         *
         * @param beans Collection
         * @return List
         */
        public List<Map<String, Object>> createMap(Collection beans) {
            List<Map<String, Object>> returnList = new ArrayList<Map<String, Object>>();

            for (X2BaseBean baseBean : ((Collection<X2BaseBean>) beans)) {
                Map<String, Object> beanFields = new HashMap<String, Object>();
                if (m_includeBeanKey) {
                    beanFields.put(KEY_BEAN, baseBean);
                }

                for (Entry<String, String> entry : m_fields.entrySet()) {
                    Object fieldValue = baseBean.getFieldValueByBeanPath(entry.getKey());
                    if (fieldValue instanceof PlainDate) {
                        fieldValue = formatDate(fieldValue);
                    } else if (m_fieldNameAliasName.containsKey(entry.getKey())) {
                        String alias = m_fieldNameAliasName.get(entry.getKey());
                        DataDictionaryField field = m_ddx.findDataDictionaryFieldByAlias(alias);
                        if (field != null) {
                            ExtendedFieldAttributes extDataField = field.getExtendedDataField();
                            if (extDataField != null) {
                                String userType = extDataField.getUserType();
                                if (userType != null && userType.equals("Date")) {
                                    fieldValue = formatDate(fieldValue);
                                }
                            }
                        }

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
         *        "fieldA001"
         *        and put "fieldA001" like key
         */
        public void markAlias(String alias, String key) {
            String javaName = translateAliasToJavaName(alias, m_ddx);
            m_fieldNameAliasName.put(javaName, alias);
            markField(javaName, key);
        }

        /**
         * mark field which will put into map.
         *
         * @param field String
         * @param key this key will used on map, if key will null - will used <code>field</code><br>
         *        example field -"id" key - null; logic put "id" like key
         */
        public void markField(String field, String key) {
            if (StringUtils.isEmpty(key)) {
                key = field;
            }
            m_fields.put(field, key);
        }



    }

    /**
     * Class helps filter List<X2BaseBean> by criteria.
     *
     * @author Follett Software Company
     * @param <T> the generic type
     */
    public class Filter<T extends X2BaseBean> implements Cloneable {

        private List<Filter> m_andFielters = new ArrayList<IlSpedHelper.Filter>();
        private Class m_beanClass = null;
        private String m_column = null;
        private String m_column_db_type = null;
        private String m_column_user_type = null;
        private DataDictionary m_ddx = null;
        private List<Filter> m_fielters = new ArrayList<IlSpedHelper.Filter>();
        private Operator m_operator = null;
        private List<Filter> m_orFielters = new ArrayList<IlSpedHelper.Filter>();
        private Object m_value = null;



        /**
         * Instantiates a new filter.
         *
         * @param beanClass Class<T>
         */
        public Filter(Class<T> beanClass) {
            m_beanClass = beanClass;
            m_ddx = getDictionary();
        }

        /**
         * Instantiates a new filter.
         *
         * @param beanClass Class<T>
         * @param ddx DataDictionary
         */
        public Filter(Class<T> beanClass, DataDictionary ddx) {
            m_beanClass = beanClass;
            m_ddx = ddx;
        }

        /**
         * add criteria equal to.
         *
         * @param alias String
         * @param value Object
         */
        public void addAliasEqualTo(String alias, Object value) {
            addEqualTo(translateAliasToJavaName(alias, m_ddx), value);
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
                // TODO Auto-generated catch block
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
            DataDictionaryField ddxField = m_ddx.findDataDictionaryField(m_beanClass.getName(), columnName);
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
            DataDictionaryField ddxField = m_ddx.findDataDictionaryField(m_beanClass.getName(), columnName);
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
            DataDictionaryField ddxField = m_ddx.findDataDictionaryField(m_beanClass.getName(), columnName);
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
            DataDictionaryField ddxField = m_ddx.findDataDictionaryField(m_beanClass.getName(), columnName);
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
                        ||
                        (value instanceof String && !((String) value).isEmpty()))) {
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
     * class for store grouped Services.
     * this class store one group and provide common data for this group.
     *
     * @author Follett Software Company
     */
    public class IlSpedGoupOfServices {
        private List<ServiceContainer> m_groupedServices = new ArrayList<ServiceContainer>();
        private IlSpedNameGroupedServices m_groupName = null;
        private DataDictionary m_gsSevsDdx = null;
        private List<String> m_serviceNames = new ArrayList<String>();

        /**
         * Instantiates a new il sped goup of services.
         *
         * @param groupName IlSpedNameGroupedServices
         * @param ddx DataDictionary
         */
        public IlSpedGoupOfServices(IlSpedNameGroupedServices groupName, DataDictionary ddx) {
            m_gsSevsDdx = ddx;
            m_groupName = groupName;
        }

        /**
         * add service to this group.
         *
         * @param iepService IepService
         */
        public void add(IepService iepService) {

            ServiceContainer container = new ServiceContainer(iepService, m_gsSevsDdx);
            m_groupedServices.add(container);

        }

        /**
         * Gets the containers.
         *
         * @return all ServiceContainer belong to this group
         */
        public List<ServiceContainer> getContainers() {
            return m_groupedServices;
        }

        /**
         * Gets the group name.
         *
         * @return name of group
         */
        public IlSpedNameGroupedServices getGroupName() {
            return m_groupName;
        }

        /**
         * Gets the service names.
         *
         * @return list service names
         */
        public List<String> getServiceNames() {

            return m_serviceNames;
        }

        /**
         * Gets the total duration integer.
         *
         * @return Total Duration (sum MPW for all services inside this group)
         */
        public Integer getTotalDurationInteger() {
            int totalDuration = 0;
            for (ServiceContainer container : m_groupedServices) {
                totalDuration += container.getDurationInteger().intValue();
            }
            return Integer.valueOf(totalDuration);
        }
    }

    /**
     * class has grouped services by <code>IlSpedNameGroupedServices</code>
     * it's provide methods for get common data about grouped services<br>
     * class also provide access to the one group see getGroupedServices method<br>
     * .
     *
     * @author Follett Software Company
     * @see ILSpedGroupsOfServices#getGroupedServices(IlSpedNameGroupedServices)
     */
    public class ILSpedGroupsOfServices {
        private static final String PARAM_PERS_INSIDE_SPED_SRV_CHLD = "persInsideSpedSrvChld";
        private static final String PARAM_PERS_INSIDE_SPED_SRV = "persInsideSpedSrv";

        private PlainDate m_biggestEndDate = null;
        private PlainDate m_limitEndDate = null;
        private DataDictionary m_gpsSrvsDdx = null;
        private Map<IlSpedNameGroupedServices, IlSpedGoupOfServices> m_groupsOfServicesMap = null;
        private List<IepService> m_services = new ArrayList<IepService>();
        private PlainDate m_smallestStartDate = null;
        private PlainDate m_limitStartDate = null;
        private PQSectionType m_type = null;

        /**
         * Instantiates a new IL sped groups of services.
         *
         * @param type PQSectionType
         */
        private ILSpedGroupsOfServices(PQSectionType type) {
            m_type = type;
            m_groupsOfServicesMap = new HashMap<IlSpedNameGroupedServices, IlSpedGoupOfServices>();
            m_gpsSrvsDdx = getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);

        }

        /**
         * Instantiates a new IL sped groups of services.
         *
         * @param type PQSectionType
         * @param limitStartDate PlainDate
         * @param limitEndDate PlainDate
         */
        private ILSpedGroupsOfServices(PQSectionType type, PlainDate limitStartDate, PlainDate limitEndDate) {
            m_type = type;
            m_groupsOfServicesMap = new HashMap<IlSpedNameGroupedServices, IlSpedGoupOfServices>();
            m_gpsSrvsDdx = getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
            m_limitEndDate = limitEndDate;
            m_limitStartDate = limitStartDate;

        }

        /**
         * add service to group/groups using some rule<br>
         * group - NO_SUPPL_AIDS; service mode - No Supplementary Aids;
         * group - SPED_REL_SERV_WITHIN_GENERAL_ED; service mode - Special Education or Related
         * Services; General Education - true; Extended Year - false;
         * group - SPED_OUTSIDE_GENERAL_ED; service mode - Special Education; General Education -
         * false; Extended Year - false;
         * group - REL_SERV_OUTSIDE_GENERAL_ED; service mode - Related Services; General Education -
         * false;
         * group - SUPPL_AIDS; service mode - Supplementary Aids;.
         *
         * @param iepService IepService
         */
        public void add(IepService iepService) {
            m_services.add(iepService);
            setSmallerBiggestDates(iepService);

            String serviceMode = iepService.getServiceMode();
            boolean extendedYearIndicator = iepService.getExtendedYearIndicator();
            boolean isGeneralEducation = getBooleanByAlias(iepService, ALIAS_SERVICE_LOCATION_REGULAR);

            if (serviceMode != null) {
                /* General Education with No Supplementary Aids... */
                if (serviceMode.equals(IlSpedNameServiceMode.NO_SUPPLEMENTARY_AIDS.getMode())) {
                    IlSpedGoupOfServices groupedServices = getGroupedServices(IlSpedNameGroupedServices.NO_SUPPL_AIDS);
                    groupedServices.add(iepService);
                }

                /* Special Education and Related Services within the General Education Classroom */
                else if ((serviceMode.equals(IlSpedNameServiceMode.SPECIAL_EDUCATION.getMode())
                        || serviceMode.equals(IlSpedNameServiceMode.RELATED_SERVICES.getMode()))
                        && isGeneralEducation && !extendedYearIndicator) {
                    IlSpedGoupOfServices groupedServices =
                            getGroupedServices(IlSpedNameGroupedServices.SPED_REL_SERV_WITHIN_GENERAL_ED);
                    groupedServices.add(iepService);
                }
                /* Special Education Services - Outside General Education */
                else if (serviceMode.equals(IlSpedNameServiceMode.SPECIAL_EDUCATION.getMode()) && !isGeneralEducation
                        && !extendedYearIndicator) {
                    IlSpedGoupOfServices groupedServices =
                            getGroupedServices(IlSpedNameGroupedServices.SPED_OUTSIDE_GENERAL_ED);
                    groupedServices.add(iepService);
                }
                /* Related Services - Outside General Education */
                else if (serviceMode.equals(IlSpedNameServiceMode.RELATED_SERVICES.getMode()) && !isGeneralEducation) {
                    IlSpedGoupOfServices groupedServices =
                            getGroupedServices(IlSpedNameGroupedServices.REL_SERV_OUTSIDE_GENERAL_ED);
                    groupedServices.add(iepService);
                }
                /* General Education with Supplementary Aids... with supports, if applicable */
                else if (serviceMode.equals(IlSpedNameServiceMode.SUPPLEMENTARY_AIDS.getMode())) {
                    IlSpedGoupOfServices groupedServices = getGroupedServices(IlSpedNameGroupedServices.SUPPL_AIDS);
                    groupedServices.add(iepService);
                }
            }
        }

        /**
         * return all group names .
         *
         * @return List
         */
        public List<IlSpedNameGroupedServices> getAllGroupedNames() {
            List<IlSpedNameGroupedServices> groupedNames =
                    new ArrayList<IlSpedNameGroupedServices>(m_groupsOfServicesMap.keySet());
            return groupedNames;
        }


        /**
         * return list grouped services.
         *
         * @return List
         */
        public List<IlSpedGoupOfServices> getAllGroupedServices() {
            List<IlSpedGoupOfServices> list = new ArrayList<IlSpedGoupOfServices>(m_groupsOfServicesMap.values());
            return list;
        }


        /**
         * Gets the biggest end date.
         *
         * @return Biggest End Date
         */
        public PlainDate getBiggestEndDate() {
            return m_biggestEndDate == null ? m_limitEndDate : m_biggestEndDate;
        }


        /**
         * Educational Environment (EE) Calculation<br>
         * <b>key</b> - value<br>
         * <b>for child hood</b><br>
         * <b>minSpentChildhoodProg</b> - Integer (Minutes spent in regular early childhood
         * program)<br>
         * <b>minSpentOutsideChildhood</b> - Integer (Minutes spent receiving special education and
         * related services outside regular early childhood )<br>
         * <b>preKPercent</b> - String (minSpentChildhoodProg/ (minSpentChildhoodProg +
         * minSpentOutsideChildhood) )<br>
         * <b>persInsideSpedSrvChld</b> - String (Minutes spent receiving special education outside
         * regular early childhood / instructional minutes)<br>
         * <b>for 6-21 ages</b><br>
         * <b>minBellToBell</b> - Integer (Total Bell to Bell Minutes; sum all mpw, or selected
         * value from dropdown)<br>
         * <b>totalMinOutsideGeneralEd</b> - Integer (Total Number of Minutes Outside of the General
         * Education Setting; SPED + Related services)<br>
         * <b>persInsideSpedSrv</b> - String (minutes spend in special education outside general
         * education(only Sped without Related services)/total instructional minutes)<br>
         * <b>totalMinInsideGeneralEd</b> - Integer (minBellToBell - totalMinOutsideGeneralEd) <br>
         * <b>preOTHPercent</b> - String (Percentage of Time Inside the General Education
         * Environment; totalMinInsideGeneralEd/minBellToBell)<br>
         * .
         *
         * @param iepData IepData
         * @return Map
         */
        public Map<String, Object> getEEMinutesMap(IepData iepData) {

            Map<String, Object> returnMap = new HashMap<String, Object>();

            boolean isAutocalc = getBooleanByAlias(iepData, m_type.getAlias(F_AUTOCALC), m_gpsSrvsDdx);

            // childhood block
            Integer minSpentChildhoodProg =
                    getIntegerByAlias(iepData, m_type.getAlias(F_MIN_SPENT_CHILD_PROG), ZERO_INTEGER, m_gpsSrvsDdx);
            Integer minSpentOutsideChildhood =
                    getIntegerByAlias(iepData, m_type.getAlias(F_MIN_SPENT_OUTS_CHILD), ZERO_INTEGER, m_gpsSrvsDdx);
            Integer instractionalMinutesChildhood =
                    getIntegerByAlias(iepData, m_type.getAlias(F_CALC_INSTR_MIN_CHILD), ZERO_INTEGER, m_gpsSrvsDdx);
            Integer minSpendInSpedSrvOutsideChildhood =
                    getIntegerByAlias(iepData, m_type.getAlias(F_CALC_MIN_SPED_OUTS_CHILD), ZERO_INTEGER, m_gpsSrvsDdx);

            returnMap.put(PARAM_MIN_SPENT_CHILDHOOD_PROG, minSpentChildhoodProg);
            returnMap.put(PARAM_MIN_SPENT_OUTSIDE_CHILDHOOD, minSpentOutsideChildhood);
            returnMap.put(PARAM_PRE_K_PERCENT,
                    calculatePercent(minSpentChildhoodProg.intValue() + minSpentOutsideChildhood.intValue(),
                            minSpentChildhoodProg.intValue()));
            returnMap.put(PARAM_PERS_INSIDE_SPED_SRV_CHLD, calculatePercent(instractionalMinutesChildhood.intValue(),
                    minSpendInSpedSrvOutsideChildhood.intValue()));

            // 6-21 ages block
            Integer minBellToBell = getMinBellToBell(iepData, isAutocalc);
            Integer totalMinOutsideGeneralEd = getTotalMinOutsideGeneralEd(iepData, isAutocalc);
            Integer totalMinInsideGeneralEd =
                    getTotalMinInsideGeneralEd(iepData, isAutocalc, minBellToBell, totalMinOutsideGeneralEd);
            String percentInsideGeneralEd =
                    getPercentInsideGeneralEd(iepData, isAutocalc, minBellToBell, totalMinOutsideGeneralEd);
            String percentInsideSpecialEd = getPersentInsideSpecialEd(iepData);
            returnMap.put(PARAM_MIN_BELL_TO_BELL, minBellToBell);
            returnMap.put(PARAM_TOTAL_MIN_OUTSIDE_GENERAL_ED, totalMinOutsideGeneralEd);
            returnMap.put(PARAM_PERS_INSIDE_SPED_SRV, percentInsideSpecialEd);
            if (totalMinInsideGeneralEd.intValue() >= 0) {
                returnMap.put(PARAM_TOTAL_MIN_INSIDE_GENERAL_ED, totalMinInsideGeneralEd);
            }
            returnMap.put(PARAM_OTH_PERCENT, percentInsideGeneralEd);
            return returnMap;
        }


        /**
         * return grouped services by group name. If group doesn't exist - create new
         *
         * @param groupName IlSpedNameGroupedServices
         * @return Il sped goup of services
         */
        public IlSpedGoupOfServices getGroupedServices(IlSpedNameGroupedServices groupName) {
            IlSpedGoupOfServices groupedServices = m_groupsOfServicesMap.get(groupName);
            if (groupedServices == null) {
                groupedServices = new IlSpedGoupOfServices(groupName, m_gpsSrvsDdx);
                m_groupsOfServicesMap.put(groupName, groupedServices);
            }
            return groupedServices;
        }


        /**
         * fill and return map which contain Total Duration for each group;
         * Key IlSpedNameGroupedServices.toString() value - total duration for this group in String
         *
         * @return Map
         */
        public Map<String, String> getGroupedTotalDuration() {
            Map<String, String> groupedTotalDuration = new HashMap<String, String>();
            for (Entry<IlSpedNameGroupedServices, IlSpedGoupOfServices> entry : m_groupsOfServicesMap.entrySet()) {
                String key = entry.getKey().toString();
                Integer totalDuration = entry.getValue().getTotalDurationInteger();
                String value = totalDuration == null ? EMPTY : String.valueOf(totalDuration.intValue());
                groupedTotalDuration.put(key, value);
            }
            return groupedTotalDuration;
        }

        /**
         * Gets the smallest start date.
         *
         * @return Smallest Start Date
         */
        public PlainDate getSmallestStartDate() {
            return m_smallestStartDate == null ? m_limitStartDate : m_smallestStartDate;
        }

        /**
         * calculate percent based on <code>total</code> and <code>part</code><br>
         * rounding to the nearest whole + '%'<br>
         * 0.5 got to 1
         *
         * @param total int
         * @param part int
         * @return String
         */
        protected String calculatePercent(int total, int part) {
            String returnValue = null;
            if (total > 0) {
                try {
                    double result = total == 0 ? 0 : ((double) (part) * 100) / total;

                    BigDecimal decimal = new BigDecimal(result);


                    returnValue = String.valueOf(decimal.setScale(0, RoundingMode.HALF_UP).intValue()) + PERCENT;
                } catch (Exception e) {
                    e.printStackTrace();
                }

            }
            return returnValue;
        }


        /**
         * calculate Total Number of Minutes Outside of the General Education; SPED + Related
         * services outside GE. (totalMinOutsideGeneralEd)
         *
         * @return Integer
         */
        private Integer calculateMinuteOutsideGeneralEd() {

            int minOutsideGeneralEd = 0;
            for (Entry<IlSpedNameGroupedServices, IlSpedGoupOfServices> entry : m_groupsOfServicesMap.entrySet()) {
                IlSpedNameGroupedServices serviceMode = entry.getKey();
                if (serviceMode.equals(IlSpedNameGroupedServices.SPED_OUTSIDE_GENERAL_ED)
                        || serviceMode.equals(IlSpedNameGroupedServices.REL_SERV_OUTSIDE_GENERAL_ED)) {
                    minOutsideGeneralEd += entry.getValue().getTotalDurationInteger().intValue();
                }
            }
            return Integer.valueOf(minOutsideGeneralEd);

        }


        /**
         * calculate total bell to bell minutes from grouped services.
         *
         * @return Integer
         */
        private Integer calculateTotalBellToBell() {
            int bellToBell = 0;

            for (Entry<IlSpedNameGroupedServices, IlSpedGoupOfServices> entry : m_groupsOfServicesMap.entrySet()) {

                IlSpedGoupOfServices groupedServices = entry.getValue();
                Integer totalGroupBellToBell = groupedServices.getTotalDurationInteger();
                bellToBell += totalGroupBellToBell.intValue();
            }
            return Integer.valueOf(bellToBell);

        }


        /**
         * get total bell to bell minutes from selected/or input value from UI.
         *
         * @param iepData IepData
         * @return Integer
         */
        private Integer getDefaultTotalBellToBellMinutes(IepData iepData) {
            Integer minBellToBell = null;

            String bellToBellCode = (String) iepData.getFieldValueByAlias(m_type.getAlias(F_SCHOOL_MPW), m_gpsSrvsDdx);
            if (!StringUtils.isEmpty(bellToBellCode)) {
                if (!bellToBellCode.equals(SCHOOL_MPW_CODE_OTHER)) {
                    String schoolMpwJavaName = translateAliasToJavaName(m_type.getAlias(F_SCHOOL_MPW), m_gpsSrvsDdx);
                    String bellToBellValue = lookupReferenceCodeByBeanPath(iepData.getClass(), schoolMpwJavaName,
                            bellToBellCode, ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(bellToBellValue)) {
                        try {
                            minBellToBell = Integer.valueOf(bellToBellValue);
                        } catch (NumberFormatException e) {
                            minBellToBell = ZERO_INTEGER;
                        }
                    }
                } else {

                    minBellToBell = getIntegerByAlias(iepData, m_type.getAlias(F_TOTAL_MIN_BELL_TO_BELL), ZERO_INTEGER,
                            m_gpsSrvsDdx);
                }
            }
            return minBellToBell;
        }


        /**
         * calculate or get input value for bell to bell minutes.
         *
         * @param iepData IepData
         * @param isAutocalc boolean
         * @return Integer
         */
        private Integer getMinBellToBell(IepData iepData, boolean isAutocalc) {
            Integer minBellToBell = getDefaultTotalBellToBellMinutes(iepData);
            if (isAutocalc) {
                if (minBellToBell == null || minBellToBell.intValue() == 0) {
                    minBellToBell = calculateTotalBellToBell();
                }
            }
            return minBellToBell;

        }


        /**
         * calculate or get input value for Percentage of Time Inside the General Education
         * Environment (preOTHPercent) for 6-21 ages.
         *
         * @param iepData IepData
         * @param isAutocalc boolean
         * @param minBellToBell Integer
         * @param totalMinOutsideGeneralEd Integer
         * @return String
         */
        private String getPercentInsideGeneralEd(IepData iepData,
                                                 boolean isAutocalc,
                                                 Integer minBellToBell,
                                                 Integer totalMinOutsideGeneralEd) {
            String percentInsideGeneralEd = null;
            if (isAutocalc) {
                percentInsideGeneralEd = calculatePercent(minBellToBell.intValue(),
                        minBellToBell.intValue() - totalMinOutsideGeneralEd.intValue());
                // set default 0%
                if (percentInsideGeneralEd == null) {
                    percentInsideGeneralEd = ZERO_INTEGER.intValue() + PERCENT;
                }
            } else {
                percentInsideGeneralEd =
                        getIntegerByAlias(iepData, m_type.getAlias(F_PERCENT_GEN_ED), ZERO_INTEGER, m_gpsSrvsDdx)
                                .toString() + PERCENT;
            }
            return percentInsideGeneralEd;
        }



        /**
         * Gets the persent inside special ed.
         *
         * @param iepData IepData
         * @return String
         */
        private String getPersentInsideSpecialEd(IepData iepData) {
            Integer instractionalMinutes =
                    getIntegerByAlias(iepData, m_type.getAlias(F_CALC_INSTR_MIN), ZERO_INTEGER, m_gpsSrvsDdx);
            Integer minInSpedServicesOutsideGE =
                    getGroupedServices(IlSpedNameGroupedServices.SPED_OUTSIDE_GENERAL_ED).getTotalDurationInteger();
            Integer minInSpedRelServicesGE =
                    getGroupedServices(IlSpedNameGroupedServices.SPED_REL_SERV_WITHIN_GENERAL_ED)
                            .getTotalDurationInteger();
            Integer minInRelServicesOutsideGE =
                    getGroupedServices(IlSpedNameGroupedServices.REL_SERV_OUTSIDE_GENERAL_ED).getTotalDurationInteger();
            return calculatePercent(instractionalMinutes.intValue(), minInSpedServicesOutsideGE.intValue() +
                    minInSpedRelServicesGE.intValue() +
                    minInRelServicesOutsideGE.intValue());
        }


        /**
         * calculate or get input value for total minutes inside general education for 6-21 ages
         * (totalMinInsideGeneralEd).
         *
         * @param iepData IepData
         * @param isAutocalc boolean
         * @param minBellToBell Integer
         * @param totalMinOutsideGeneralEd Integer
         * @return Integer
         */
        private Integer getTotalMinInsideGeneralEd(IepData iepData,
                                                   boolean isAutocalc,
                                                   Integer minBellToBell,
                                                   Integer totalMinOutsideGeneralEd) {
            Integer totalMinInsideGeneralEd = null;
            if (isAutocalc) {
                totalMinInsideGeneralEd =
                        Integer.valueOf(minBellToBell.intValue() - totalMinOutsideGeneralEd.intValue());
            } else {
                totalMinInsideGeneralEd =
                        getIntegerByAlias(iepData, m_type.getAlias(F_TOTAL_MIN_GEN_ED), ZERO_INTEGER, m_gpsSrvsDdx);
            }
            return totalMinInsideGeneralEd;
        }


        /**
         * calculate or get input value for: Total Number of Minutes Outside of the General
         * Education; SPED + Related services outside GE. (totalMinOutsideGeneralEd)
         *
         * @param iepData IepData
         * @param isAutocalc boolean
         * @return Integer
         */
        private Integer getTotalMinOutsideGeneralEd(IepData iepData, boolean isAutocalc) {
            Integer totalMinOutsideGeneralEd = null;
            if (isAutocalc) {
                totalMinOutsideGeneralEd = calculateMinuteOutsideGeneralEd();
            } else {
                totalMinOutsideGeneralEd = getIntegerByAlias(iepData, m_type.getAlias(F_TOTAL_MIN_OUTS_GEN_ED),
                        ZERO_INTEGER, m_gpsSrvsDdx);
            }
            return totalMinOutsideGeneralEd;
        }


        /**
         * set smaller and biggest date<br>
         * check - does this service has smaller and/or biggest date then other services. If true -
         * set this date/s like smaller and/or biggest
         *
         * @param iepService void
         */
        private void setSmallerBiggestDates(IepService iepService) {

            List<String> serviceModes = new ArrayList<String>(
                    Arrays.asList(SERVICE_MODE_NO_SUPPLEMENTARY_AIDS, SERVICE_MODE_SUPPLEMENTARY_AIDS,
                            SERVICE_MODE_SPECIAL_EDUCATION, SERVICE_MODE_RELATED_SERVICES));


            if (serviceModes.contains(iepService.getServiceMode())) {

                PlainDate currentStartDate = m_type.getServiceStartDate(iepService, m_gpsSrvsDdx);
                if (currentStartDate != null && m_limitStartDate != null && m_limitStartDate.after(currentStartDate)) {
                    currentStartDate = m_limitStartDate;
                }
                PlainDate currentEndDate = m_type.getServiceEndDate(iepService, m_gpsSrvsDdx);
                if (currentEndDate != null && m_limitEndDate != null && m_limitEndDate.before(currentEndDate)) {
                    currentEndDate = m_limitEndDate;
                }

                if (m_biggestEndDate == null || (currentEndDate != null && currentEndDate.after(m_biggestEndDate))) {
                    m_biggestEndDate = currentEndDate;
                }
                if (m_smallestStartDate == null
                        || (currentStartDate != null && currentStartDate.before(m_smallestStartDate))) {
                    m_smallestStartDate = currentStartDate;
                }
            }

        }

    }

    /**
     * group names for services .
     *
     * @author Follett Software Company
     */
    public enum IlSpedNameGroupedServices {
        /* "General Education with No Supplementary Aids" */
        NO_SUPPL_AIDS("noSuplAids"),
        /* "Special Education and Related Services within the General Education Classroom" */
        SPED_REL_SERV_WITHIN_GENERAL_ED("spedRelServWitinGenED"),
        /* "Special Education Services - Outside General Education" */
        SPED_OUTSIDE_GENERAL_ED("spedOutsideGenED"),
        /* "Related Services - Outside General Education" */
        REL_SERV_OUTSIDE_GENERAL_ED("relSerOutsideGenED"),
        /* "General Education with Supplementary Aids" */
        SUPPL_AIDS("suplAids");


        private String m_groupName = null;

        /**
         * Instantiates a new il sped name grouped services.
         *
         * @param groupName String
         */
        private IlSpedNameGroupedServices(String groupName) {

            m_groupName = groupName;
        }

        /**
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {

            return m_groupName;
        }


    }

    /**
     * service mode names for IepService.
     *
     * @author Follett Software Company
     */
    public enum IlSpedNameServiceMode {
        NO_SUPPLEMENTARY_AIDS("No Supplementary Aids"), SPECIAL_EDUCATION("Special Education"), RELATED_SERVICES(
                "Related Services"), SUPPLEMENTARY_AIDS("Supplementary Aids");

        private String m_mode = null;

        /**
         * Instantiates a new il sped name service mode.
         *
         * @param mode String
         */
        private IlSpedNameServiceMode(String mode) {
            m_mode = mode;
        }

        /**
         * Gets the mode.
         *
         * @return String
         */
        public String getMode() {
            return m_mode;
        }
    }

    /**
     * class provide method for determine is date belong to period.
     *
     * @author Follett Software Company
     */
    private class PeriodSchoolYearContext {

        private PlainDate m_end;
        private PlainDate m_start;
        private int m_year;

        /**
         * Instantiates a new period school year context.
         *
         * @param start PlainDate
         * @param end PlainDate
         * @param year int
         */
        public PeriodSchoolYearContext(PlainDate start, PlainDate end, int year) {
            m_start = start;
            m_end = end;
            m_year = year;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            PeriodSchoolYearContext other = (PeriodSchoolYearContext) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_end == null) {
                if (other.m_end != null) {
                    return false;
                }
            } else if (!m_end.equals(other.m_end)) {
                return false;
            }
            if (m_start == null) {
                if (other.m_start != null) {
                    return false;
                }
            } else if (!m_start.equals(other.m_start)) {
                return false;
            }
            if (m_year != other.m_year) {
                return false;
            }
            return true;
        }

        /**
         * method for determine is date belong to period.
         *
         * @param date PlainDate
         * @return true, if is between
         */
        public boolean isBetween(PlainDate date) {
            boolean returnValue = false;
            if (m_start != null && m_end != null && date != null &&
                    (m_start.before(date) || m_start.equals(date)) &&
                    (m_end.after(date) || m_end.equals(date))) {
                returnValue = true;
            }
            return returnValue;
        }

        /**
         * Checks if is in year.
         *
         * @param year int
         * @return true, if is in year
         */
        public boolean isInYear(int year) {
            boolean returnValue = false;
            if (m_year == year) {
                returnValue = true;
            }
            return returnValue;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "PeriodSchoolYearContext class " + m_start + SPACE + m_end;
        }



        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_end == null) ? 0 : m_end.hashCode());
            result = prime * result + ((m_start == null) ? 0 : m_start.hashCode());
            result = prime * result + m_year;
            return result;
        }

        /**
         * Gets the outer type.
         *
         * @return Il sped helper
         */
        private IlSpedHelper getOuterType() {
            return IlSpedHelper.this;
        }
    }

    /**
     * data source for pq form;<br>
     * this data source help translate ireport field name to the alias name;<br>
     * Mapping ireport name and alias name pull from PQSectionType;<br>
     * it's needed for have one ireport format for current and next year section.<br>
     * this sections contain difference aliases name but the same logic, the same view<br>
     * mapping help to have only one ireport format. <br>
     *
     * @author Follett Software Company
     */
    public class PQDataSource extends SimpleFormDataSource {
        private static final String PREFIX_ALIAS = "a:";
        private static final String PREFIX_FIELD = "f:";

        private Map<String, String> m_aliases = null;
        private PQSectionType m_type;

        /**
         * Instantiates a new PQ data source.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param dictionary DataDictionary
         * @param locale Locale
         * @param type PQSectionType
         */
        public PQDataSource(X2BaseBean formStorage, X2BaseBean formOwner, DataDictionary dictionary, Locale locale,
                PQSectionType type) {
            super(formStorage, formOwner, dictionary, locale);
            m_type = type;
            m_aliases = m_type.getAliases();
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource#getFieldValue(java.lang.
         *      String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            if (fieldName.startsWith(PREFIX_FIELD)) {
                String tempFieldName = fieldName.substring(PREFIX_FIELD.length());
                String alias = m_aliases.get(tempFieldName);
                if (!StringUtils.isEmpty(alias)) {
                    fieldName = PREFIX_ALIAS + alias;
                }

            }
            return super.getFieldValue(fieldName);
        }

    }

    /**
     * sections for PQ form.
     *
     * @author Follett Software Company
     */
    public enum PQSectionType {

        CURRENT("EDUCATIONAL SERVICES AND PLACEMENT",
                getCurrentAliases()), NEXT("NEXT YEAR EDUCATIONAL SERVICES AND PLACEMENT", getNextAliases());



        private static final String ALIAS_SERVICE_NY_INITIATION_DATE = "service-ny-initiation-date";
        private static final String ALIAS_ATTR_NY_DURATION_DATE = "attr-ny-duration-date";
        private static final String ALIAS_ATTR_CY_DURATION_DATE = "attr-cy-duration-date";
        private static final String ALIAS_ATTR_NY_INITIATION_DATE = "attr-ny-initiation-date";
        private static final String ALIAS_ATTR_CY_INITIATION_DATE = "attr-cy-initiation-date";
        private String m_title = null;
        private Map<String, String> m_aliases = null;


        /**
         * Instantiates a new PQ section type.
         *
         * @param title String
         * @param aliases Map<String,String>
         */
        private PQSectionType(String title, Map<String, String> aliases) {
            m_title = title;
            m_aliases = aliases;
        }

        /**
         * return mapping
         * key alias which in not depend on current/next context
         * value alias which depend on current/next context.
         *
         * @return Map
         */
        public Map<String, String> getAliases() {
            return m_aliases;
        }

        /**
         * translate field(alias) which in not depend on current/next context to alias which depend
         * on current/next context<br>
         * current/next context - it is PQSection enum element (CURRENT or NEXT).
         *
         * @param field String
         * @return String
         */
        public String getAlias(String field) {
            return m_aliases.get(field);
        }

        /**
         * get District End Date depend on current PQSectionType.
         *
         * @param current DistrictSchoolYearContext
         * @param broker X2Broker
         * @return Plain date
         */
        public PlainDate getDistrictEndDate(DistrictSchoolYearContext current, X2Broker broker) {
            return getContext(current, broker).getEndDate();
        }

        /**
         * get District Start Date depend on current PQSectionType.
         *
         * @param current DistrictSchoolYearContext
         * @param broker X2Broker
         * @return Plain date
         */
        public PlainDate getDistrictStartDate(DistrictSchoolYearContext current, X2Broker broker) {
            return getContext(current, broker).getStartDate();
        }

        /**
         * return service start date from iep for current or next year.
         *
         * @param iepData IepData
         * @param ddx DataDictionary
         * @return Plain date
         */
        public PlainDate getIepStartServiceDate(IepData iepData, DataDictionary ddx) {
            PlainDate returnDate = null;
            if (this.ordinal() == 0) {
                returnDate = getPlainDateByBeenAlias(iepData, ALIAS_ATTR_CY_INITIATION_DATE, ddx);
            } else if (this.ordinal() == 1) {
                returnDate = getPlainDateByBeenAlias(iepData, ALIAS_ATTR_NY_INITIATION_DATE, ddx);
            }
            return returnDate;
        }

        /**
         * return service end date from iep for current or next year.
         *
         * @param iepData IepData
         * @param ddx DataDictionary
         * @return Plain date
         */
        public PlainDate getIepEndServiceDate(IepData iepData, DataDictionary ddx) {
            PlainDate returnDate = null;
            if (this.ordinal() == 0) {
                returnDate = getPlainDateByBeenAlias(iepData, ALIAS_ATTR_CY_DURATION_DATE, ddx);
            } else if (this.ordinal() == 1) {
                returnDate = getPlainDateByBeenAlias(iepData, ALIAS_ATTR_NY_DURATION_DATE, ddx);
            }
            return returnDate;
        }

        /**
         * get School Calendar End Date for current student for current PQ Section type.
         *
         * @param student SisStudent
         * @param helper IlSpedHelper
         * @param useDistrictCalendarIfNotFound if true and SchoolCalendar is not found - use
         *        DistrictCalendar
         * @return Plain date
         */
        public PlainDate getSchoolCalendarEndDate(SisStudent student,
                                                  IlSpedHelper helper,
                                                  boolean useDistrictCalendarIfNotFound) {
            return getStartEndSchoolCalendarDates(student, helper, useDistrictCalendarIfNotFound).getRight();
        }

        /**
         * get School Calendar StartDate for current student for current PQ Section type.
         *
         * @param student SisStudent
         * @param helper IlSpedHelper
         * @param useDistrictCalendarIfNotFound if true and SchoolCalendar is not found - use
         *        DistrictCalendar
         * @return Plain date
         */
        public PlainDate getSchoolCalendarStartDate(SisStudent student,
                                                    IlSpedHelper helper,
                                                    boolean useDistrictCalendarIfNotFound) {
            return getStartEndSchoolCalendarDates(student, helper, useDistrictCalendarIfNotFound).getLeft();
        }

        /**
         * Gets the school year context.
         *
         * @param current DistrictSchoolYearContext
         * @param broker X2Broker
         * @return District school year context
         */
        public DistrictSchoolYearContext getSchoolYearContext(DistrictSchoolYearContext current, X2Broker broker) {
            return getContext(current, broker);
        }

        /**
         * return start date from IepService for current or next year .
         *
         * @param iepService IepService
         * @param ddx DataDictionary
         * @return Plain date
         */
        public PlainDate getServiceStartDate(IepService iepService, DataDictionary ddx) {
            PlainDate returnDate = null;
            if (this.ordinal() == 0) {
                returnDate = iepService.getStartDate();
            } else if (this.ordinal() == 1) {
                returnDate = getPlainDateByBeenAlias(iepService, ALIAS_SERVICE_NY_INITIATION_DATE, ddx);
            }
            return returnDate;
        }

        /**
         * return end date from IepService for current or next year.
         *
         * @param iepService IepService
         * @param ddx DataDictionary
         * @return Plain date
         */
        public PlainDate getServiceEndDate(IepService iepService, DataDictionary ddx) {
            PlainDate returnDate = null;
            if (this.ordinal() == 0) {
                returnDate = getPlainDateByBeenAlias(iepService, ALIAS_SERVICE_CY_DURATION_DATE, ddx);
            } else if (this.ordinal() == 1) {
                returnDate = iepService.getEndDate();
            }
            return returnDate;
        }


        /**
         * title for current enum element.
         *
         * @return String
         */
        public String getTitle() {
            return m_title;
        }

        /**
         * get DistrictSchoolYearContext depend on current/next case.
         *
         * @param current DistrictSchoolYearContext
         * @param broker X2Broker
         * @return District school year context
         */
        private DistrictSchoolYearContext getContext(DistrictSchoolYearContext current, X2Broker broker) {
            // current case
            DistrictSchoolYearContext finalContext = null;
            if (this.ordinal() == 0) {
                finalContext = current;
            }
            // next case
            else if (this.ordinal() == 1) {
                finalContext = getNextSchoolYearContext(current, broker);
            }
            return finalContext;
        }

        /**
         * alias mapping for current year case
         * map key it is alias(alias which doesn't depend on current/next year context)
         * value - real alias which use in current year context.
         *
         * @return Map
         */
        private static Map<String, String> getCurrentAliases() {
            Map<String, String> aliases = new HashMap<String, String>();
            aliases.put(F_AUTOCALC, ALIAS_EE_AUTOCALC);
            aliases.put(F_TOTAL_MIN_OUTS_GEN_ED, ALIAS_EE_TOTAL_NUM_MIN_OUT_GEN_ED);
            aliases.put(F_TOTAL_MIN_GEN_ED, ALIAS_EE_CALC_TOTAL_MIN_GEN_ED);
            aliases.put(F_PERCENT_GEN_ED, ALIAS_EE_CALC_PERCENT_GEN_ED);
            aliases.put(F_MIN_SPENT_CHILD_PROG, ALIAS_EE_MIN_SP_REG_EARLY_CHILD_PR);
            aliases.put(F_MIN_SPENT_OUTS_CHILD, ALIAS_EE_MIN_SP_REC_SPEC_ED_SERV);
            aliases.put(F_SCHOOL_MPW, ALIAS_IEP_SCHOOL_MPW);
            aliases.put(F_TOTAL_MIN_BELL_TO_BELL, ALIAS_EE_TOTAL_MIN);
            aliases.put(F_NEED_SPED_CLASSES, ALIAS_ENV_NEED_SPED_CLASSES);
            aliases.put(F_NEED_SPED_CLASSES_EXPL, ALIAS_ENV_NEED_SPED_CLASSES_EXPL);
            aliases.put(F_NONACADEM_ACTIV, ALIAS_ENV_NONACADEM_ACTIV);
            aliases.put(F_NONACADEM_ACTIV_EXPL, ALIAS_ENV_NONACADEM_ACTIV_EXPL);
            aliases.put(F_ATTEND_IF_NONDIS, ALIAS_ENV_ATTEND_IF_NONDIS);
            aliases.put(F_ATTEND_IF_NONDIS_EXPL, ALIAS_ENV_ATTEND_IF_NONDIS_EXPL);
            aliases.put(F_INFORM_SIMILAR_SERV, ALIAS_ENV_INFORM_SIMILAR_SERV);
            aliases.put(F_SPEC_TRANSP_BETWEEN_SCH, ALIAS_ENV_SPEC_TRANSP_BETWEEN_SCH);
            aliases.put(F_SPEC_TRANSP_AROUND_SCH, ALIAS_ENV_SPEC_TRANSP_AROUND_SCH);
            aliases.put(F_SPEC_EQUIP, ALIAS_ENV_SPEC_EQUIP);
            aliases.put(F_TRANSP_PLAN, ALIAS_ENV_TRANSP_PLAN);
            aliases.put(F_EXT_SCH_YEAR_NEED, ALIAS_ED_ENV_EXT_SCH_YEAR_NEED);
            aliases.put(F_CALC_INSTR_MIN_CHILD, ALIAS_EE_CALC_INSTR_MIN_CHILD);
            aliases.put(F_CALC_INSTR_MIN, ALIAS_EE_CALC_INSTR_MIN);
            aliases.put(F_CALC_MIN_SPED_OUTS_CHILD, ALIAS_EE_CALC_MIN_SPED_OUTS_CHILD);

            return aliases;
        }

        /**
         * alias mapping for next year case
         * map key it is alias(alias which doesn't depend on current/next year context)
         * value - real alias which use in next year context.
         *
         * @return Map
         */
        private static Map<String, String> getNextAliases() {
            Map<String, String> aliases = new HashMap<String, String>();
            aliases.put(F_AUTOCALC, NY_AUTOCALC);
            aliases.put(F_TOTAL_MIN_OUTS_GEN_ED, NY_CALC_TOTAL_MIN_OUTS_GEN_ED);
            aliases.put(F_TOTAL_MIN_GEN_ED, NY_CALC_TOTAL_MIN_GEN_ED);
            aliases.put(F_PERCENT_GEN_ED, NY_CALC_PERCENT_GEN_ED);
            aliases.put(F_MIN_SPENT_CHILD_PROG, NY_CALC_MIN_SPENT_CHILD_PROG);
            aliases.put(F_MIN_SPENT_OUTS_CHILD, NY_CALC_MIN_SPENT_OUTS_CHILD);
            aliases.put(F_SCHOOL_MPW, NY_IEP_SCHOOL_MPW);
            aliases.put(F_TOTAL_MIN_BELL_TO_BELL, NY_CALC_TOTAL_MIN_BELL_TO_BELL);
            aliases.put(F_NEED_SPED_CLASSES, NY_ENV_NEED_SPED_CLASSES);
            aliases.put(F_NEED_SPED_CLASSES_EXPL, NY_ENV_NEED_SPED_CLASSES_EXPL);
            aliases.put(F_NONACADEM_ACTIV, NY_ENV_NONACADEM_ACTIV);
            aliases.put(F_NONACADEM_ACTIV_EXPL, NY_ENV_NONACADEM_ACTIV_EXPL);
            aliases.put(F_ATTEND_IF_NONDIS, NY_ENV_ATTEND_IF_NONDIS);
            aliases.put(F_ATTEND_IF_NONDIS_EXPL, NY_ENV_ATTEND_IF_NONDIS_EXPL);
            aliases.put(F_INFORM_SIMILAR_SERV, NY_ENV_INFORM_SIMILAR_SERV);
            aliases.put(F_SPEC_TRANSP_BETWEEN_SCH, NY_ENV_SPEC_TRANSP_BETWEEN_SCH);
            aliases.put(F_SPEC_TRANSP_AROUND_SCH, NY_ENV_SPEC_TRANSP_AROUND_SCH);
            aliases.put(F_SPEC_EQUIP, NY_ENV_SPEC_EQUIP);
            aliases.put(F_TRANSP_PLAN, NY_ENV_TRANSP_PLAN);
            aliases.put(F_EXT_SCH_YEAR_NEED, NY_ENV_EXT_SCH_YEAR_NEED);
            aliases.put(F_CALC_INSTR_MIN_CHILD, ALIAS_NY_CALC_INSTR_MIN_CHILD);
            aliases.put(F_CALC_INSTR_MIN, ALIAS_NY_CALC_INSTR_MIN);
            aliases.put(F_CALC_MIN_SPED_OUTS_CHILD, ALIAS_NY_CALC_MIN_SPED_OUTS_CHILD);

            return aliases;
        }

        /**
         * try find next DistrictSchoolYearContext.
         *
         * @param current DistrictSchoolYearContext
         * @param broker X2Broker
         * @return District school year context
         */
        private DistrictSchoolYearContext getNextSchoolYearContext(DistrictSchoolYearContext current, X2Broker broker) {
            DistrictSchoolYearContext nextSchoolYearContext = null;
            Criteria criteria = new X2Criteria();
            criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                    Integer.valueOf(current.getSchoolYear() + 1));
            nextSchoolYearContext = (DistrictSchoolYearContext) broker
                    .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
            if (nextSchoolYearContext == null) {
                nextSchoolYearContext =
                        X2BaseBean.newInstance(DistrictSchoolYearContext.class, broker.getPersistenceKey());
                nextSchoolYearContext.setSchoolYear(current.getSchoolYear() + 1);
                nextSchoolYearContext.setStartDate(getNextYearDate(current.getStartDate()));
                nextSchoolYearContext.setEndDate(getNextYearDate(current.getEndDate()));
            }
            return nextSchoolYearContext;

        }

        /**
         * add + 1 year to input date.
         *
         * @param date PlainDate
         * @return Plain date
         */
        private PlainDate getNextYearDate(PlainDate date) {
            Calendar calendar = new GregorianCalendar();
            calendar.setTime(date);
            calendar.roll(Calendar.YEAR, true);
            return new PlainDate(calendar.getTime());
        }

        /**
         * try find start end date form School calendar for student and current PQSectionType.
         *
         * @param student SisStudent
         * @param helper IlSpedHelper
         * @param useDistrictCalendarIfNotFound if true and School calendar didn't find - use
         *        District Calendar
         * @return Pair
         */
        private Pair<PlainDate, PlainDate> getStartEndSchoolCalendarDates(SisStudent student,
                                                                          IlSpedHelper helper,
                                                                          boolean useDistrictCalendarIfNotFound) {
            Pair<PlainDate, PlainDate> dates = null;
            SchoolCalendarManager calendarManager = helper.getSchoolCalendarManager();
            int schoolYear = student.getOrganization1().getCurrentContext().getSchoolYear();
            // current case
            if (this.ordinal() == 0) {
                dates = calendarManager.getStartEndDateBySchoolYear(student, schoolYear, false);
            } else if (this.ordinal() == 1) {
                dates = calendarManager.getStartEndDateBySchoolYear(student, schoolYear + 1, false);


                if (dates.getLeft() == null || dates.getRight() == null) {
                    dates = calendarManager.getStartEndDateBySchoolYear(student, schoolYear, false);
                    if (dates.getLeft() != null && dates.getRight() != null) {
                        dates = Pair.of(getNextYearDate(dates.getLeft()),
                                getNextYearDate(dates.getRight()));
                    }
                }
            }
            if (useDistrictCalendarIfNotFound && (dates.getLeft() == null || dates.getRight() == null)) {
                DistrictSchoolYearContext currentContext = helper.getSchoolYearContextByYear(schoolYear);
                DistrictSchoolYearContext targetContext = getSchoolYearContext(currentContext, helper.getBroker());
                dates = Pair.of(targetContext.getStartDate(), targetContext.getEndDate());
            }


            return dates;
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
     * School Calendar manager for provide common method how to find appropriate SchoolCalendar data
     * It doesn't work with district school year calendar.
     *
     * @author Follett Software Company
     */
    public class SchoolCalendarManager {
        public static final String DEFAULT_CALENDAR_CODE = "Standard";
        private Map<String, SchoolCalendar> m_calendarMap = new HashMap<String, SchoolCalendar>();
        private Map<String, Pair<PlainDate, PlainDate>> m_calendarStartEndDates =
                new HashMap<String, Pair<PlainDate, PlainDate>>();

        /**
         * Instantiates a new school calendar manager.
         */
        public SchoolCalendarManager() {

        }

        /**
         * try find SchoolCalendar for student for input schoolYear.
         *
         * @param student SisStudent
         * @param schoolYear int
         * @return School calendar
         */
        public SchoolCalendar getSchoolCalendarByYear(SisStudent student, int schoolYear) {
            String key = getCalendarMapKey(student, schoolYear);
            SchoolCalendar calendar = m_calendarMap.get(key);
            if (calendar == null) {
                String schoolYearStr = String.valueOf(schoolYear);
                String schoolOid = getCurrentSchoolOid(student);

                String calendarCode = getCalendarCodeByYear(student, schoolYear);
                calendar = getSchoolCalendar(schoolOid, schoolYearStr, calendarCode, true);
                m_calendarMap.put(key, calendar);
            }
            return calendar;
        }

        /**
         * try find start and end date from SchoolCalendar for input student, schoolYear.
         *
         * @param student SisStudent
         * @param schoolYear int
         * @param insesionDates - search only in session dates
         * @return Pair
         */
        public Pair<PlainDate, PlainDate> getStartEndDateBySchoolYear(SisStudent student,
                                                                      int schoolYear,
                                                                      boolean insesionDates) {
            String key = getStartEndDatesMapKey(student, schoolYear, insesionDates);
            PlainDate startDate = null;
            PlainDate endDate = null;
            Pair<PlainDate, PlainDate> dates = m_calendarStartEndDates.get(key);
            if (dates == null) {
                SchoolCalendar calendar = getSchoolCalendarByYear(student, schoolYear);
                if (calendar != null) {
                    String calendarOid = calendar.getOid();
                    startDate = getDateFromCalendar(calendarOid, true, insesionDates);
                    endDate = getDateFromCalendar(calendarOid, false, insesionDates);
                }
                dates = Pair.of(startDate, endDate);
                m_calendarStartEndDates.put(key, dates);
            }
            return dates;
        }

        // if calendar will depend on year - change this method
        /**
         * get calendar code from student<br>
         * .
         *
         * @param student SisStudent
         * @param schoolYear int
         * @return String
         */
        private String getCalendarCodeByYear(SisStudent student, int schoolYear) {
            String calendarCode =
                    StringUtils.isEmpty(student.getCalendarCode()) ? DEFAULT_CALENDAR_CODE : student.getCalendarCode();
            return calendarCode;

        }

        // if manager will share map - make this method public
        /**
         * get key for m_calendarMap map.
         *
         * @param student SisStudent
         * @param schoolYear int
         * @return String
         */
        private String getCalendarMapKey(SisStudent student, int schoolYear) {
            String key = null;
            String schoolOid = getCurrentSchoolOid(student);
            String calendarCode = getCalendarCodeByYear(student, schoolYear);
            key = schoolOid + schoolYear + calendarCode;
            return key;
        }

        /**
         * get first date from calendar.
         *
         * @param calendarOid - calendar oid
         * @param sortAscending boolean
         * @param insesionDates - use only in session dates
         * @return Plain date
         */
        private PlainDate getDateFromCalendar(String calendarOid, boolean sortAscending, boolean insesionDates) {
            PlainDate returnDate = null;
            Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, calendarOid);
            if (insesionDates) {
                criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(insesionDates));
            }

            QueryByCriteria byCriteria = new QueryByCriteria(SchoolCalendarDate.class, criteria);
            byCriteria.addOrderBy(SchoolCalendarDate.COL_DATE, sortAscending);
            SchoolCalendarDate calendarDate = (SchoolCalendarDate) getBroker().getBeanByQuery(byCriteria);
            if (calendarDate != null) {
                returnDate = calendarDate.getDate();
            }
            return returnDate;
        }

        /**
         * try find SchoolCalendar by school oid, schoolYear, calendar code.
         *
         * @param schoolOid String
         * @param schoolYear String
         * @param calendarCode String
         * @param anyIfDidntFind if true and SchoolCalendar for calendarCode doesn't exist - try
         *        find any SchoolCalendar
         * @return School calendar
         */
        private SchoolCalendar getSchoolCalendar(String schoolOid,
                                                 String schoolYear,
                                                 String calendarCode,
                                                 boolean anyIfDidntFind) {
            SchoolCalendar returnCalendar = null;
            Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, schoolOid);
            criteria.addEqualTo(SchoolCalendar.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER
                    + DistrictSchoolYearContext.COL_SCHOOL_YEAR, schoolYear);
            criteria.addEqualTo(SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            QueryByCriteria queryByCriteria = new QueryByCriteria(SchoolCalendar.class, criteria);
            returnCalendar = (SchoolCalendar) getBroker().getBeanByQuery(queryByCriteria);
            if (returnCalendar == null && anyIfDidntFind) {
                criteria = new X2Criteria();
                criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, schoolOid);
                criteria.addEqualTo(SchoolCalendar.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER
                        + DistrictSchoolYearContext.COL_SCHOOL_YEAR, schoolYear);
                queryByCriteria = new QueryByCriteria(SchoolCalendar.class, criteria);
                returnCalendar = (SchoolCalendar) getBroker().getBeanByQuery(queryByCriteria);
            }
            return returnCalendar;
        }

        // if manager will share map - make this method public
        /**
         * get key for m_calendarStartEndDates map.
         *
         * @param student SisStudent
         * @param schoolYear int
         * @param insesionDates boolean
         * @return String
         */
        private String getStartEndDatesMapKey(SisStudent student, int schoolYear, boolean insesionDates) {
            String key = null;
            SchoolCalendar calendar = getSchoolCalendarByYear(student, schoolYear);
            String calendarOid;
            if (calendar == null) {
                calendarOid = "null";
            } else {
                calendarOid = calendar.getOid();
            }
            key = calendarOid + insesionDates;
            return key;
        }

    }

    /**
     * container which Wraps IepService<br>
     * and provide some new methods .
     *
     * @author Follett Software Company
     */
    public class ServiceContainer {
        private IepService m_service = null;
        private DataDictionary m_scDdx = null;

        /**
         * Instantiates a new service container.
         *
         * @param iepService IepService
         * @param ddx DataDictionary
         */
        public ServiceContainer(IepService iepService, DataDictionary ddx) {
            m_service = iepService;
            m_scDdx = ddx;
        }

        /**
         * Gets the duration integer.
         *
         * @return Integer
         */
        public Integer getDurationInteger() {
            return Integer.valueOf(m_service.getDuration());
        }

        /**
         * Gets the duration string.
         *
         * @return String
         */
        public String getDurationString() {
            return String.valueOf(getDurationInteger());
        }

        /**
         * Gets the field value by alias.
         *
         * @param alias String
         * @return Object
         */
        public Object getFieldValueByAlias(String alias) {
            return m_service.getFieldValueByAlias(alias, m_scDdx);
        }

        /**
         * Gets the iep service.
         *
         * @return Iep service
         */
        public IepService getIepService() {
            return m_service;
        }

        /**
         * Gets the ignored duration.
         *
         * @return String
         */
        public String getIgnoredDuration() {
            int ignoreDuration =
                    getIntegerByAlias(m_service, ALIAS_SRV_NOT_CALCULATE_MPW, ZERO_INTEGER, m_scDdx).intValue();
            return String.valueOf(ignoreDuration);
        }

        /**
         * Gets the service name.
         *
         * @return String
         */
        public String getServiceName() {
            return getServiceName(m_service);
        }

        /**
         * Gets the service name.
         *
         * @param iepService IepService
         * @return String
         */
        protected String getServiceName(IepService iepService) {
            String service = null;
            if (iepService.getServiceMode() != null
                    && iepService.getServiceMode().equals(SERVICE_MODE_RELATED_SERVICES)) {
                service = (String) iepService.getFieldValueByAlias(ALIAS_SERVICE_PROVIDER, getDictionary());
            } else if (iepService.getServiceMode() != null) {
                service = (String) iepService.getFieldValueByAlias(ALIAS_SERVICE_PROVIDER_OTHER, getDictionary());
            }

            if (StringUtils.isEmpty(service)) {
                service = (String) iepService.getFieldValueByAlias(ALIAS_SERVICE_DESCRIPTION, getDictionary());
            }
            return service;
        }



    }

    // Aliases
    public static final String ALIAS_SRV_NOT_CALCULATE_MPW = "srv-not-calculate-mpw";
    public static final String ALIAS_SERVICE_DESCRIPTION = "service-description";
    public static final String ALIAS_SERVICE_PROVIDER_OTHER = "service-provider-other";
    public static final String ALIAS_SERVICE_PROVIDER = "service-provider";
    public static final String ALIAS_IEP_SCHOOL_MPW = "iep-school-mpw";
    public static final String ALIAS_EE_AUTOCALC = "ee-autocalc";
    public static final String ALIAS_DOE_OUTPLACEMENT_DI = "DOE OUTPLACEMENT DI";
    public static final String ALIAS_DOE_SCHOOL_SERVICE = "DOE SCHOOL SERVICE";
    public static final String ALIAS_DSB_PRIORITY = "dsb-priority";
    public static final String ALIAS_EE_CALC_PERCENT_GEN_ED = "ee-calc-percent-gen-ed";
    public static final String ALIAS_EE_CALC_TOTAL_MIN_GEN_ED = "ee-calc-total-min-gen-ed";
    public static final String ALIAS_EE_MIN_SP_REG_EARLY_CHILD_PR = "ee-calc-min-spent-child-prog";
    public static final String ALIAS_EE_MIN_SP_REC_SPEC_ED_SERV = "ee-calc-min-spent-outs-child";
    public static final String ALIAS_EE_TOTAL_MIN = "ee-calc-total-min-bell-to-bell";
    public static final String ALIAS_EE_TOTAL_NUM_MIN_OUT_GEN_ED = "ee-calc-total-min-outs-gen-ed";
    public static final String ALIAS_ITM_COMMUNIC_MODE = "itm-communic-mode";
    public static final String ALIAS_ITM_INTERPRETER = "itm-interpreter";
    public static final String ALIAS_ITM_SURROGATE_PARENT = "itm-surrogate-parent";
    public static final String ALIAS_ITM_PRIMARY_LANGUAGE = "itm-primary-language";
    public static final String ALIAS_SERVICE_LOCATION_REGULAR = "service-location-regular";
    public static final String ALIAS_SERVICE_CY_DURATION_DATE = "service-cy-duration-date";
    public static final String ALIAS_ED_ENV_PLACE_OPTION_CONSIDER = "ed-env-place-option-consider";
    public static final String ALIAS_ED_ENV_HARMFUL_EFFECT = "ed-env-harmful-effect";
    public static final String ALIAS_ED_ENV_TEAM_ACCEPT_PLACE = "ed-env-team-accept-place";
    public static final String ALIAS_NY_CALC_INSTR_MIN = "ny-calc-instr-min";
    public static final String ALIAS_EE_CALC_INSTR_MIN = "ee-calc-instr-min";
    public static final String ALIAS_NY_CALC_INSTR_MIN_CHILD = "ny-calc-instr-min-child";
    public static final String ALIAS_EE_CALC_INSTR_MIN_CHILD = "ee-calc-instr-min-child";
    public static final String ALIAS_ENV_TRANSP_PLAN = "ed-env-transp-plan";
    public static final String ALIAS_ENV_SPEC_EQUIP = "ed-env-spec-equip";
    public static final String ALIAS_ENV_SPEC_TRANSP_AROUND_SCH = "ed-env-spec-transp-around-sch";
    public static final String ALIAS_ENV_SPEC_TRANSP_BETWEEN_SCH = "ed-env-spec-transp-between-sch";
    public static final String ALIAS_ENV_INFORM_SIMILAR_SERV = "ed-env-inform-similar-serv";
    public static final String ALIAS_ENV_ATTEND_IF_NONDIS_EXPL = "ed-env-attend-if-nondis-expl";
    public static final String ALIAS_ENV_ATTEND_IF_NONDIS = "ed-env-attend-if-nondis";
    public static final String ALIAS_ENV_NONACADEM_ACTIV_EXPL = "ed-env-nonacadem-activ-expl";
    public static final String ALIAS_ENV_NONACADEM_ACTIV = "ed-env-nonacadem-activ";
    public static final String ALIAS_ENV_NEED_SPED_CLASSES_EXPL = "ed-env-need-sped-classes-expl";
    public static final String ALIAS_ENV_NEED_SPED_CLASSES = "ed-env-need-sped-classes";
    public static final String ALIAS_EE_CALC_MIN_SPED_OUTS_CHILD = "ee-calc-min-sped-outs-child";
    public static final String ALIAS_NY_CALC_MIN_SPED_OUTS_CHILD = "ny-calc-min-sped-outs-child";
    private static final String ALIAS_ATTR_NY_ESY_RATIONAL = "attr-ny-esy-rational";
    private static final String ALIAS_ED_ENV_EXT_SCH_YEAR_NEED = "ed-env-ext-sch-year-need";

    public static final String F_CALC_INSTR_MIN = "calc-instr-min";
    public static final String F_CALC_INSTR_MIN_CHILD = "calc-instr-min-child";
    public static final String F_EXT_SCH_YEAR_NEED = "env-ext-sch-year-need";
    public static final String F_TRANSP_PLAN = "env-transp-plan";
    public static final String F_SPEC_EQUIP = "env-spec-equip";
    public static final String F_SPEC_TRANSP_AROUND_SCH = "env-spec-transp-around-sch";
    public static final String F_SPEC_TRANSP_BETWEEN_SCH = "env-spec-transp-between-sch";
    public static final String F_INFORM_SIMILAR_SERV = "env-inform-similar-serv";
    public static final String F_ATTEND_IF_NONDIS_EXPL = "env-attend-if-nondis-expl";
    public static final String F_ATTEND_IF_NONDIS = "env-attend-if-nondis";
    public static final String F_NONACADEM_ACTIV_EXPL = "env-nonacadem-activ-expl";
    public static final String F_NONACADEM_ACTIV = "env-nonacadem-activ";
    public static final String F_NEED_SPED_CLASSES_EXPL = "env-need-sped-classes-expl";
    public static final String F_NEED_SPED_CLASSES = "env-need-sped-classes";
    public static final String F_TOTAL_MIN_BELL_TO_BELL = "calc-total-min-bell-to-bell";
    public static final String F_SCHOOL_MPW = "iep-school-mpw";
    public static final String F_MIN_SPENT_OUTS_CHILD = "calc-min-spent-outs-child";
    public static final String F_MIN_SPENT_CHILD_PROG = "calc-min-spent-child-prog";
    public static final String F_PERCENT_GEN_ED = "calc-percent-gen-ed";
    public static final String F_TOTAL_MIN_GEN_ED = "calc-total-min-gen-ed";
    public static final String F_TOTAL_MIN_OUTS_GEN_ED = "calc-total-min-outs-gen-ed";
    public static final String F_AUTOCALC = "autocalc";
    private static final String F_CALC_MIN_SPED_OUTS_CHILD = "calc-min-sped-outs-child";

    public static final String NY_ENV_EXT_SCH_YEAR_NEED = "ny-env-ext-sch-year-need";
    public static final String NY_ENV_TRANSP_PLAN = "ny-env-transp-plan";
    public static final String NY_ENV_SPEC_EQUIP = "ny-env-spec-equip";
    public static final String NY_ENV_SPEC_TRANSP_AROUND_SCH = "ny-env-spec-transp-around-sch";
    public static final String NY_ENV_SPEC_TRANSP_BETWEEN_SCH = "ny-env-spec-transp-between-sch";
    public static final String NY_ENV_INFORM_SIMILAR_SERV = "ny-env-inform-similar-serv";
    public static final String NY_ENV_ATTEND_IF_NONDIS_EXPL = "ny-env-attend-if-nondis-expl";
    public static final String NY_ENV_ATTEND_IF_NONDIS = "ny-env-attend-if-nondis";
    public static final String NY_ENV_NONACADEM_ACTIV_EXPL = "ny-env-nonacadem-activ-expl";
    public static final String NY_ENV_NONACADEM_ACTIV = "ny-env-nonacadem-activ";
    public static final String NY_ENV_NEED_SPED_CLASSES_EXPL = "ny-env-need-sped-classes-expl";
    public static final String NY_ENV_NEED_SPED_CLASSES = "ny-env-need-sped-classes";
    public static final String NY_CALC_TOTAL_MIN_BELL_TO_BELL = "ny-calc-total-min-bell-to-bell";
    public static final String NY_IEP_SCHOOL_MPW = "ny-iep-school-mpw";
    public static final String NY_CALC_MIN_SPENT_OUTS_CHILD = "ny-calc-min-spent-outs-child";
    public static final String NY_CALC_MIN_SPENT_CHILD_PROG = "ny-calc-min-spent-child-prog";
    public static final String NY_CALC_PERCENT_GEN_ED = "ny-calc-percent-gen-ed";
    public static final String NY_CALC_TOTAL_MIN_GEN_ED = "ny-calc-total-min-gen-ed";
    public static final String NY_CALC_TOTAL_MIN_OUTS_GEN_ED = "ny-calc-total-min-outs-gen-ed";
    public static final String NY_AUTOCALC = "ny-autocalc";


    // Punctuation marks
    public static final String SPACE = " ";
    public static final String COLON = ":";
    public static final String EMPTY = "";
    public static final String ENTER = "\n";
    public static final String SEMICOLON = ";";
    public static final String COMMA = ",";
    public static final String PERCENT = "%";

    // Extended dictionary id
    public static final String EXTENDED_DICTIOANRY_ID_SPED_IL_IEP = "SPED-IL-IEP";

    public static final int REFERENCE_LOOKUP_CODE_DESCRIPTION = 5;


    // Keys for maps
    // Phones map
    public static final String KEY_CELL = "CELL";
    public static final String KEY_WORK = "WORK";
    public static final String KEY_HOME = "HOME";
    // Disability map
    public static final String DSBL_KEY_SECONDARY_NAMES = "secondaryNames";
    public static final String DSBL_KEY_SECONDARY_CODES = "secondaryCodes";
    public static final String DSBL_KEY_PRIMARY_NAME = "primaryName";
    public static final String DSBL_KEY_PRIMARY_CODE = "primaryCode";

    // service modes keys
    public static final String KEY_REL_SERV_OUTSIDE_GENERAL_ED = "relSerOutsideGenED";
    public static final String KEY_SUPPL_AIDS = "suplAids";
    public static final String KEY_SPED_OUTSIDE_GENERAL_ED = "spedOutsideGenED";
    public static final String KEY_SPED_REL_SERV_WITHIN_GENERAL_ED = "spedRelServWitinGenED";
    public static final String KEY_SPED_SERV = "spedServices";


    public static final String KEY_INITIATION_DATE = "initiationDate";
    public static final String KEY_LIMIT_START_DATE = "limitStartDate";
    public static final String KEY_LIMIT_END_DATE = "limitEndDate";
    public static final String KEY_DURATION_DATE = "durationDate";
    private static final String KEY_MAP_TOTAL_DURATIONS = "mapTotalDurations";

    // Team member roles
    public static final String MEMBER_ROLE_PARENT = "Parent";
    public static final String MEMBER_ROLE_GUARDIAN = "Guardian";

    // params
    public static final String PARAM_COMMUNICATION_MODE = "communicationMode";
    public static final String PARAM_INTERPRETER = "Interpreter";
    public static final String PARAM_MIN_SPENT_CHILDHOOD_PROG = "minSpentChildhoodProg";
    public static final String PARAM_MIN_SPENT_OUTSIDE_CHILDHOOD = "minSpentOutsideChildhood";
    public static final String PARAM_MIN_BELL_TO_BELL = "minBellToBell";
    public static final String PARAM_OTH_PERCENT = "preOTHPercent";
    public static final String PARAM_PRE_K_PERCENT = "preKPercent";
    public static final String PARAM_SURROGATE_PARENT = "SurrogateParent";
    public static final String PARAM_TOTAL_MIN_OUTSIDE_GENERAL_ED = "totalMinOutsideGeneralEd";
    public static final String PARAM_TOTAL_MIN_INSIDE_GENERAL_ED = "totalMinInsideGeneralEd";
    public static final String PARAM_RELATIONSHIP_CODE = "RelationshipCode";
    public static final String PARAM_PARENT = "parent";
    public static final String PARAM_PRIMARY_LANGUAGE = "primaryLanguage";
    public static final String PARAM_PLACEMENT_CONSIDERATIONS_LIST = "placementConsiderationsList";
    private static final String PARAM_OWNER = "owner";
    private static final String PARAM_OWNER_OID = "owner.oid";
    private static final String PARAM_TYPE = "type";
    private static final String PARAM_TITLE = "title";

    // service modes
    public static final String SERVICE_MODE_NO_SUPPLEMENTARY_AIDS = "No Supplementary Aids";
    public static final String SERVICE_MODE_RELATED_SERVICES = "Related Services";
    public static final String SERVICE_MODE_SPECIAL_EDUCATION = "Special Education";
    public static final String SERVICE_MODE_SUPPLEMENTARY_AIDS = "Supplementary Aids";

    private static final String CURRENT_CONTEXT_KEY = "currentContext";

    // keys for subreport
    private static final String GRID_FIELD_DURATION = "duration";
    private static final String GRID_FIELD_IGNORE_DURATION = "ignoreDuration";
    private static final String GRID_FIELD_SERVICE = "service";
    public static final String SUB_REPORT_ID_PQSEC1 = "SYS-SPED-IL-PQ-SEC1";
    public static final String SUB_REPORT_ID_PQESY = "SYS-SPED-IL-PQ-ESY";
    public static final String SUB_REPORT_ID_PQ1 = "SYS-SPED-IL-PQ-SUB1";
    public static final String SUB_REPORT_ID_PQ2 = "SYS-SPED-IL-PQ-SUB2";

    // other keys
    public static final String KEY_DATASOURCE = "datasource";
    public static final String KEY_FORMAT = "format";
    private static final String KEY_GOALS = "goals";
    private static final String KEY_MPV = "mpv";
    public static final String KEY_PARAMETERS_MAP = "PARAMETERS_MAP";


    // other
    public static final String SCHOOL_MPW_CODE_OTHER = "Other";
    protected static final String ENROLLMENT_STATUS_CODE_ACTIVE = "Active";
    protected static final String HISPANIC_OR_LATINO = "Hispanic or Latino; ";
    protected static final String PHONE_TYPE_UNKNOWN = "unknown";
    protected static final String SCHOOL_OUTPLACEMENT = "OUTPLACEMENT";
    protected static final Integer ZERO_INTEGER = Integer.valueOf(0);

    protected X2Broker m_broker;
    protected DataDictionary m_dictionary = null;
    protected Map<String, DataDictionary> m_dictionaryMap = new HashMap<String, DataDictionary>();
    protected Map<String, ReferenceCode> m_disabilityCode = null;
    protected Map<String, Pair<PlainDate, PlainDate>> m_firsLastCalendardates =
            new HashMap<String, Pair<PlainDate, PlainDate>>();



    protected String m_fieldStdEnrollOutplacement = null;
    protected String m_fieldStdSchoolOutplacement = null;
    protected static final SimpleDateFormat FORMATTER_TO = new SimpleDateFormat("M/d/yyyy");

    protected static final SimpleDateFormat FORMATTER_FROM = new SimpleDateFormat("yyyy-MM-dd");

    protected Map<PeriodSchoolYearContext, DistrictSchoolYearContext> m_yearContextMap =
            new HashMap<PeriodSchoolYearContext, DistrictSchoolYearContext>();



    /**
     * A map of maps of reference code.
     * The outer map is indexed by the reference table OID. It contains maps of reference codes.
     * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
     * code.
     */
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;


    private SchoolCalendarManager m_schoolCalendarManager = null;

    /**
     * return Ethnic for <code>person</code>.
     *
     * @param person Person
     * @return String
     */
    public String calculateEthnic(Person person) {
        StringBuilder returnValue = new StringBuilder();
        if (person != null) {
            if (person.getHispanicLatinoIndicator()) {
                returnValue.append(HISPANIC_OR_LATINO);
            }
            if (!StringUtils.isEmpty(person.getRaceView())) {
                returnValue.append(person.getRaceView());
            }
        }
        return returnValue.toString();
    }

    /**
     * Use FieldHelper instead this.
     *
     * @param beans Collection
     * @param fields Map<String,String>
     * @param includeObject boolean
     * @return List
     */
    @Deprecated
    public List<Map<String, Object>> createFiledMapping(Collection beans,
                                                        Map<String, String> fields,
                                                        boolean includeObject) {
        List<Map<String, Object>> returnList = new ArrayList<Map<String, Object>>();

        for (X2BaseBean baseBean : ((Collection<X2BaseBean>) beans)) {
            Map<String, Object> beanFields = new HashMap<String, Object>();
            if (includeObject) {
                beanFields.put(FieldHelper.KEY_BEAN, baseBean);
            }

            for (Entry<String, String> entry : fields.entrySet()) {
                Object fieldValue = baseBean.getFieldValueByBeanPath(entry.getKey());
                if (fieldValue instanceof PlainDate) {
                    fieldValue = formatDate(fieldValue);
                }
                beanFields.put(entry.getValue(), fieldValue);
            }
            returnList.add(beanFields);
        }
        return returnList;
    }

    /**
     * change date up or down by <code>calendarField</code>.
     *
     * @param date PlainDate
     * @param calendarField int
     * @param up boolean
     * @return PlainDate
     * @see Calendar#roll(int, int)
     */
    public PlainDate changeDate(PlainDate date, int calendarField, boolean up) {
        Calendar calendar = new GregorianCalendar();
        calendar.setTime(date);
        calendar.roll(calendarField, up);
        return new PlainDate(calendar.getTime());

    }

    /**
     * prepare Extended Year Service block on PQ form<br>
     * put key <code>extYearServiceFields<code> into <code>map</code> with value - new HashMap<br>
     * <br>
     * new HashMap contain next key - values<br>
     * service-description - String<br>
     * service-location-regular - String <br>
     * mpv - String <br>
     * startDate - String<br>
     * duration - String<br>
     * goals - String <br>
     * .
     *
     * @param iepData IepData
     * @param grid ReportDataGrid
     * @param map Map<String,Object>
     */
    public void fillExtendedYearServiceBlock(IepData iepData, ReportDataGrid grid, Map<String, Object> map) {
        Map<String, Object> mapForExtendedYear = new HashMap<String, Object>(map);
        mapForExtendedYear.put(PARAM_TITLE, PQSectionType.CURRENT.getTitle());
        mapForExtendedYear.put(PARAM_TYPE, PQSectionType.CURRENT.ordinal() + EMPTY);
        mapForExtendedYear.put(PARAM_OWNER, iepData);
        mapForExtendedYear.put(PARAM_OWNER_OID, iepData.getOid());
        mapForExtendedYear.put(ALIAS_ED_ENV_EXT_SCH_YEAR_NEED,
                iepData.getFieldValueByAlias(ALIAS_ED_ENV_EXT_SCH_YEAR_NEED, getDictionary()));
        mapForExtendedYear.put(ALIAS_ATTR_NY_ESY_RATIONAL,
                iepData.getFieldValueByAlias(ALIAS_ATTR_NY_ESY_RATIONAL, getDictionary()));

        Filter<IepService> filter = new Filter<IepService>(IepService.class);
        filter.addEqualTo(IepService.COL_EXTENDED_YEAR_INDICATOR, Boolean.TRUE);
        filter.addEqualTo(IepService.COL_SERVICE_MODE, IlSpedNameServiceMode.SPECIAL_EDUCATION.getMode());

        Collection<IepService> extYearServices = filter.applyFilter(iepData.getIepServices());

        FieldHelper fieldHelper = new FieldHelper(false);
        fieldHelper.markAlias(ALIAS_SERVICE_DESCRIPTION, ALIAS_SERVICE_DESCRIPTION);
        fieldHelper.markField(IepService.COL_DURATION, KEY_MPV);
        fieldHelper.markField(IepService.COL_START_DATE, null);
        fieldHelper.markAlias(ALIAS_SERVICE_CY_DURATION_DATE, GRID_FIELD_DURATION);
        fieldHelper.markField(IepService.COL_GOAL_VIEW, KEY_GOALS);
        List<Map<String, Object>> rowFieldsMap = fieldHelper.createMap(extYearServices);

        ReportDataGrid subgrid = new ReportDataGrid();
        if (rowFieldsMap.size() > 0) {
            for (Map<String, Object> row : rowFieldsMap) {
                subgrid.append();
                for (Entry<String, Object> entry : row.entrySet()) {
                    subgrid.set(entry.getKey(), entry.getValue());
                }
            }
        }

        for (int i = rowFieldsMap.size(); i < 4; i++) {
            subgrid.append();
        }


        subgrid.beforeTop();

        Report subreport = ReportUtils.getReport(SUB_REPORT_ID_PQESY, getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());

        grid.append();
        grid.set(KEY_DATASOURCE, subgrid);
        grid.set(KEY_FORMAT, format);
        grid.set(KEY_PARAMETERS_MAP, mapForExtendedYear);

    }

    /**
     * fill information about parent <code>teanNember</code><br>
     * key - value<br>
     * parent - StudentContact (parent in contacts)<br>
     * SurrogateParent - String<br>
     * Interpreter - String<br>
     * communicationMode - String<br>
     * itm-primary-language - String <br>
     * RelationshipCode - String<br>
     * + phones see fillPhones<br>
     * .
     *
     * @param teamMember IepTeamMember
     * @param information Map<String,Object>
     * @see #fillPhones(Person, Map)
     */
    public void fillParentInformation(IepTeamMember teamMember, Map<String, Object> information) {
        String personOid = teamMember.getPerson().getOid();
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, teamMember.getStudentOid());
        criteria.addEqualTo(StudentContact.REL_CONTACT + ModelProperty.PATH_DELIMITER +
                Contact.COL_PERSON_OID, personOid);
        StudentContact studentContact =
                (StudentContact) m_broker.getBeanByQuery(new QueryByCriteria(StudentContact.class, criteria));
        if (studentContact != null) {
            information.put(PARAM_PARENT, studentContact);

            information.put(PARAM_SURROGATE_PARENT,
                    teamMember.getFieldValueByAlias(ALIAS_ITM_SURROGATE_PARENT, getDictionary()));

            information.put(PARAM_INTERPRETER,
                    teamMember.getFieldValueByAlias(ALIAS_ITM_INTERPRETER, getDictionary()));

            information.put(PARAM_COMMUNICATION_MODE,
                    teamMember.getFieldValueByAlias(ALIAS_ITM_COMMUNIC_MODE, getDictionary()));

            information.put(ALIAS_ITM_PRIMARY_LANGUAGE,
                    teamMember.getFieldValueByAlias(ALIAS_ITM_PRIMARY_LANGUAGE, getDictionary()));

            Person personContact = studentContact == null ? null : studentContact.getPerson();
            fillPhones(personContact, information);
            information.put(PARAM_RELATIONSHIP_CODE, studentContact.getRelationshipCode());

        }
    }

    /**
     * prepare Participant Services block on PQ/facts form<br>
     * put into <code>map</code><br>
     * next key - value<br>
     * datasource - HashMap<String, JRDataSource>() (datasource for each - IlSpedNameGroupedServices
     * )
     * format - Map<String, InputStream> (InputStream for each - IlSpedNameGroupedServices)
     * mapTotalDurations - Map<String, String> (see getGroupedTotalDuration)
     * initiationDate - String
     * limitStartDate - PlainDate
     * durationDate - String
     * limitEndDate - PlainDate
     * + values form getEEMinutesMap() method.
     *
     * @param iepData IepData
     * @param map Map<String,Object>
     * @param type PQSectionType
     * @see ILSpedGroupsOfServices#getEEMinutesMap(IepData)
     * @see ILSpedGroupsOfServices#getGroupedTotalDuration()
     */
    public void fillParticipantServicesBlock(IepData iepData, Map<String, Object> map, PQSectionType type) {
        DistrictSchoolYearContext currentContext = getCurrentContext(map);

        ILSpedGroupsOfServices groupesOfServices = getGroupsOfServices(iepData, type, currentContext);

        Report subreport = ReportUtils.getReport(SUB_REPORT_ID_PQ1, getBroker());
        Report subreport2 = ReportUtils.getReport(SUB_REPORT_ID_PQ2, getBroker());

        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        ByteArrayInputStream format2 = new ByteArrayInputStream(subreport2.getCompiledFormat());
        Map<String, JRDataSource> datasourceMap = new HashMap<String, JRDataSource>();
        Map<String, InputStream> formatMap = new HashMap<String, InputStream>();

        // "noSuplAids","suplAids", "spedRelServWitinGenED", "spedOutsideGenED","relSerOutsideGenED"
        for (IlSpedGoupOfServices groupedServices : groupesOfServices.getAllGroupedServices()) {
            ReportDataGrid grid = new ReportDataGrid();
            String servicesKey = groupedServices.getGroupName().toString();

            for (ServiceContainer container : groupedServices.getContainers()) {
                grid.append();
                grid.set(GRID_FIELD_SERVICE, container.getServiceName());
                grid.set(GRID_FIELD_DURATION, container.getDurationString());
                grid.set(GRID_FIELD_IGNORE_DURATION, container.getIgnoredDuration());
                grid.beforeTop();
            }
            datasourceMap.put(servicesKey, grid);
            if (servicesKey.equals(KEY_REL_SERV_OUTSIDE_GENERAL_ED)
                    || servicesKey.equals(KEY_SPED_REL_SERV_WITHIN_GENERAL_ED)) {
                formatMap.put(servicesKey, format2);
            } else {
                formatMap.put(servicesKey, format);
            }

        }
        map.put(KEY_DATASOURCE, datasourceMap);
        map.put(KEY_FORMAT, formatMap);

        map.put(KEY_MAP_TOTAL_DURATIONS, groupesOfServices.getGroupedTotalDuration());
        //
        map.putAll(groupesOfServices.getEEMinutesMap(iepData));

        PlainDate iepSrvStartDate = type.getIepStartServiceDate(iepData, getDictionary());
        PlainDate iepSrvEndDate = type.getIepEndServiceDate(iepData, getDictionary());
        if (iepSrvStartDate == null || iepSrvEndDate == null) {
            iepSrvStartDate = groupesOfServices.getSmallestStartDate();
            iepSrvEndDate = groupesOfServices.getBiggestEndDate();
        }
        map.put(KEY_INITIATION_DATE, formatDate(iepSrvStartDate));
        map.put(KEY_LIMIT_START_DATE, iepSrvStartDate);


        map.put(KEY_DURATION_DATE, formatDate(iepSrvEndDate));
        map.put(KEY_LIMIT_END_DATE, iepSrvEndDate);
    }

    /**
     * fill phones into <code>information</code> for this <code>person</code> <br>
     * keys HOME,WORK,CELL.
     *
     * @param person Person
     * @param information Map<String,Object>
     */
    public void fillPhones(Person person, Map<String, Object> information) {
        Map<String, String> phones = getPhones(person);
        StringBuilder homePhone = new StringBuilder();
        StringBuilder workPhone = new StringBuilder();
        StringBuilder cellPhone = new StringBuilder();
        StringBuilder firstPosition = new StringBuilder();
        StringBuilder secondPosition = new StringBuilder();
        StringBuilder thirdPosition = new StringBuilder();
        int i = 0;
        for (Entry<String, String> entry : phones.entrySet()) {
            boolean foundKey = false;
            if (entry.getKey().contains(KEY_HOME)) {
                homePhone.append(StringUtils.isEmpty(entry.getValue()) ? EMPTY : entry.getValue() + SPACE);
                foundKey = true;
            } else if (entry.getKey().contains(KEY_WORK)) {
                workPhone.append(StringUtils.isEmpty(entry.getValue()) ? EMPTY : entry.getValue() + SPACE);
                foundKey = true;
            }
            if (entry.getKey().contains(KEY_CELL)) {
                cellPhone.append(StringUtils.isEmpty(entry.getValue()) ? EMPTY : entry.getValue() + SPACE);
            }
            // if fist phone is not KEY_HOME or KEY_WORK - count it like home phone(only in case if
            // home phone didn't find)
            if (i == 0 && !foundKey) {
                firstPosition.append(StringUtils.isEmpty(entry.getValue()) ? EMPTY : entry.getValue() + SPACE);
            }
            // if second phone is not KEY_HOME or KEY_WORK - count it like work phone (only in case
            // if home work didn't find)
            else if (i == 1 && !foundKey) {
                secondPosition.append(StringUtils.isEmpty(entry.getValue()) ? EMPTY : entry.getValue() + SPACE);
            } else if (i == 2 && !foundKey) {
                thirdPosition.append(StringUtils.isEmpty(entry.getValue()) ? EMPTY : entry.getValue() + SPACE);
            }
            i++;
        }

        if (homePhone.length() == 0) {
            homePhone = firstPosition;
        }
        if (workPhone.length() == 0) {
            workPhone = secondPosition;
        }
        if (cellPhone.length() == 0) {
            cellPhone = thirdPosition;
        }

        information.put(KEY_HOME, homePhone.toString());
        information.put(KEY_WORK, workPhone.toString());
        information.put(KEY_CELL, cellPhone.toString());

    }

    /**
     * prepare Placement Considerations block on PQ form<br>
     * put key <code>placementConsiderationsList<code> into <code>map</code> with value - new
     * HashMap<br>
     * <br>
     * new HashMap contain next key - value<br>
     * ed-env-place-option-consider - String<br>
     * ed-env-harmful-effect - String<br>
     * ed-env-team-accept-place - String<br>
     * .
     *
     * @param iepData IepData
     * @param map Map<String,Object>
     * @param type PQSectionType
     */
    public void fillPlacementCOnsiderationsBlock(IepData iepData, Map<String, Object> map, PQSectionType type) {
        DistrictSchoolYearContext currentContext = (DistrictSchoolYearContext) map.get(CURRENT_CONTEXT_KEY);
        if (currentContext == null) {
            currentContext = getSchoolYearContextByDate(new PlainDate());
        }
        Filter filter = new Filter(IepPlacement.class);
        filter.addGreaterOrEqualThan(IepPlacement.COL_START_DATE,
                type.getDistrictStartDate(currentContext, getBroker()));
        filter.addLessOrEqualThan(IepPlacement.COL_START_DATE, type.getDistrictEndDate(currentContext, getBroker()));
        Collection<X2BaseBean> placements = filter.applyFilter(iepData.getPlacements());
        FieldHelper fields = new FieldHelper(false);
        fields.markAlias(ALIAS_ED_ENV_PLACE_OPTION_CONSIDER, ALIAS_ED_ENV_PLACE_OPTION_CONSIDER);
        fields.markAlias(ALIAS_ED_ENV_HARMFUL_EFFECT, ALIAS_ED_ENV_HARMFUL_EFFECT);
        fields.markAlias(ALIAS_ED_ENV_TEAM_ACCEPT_PLACE, ALIAS_ED_ENV_TEAM_ACCEPT_PLACE);
        List<Map<String, Object>> listPlacementMap = fields.createMap(placements);
        map.put(PARAM_PLACEMENT_CONSIDERATIONS_LIST, listPlacementMap);
    }

    /**
     * fill pq sections
     * Participant Services
     * ExtendedYear Service
     * Placement Considerations
     * may be this can move to pq report java source???.
     *
     * @param iepData IepData
     * @param grid ReportDataGrid
     * @param map Map<String,Object>
     * @param type PQSectionType
     */
    public void fillSection(IepData iepData, ReportDataGrid grid, Map<String, Object> map, PQSectionType type) {
        Map<String, Object> mapForSection = new HashMap<String, Object>(map);
        mapForSection.put(PARAM_TITLE, type.getTitle());
        mapForSection.put(PARAM_TYPE, type.ordinal() + EMPTY);
        Report subreport = ReportUtils.getReport(SUB_REPORT_ID_PQSEC1, getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        JRDataSource subgrid = new PQDataSource(iepData, iepData, getDictionary(), getDefaultLocale(), type);
        fillParticipantServicesBlock(iepData, mapForSection, type);
        fillPlacementCOnsiderationsBlock(iepData, mapForSection, type);
        grid.append();
        grid.set(KEY_DATASOURCE, subgrid);
        grid.set(KEY_FORMAT, format);
        grid.set(KEY_PARAMETERS_MAP, mapForSection);


    }

    /**
     * use Filter instead this.
     *
     * @param beans Collection
     * @param filter Map<String,Object>
     * @return List
     */
    @Deprecated
    public List<X2BaseBean> filterBeans(Collection beans, Map<String, Object> filter) {
        List<X2BaseBean> resultCollection = new ArrayList<X2BaseBean>();
        for (X2BaseBean x2BaseBean : ((Collection<X2BaseBean>) beans)) {
            boolean isSuccess = true;
            for (Entry<String, Object> entry : filter.entrySet()) {
                Object valueFromIepService = x2BaseBean.getFieldValueByBeanPath(entry.getKey());
                if (valueFromIepService == null || !valueFromIepService.equals(entry.getValue())) {
                    isSuccess = false;
                    break;
                }
            }
            if (isSuccess) {
                resultCollection.add(x2BaseBean);
            }
        }
        return resultCollection;
    }

    /**
     * cast Object (String("yyyy-MM-dd") or Date/PlainDate into String with "M/d/yyyy" format.
     *
     * @param paramDate Object
     * @return String
     */
    public String formatDate(Object paramDate) {
        String returnValue = null;
        Date date = translateObjectToPlainDate(paramDate);
        if (date != null) {
            returnValue = FORMATTER_TO.format(date);
        }

        return returnValue;
    }

    /**
     * try cast value from alias to boolean<br>
     * working only for extended fields or fields with string type.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @return boolean
     */
    public boolean getBooleanByAlias(X2BaseBean baseBean, String alias) {
        boolean returnValue = getBooleanByAlias(baseBean, alias, getDictionary());
        return returnValue;
    }


    /**
     * try cast value from alias to boolean<br>
     * working only for extended fields or fields with string type.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return boolean
     */
    public static boolean getBooleanByAlias(X2BaseBean baseBean, String alias, DataDictionary dataDictionary) {
        String fieldValue = (String) baseBean.getFieldValueByAlias(alias, dataDictionary);
        if (fieldValue == null) {
            fieldValue = BooleanAsStringConverter.FALSE;
        }
        boolean returnValue = fieldValue.equals(BooleanAsStringConverter.TRUE) ? true : false;
        return returnValue;
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     */
    /*
     * public <T> List<T> getListFromMap(Map <String, List<T> > map, String key)
     * {
     * List<T> someList = map.get(key);
     * if (someList == null)
     * {
     * someList = new ArrayList();
     * map.put(key, someList);
     * }
     * return someList;
     * }
     */

    public X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the current school oid.
     *
     * @param student Student
     * @return String
     */
    public String getCurrentSchoolOid(Student student) {
        String schoolOid = null;
        StudentEnrollment stdEnrollment = getLastStudentEnrollment(student);
        if (stdEnrollment != null) {
            schoolOid = stdEnrollment.getSchoolOid();
        } else {
            schoolOid = student.getSchoolOid();
        }
        return schoolOid;

    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
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
     * try cast value form alias to String date with "M/d/yyyy" format<br>
     * working with String and Date type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    public String getDateByAlias(X2BaseBean bean, String alias) {
        return getDateByAlias(bean, alias, getDictionary());
    }

    /**
     * try cast value form alias to String date with "M/d/yyyy" format<br>
     * working with String and Date type.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return String
     */
    public String getDateByAlias(X2BaseBean baseBean, String alias, DataDictionary dictionary) {
        Object date = baseBean.getFieldValueByAlias(alias, dictionary);
        String returnValue = formatDate(date);
        return returnValue;
    }

    /**
     * return Default locale.
     *
     * @return Locale
     */
    public Locale getDefaultLocale() {
        String locale = LocalizationCache.getPrimaryLocale(getBroker().getPersistenceKey()).getLocale();
        return new Locale(locale);
    }

    /**
     * Gets the dictionary.
     *
     * @return DataDictionary
     */
    public DataDictionary getDictionary() {
        return m_dictionary;
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
     * return Disability for current IEP
     * key primaryCode - contain code for primary disability<br>
     * key primaryName - contain primary disability name<br>
     * key secondaryCodes - contain code/es for primary disability/ies<br>
     * key secondaryNames - contain primary disability/ies name/es<br>
     * .
     *
     * @param iepData IepData
     * @param onlyFirst if true - return only one disability for each type. (exist two types -
     *        primary and secondary)
     * @return Map
     */
    public Map<String, String> getDisabilities(IepData iepData, boolean onlyFirst) {
        boolean hasFirstSecondary = false;
        loadDisabilityCodes();
        Map<String, String> returnMap = new HashMap<String, String>();
        returnMap.put(DSBL_KEY_PRIMARY_CODE, EMPTY);
        returnMap.put(DSBL_KEY_PRIMARY_NAME, EMPTY);
        returnMap.put(DSBL_KEY_SECONDARY_CODES, EMPTY);
        returnMap.put(DSBL_KEY_SECONDARY_NAMES, EMPTY);
        List<String> secondaryCondesList = new ArrayList<String>();
        List<String> secondaryNamesList = new ArrayList<String>();

        TreeSet<IepDisability> sortingDisabilities = new TreeSet<IepDisability>(new Comparator<IepDisability>() {

            @Override
            public int compare(IepDisability o1, IepDisability o2) {
                DataDictionary dictionary = getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
                int priority1 = getIntegerByAlias(o1, ALIAS_DSB_PRIORITY, ZERO_INTEGER, dictionary).intValue();
                int priority2 = getIntegerByAlias(o2, ALIAS_DSB_PRIORITY, ZERO_INTEGER, dictionary).intValue();
                int compare = priority1 - priority2;
                if (compare == 0) {
                    compare = (o1.toString() + o1.getPrimaryIndicator())
                            .compareTo(o2.toString() + o2.getPrimaryIndicator());
                }
                return compare;
            }

        });
        sortingDisabilities.addAll(iepData.getIepDisability());

        for (IepDisability disability : sortingDisabilities) {

            String key = disability.getDisabilityCode() == null ? EMPTY : disability.getDisabilityCode();
            String localCode = m_disabilityCode.get(key) == null ? EMPTY : m_disabilityCode.get(key).getLocalCode();
            if (localCode != null) {
                if (disability.getPrimaryIndicator()) {
                    returnMap.put(DSBL_KEY_PRIMARY_CODE, localCode == null ? EMPTY : localCode);
                    returnMap.put(DSBL_KEY_PRIMARY_NAME, key == null ? EMPTY : key);
                } else if (!onlyFirst || !hasFirstSecondary) {
                    secondaryCondesList.add(localCode);
                    secondaryNamesList.add(key);
                    if (onlyFirst && !hasFirstSecondary) {
                        hasFirstSecondary = true;
                    }
                }
            }
        }
        String secondaryCodesDsbl = listToString(secondaryCondesList, COMMA + SPACE);
        String secondaryNamesDsbl = listToString(secondaryNamesList, COMMA + SPACE);
        returnMap.put(DSBL_KEY_SECONDARY_CODES, secondaryCodesDsbl == null ? EMPTY : secondaryCodesDsbl);
        returnMap.put(DSBL_KEY_SECONDARY_NAMES, secondaryNamesDsbl == null ? EMPTY : secondaryNamesDsbl);
        return returnMap;
    }

    /**
     * try find first/or latest in session date for <code>school</code> <code>calendarID</code>
     * <code>currentContext</code>.
     *
     * @param school School
     * @param calendarID String
     * @param currentContext DistrictSchoolYearContext
     * @param sortAscending boolean
     * @return School calendar date
     */
    public SchoolCalendarDate getFirstCalendarDateOrderedByDate(School school,
                                                                String calendarID,
                                                                DistrictSchoolYearContext currentContext,
                                                                boolean sortAscending) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_SCHOOL_OID, school.getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_CALENDAR_ID, calendarID);
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, currentContext.getOid());
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);


        QueryByCriteria byCriteria = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        byCriteria.addOrderBy(SchoolCalendarDate.COL_DATE, sortAscending);
        return (SchoolCalendarDate) getBroker().getBeanByQuery(byCriteria);
    }

    /**
     * return first, last in session dates. If not fount return not null Pair with null members
     * if dates not found and defaultCalendarId is not empty - try to find dates using
     * defaultCalendarId
     *
     * @param school School
     * @param calendarID String
     * @param currentContext DistrictSchoolYearContext
     * @param defaultCalendarId String
     * @return Pair
     */
    public Pair<PlainDate, PlainDate> getFirstLastInSessionDates(School school,
                                                                 String calendarID,
                                                                 DistrictSchoolYearContext currentContext,
                                                                 String defaultCalendarId) {
        String casKey = currentContext.getSchoolYear() + school.getOid() + calendarID;
        Pair<PlainDate, PlainDate> pair = m_firsLastCalendardates.get(casKey);
        if (pair == null) {
            SchoolCalendarDate firstDate = getFirstCalendarDateOrderedByDate(school, calendarID, currentContext, true);
            SchoolCalendarDate lastDate = null;
            if (firstDate == null && !StringUtils.isEmpty(defaultCalendarId)) {
                firstDate = getFirstCalendarDateOrderedByDate(school, defaultCalendarId, currentContext, true);
                lastDate = getFirstCalendarDateOrderedByDate(school, defaultCalendarId, currentContext, false);
            } else {
                lastDate = getFirstCalendarDateOrderedByDate(school, calendarID, currentContext, false);
            }
            if (firstDate != null && lastDate != null) {
                pair = Pair.of(firstDate.getDate(), lastDate.getDate());
            } else {
                pair = Pair.of(null, null);
            }
            m_firsLastCalendardates.put(casKey, pair);

        }
        return pair;
    }

    /**
     * trying to fill ILSpedGroupsOfServices container using iepServices which will filtered by
     * span(current and next year). Container make services grouped by
     * <code>IlSpedNameGroupedServices</code> type<br>
     * and provide methods for next level<br>
     * level ILSpedGroupsOfServices - common data about all services. ILSpedGroupsOfServices
     * represented data only about all services.<br>
     * ILSpedGroupsOfServices contain one or few group (list of gropes). It can return one group by
     * <code>IlSpedNameGroupedServices</code> key<br>
     * one group represented by the following class:
     * level ILSpedGroupOfServices- common data about services belong to one group.
     * ILSpedGroupOfServices represented data only about one group.<br>
     * ILSpedGroupOfServices can get list ServiceContainer which belong to current group.
     * level ServiceContainer - contain data for one service;
     *
     * @param iepData IepData
     * @param type - current and next year.
     * @param currentContext need to determine current and next year.
     * @return IL sped groups of services
     * @see IlSpedNameGroupedServices
     */
    @SuppressWarnings("synthetic-access")
    public ILSpedGroupsOfServices getGroupsOfServices(IepData iepData,
                                                      PQSectionType type,
                                                      DistrictSchoolYearContext currentContext) {


        Collection<IepService> iepServices = iepData.getIepServices();
        PlainDate limitStartDate = type.getSchoolCalendarStartDate(iepData.getStudent(), this, true);
        PlainDate limitEndDate = type.getSchoolCalendarEndDate(iepData.getStudent(), this, true);

        ILSpedGroupsOfServices groupedServices = new ILSpedGroupsOfServices(type, limitStartDate, limitEndDate);

        Filter filter = new Filter(IepService.class);

        filter.addEqualTo(IepService.COL_EXTENDED_YEAR_INDICATOR, Boolean.valueOf(false));
        if (type.equals(PQSectionType.NEXT)) {
            filter.addIsNotEmpty(translateAliasToJavaName("service-ny-initiation-date"));
        }
        if (type.equals(PQSectionType.CURRENT)) {
            filter.addIsNotEmpty(IepService.COL_START_DATE);
        }

        List<IepService> filteredServices = filter.applyFilter(iepServices);

        for (IepService iepService : filteredServices) {
            groupedServices.add(iepService);
        }
        return groupedServices;
    }

    /**
     * try cast value from alias to Integer<br>
     * working for extended field or fields with String type<br>
     * .
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param defaultValue Integer
     * @return Integer
     */
    public Integer getIntegerByAlias(X2BaseBean baseBean, String alias, Integer defaultValue) {
        Integer value = getIntegerByAlias(baseBean, alias, defaultValue, getDictionary());
        return value;
    }

    /**
     * try cast value from alias to Integer<br>
     * working for extended field or fields with String type<br>
     * .
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param defaultValue Integer
     * @param dictionary DataDictionary
     * @return Integer
     */
    public static Integer getIntegerByAlias(X2BaseBean baseBean,
                                            String alias,
                                            Integer defaultValue,
                                            DataDictionary dictionary) {
        String value = (String) baseBean.getFieldValueByAlias(alias, dictionary);
        return value != null ? Integer.valueOf(value) : defaultValue;
    }

    /**
     * return last secondary StudentSchool with "OUTPLACEMENT" name<br>
     * search limited by <code>schoolYearContextOid</code> or <code>date</code> .
     *
     * @param student Student
     * @param schoolYearContextOid String
     * @param date PlainDate
     * @return Student school
     */
    public StudentSchool getLastOutplacement(Student student, String schoolYearContextOid, PlainDate date) {
        boolean isSchoolContext = schoolYearContextOid == null ? false : true;
        boolean isDateContext = date == null ? false : true;
        StudentSchool lastStudentSchool = null;
        for (StudentSchool studentSchool : student.getStudentSchools()) {
            if (studentSchool.getType() == StudentSchool.SECONDARY
                    && studentSchool.getSchool().getName().equals(SCHOOL_OUTPLACEMENT)) {
                PlainDate startDate = studentSchool.getStartDate();
                PlainDate endDate = studentSchool.getEndDate();

                if (!isSchoolContext || studentSchool.getDistrictContextOid().equals(schoolYearContextOid)) {
                    if (!isDateContext ||
                            (startDate != null && !startDate.after(date)
                                    && (endDate == null || !endDate.before(date)))) {
                        if (lastStudentSchool == null
                                || studentSchool.getStartDate().after(lastStudentSchool.getStartDate())) {
                            lastStudentSchool = studentSchool;
                        }
                    }
                }

            }
        }
        return lastStudentSchool;
    }

    /**
     * return last Student Enrollment.
     *
     * @param student Student
     * @return Student enrollment
     */
    public StudentEnrollment getLastStudentEnrollment(Student student) {
        SisStudent sisStudent = (SisStudent) student;
        StudentEnrollment lastEnrollment = null;

        for (StudentEnrollment studentEnrollment : sisStudent.getEnrollments()) {
            if (studentEnrollment.getStatusCode().equals(ENROLLMENT_STATUS_CODE_ACTIVE)
                    && studentEnrollment.getEnrollmentType().equals(ENTRY)) {
                if (lastEnrollment == null
                        ||
                        studentEnrollment.getEnrollmentDate().after(lastEnrollment.getEnrollmentDate())
                        ||
                        (studentEnrollment.getEnrollmentDate().equals(lastEnrollment.getEnrollmentDate()) &&
                                studentEnrollment.getTimestamp() > lastEnrollment.getTimestamp())) {
                    lastEnrollment = studentEnrollment;
                }
            }
        }
        return lastEnrollment;
    }


    /**
     * if value by alias represent bean oid - method return bean founded by this oid<br>
     * it working for FieldO extended field.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @return X 2 base bean
     */
    public X2BaseBean getObjectByAlias(X2BaseBean baseBean, String alias) {
        X2BaseBean returnBean = getObjectByAlias(baseBean, alias, getDictionary());
        return returnBean;
    }

    /**
     * if value by alias represent bean oid - method return bean founded by this oid<br>
     * it working for FieldO extended field.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return X 2 base bean
     */
    public X2BaseBean getObjectByAlias(X2BaseBean baseBean, String alias, DataDictionary dataDictionary) {
        X2BaseBean returnBean = null;
        String objectOid = (String) baseBean.getFieldValueByAlias(alias, dataDictionary);
        if (objectOid != null && objectOid.length() > 3) {
            String prefix = objectOid.substring(0, 3);
            DataDictionaryTable table = dataDictionary.findDataDictionaryTableByPrefix(prefix);
            if (table != null) {
                returnBean = getBroker().getBeanByOid(table.getBeanClass(), objectOid);
            }
        }
        return returnBean;
    }

    /**
     * return information about team member with parent role<br>
     * or similar role, for example guardian.
     *
     * @param iepData IepData
     * @return List
     */
    public List<Map<String, Object>> getParentsInformation(IepData iepData) {
        List<Map<String, Object>> parents = new ArrayList<Map<String, Object>>(2);

        for (IepTeamMember teamMember : iepData.getTeamMembers()) {
            if (teamMember.getMemberRoleCode().equals(MEMBER_ROLE_PARENT)
                    || teamMember.getMemberRoleCode().equals(MEMBER_ROLE_GUARDIAN)) {

                Map<String, Object> information = new HashMap<String, Object>();

                fillParentInformation(teamMember, information);
                parents.add(information);
            }
        }
        return parents;
    }


    /**
     * try cast value from alias to PlainDate<br>
     * working only for <code>alias</code> which contain date in string format .
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @return Plain date
     */
    public PlainDate getPlainDateByBeenAlias(X2BaseBean baseBean, String alias) {
        PlainDate returnValue = getPlainDateByBeenAlias(baseBean, alias, getDictionary());
        return returnValue;
    }

    /**
     * try cast value from alias to PlainDate<br>
     * working only for <code>alias</code> which contain date in string format .
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return Plain date
     */
    public static PlainDate getPlainDateByBeenAlias(X2BaseBean baseBean, String alias, DataDictionary dictionary) {
        PlainDate returnValue = null;
        String value = (String) baseBean.getFieldValueByAlias(alias, dictionary);
        if (value != null && !value.isEmpty()) {
            try {
                returnValue = new PlainDate(FORMATTER_FROM.parse(value));
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return returnValue;
    }

    /**
     * try cast value from beanPath to PlainDate<br>
     * value for beanPath can has PlaneDate or String format .
     *
     * @param baseBean X2BaseBean
     * @param beanPath String
     * @return Plain date
     */
    public PlainDate getPlainDateByBeanPath(X2BaseBean baseBean, String beanPath) {
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
     * get District name for Resident school.
     *
     * @param studentEnrollment StudentEnrollment
     * @return String
     */
    public String getResidentDistrict(StudentEnrollment studentEnrollment) {
        return getResidentDistrict(studentEnrollment, false);
    }


    /**
     * get District name for Resident school.
     *
     * @param studentEnrollment StudentEnrollment
     * @param tryFindOutplacementFirst - if true try get value form federal code form outplacment
     *        field.<br>
     *        if not found using district form current school
     * @return String
     * @see #getResidentSchool(StudentEnrollment, boolean)
     */
    public String getResidentDistrict(StudentEnrollment studentEnrollment, boolean tryFindOutplacementFirst) {
        String returnValue = null;

        if (studentEnrollment != null) {
            if (tryFindOutplacementFirst) {

                String outPlacementName =
                        (String) studentEnrollment.getFieldValueByBeanPath(m_fieldStdEnrollOutplacement);
                returnValue = lookupReferenceCodeByBeanPath(StudentSchool.class,
                        m_fieldStdEnrollOutplacement,
                        outPlacementName,
                        ReferenceMapTypeCode.FEDERAL.ordinal());
            }
            if (returnValue == null || returnValue.isEmpty()) {
                returnValue = studentEnrollment.getSchool() == null ? null
                        : studentEnrollment.getSchool().getOrganization1().getName();
            }
            if (returnValue == null || returnValue.isEmpty()) {
                returnValue = studentEnrollment.getStudent().getSchool().getOrganization1().getName();
            }
        }
        return returnValue;
    }

    /**
     * return resident school name<br>
     * .
     *
     * @param studentEnrollment current student enrollment
     * @return String
     */
    public String getResidentSchool(StudentEnrollment studentEnrollment) {
        return getResidentSchool(studentEnrollment, false);
    }


    /**
     * return resident school name<br>
     * .
     *
     * @param studentEnrollment current student enrollment
     * @param tryFindOutplacementFirst if true - try get value form <code>DOE OUTPLACEMENT DI</code>
     *        alias<br>
     *        if not found - using from school from <code>studentEnrollment</code>
     * @return String
     */
    public String getResidentSchool(StudentEnrollment studentEnrollment, boolean tryFindOutplacementFirst) {
        String returnValue = null;

        if (studentEnrollment != null) {
            if (tryFindOutplacementFirst) {
                returnValue = (String) studentEnrollment.getFieldValueByAlias(ALIAS_DOE_OUTPLACEMENT_DI);
            }
            if (returnValue == null || returnValue.isEmpty()) {
                returnValue = studentEnrollment.getSchool() == null ? null : studentEnrollment.getSchool().getName();
            }
            if (returnValue == null || returnValue.isEmpty()) {
                returnValue = studentEnrollment.getStudent().getSchool().getName();
            }

        }
        return returnValue;
    }

    /**
     * Gets the school calendar manager.
     *
     * @return School calendar manager
     */
    public SchoolCalendarManager getSchoolCalendarManager() {
        if (m_schoolCalendarManager == null) {
            m_schoolCalendarManager = new SchoolCalendarManager();
        }
        return m_schoolCalendarManager;
    }


    /**
     * Gets the school year context by year.
     *
     * @param year int
     * @return District school year context
     */
    public DistrictSchoolYearContext getSchoolYearContextByYear(int year) {
        DistrictSchoolYearContext schoolYearContext = null;
        for (PeriodSchoolYearContext period : m_yearContextMap.keySet()) {
            if (period.isInYear(year)) {
                schoolYearContext = m_yearContextMap.get(period);
                break;
            }
        }
        if (schoolYearContext == null) {
            Criteria criteria = new X2Criteria();
            criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, String.valueOf(year));
            QueryByCriteria byCriteria = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
            schoolYearContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(byCriteria);
            if (schoolYearContext != null) {
                PeriodSchoolYearContext period =
                        new PeriodSchoolYearContext(schoolYearContext.getStartDate(), schoolYearContext.getEndDate(),
                                schoolYearContext.getSchoolYear());
                m_yearContextMap.put(period, schoolYearContext);
            }
        }

        return schoolYearContext;
    }

    /**
     * determine DistrictSchoolYearContext by date<br>
     * try find context where <code>date</code> between span start and end date context<br>
     * start and end date include in span .
     *
     * @param date PlainDate
     * @return District school year context
     */
    public DistrictSchoolYearContext getSchoolYearContextByDate(PlainDate date) {
        DistrictSchoolYearContext schoolYearContext = null;
        for (PeriodSchoolYearContext period : m_yearContextMap.keySet()) {
            if (period.isBetween(date)) {
                schoolYearContext = m_yearContextMap.get(period);
                break;
            }
        }
        if (schoolYearContext == null) {
            Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, date);
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, date);
            QueryByCriteria byCriteria = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
            schoolYearContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(byCriteria);
            if (schoolYearContext != null) {
                PeriodSchoolYearContext period =
                        new PeriodSchoolYearContext(schoolYearContext.getStartDate(), schoolYearContext.getEndDate(),
                                schoolYearContext.getSchoolYear());
                m_yearContextMap.put(period, schoolYearContext);
            }
        }

        return schoolYearContext;
    }

    /**
     * determine SchoolYearContextOid by date<br>
     * try find context where <code>date</code> between span start and end date context<br>
     * start and end date include in span .
     *
     * @param date PlainDate
     * @return String
     */
    public String getSchoolYearContextOidByDate(PlainDate date) {
        String returnValue = null;
        DistrictSchoolYearContext schoolYearContext = getSchoolYearContextByDate(date);
        if (schoolYearContext != null) {
            returnValue = schoolYearContext.getOid();
        }
        return returnValue;
    }

    /**
     * get District name for serving school.
     *
     * @param studentSchoolOutplacement StudentSchool
     * @param studentEnrollment StudentEnrollment
     * @return String
     * @see #getServingSchool(StudentSchool, StudentEnrollment)
     */
    public String getServingDistrict(StudentSchool studentSchoolOutplacement, StudentEnrollment studentEnrollment) {
        String returnValue = null;
        String outPlacementName = null;

        if (studentSchoolOutplacement != null) {
            School school = studentSchoolOutplacement.getSchool();
            if (school.getName().equals(SCHOOL_OUTPLACEMENT)) {

                outPlacementName =
                        (String) studentSchoolOutplacement.getFieldValueByBeanPath(m_fieldStdSchoolOutplacement);
                returnValue = lookupReferenceCodeByBeanPath(StudentSchool.class,
                        m_fieldStdSchoolOutplacement,
                        outPlacementName,
                        ReferenceMapTypeCode.FEDERAL.ordinal());

            }
        }

        if (outPlacementName == null || outPlacementName.isEmpty()) {
            returnValue = getResidentDistrict(studentEnrollment, true);
        }

        return returnValue;

    }


    /**
     * return serving school name<br>
     * information get form outplacement school and if it is doesn't exist - from resident school.
     *
     * @param studentSchoolOutplacement outplacment school if has. Outplacement it is school object
     *        with "OUTPLACEMENT" school name<br>
     *        real school name get by field school outplacement.
     * @param studentEnrollment current student enrollment
     * @return String
     */
    public String getServingSchool(StudentSchool studentSchoolOutplacement, StudentEnrollment studentEnrollment) {
        String returnValue = null;

        if (studentSchoolOutplacement != null) {
            if (studentSchoolOutplacement.getSchool().getName().equals(SCHOOL_OUTPLACEMENT)) {
                returnValue = (String) studentSchoolOutplacement.getFieldValueByBeanPath(m_fieldStdSchoolOutplacement);
            }
        }

        if (returnValue == null || returnValue.isEmpty()) {
            returnValue = getResidentSchool(studentEnrollment, true);
        }
        return returnValue;

    }


    /**
     * method for initialize common resources used on this helper.
     *
     * @param broker X2Broker
     * @param dictionary DataDictionary
     * @return true, if successful
     */
    public boolean initializeHelper(X2Broker broker, DataDictionary dictionary) {
        boolean returnValue = false;
        if (broker != null && dictionary != null) {
            m_broker = broker;
            m_dictionary = dictionary;
            returnValue = true;
        }
        initializeFields();
        return returnValue;
    }

    /**
     * method for initialize common resources used on this helper.
     *
     * @param broker X2Broker
     * @param extendedDictionaryID String
     * @return true, if successful
     */
    public boolean initializeHelper(X2Broker broker, String extendedDictionaryID) {
        boolean returnValue = false;
        if (broker != null && extendedDictionaryID != null) {
            m_broker = broker;
            m_dictionary = getDictionaryByExtendedDictionaryId(extendedDictionaryID);
            if (m_dictionary != null) {
                returnValue = true;
            }
        }
        initializeFields();
        return returnValue;
    }

    /**
     * build string from values in <code>list</code><br>
     * between values put <code>delimiter</code>.
     *
     * @param list List<String>
     * @param delimiter String
     * @return String
     */
    public String listToString(List<String> list, String delimiter) {
        StringBuilder returnValue = new StringBuilder();
        if (list != null) {

            for (int i = 0; i < list.size(); i++) {
                returnValue.append(list.get(i) == null ? SPACE : list.get(i));
                if (i != list.size() - 1) {
                    returnValue.append(delimiter);
                }
            }
        }
        return returnValue.toString();
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

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @return String
     */
    public String translateAliasToJavaName(String alias) {
        return translateAliasToJavaName(alias, getDictionary());
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return String
     */
    public String translateAliasToJavaName(String alias, DataDictionary dataDictionary) {
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
    public PlainDate translateObjectToPlainDate(Object value) {
        PlainDate returnValue = null;
        if (value == null) {
            returnValue = null;
        } else if (value instanceof PlainDate) {
            returnValue = (PlainDate) value;
        } else if (value instanceof Date) {
            returnValue = new PlainDate((Date) value);
        } else if (value instanceof String && !((String) value).isEmpty()) {
            try {
                returnValue = new PlainDate(FORMATTER_FROM.parse((String) value));
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        return returnValue;
    }

    /**
     * Gets the phones.
     *
     * @param person Person
     * @return Map<String, String>, key prefix "1", "2" or "3" + phoneType , value phone
     *         if StudentContact has few the same phoneTypes
     */
    protected Map<String, String> getPhones(Person person) {

        Map<String, String> phones = new TreeMap<String, String>();

        if (person != null) {
            // first phone
            String phoneType = person.getFieldA001();
            phoneType = phoneType == null ? PHONE_TYPE_UNKNOWN : phoneType;
            phoneType = 1 + phoneType;
            phones.put(phoneType, person.getPhone01());

            // second phone
            phoneType = person.getFieldA002();
            phoneType = phoneType == null ? PHONE_TYPE_UNKNOWN : phoneType;
            phoneType = 2 + phoneType;
            phones.put(phoneType, person.getPhone02());

            // third phone
            phoneType = person.getFieldA003();
            phoneType = phoneType == null ? PHONE_TYPE_UNKNOWN : phoneType;
            phoneType = 3 + phoneType;
            phones.put(phoneType, person.getPhone03());
        }

        return phones;
    }

    /**
     * initialize fields<br>
     * in this method you can initialize fields name from alias name.
     */
    protected void initializeFields() {
        m_fieldStdEnrollOutplacement = translateAliasToJavaName(ALIAS_DOE_SCHOOL_SERVICE);
        m_fieldStdSchoolOutplacement = translateAliasToJavaName(ALIAS_DOE_OUTPLACEMENT_DI);

    }

    /**
     * initialize m_disabilityCode map.
     */
    protected void loadDisabilityCodes() {
        if (m_disabilityCode == null || m_disabilityCode.isEmpty()) {
            if (m_broker == null) {
                m_broker = getBroker();
            }

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField conductionActionCodeField =
                    dictionary.findDataDictionaryField(IepDisability.class.getName(),
                            IepDisability.COL_DISABILITY_CODE);

            ReferenceTable refTable = conductionActionCodeField.getReferenceTable();
            if (refTable == null) {
                m_disabilityCode = new HashMap<String, ReferenceCode>();
            } else {
                m_disabilityCode = refTable.getCodeMap(m_broker);
            }
        }
    }

    /**
     * return District School Year Context form map<br>
     * map get form <code>getParameters</code> method in <code>ToolJavaSource</code><br>
     * when formReportJavaSource initialize map can contain key <code>currentContext</code> with
     * current District School Year Context value.
     *
     * @param map Map<String,Object>
     * @return District school year context
     */
    private DistrictSchoolYearContext getCurrentContext(Map<String, Object> map) {
        DistrictSchoolYearContext currentContext = (DistrictSchoolYearContext) map.get(CURRENT_CONTEXT_KEY);
        if (currentContext == null) {
            currentContext = getSchoolYearContextByDate(new PlainDate());
        }
        return currentContext;
    }

}

