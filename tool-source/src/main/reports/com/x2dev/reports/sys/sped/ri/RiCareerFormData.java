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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisGenericFormData;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class RiCareerFormData extends BaseFormReportJavaSource {

    /**
     *
     * it was needed when embedded list had fixed size and I can get data use next expression:<br>
     * emb:transitionAssessments.a:crrpln-ch-method.1 <br>
     * where emb: prefix which mean that data get form embedded list<br>
     * transitionAssessments - embedded ID<br>
     * a:crrpln-ch-method - alias in child bean<br>
     * 1 number of row. Start form 1.<br>
     * It can be used instead subreports.<br>
     * method getAge still used<br>
     * but now embedded size is dynamic and values for embedded list filled by fillWorkExperiences
     * and fillTranstionAssessments method
     * I don't delete this class because in can be useful in future for other reports and it can be
     * copied
     * 
     * @author Follett Software Company
     */
    public class RiFormDataSource extends SimpleBeanDataSource {
        private static final String REGEXP_POINT = "\\.";
        private static final String ALIAS_PREFIX_CRRPLN_CH_DATE = "crrpln-ch-date";
        private static final String OWNER_PREFIX = "owner.";
        private static final String JAVA_PREFIX = "java:";
        private static final String EMBEDDED_PREFIX = "emb:";

        private Map<String, List<GenericFormChildData>> m_embMap;

        private X2BaseBean m_formOwner;

        /**
         * Constructs a new SimpleFormDataSource.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public RiFormDataSource(X2BaseBean formStorage, X2BaseBean formOwner, DataDictionary dictionary,
                Locale locale) {
            super(formStorage, dictionary, locale);
            m_embMap = new HashMap<String, List<GenericFormChildData>>();
            m_formOwner = formOwner;
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            /*
             * If the field starts with the owner prefix, set the current bean for value retrieval
             * to
             * the owner bean. Also strip off the owner prefix (note the special handling for doing
             * this
             * if a prefix is present - e.g. a:owner.alias)
             */
            X2BaseBean storageBean = getCurrentBean();
            Object fieldValue = null;
            if (fieldName.startsWith(EMBEDDED_PREFIX)) {
                String embeddedPath = fieldName.substring(EMBEDDED_PREFIX.length());
                String pathes[] = embeddedPath.split(REGEXP_POINT);
                if (pathes.length == 3) {
                    String embeddedId = pathes[0];
                    String embeddedField = pathes[1];
                    int embeddedLine;
                    try {
                        embeddedLine = Integer.parseInt(pathes[2].trim());
                    } catch (NumberFormatException e) {
                        embeddedLine = 0;
                    }
                    if (embeddedLine != 0) {
                        fieldValue = getValueFromEmbedded(embeddedId, embeddedField, embeddedLine);
                    }

                }
            } else {
                fieldValue = getValueFromBean(storageBean, fieldName);
            }
            setCurrentBean(storageBean);

            return fieldValue;
        }

        /**
         * calculate age between dates.
         *
         * @param birthday PlainDate
         * @param toDay PlainDate
         * @return int
         */
        private int calculateAge(PlainDate birthday, PlainDate toDay) {
            int age = -1;
            if (!birthday.after(toDay)) {
                Calendar cDob = Calendar.getInstance();
                Calendar cToDay = Calendar.getInstance();
                cToDay.setTime(toDay);
                cDob.setTime(birthday);
                // include day of birth
                cDob.add(Calendar.DAY_OF_MONTH, -1);

                age = cToDay.get(Calendar.YEAR) - cDob.get(Calendar.YEAR);
                if (cToDay.get(Calendar.DAY_OF_YEAR) <= cDob.get(Calendar.DAY_OF_YEAR)) {
                    age--;
                }
            }
            return age;
        }

        /**
         * return age for current student.
         *
         * @param formData SisGenericFormData
         * @return String
         */
        @SuppressWarnings("unused")
        private String getAge(SisGenericFormData formData) {
            String age = null;
            IepData iepData = (IepData) m_formOwner;
            PlainDate dob = iepData.getStudent().getPerson().getDob();
            if (dob != null) {
                PlainDate currentDate = new PlainDate(new Date());
                age = String.valueOf(calculateAge(dob, currentDate));
            }
            return age;

        }

        // 9/27/2016 this method was changed and it return only one date.
        // I stay this implementation because in future it can return multiple dates
        /**
         * return dates for <code>dates</code> column in "My Transition Assessments table"
         * it used in java reflection.
         *
         * @param child GenericFormChildData
         * @return Object
         */
        @SuppressWarnings("unused")
        private Object getDates(GenericFormChildData child) {
            StringBuilder builder = new StringBuilder();
            for (int i = 1; i < 2; i++) {
                String date = getDateByAlias(child, ALIAS_PREFIX_CRRPLN_CH_DATE + i, getDictionary());
                if (!StringUtils.isEmpty(date)) {
                    builder.append(date/* + "; " */);
                }
            }
            return builder.toString();
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
        private String getDateByAlias(X2BaseBean baseBean, String alias, DataDictionary dictionary) {
            String date = (String) baseBean.getFieldValueByAlias(alias, dictionary);
            ModelProperty property = new ModelProperty(dictionary.findDataDictionaryFieldByAlias(alias), dictionary);
            date = WebUtils.getPropertyAsString(date, property, Locale.getDefault(), null);
            return date;
        }

        /**
         * Gets the value from bean.
         *
         * @param baseBean X2BaseBean
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getValueFromBean(X2BaseBean baseBean, String fieldName) throws X2BaseException {
            String fieldNameCopy = fieldName;
            Object fieldValue = null;
            if (fieldName.startsWith(OWNER_PREFIX)) {
                setCurrentBean(m_formOwner);
                fieldNameCopy = fieldName.substring(OWNER_PREFIX.length());
            } else if (fieldName.matches(ALIAS_PREFIX + ":" + OWNER_PREFIX + ".*")) {
                setCurrentBean(m_formOwner);
                int separatorIndex = fieldName.indexOf(":");
                fieldNameCopy = fieldName.substring(0, separatorIndex + 1)
                        + fieldName.substring(fieldName.indexOf(OWNER_PREFIX) + OWNER_PREFIX.length());
            } else if (fieldName.startsWith(ALIAS_PREFIX)) {
                setCurrentBean(baseBean);

            }
            if (fieldName.startsWith(JAVA_PREFIX)) {
                fieldNameCopy = fieldName.substring(JAVA_PREFIX.length());
                try {
                    String methodName =
                            "get" + fieldNameCopy.substring(0, 1).toUpperCase() + fieldNameCopy.substring(1);
                    Method method =
                            RiFormDataSource.class.getDeclaredMethod(methodName, new Class[] {baseBean.getClass()});

                    fieldValue = method.invoke(this, baseBean);

                } catch (NoSuchMethodException e) {
                    // TODO Auto-generated catch block
                } catch (SecurityException e) {
                    // TODO Auto-generated catch block
                } catch (IllegalAccessException e) {
                    // TODO Auto-generated catch block
                } catch (IllegalArgumentException e) {
                    // TODO Auto-generated catch block
                } catch (InvocationTargetException e) {
                    // TODO Auto-generated catch block
                }
            } else {
                fieldValue = super.getFieldValue(fieldNameCopy);
            }
            return fieldValue;
        }

        /**
         * calculate value from child in embedded table.
         *
         * @param emdId String
         * @param fieldName String
         * @param numberLine int
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getValueFromEmbedded(String emdId, String fieldName, int numberLine) throws X2BaseException {
            Object fieldValue = null;
            List<GenericFormChildData> listRow = m_embMap.get(emdId);
            if (listRow == null) {
                listRow = pupulateEmbeddedList(emdId);
                m_embMap.put(emdId, listRow);
            }
            if (listRow.size() >= numberLine) {
                GenericFormChildData row = listRow.get(numberLine - 1);
                if (row != null) {
                    fieldValue = getValueFromBean(row, fieldName);
                }
            }
            return fieldValue;
        }

        /**
         * find all children for <code>embId</code>.
         *
         * @param embId - id for embedded
         * @return List
         */
        private List<GenericFormChildData> pupulateEmbeddedList(String embId) {
            GenericFormData formData = (GenericFormData) getCurrentBean();
            List<GenericFormChildData> childs = new ArrayList<GenericFormChildData>();
            for (GenericFormChildData child : formData.getGenericFormDataChildren()) {
                String embeddedType = (String) child.getFieldValueByAlias(EMBEDDED_FILTER_FIELD, getDictionary());
                if (!StringUtils.isEmpty(embeddedType) && embeddedType.equals(getEmbeddedIdFilter().get(embId))) {
                    childs.add(child);
                }
            }
            return childs;
        }
    }

    /**
     * data source for list of beans.
     * can call java method using "java:methodName" like field name.
     * In this case method should have one parameter - bean
     * bean it is current element in list of beans
     * 
     * @author Follett Software Company
     */
    public class RiMultipleFormDataSource extends BeanCollectionDataSource {

        private static final String ALIAS_CRRPLN_CH_ANTICIPATED_DATES = "crrpln-ch-anticipated-dates";
        private static final String COMMA = ",";
        private static final String JAVA_PREFIX = "java:";
        private static final String PREFIX_GET = "get";

        /**
         * Instantiates a new ri multiple form data source.
         *
         * @param beanCollection Collection<? extends X2BaseBean>
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public RiMultipleFormDataSource(Collection<? extends X2BaseBean> beanCollection, DataDictionary dictionary,
                Locale locale) {
            super(beanCollection, dictionary, locale);
            // TODO Auto-generated constructor stub
        }


        /**
         * @see com.follett.fsc.core.k12.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {

            return getValueFromBean(getCurrentBean(), fieldName);
        }

        /**
         * Gets the value from bean.
         *
         * @param baseBean X2BaseBean
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getValueFromBean(X2BaseBean baseBean, String fieldName) throws X2BaseException {
            String fieldNameCopy = fieldName;
            Object fieldValue = null;

            if (fieldName.startsWith(JAVA_PREFIX)) {
                fieldNameCopy = fieldName.substring(JAVA_PREFIX.length());
                try {
                    String methodName =
                            PREFIX_GET + fieldNameCopy.substring(0, 1).toUpperCase() + fieldNameCopy.substring(1);
                    Method method = RiMultipleFormDataSource.class.getDeclaredMethod(methodName,
                            new Class[] {baseBean.getClass()});

                    fieldValue = method.invoke(this, baseBean);

                } catch (NoSuchMethodException e) {
                    // TODO Auto-generated catch block
                } catch (SecurityException e) {
                    // TODO Auto-generated catch block
                } catch (IllegalAccessException e) {
                    // TODO Auto-generated catch block
                } catch (IllegalArgumentException e) {
                    // TODO Auto-generated catch block
                } catch (InvocationTargetException e) {
                    // TODO Auto-generated catch block
                }
            } else {
                fieldValue = super.getFieldValue(fieldNameCopy);
            }
            return fieldValue;
        }

        /**
         * Gets the anticipated dates.
         *
         * @param child GenericFormChildData
         * @return String
         */
        @SuppressWarnings("unused")
        private String getAnticipatedDates(GenericFormChildData child) {
            StringBuilder anticipatedDates = new StringBuilder();
            String dates = (String) child.getFieldValueByAlias(ALIAS_CRRPLN_CH_ANTICIPATED_DATES, getDictionary());
            if (!StringUtils.isEmpty(dates)) {
                String[] splitDates = dates.split(COMMA);
                for (String date : splitDates) {
                    if (!StringUtils.isEmpty(date)) {
                        if (anticipatedDates.length() > 0) {
                            anticipatedDates.append("\n");
                        }
                        anticipatedDates.append(date);
                    }
                }

            }

            return String.valueOf(anticipatedDates);

        }

    }

    private static final String EMBEDDED_FILTER_FIELD = "crrpln-ch-embedded-type";
    private static final String SUB_REPORT_ID_CRR_SUB1 = "SYS-SPED-RI-CRR-SUB1";
    private static final String SUB_REPORT_ID_CRR_SUB2 = "SYS-SPED-RI-CRR-SUB2";
    private static final String TRANSITION_ASSESSMENTS_FILTER = "Transition Assessments";
    private static final String TRANSITION_ASSESSMENTS_ID = "transitionAssessments";
    private static final String WORK_EXPERIENCE_FILTER = "Work Experiences";
    private static final String WORK_EXPERIENCE_ID = "workExperiences";
    private boolean m_isBlank = false;

    private Map<String, String> m_embIdFilter = new HashMap<String, String>();
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        if (getFormStorage() == null || StringUtils.isEmpty(getFormStorage().getOid())) {
            m_isBlank = true;
        }

        fillTranstionAssessments();
        fillWorkExperiences();
        return new RiFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * add into <code>list</code> new GenreicFormChildData while list size less then limit.
     *
     * @param childs List<GenericFormChildData>
     * @param limit int
     */
    private void addEmptyChild(List<GenericFormChildData> childs, int limit) {
        int listSize = childs.size();
        if (listSize < limit) {
            GenericFormChildData child =
                    X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());

            for (int i = 0; i < limit - listSize; i++) {
                childs.add(child);
            }
        }
    }

    /**
     * fill workExperiences emebedded list.
     */
    private void fillWorkExperiences() {

        Report subreport = ReportUtils.getReport(SUB_REPORT_ID_CRR_SUB2, getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        addParameter("weFormat", format);
        List<GenericFormChildData> list = pupulateEmbeddedList(WORK_EXPERIENCE_ID);
        addEmptyChild(list, m_isBlank ? 6 : 1);
        addParameter("weDataSource", new RiMultipleFormDataSource(list, getDictionary(), getLocale()));
    }

    /**
     * fill transitionAssessments embedded list.
     */
    private void fillTranstionAssessments() {
        Report subreport = ReportUtils.getReport(SUB_REPORT_ID_CRR_SUB1, getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        addParameter("taFormat", format);
        List<GenericFormChildData> list = pupulateEmbeddedList(TRANSITION_ASSESSMENTS_ID);
        addEmptyChild(list, 4);
        addParameter("taDataSource", new BeanCollectionDataSource(list, getDictionary(), getLocale()));
    }

    /**
     * find all children for <code>embId</code>.
     *
     * @param embId - id for embedded
     * @return List
     */
    private List<GenericFormChildData> pupulateEmbeddedList(String embId) {
        GenericFormData formData = (GenericFormData) getFormStorage();
        List<GenericFormChildData> childs = new ArrayList<GenericFormChildData>();
        for (GenericFormChildData child : formData.getGenericFormDataChildren()) {
            String embeddedType = (String) child.getFieldValueByAlias(EMBEDDED_FILTER_FIELD, getDictionary());
            if (!StringUtils.isEmpty(embeddedType) && embeddedType.equals(getEmbeddedIdFilter().get(embId))) {
                childs.add(child);
            }
        }
        return childs;
    }

    /**
     * each embedded list has unique value for "crrpln-ch-embedded-type" alias
     * this map provide key - embedded id, value - appropriate value for "crrpln-ch-embedded-type".
     *
     * @return Map
     */
    Map<String, String> getEmbeddedIdFilter() {
        if (m_embIdFilter.isEmpty()) {
            m_embIdFilter.put(TRANSITION_ASSESSMENTS_ID, TRANSITION_ASSESSMENTS_FILTER);
            m_embIdFilter.put(WORK_EXPERIENCE_ID, WORK_EXPERIENCE_FILTER);
        }
        return m_embIdFilter;
    }
}
