/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Connecticut SPED state report for SEDAC-G Report export.
 * This class implements the data export for SEDAC-G Report export.
 *
 * @author X2 Development Corporation
 */
public class SedacG extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SEDAC-G export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SedacGEntity extends StateReportEntity {
        /**
         * The student for this entity
         */
        private SisStudent m_student = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SedacGEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_student.getNameView() +
                    " [LASID: " + m_student.getLocalId() +
                    ", SASID: " + m_student.getStateId() + "]";
            return name;
        }

        /**
         * Returns the student that owns this form.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports#initialize()
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            GenericFormData genericFormData = (GenericFormData) bean;
            FormInstance instance = ((SedacG) data).m_formMap.get(genericFormData.getOid());
            m_student = (SisStudent) instance.getOwnerObject();
        }
    }

    /*
     * Parameters.
     */
    protected static final String ALIAS_SEDACG_START = "sedacg-contract-start";
    protected static final String ALIAS_SEDACG_END = "sedacg-contract-end";
    protected static final String ID_SEDACG_FORM = "SEDACG";
    protected static final String PARAM_DISABILITY_REF_TABLE = "rtbCTSpedDisab";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /*
     * Map of form instances by the storage's oid (which is generic form data), used in retrieving
     * the student's information
     */
    protected String m_contractStart;
    protected String m_contractEnd;
    protected Map<String, FormInstance> m_formMap;
    protected PlainDate m_reportDate;
    protected DateFormat SYSTEM_STRING_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * Class RetrieveAliasValue is the FieldRetriever used to retrieve alias values.
     * If the alias is associated with a reference table, that value is retrieved,
     * otherwise the field value itself is retrieved.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAliasValue implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SedacG sedacG = (SedacG) data;
            String alias = (String) field.getParameter();

            String beanField = translateAliasToJavaName(alias, true);
            GenericFormData gfd = (GenericFormData) ((SedacGEntity) entity).getBean();

            if (beanField != null) {
                value = sedacG.getPropertyAsJavaType(gfd, beanField);
            }
            return value;
        }
    }

    /**
     * Class RetrieveValueByBeanPath is the FieldRetriever used to retrieve
     * the value requested by the Calc Id of the SEDAC-G field, which becomes
     * translated into the beanPath.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveValueByBeanPath implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            SedacGEntity sedacGEntity = (SedacGEntity) entity;
            Object value = null;
            String beanPath = (String) field.getParameter();
            value = sedacGEntity.getStudent().getFieldValueByBeanPath(beanPath);
            return value;
        }
    }

    /**
     * Class RetrieveValueByBeanPath is the FieldRetriever used to retrieve
     * the value requested by the Calc Id of the SEDAC-G field, which becomes
     * translated into the beanPath.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudentAliasValue implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            SedacGEntity sedacGEntity = (SedacGEntity) entity;
            String value = null;
            String alias = (String) field.getParameter();
            value = (String) sedacGEntity.getStudent().getFieldValueByAlias(alias);
            value = lookupReferenceCodeByAlias(alias, value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return value;
        }
    }

    /**
     * Class RetrievePrimaryDisability is the FieldRetriever used to retrieve
     * the primary disability state code for the current student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisability implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            SedacGEntity sedacGEntity = (SedacGEntity) entity;
            IepDisability primaryDisability = null;
            String disabilityStateCode = null;

            IepData iepData = sedacGEntity.getStudent().getActiveIep(getBroker());
            if (iepData == null) {
                iepData = sedacGEntity.getStudent().getPreviousIep(getBroker());
            }
            if (iepData != null) {
                primaryDisability = iepData.getPrimaryDisability(getBroker());
                if (primaryDisability != null) {
                    disabilityStateCode = lookupReferenceCodeByBeanPath(IepDisability.class,
                            IepDisability.COL_DISABILITY_CODE,
                            primaryDisability.getDisabilityCode(),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            return disabilityStateCode;
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Initialize
         */
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        initializeFields();

        Criteria studentCriteria = getStudentCriteria();
        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        /*
         * Create map of form instances applicable to the form storage for the report.
         */
        Criteria fmiCriteria = new Criteria();
        fmiCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER +
                FormDefinition.COL_ID,
                ID_SEDACG_FORM);
        fmiCriteria.addIn(FormInstance.COL_OWNER_OBJECT_OID, studentQuery);
        QueryByCriteria instanceQuery = new QueryByCriteria(FormInstance.class, fmiCriteria);
        m_formMap = getBroker().getMapByQuery(instanceQuery, FormInstance.COL_STORAGE_OBJECT_OID, 64);

        /*
         * Build query of GenericFormData for the form instance and relevant dates.
         */
        Criteria gfdCriteria = new Criteria();
        SubQuery instanceSub = new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, fmiCriteria);
        gfdCriteria.addIn(X2BaseBean.COL_OID, instanceSub);

        String todayStr = SYSTEM_STRING_DATE_FORMAT.format(m_reportDate);
        gfdCriteria.addLessOrEqualThan(m_contractStart, todayStr);
        Criteria gfd2Criteria = new Criteria();
        Criteria gfd3Criteria = new Criteria();
        gfd2Criteria.addGreaterOrEqualThan(m_contractEnd, getCurrentContext().getStartDate());
        gfd3Criteria.addIsNull(m_contractEnd);
        gfd2Criteria.addOrCriteria(gfd3Criteria);
        gfdCriteria.addAndCriteria(gfd2Criteria);

        QueryByCriteria formQuery = new QueryByCriteria(GenericFormData.class, gfdCriteria);
        applyInputSort(formQuery, null);

        setQuery(formQuery);
        setEntityClass(SedacGEntity.class);

        /*
         * Build maps of retriever functions
         */
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("SEDACG-VALUE", new RetrieveAliasValue());
        calcs.put("SEDACG-DISABILITY", new RetrievePrimaryDisability());
        calcs.put("SEDACG-PATH", new RetrieveValueByBeanPath());
        calcs.put("SEDACG-STD-ALIAS", new RetrieveStudentAliasValue());
        super.addCalcs(calcs);
    }

    /**
     * Biuld criteria for students to be included in the selection base on user selection.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            criteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        applyInputCriteria(criteria, false, null);
        return criteria;
    }

    /**
     * Initialize fields and dictionaries for this export.
     */
    private void initializeFields() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, ID_SEDACG_FORM);
        BeanQuery sedacQuery = new BeanQuery(FormDefinition.class, criteria);
        FormDefinition sedacGFormDef = (FormDefinition) getBroker().getBeanByQuery(sedacQuery);
        ExtendedDataDictionary sedacGExtendDictionary = sedacGFormDef.getExtendedDataDictionary();
        DataDictionary sedacGDictionary =
                DataDictionary.getDistrictDictionary(sedacGExtendDictionary, getBroker().getPersistenceKey());
        setDataDictionary(sedacGDictionary);

        // Once the dictionary is set above, we can use lookup to translate aliases.
        m_contractStart = translateAliasToJavaName(ALIAS_SEDACG_START, true);
        m_contractEnd = translateAliasToJavaName(ALIAS_SEDACG_END, true);
    }
}
