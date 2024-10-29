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
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;

/**
 * The Class MaStudentAccommodationsExport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaStudentAccommodationsExport extends ExportJavaSource {
    // FIELDs in report order
    private static final String FIELD_504_STATUS = "504 Status";
    private static final String FIELD_504_END_DATE = "504 End Date";
    private static final String FIELD_CATEGORY = "Category";
    private static final String FIELD_CONTENT_AREA = "Content Area";
    private static final String FIELD_DESCRIPTION = "Description";
    private static final String FIELD_IEP_END_DATE = "IEP End Date";
    private static final String FIELD_IEP_STAFF = "Staff Name";
    private static final String FIELD_IEP_START_DATE = "IEP Start Date";
    private static final String FIELD_NAME = "Name";
    private static final String FIELD_SOURCE = "Source";
    private static final String FIELD_STUDENT_GRADE = "Grade";
    private static final String FIELD_STUDENT_NAME = "Student Name";
    private static final String FIELD_STUDENT_SCHOOL = "School";
    private static final String FIELD_TYPE = "Type";

    private static final String INCLUDE_504 = "504";
    private static final String INCLUDE_IEP = "IEP";
    private static final String INCLUDE_GENERAL = "GENERAL";

    private static final String METHOD_PREFIX = "method:";

    private static final String PARAM_CATEGORIES = "categories";
    private static final String PARAM_INCLUDE = "include";
    private static final String PARAM_SORT_BY = "sortBy";

    private static final Map<String, String> FIELDS_MAP;
    static {
        FIELDS_MAP = new LinkedHashMap<String, String>();
        FIELDS_MAP.put(FIELD_SOURCE, METHOD_PREFIX + "getSource");
        FIELDS_MAP.put(FIELD_STUDENT_NAME, "student.nameView");
        FIELDS_MAP.put(FIELD_STUDENT_SCHOOL, "student.school.name");
        FIELDS_MAP.put(FIELD_STUDENT_GRADE, "student.gradeLevel");
        FIELDS_MAP.put(FIELD_CATEGORY, "category");
        FIELDS_MAP.put(FIELD_504_STATUS, "student.section504StatusCode");
        FIELDS_MAP.put(FIELD_504_END_DATE, "student.section504LastEndDate");
        FIELDS_MAP.put(FIELD_IEP_STAFF, "iepData.staff.nameView");
        FIELDS_MAP.put(FIELD_IEP_START_DATE, "iepData.startDate");
        FIELDS_MAP.put(FIELD_IEP_END_DATE, "iepData.endDate");
        FIELDS_MAP.put(FIELD_NAME, "name");
        FIELDS_MAP.put(FIELD_TYPE, "type");
        FIELDS_MAP.put(FIELD_DESCRIPTION, METHOD_PREFIX + "getDescription");
        FIELDS_MAP.put(FIELD_CONTENT_AREA, "contentArea");
    }

    /*
     * Map of Accommodation reference codes.
     */
    Map<String, ReferenceCode> m_referenceCodes;

    /**
     * Get the description, called using reflection.
     *
     * @param iac
     *
     * @return
     */
    public String getDescription(IepAccommodation iac) {
        String name = iac.getName();
        String description = iac.getDescription();
        ReferenceCode refCode = m_referenceCodes.get(name);
        if (refCode != null && !StringUtils.isEmpty(refCode.getStateCode())) {
            String codeDescription = refCode.getDescription();
            if (codeDescription != null && codeDescription.startsWith("label.")) {
                String decodedDescription =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale().toString(), true)
                                .getMessage(codeDescription);
                if (decodedDescription != null) {
                    codeDescription = decodedDescription;
                }
            }

            if (codeDescription != null) {
                description = codeDescription;
                if (!StringUtils.isEmpty(refCode.getStateCode())) {
                    description += " (" + refCode.getStateCode() + ")";
                }
            }
        }
        return description;
    }

    /**
     * Gets the source. Called using reflection.
     *
     * @param iac IepAccommodation
     * @return String
     */
    public String getSource(IepAccommodation iac) {
        String value = "General";
        if (!StringUtils.isEmpty(iac.getIepDataOid())) {
            value = "Iep";
        } else if (!StringUtils.isEmpty(iac.getStudentEdPlanOid())) {
            value = "504";
        }
        return value;
    }

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        loadReferenceCodes();
        DataGrid grid = new DataGrid();

        X2Criteria iacCriteria = new X2Criteria();
        iacCriteria.addNotNull(IepAccommodation.COL_NAME);
        iacCriteria.addIn(IepAccommodation.COL_CATEGORY, getIncludedCategories());
        iacCriteria.addAndCriteria(getActiveCriteria());
        if (isSchoolContext()) {
            iacCriteria.addEqualTo(
                    IepAccommodation.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
        Collection<String> activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization());
        iacCriteria.addIn(
                IepAccommodation.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                activeStudentCodes);

        BeanQuery query = new BeanQuery(IepAccommodation.class, iacCriteria);
        try (QueryIterator iter = getBroker().getIteratorByQuery(query)) {
            while (iter.hasNext()) {
                IepAccommodation iac = (IepAccommodation) iter.next();
                grid.append();
                for (Entry<String, String> entry : FIELDS_MAP.entrySet()) {
                    Object value = null;
                    String lookupPath = entry.getValue();
                    if (lookupPath.startsWith(METHOD_PREFIX)) {
                        // get result using reflection
                        String methodName = lookupPath.replaceAll(METHOD_PREFIX, "");

                        Exception e = null;
                        try {
                            Method method = getClass().getMethod(methodName, new Class[] {IepAccommodation.class});
                            value = method.invoke(this, new Object[] {iac});
                        } catch (NoSuchMethodException nsme) {
                            e = nsme;
                        } catch (IllegalAccessException iae) {
                            e = iae;
                        } catch (InvocationTargetException ite) {
                            e = ite;
                        } finally {
                            if (e != null) {
                                String bundle = null;
                                throw new X2BaseException(bundle, "Error invoking method " + methodName, e);
                            }
                        }
                    } else {
                        // process as bean path
                        value = WebUtils.getProperty(iac, lookupPath);
                        if (value != null && !(value instanceof String)) {
                            Converter converter =
                                    ConverterFactory.getConverterForClass(value.getClass().getName(), getLocale(),
                                            false);
                            if (converter != null) {
                                value = converter.javaToString(value);
                            }
                        }
                    }
                    grid.set(entry.getKey(), value);
                }
            }
        }

        // Sort using input option
        String sortBy = (String) getParameter(PARAM_SORT_BY);
        if (!StringUtils.isEmpty(sortBy)) {
            grid.sort(Arrays.asList(sortBy.split("\\s*,\\s*")), true);
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return new ArrayList(FIELDS_MAP.keySet());
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return new ArrayList(FIELDS_MAP.keySet());
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Gets the criteria for active 504 accommodations.
     *
     * @return X 2 criteria
     */
    private X2Criteria getActive504Criteria() {
        X2Criteria active504EdPlanCriteria = new X2Criteria();

        active504EdPlanCriteria.addEqualTo(StudentEdPlan.COL_EXTENDED_DATA_DICTIONARY_OID, "ddxStandard504");
        active504EdPlanCriteria.addIn(StudentEdPlan.COL_STATUS_CODE, Arrays.asList(
                Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(StudentEdPlan.StatusCode.PREVIOUS.ordinal())));
        active504EdPlanCriteria.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, new PlainDate());
        active504EdPlanCriteria.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, new PlainDate());

        SubQuery active504s = new SubQuery(StudentEdPlan.class, X2BaseBean.COL_OID, active504EdPlanCriteria);

        X2Criteria active504Criteria = new X2Criteria();
        active504Criteria.addIn(IepAccommodation.COL_STUDENT_ED_PLAN_OID, active504s);
        return active504Criteria;
    }

    /**
     * Gets the criteria for active accommodations based on input type selection.
     *
     * @return X 2 criteria
     */
    private X2Criteria getActiveCriteria() {
        String includes = (String) getParameter(PARAM_INCLUDE);

        boolean andCriteriaAdded = false;
        List includeList = Arrays.asList(includes.split("\\s*,\\s*"));
        X2Criteria activeCriteria = new X2Criteria();
        if (includeList.contains(INCLUDE_IEP)) {
            if (!andCriteriaAdded) {
                activeCriteria.addAndCriteria(getActiveIepsCriteria());
                andCriteriaAdded = true;
            }
        }
        if (includeList.contains(INCLUDE_504)) {
            if (!andCriteriaAdded) {
                activeCriteria.addAndCriteria(getActive504Criteria());
                andCriteriaAdded = true;
            } else {
                activeCriteria.addOrCriteria(getActive504Criteria());
            }
        }
        if (includeList.contains(INCLUDE_GENERAL)) {
            if (!andCriteriaAdded) {
                activeCriteria.addAndCriteria(getGeneralAccommodationCriteria());
                andCriteriaAdded = true;
            } else {
                activeCriteria.addOrCriteria(getGeneralAccommodationCriteria());
            }
        }
        if (!andCriteriaAdded) {
            // insure nothing loaded
            activeCriteria.addEqualTo(X2BaseBean.COL_OID, "--dummy--");
        }
        return activeCriteria;

    }

    /**
     * Gets the criteria for active IEP accommodations.
     *
     * @return X2Criteria
     */
    private X2Criteria getActiveIepsCriteria() {
        X2Criteria activeIepCriteria = new X2Criteria();
        activeIepCriteria.addEqualTo(IepData.COL_STATUS_CODE,
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        activeIepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, new PlainDate());

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(IepAccommodation.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, activeIepCriteria));
        return criteria;
    }

    /**
     * Gets the criteria for active general accommodations.
     *
     * @return Criteria
     */
    private Criteria getGeneralAccommodationCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEmpty(IepAccommodation.COL_STUDENT_ED_PLAN_OID, getBroker().getPersistenceKey());
        criteria.addEmpty(IepAccommodation.COL_IEP_DATA_OID, getBroker().getPersistenceKey());
        criteria.addGreaterOrEqualThan(IepAccommodation.COL_IMPLEMENTATION_DATE,
                getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(IepAccommodation.COL_IMPLEMENTATION_DATE, getCurrentContext().getEndDate());
        return criteria;
    }

    /**
     * Gets the categories selected in the input definition.
     *
     * @return Collection
     */
    private Collection<String> getIncludedCategories() {
        List<String> list = new ArrayList();
        list.add("--WillNotMatch--");
        String categories = (String) getParameter(PARAM_CATEGORIES);
        if (!StringUtils.isEmpty(categories)) {
            for (String rcdOid : Arrays.asList(categories.split("\\s*,\\s*"))) {
                ReferenceCode rcd = getBroker().getBeanByOid(ReferenceCode.class, rcdOid);
                if (rcd != null) {
                    list.add(rcd.getCode());
                }
            }
        }
        return list;
    }

    /**
     * Preload reference codes to calculate the description from the
     */
    private void loadReferenceCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_NAME);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
        m_referenceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 64);
    }
}
