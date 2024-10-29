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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedAssessmentFormBData.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class SpedAssessmentFormBData extends BaseFormReportJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * Alias that will contain form owner oid that fill the form
     */
    public static final String ALIAS_DATE = "ed-ass-a-date";
    public static final String ALIAS_OTHER_NAME = "other-name";
    public static final String ALIAS_OWNER_STAFF_OID = "owner-staff-oid";

    /**
     * Report parameter containing the owner Staff object.
     */
    public static final String PARAM_OWNER_STAFF = "ownerStaff";
    public static final String PARAM_OWNER_STUDENT = "ownerStudent";

    private static final String REPORT_DATE_DISPLAY = "formCreationDate";
    private static final String STD_GRADE_LEVEL = "gradeLevel";

    private MaSpedAttribHelper m_attribHelper;
    private IepData m_currentIep = null;

    /**
     * Prepares the data source that will be used by the Jasper design. This method is called after
     * <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        X2BaseBean bean = getFormOwner();
        // owner is either student or iep depending on type of form
        if (bean != null) {
            SisStudent student = (SisStudent) (bean instanceof IepData ? ((IepData) bean).getStudent() : bean);
            addParameter(PARAM_OWNER_STUDENT, student);
            setGradeLevel(student, getFormStartDate(bean));
            if (getFormInstance() != null) {
                addParameter(REPORT_DATE_DISPLAY, new Date(getFormInstance().getCreatedTime()));
            }
            addParameter(PARAM_OWNER_STAFF, getOwnerStaff());
        }
        SimpleFormDataSource dataSource =
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        }
    }

    /**
     * Gets the form start date.
     *
     * @param bean X2BaseBean
     * @return Plain date
     * @throws X2BaseException exception
     */
    private PlainDate getFormStartDate(X2BaseBean bean) throws X2BaseException {
        PlainDate startDate = new PlainDate();
        if (bean instanceof IepData && ((IepData) bean).getStartDate() != null) {
            startDate = ((IepData) bean).getStartDate();
        } else {
            if (getFormDefinition() != null) {
                DataDictionary dictionary = getDictionary();
                DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DATE);
                if (field != null) {
                    Object value = WebUtils.getProperty(getFormStorage(), field.getJavaName());

                    if (value instanceof PlainDate) {
                        startDate = (PlainDate) value;
                    }
                    if (value instanceof String) {
                        if (field.isString()) {
                            String format = WebUtils.generateFormat(field,
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
                            Converter baseConverter = ConverterFactory.getConverterForClass(
                                    field.getEffectiveJavaType(),
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                                    field.isString(), format);
                            if (baseConverter instanceof SystemStringConverter) {
                                SystemStringConverter converter = ((SystemStringConverter) baseConverter);
                                if (converter != null) {
                                    value = converter.parseSystemString((String) value);
                                    if (value instanceof PlainDate) {
                                        startDate = (PlainDate) value;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return startDate;
    }

    /**
     * Gets the owner staff.
     *
     * @return String
     */
    private String getOwnerStaff() {
        DataDictionary actionDictionary = getDictionary();
        X2BaseBean actionFormStorage = getFormStorage();
        String otherName = (String) actionFormStorage.getFieldValueByAlias(ALIAS_OTHER_NAME, actionDictionary);
        if (StringUtils.isEmpty(otherName)) {
            String ownerStaffOid =
                    (String) actionFormStorage.getFieldValueByAlias(ALIAS_OWNER_STAFF_OID, actionDictionary);
            if (!StringUtils.isEmpty(ownerStaffOid)) {
                SisStaff ownerStaff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, ownerStaffOid);
                if (ownerStaff != null) {
                    otherName = ownerStaff.getNameView();
                    if (!StringUtils.isEmpty(ownerStaff.getSpedRole())) {
                        otherName += ", " + ownerStaff.getSpedRole();
                    }
                }
            }
        }
        return StringUtils.unNullify(otherName);
    }

    /**
     * Sets the grade level.
     *
     * @param student SisStudent
     * @param startDate PlainDate
     */
    private void setGradeLevel(SisStudent student, PlainDate startDate) {
        // get grade level on creation time based on iep start date, if not form creation date, on
        // most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        String gradeLevel = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

        for (StudentEnrollment e : enrollments) {
            if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                // student's YOG at this particular time
                int yog = e.getYog();

                // get the school year from basedDate
                X2Criteria schoolYearCriteria = new X2Criteria();
                schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                QueryByCriteria schoolYearQuery =
                        new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                String currentContextOid = getCurrentContext().getContextId();
                if (!StringUtils.isEmpty(currentContextOid) && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                    gradeLevel = student.getGradeLevel();
                } else {
                    int schoolYear = ctx.getSchoolYear();
                    List<String> grades =
                            StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                    gradeLevel = grades.get(0);
                }
                break;
            }
        }
        if (StringUtils.isEmpty(gradeLevel)) {
            gradeLevel = student.getGradeLevel();
        }
        addParameter(STD_GRADE_LEVEL, gradeLevel);
    }

}
