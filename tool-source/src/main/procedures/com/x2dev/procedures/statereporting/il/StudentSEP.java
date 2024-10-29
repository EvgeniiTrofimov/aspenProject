/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataTableConfig.OrganizationAccess;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEdPlan.StatusCode;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for 504 plan.
 *
 * @author X2 Development Corporation
 */
public class StudentSEP extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentSEPEntity extends StateReportEntity {
        /**
         * The effective Entry student enrollment record for report date.
         */
        StudentEnrollment m_enrollment = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentSEPEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentEdPlan sedPlan = (StudentEdPlan) getBean();
            return sedPlan.getStudent().getNameView() +
                    " [effectiveDate: " + sedPlan.getEffectiveDate() +
                    ", endDate: " + sedPlan.getEndDate() + "]";
        }

        /**
         * Initialize and increment counter.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            StudentSEP sepData = (StudentSEP) data;
            SisStudent student = ((StudentEdPlan) bean).getStudent();
            PlainDate enrollmentDate = sepData.m_reportDate;
            if (sepData.m_endDate.compareTo(enrollmentDate) < 0) {
                enrollmentDate = sepData.m_endDate;
            }
            m_enrollment = sepData.m_helper.getEnrollmentForDate(student.getOid(), enrollmentDate, "EYSW");

            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(sepData.m_excludeSklField))) {
                // keep count of records
                sepData.m_totalStudentCount++;
            } else {
                setRowCount(0);
            }
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

    }

    /**
     * The Class Retrieve504EndDate.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve504EndDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StudentEdPlan sep = (StudentEdPlan) entity.getBean();
            return StatusCode.PREVIOUS == sep.getStatusCodeEnum() ? m_dateFormat.format(sep.getEndDate()) : "";
        }
    }

    protected class RetrieveISBE implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StudentEdPlan sep = (StudentEdPlan) entity.getBean();
            return BooleanAsStringConverter.TRUE.equals(sep.getFieldValueByBeanPath(m_fieldSepISBE)) ? "99" : "";
        }
    }

    /**
     * Retrieve the RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRCDTS implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            StudentSEPEntity sepEntity = (StudentSEPEntity) entity;
            StudentEnrollment primEnr = sepEntity.getEffectiveEnrollment();
            StudentSEP sepData = (StudentSEP) data;
            String rcdts = null;
            if (primEnr != null) {
                if (param.equals("H")) {
                    if (primEnr.getSchool() != null) {
                        String sklHome = (String) primEnr.getFieldValueByBeanPath(sepData.m_fieldEnrSklHome);
                        if (!StringUtils.isBlank(sklHome)) {
                            rcdts = sepData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, sklHome);
                        }
                    }
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(sepData.m_fieldSchoolCode);
                    }
                } else if (param.equals("S")) {
                    String servingCode = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                    if (!StringUtils.isEmpty(servingCode)) {
                        rcdts = sepData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService, servingCode);
                    }
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(sepData.m_fieldSchoolCode);
                    }
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DELETE_AT_ISBE = "DOE DELETE AT ISBE";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_ENR_SKL_SERVICE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SEP_ISBE = "DOE DELETE AT ISBE";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z ]";

    /*
     * Parameters
     */
    private static final String PARAM_BEGIN_DATE = "beginDate";
    private static final String PARAM_END_DATE = "endDate";

    /*
     * Instance variables
     */
    protected PlainDate m_beginDate;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected PlainDate m_endDate;
    protected String m_excludeSklField;
    protected String m_excludeStdField;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSklHome;
    protected String m_fieldEnrSklService;
    protected String m_fieldSchoolCode;
    protected String m_fieldSepISBE;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected PlainDate m_reportDate = new PlainDate();

    /**
     * Helper class:
     * For student selection by enrollment.
     */
    protected StudentHistoryHelper m_helper;

    /**
     * Keep track of number of students
     */
    protected int m_totalStudentCount;

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("504 Plan");
        heading.append(',');
        heading.append(m_totalStudentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        if (m_reportDate != null) {
            heading.append(m_dateFormat.format(m_reportDate));
        }
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");

        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        setEntityClass(StudentSEPEntity.class);

        m_beginDate = ((PlainDate) getParameter(PARAM_BEGIN_DATE));
        m_endDate = ((PlainDate) getParameter(PARAM_END_DATE));

        m_helper = new StudentHistoryHelper(this);

        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_beginDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

        X2Criteria educationPlanCriteria = buildSEPCriteria();

        SubQuery sepSubQuery = new SubQuery(StudentEdPlan.class, StudentEdPlan.COL_STUDENT_OID,
                educationPlanCriteria);
        m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, sepSubQuery);

        QueryByCriteria queryByEdPlanCriteria = new QueryByCriteria(StudentEdPlan.class, educationPlanCriteria);
        applyInputSort(queryByEdPlanCriteria, StudentEdPlan.REL_STUDENT);

        setQuery(queryByEdPlanCriteria);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();

        calcs.put("SEP-RCDTS", new RetrieveRCDTS());
        calcs.put("SEP-STRIPCHAR", new RetrieveStripNameChar());
        calcs.put("SEP-504-END-DATE", new Retrieve504EndDate());
        calcs.put("SEP-ISBE", new RetrieveISBE());

        super.addCalcs(calcs);

    }

    /**
     * Apply criteria for school input field.
     *
     * @param criteria X2Criteria
     */
    private void applySchoolInputCriteria(X2Criteria criteria) {
        /*
         * Add School criteria.
         */
        if (isSchoolContext()) {
            DataDictionaryTable table =
                    getDataDictionary().findDataDictionaryTableByClass(getBeanClass().getName());
            if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getSchoolPath())) {
                ModelProperty property = new ModelProperty(table.getDataTableConfig().getSchoolPath(),
                        getBroker().getPersistenceKey());
                criteria.addEqualTo(property.getBeanPath(), getSchool().getOid());
            }
        }
        /*
         * Add Organization criteria.
         */
        DataDictionaryTable table = getDataDictionary().findDataDictionaryTableByClass(getBeanClass().getName());
        if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getOrganizationPath())) {
            int level = getOrganization().getOrganizationDefinition().getLevel();
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            Collection<ModelProperty> properties =
                    OrganizationManager.getOrganizationPaths(table.getBeanClass(), dictionary, level);

            if (!CollectionUtils.isEmpty(properties)) {
                for (ModelProperty property : properties) {
                    criteria.addAndCriteria(OrganizationManager.getOrganizationAccessCriteria(getOrganization(),
                            property, OrganizationAccess.NONE, OrganizationAccess.READ_WRITE));
                }
            }
        }
    }

    /**
     * Build criteria for education plan query.
     *
     * @return Reference code
     */
    private X2Criteria buildSEPCriteria() {
        X2Criteria educationPlanCriteria = new X2Criteria();

        // exclude students where student.DOE EXCLUDE STD = True
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            educationPlanCriteria.addNotEqualTo(StudentEdPlan.REL_STUDENT + PATH_DELIMITER + m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        // 504 plan
        educationPlanCriteria.addEqualTo(StudentEdPlan.COL_EXTENDED_DATA_DICTIONARY_OID,
                PreferenceManager.getPreferenceValue(getOrganization(),
                        SisPreferenceConstants.SECTION_504_DEFAULT_DEFINITION));

        // Active status
        X2Criteria statusActiveCriteria = new X2Criteria();
        statusActiveCriteria.addEqualTo(StudentEdPlan.COL_STATUS_CODE, Integer.valueOf(StatusCode.ACTIVE.ordinal()));

        // Previous status
        X2Criteria statusPreviousCriteria = new X2Criteria();
        statusPreviousCriteria.addEqualTo(StudentEdPlan.COL_STATUS_CODE,
                Integer.valueOf(StatusCode.PREVIOUS.ordinal()));

        // educationPlanCriteria
        statusPreviousCriteria.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, m_endDate);

        X2Criteria statusPreviousEndCriteria = new X2Criteria();

        X2Criteria statusPreviousEndGreaterCriteria = new X2Criteria();
        statusPreviousEndGreaterCriteria.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, m_beginDate);
        X2Criteria statusPreviousEndNullCriteria = new X2Criteria();
        statusPreviousEndNullCriteria.addIsNull(StudentEdPlan.COL_END_DATE);

        statusPreviousEndCriteria.addOrCriteria(statusPreviousEndGreaterCriteria);
        statusPreviousEndCriteria.addOrCriteria(statusPreviousEndNullCriteria);

        statusPreviousCriteria.addAndCriteria(statusPreviousEndCriteria);

        // apply statuses
        X2Criteria statusOrCriteria = new X2Criteria();
        statusOrCriteria.addOrCriteria(statusActiveCriteria);
        statusOrCriteria.addOrCriteria(statusPreviousCriteria);

        educationPlanCriteria.addAndCriteria(statusOrCriteria);

        // apply input criteria
        applyInputCriteria(educationPlanCriteria, false, StudentEdPlan.REL_STUDENT);
        applySchoolInputCriteria(educationPlanCriteria);

        return educationPlanCriteria;
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (getOrganization() != null) {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        if (m_reportDate != null) {
            fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        }
        fileName.append("_");
        fileName.append("001.txt");

        return fileName.toString();
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
        m_fieldEnrSklService = translateAliasToJavaName(ALIAS_ENR_SKL_SERVICE, true);
        m_fieldSepISBE = translateAliasToJavaName(ALIAS_SEP_ISBE, true);
    }

}
