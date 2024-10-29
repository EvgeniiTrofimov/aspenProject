/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramDetail;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Early Childhood Programs.
 *
 * @author Follett School Solutions
 */
public class EarlyChildhoodPrograms extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author Follett School Solutions
     */
    public static class EarlyChildhoodProgramsEntity extends StateReportEntity {
        /**
         * EarlyChildhood data.
         */
        EarlyChildhoodPrograms m_ecData = null;

        /**
         * The effective Entry student enrollment record for report date.
         */
        StudentEnrollment m_enrollment = null;

        List<StudentProgramDetail> m_programDetails = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EarlyChildhoodProgramsEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_ecData.m_fieldSchoolCode);
            }

            return school;
        }

        /**
         * Gets the current program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getCurrentProgram() {
            return m_programDetails.get(getCurrentRow()).getProgram();
        }

        /**
         * Gets the current program detail.
         *
         * @return Student program detail
         */
        public StudentProgramDetail getCurrentProgramDetail() {
            return m_programDetails.get(getCurrentRow());
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
         * Initialize and increment counter
         *
         * If there is no recent entry enrollment record, ignore it.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_ecData = (EarlyChildhoodPrograms) data;
            SisStudent student = (SisStudent) bean;

            // Do not report students not active on report date.
            m_enrollment = m_ecData.m_helper.getEnrollmentForDate(student.getOid(),
                    m_ecData.getCurrentContext().getEndDate(), "EYS");

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_ecData.getBroker().getPersistenceKey());
            ModelProperty prop =
                    new ModelProperty(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE,
                            m_ecData.getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            ReferenceTable referenceTable = field.getReferenceTable();
            Map<String, ReferenceCode> map = referenceTable.getCodeMap();
            List<String> codes = new ArrayList<String>();
            for (String code : map.keySet()) {
                ReferenceCode refCode = map.get(code);
                if ("EC".equals(refCode.getStateCode())) {
                    codes.add(code);
                }
            }

            m_programDetails = new ArrayList<StudentProgramDetail>();

            X2Criteria pgmCriteria = new X2Criteria();
            pgmCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());
            pgmCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                    m_ecData.getCurrentContext().getStartDate());
            pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                    m_ecData.getCurrentContext().getEndDate());
            pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes);
            SubQuery subQuery = new SubQuery(StudentProgramParticipation.class, X2BaseBean.COL_OID, pgmCriteria);

            X2Criteria pgdCriteria = new X2Criteria();
            pgdCriteria.addIn(StudentProgramDetail.COL_PROGRAM_OID, subQuery);
            QueryByCriteria query = new QueryByCriteria(StudentProgramDetail.class, pgdCriteria);

            Collection<StudentProgramDetail> programDetails = m_ecData.getBroker().getCollectionByQuery(query);
            m_programDetails.addAll(programDetails);
            setRowCount(programDetails.size());
            m_ecData.m_totalRecordCount += programDetails.size();
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";

            return name;
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
     * Retrieve the PGD data
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePgdData implements FieldRetriever {
        protected static final String PROVIDER_TYPE = "PROVIDER-TYPE";
        protected static final String PROVIDER_IEIN = "IEIN";
        protected static final String PROVIDER_START = "PROVIDER-START";
        protected static final String PROVIDER_END = "PROVIDER-END";
        protected static final String PROVIDER_END_REASON = "PROVIDER-END-REASON";
        protected static final String GATEWAYS_CREDENTIAL = "GATEWAYS-CREDENTIAL";
        protected static final String GATEWAYS_ID = "GATEWAYS-ID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            EarlyChildhoodProgramsEntity ecpEntity = (EarlyChildhoodProgramsEntity) entity;
            StudentProgramDetail programDetail = ecpEntity.getCurrentProgramDetail();
            EarlyChildhoodPrograms ecpData = (EarlyChildhoodPrograms) data;

            if (PROVIDER_TYPE.equals(param)) {
                String providerType = (String) programDetail.getFieldValueByBeanPath(m_fieldProviderType);
                return ecpData.lookupStateValue(StudentProgramDetail.class, m_fieldProviderType, providerType);
            } else if (PROVIDER_IEIN.equals(param)) {
                String iein = (String) programDetail.getFieldValueByBeanPath(m_fieldProviderIein);
                return ecpData.lookupStateValue(StudentProgramDetail.class, m_fieldProviderIein, iein);
            } else if (PROVIDER_START.equals(param)) {
                String providerStartString = (String) programDetail.getFieldValueByBeanPath(m_fieldProviderStart);
                if (providerStartString == null) {
                    return "";
                }

                try {
                    return m_dateFormatUdf.parse(providerStartString);
                } catch (ParseException e) {
                    return "";
                }
            } else if (PROVIDER_END.equals(param)) {
                String providerEndString = (String) programDetail.getFieldValueByBeanPath(m_fieldProviderEnd);
                if (providerEndString == null) {
                    return "";
                }

                try {
                    return m_dateFormatUdf.parse(providerEndString);
                } catch (ParseException e) {
                    return "";
                }
            } else if (PROVIDER_END_REASON.equals(param)) {
                String endReason = (String) programDetail.getFieldValueByBeanPath(m_fieldProviderEndReason);
                return ecpData.lookupStateValue(StudentProgramDetail.class, m_fieldProviderEndReason, endReason);
            } else if (GATEWAYS_ID.equals(param)) {
                return programDetail.getFieldValueByBeanPath(m_fieldGatewaysId);
            } else if (GATEWAYS_CREDENTIAL.equals(param)) {
                return programDetail.getFieldValueByBeanPath(m_fieldGatewaysCredential);
            }
            return "";
        }
    }
    /**
     * Retrieve the PGM data
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePgmData implements FieldRetriever {
        protected static final String PROGRAM = "PROGRAM";
        protected static final String SERVICE_LOCATION = "SERVICE-LOCATION";
        protected static final String SERVICE_TYPE = "SERVICE-TYPE";
        protected static final String START = "START";
        protected static final String END = "END";
        protected static final String END_REASON = "END-REASON";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            EarlyChildhoodProgramsEntity ecpEntity = (EarlyChildhoodProgramsEntity) entity;
            StudentProgramParticipation program = ecpEntity.getCurrentProgram();
            EarlyChildhoodPrograms ecpData = (EarlyChildhoodPrograms) data;

            if (PROGRAM.equals(param)) {
                String programType = (String) program.getFieldValueByBeanPath(m_fieldProgramType);
                return ecpData.lookupStateValue(m_pgmEcDictionary, StudentProgramParticipation.class,
                        m_fieldProgramType, programType);
            } else if (SERVICE_LOCATION.equals(param)) {
                String serviceLocation = (String) program.getFieldValueByBeanPath(m_fieldServiceLocation);
                return ecpData.lookupStateValue(m_pgmEcDictionary, StudentProgramParticipation.class,
                        m_fieldServiceLocation,
                        serviceLocation);
            } else if (SERVICE_TYPE.equals(param)) {
                String serviceType = (String) program.getFieldValueByBeanPath(m_fieldServiceType);
                return ecpData.lookupStateValue(m_pgmEcDictionary, StudentProgramParticipation.class,
                        m_fieldServiceType, serviceType);
            } else if (START.equals(param)) {
                return program.getStartDate();
            } else if (END.equals(param)) {
                return program.getEndDate();
            } else if (END_REASON.equals(param)) {
                String endReason = (String) program.getFieldValueByBeanPath(m_fieldProgramEndReason);
                return ecpData.lookupStateValue(m_pgmEcDictionary, StudentProgramParticipation.class,
                        m_fieldProgramEndReason, endReason);
            }
            return "";
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            EarlyChildhoodProgramsEntity ecpEntity = (EarlyChildhoodProgramsEntity) entity;
            StudentEnrollment primEnr = ecpEntity.getEffectiveEnrollment();
            EarlyChildhoodPrograms ecpData = (EarlyChildhoodPrograms) data;
            String rcdts = null;
            if (primEnr != null) {
                if (param.equals("H") && ecpEntity.getEffectiveEnrollment() != null
                        && ecpEntity.getEffectiveEnrollment().getSchool() != null) {
                    if (primEnr != null) {
                        String codeForNonFte = (String) primEnr.getFieldValueByBeanPath(ecpData.m_fieldEnrSklHome);
                        if (!StringUtils.isEmpty(codeForNonFte)) {
                            rcdts = ecpData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, codeForNonFte);
                        }
                    }
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(ecpData.m_fieldSchoolCode);
                    }
                } else if (param.equals("S")) {
                    String servingCode = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                    if (!StringUtils.isEmpty(servingCode)) {
                        rcdts = ecpData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService, servingCode);
                    }
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(ecpData.m_fieldSchoolCode);
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
        private static final String CALC_ID = "ECPE-STRIPCHAR";
        private static final String LEGAL_NAME_CHARACTERS = "[^A-Za-z -]";

        private Pattern m_legalNameCharacters = Pattern.compile(LEGAL_NAME_CHARACTERS);

        /**
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
                Matcher matcher = m_legalNameCharacters.matcher(nameValue);
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
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_ENR_SKL_SERVICE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";

    protected static final String ALIAS_PROGRAM_TYPE = "all-pgm-ECProgramType";
    protected static final String ALIAS_SERVICE_LOCATION = "all-pgm-ECServiceLocation";
    protected static final String ALIAS_SERVICE_TYPE = "all-pgm-ECServiceType";
    protected static final String ALIAS_END_REASON = "all-pgm-ECEndReason";
    protected static final String ALIAS_PROVIDER_TYPE = "all-pgd-ServiceProviderType";
    protected static final String ALIAS_PROVIDER_IEIN = "all-pgd-IEIN";
    protected static final String ALIAS_PROVIDER_START = "all-pgd-ProviderStartDate";
    protected static final String ALIAS_PROVIDER_END = "all-pgd-ProviderEndDate";
    protected static final String ALIAS_PROVIDER_END_REASON = "all-pgd-ReasonforEnding";
    protected static final String ALIAS_GATEWAYS_CREDENTIAL = "all-pgd-GatewaysCredential";
    protected static final String ALIAS_GATEWAYS_ID = "all-pgd-GatewaysRegistrationMemberID";

    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /*
     * Instance variables
     */
    protected SimpleDateFormat m_dateFormatUdf = new SimpleDateFormat("yyyy-MM-dd");
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    protected String m_fieldEnrSklHome;
    protected String m_fieldEnrSklService;
    protected String m_fieldDistrictCode;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;

    protected String m_fieldProgramType;
    protected String m_fieldServiceLocation;
    protected String m_fieldServiceType;
    protected String m_fieldProgramEndReason;
    protected String m_fieldProviderType;
    protected String m_fieldProviderIein;
    protected String m_fieldProviderStart;
    protected String m_fieldProviderEnd;
    protected String m_fieldProviderEndReason;
    protected String m_fieldGatewaysCredential;
    protected String m_fieldGatewaysId;

    protected List<String> m_referenceGradeCodeList;

    /**
     * Helper class:
     * For student selection by enrollment.
     */
    protected StudentHistoryHelper m_helper;

    /**
     * Keep track of number of records
     */
    protected int m_totalRecordCount = 0;

    protected DataDictionary m_pgmEcDictionary;

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, "PGM-EC");
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
        m_pgmEcDictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

        initializeFields();
        setEntityClass(EarlyChildhoodProgramsEntity.class);

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);

        // Set the criteria to be used for student selection.
        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addIn(Student.COL_GRADE_LEVEL, m_referenceGradeCodeList);

        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);
        setQuery(studentQuery);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("ECPE-RCDTS", new RetrieveRcdts());
        calcs.put("ECPE-PGM", new RetrievePgmData());
        calcs.put("ECPE-PGD", new RetrievePgdData());
        calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
        super.addCalcs(calcs);
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     * @param dictionary
     * @param beanClass Class
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(DataDictionary dictionary, Class beanClass, String path) {
        ModelProperty prop = new ModelProperty(beanClass, path, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = dictionary.findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("Early Childhood Programs");
        heading.append(',');
        heading.append(m_totalRecordCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(new PlainDate()));
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");

        return heading.toString();
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param dictionary
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupStateValue(DataDictionary dictionary, Class beanClass, String beanPath, String value) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(dictionary, beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        }

        return stateValue;
    }

    /**
     * Translates an alias into a Java bean path name from an extended dictionary. An initialization
     * error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param dictionary
     * @param required boolean
     * @return String
     */

    public String translateAliasToJavaName(String alias, DataDictionary dictionary, boolean required) {
        String javaName = null;

        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }

        return javaName;
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
        fileName.append(new SimpleDateFormat("MMddyyyy").format(new PlainDate()));
        fileName.append("_");
        fileName.append("001.csv");

        return fileName.toString();
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
        m_fieldEnrSklService = translateAliasToJavaName(ALIAS_ENR_SKL_SERVICE, true);

        m_fieldProgramType = translateAliasToJavaName(ALIAS_PROGRAM_TYPE, m_pgmEcDictionary, true);
        m_fieldServiceLocation = translateAliasToJavaName(ALIAS_SERVICE_LOCATION, m_pgmEcDictionary, true);
        m_fieldServiceType = translateAliasToJavaName(ALIAS_SERVICE_TYPE, m_pgmEcDictionary, true);
        m_fieldProgramEndReason = translateAliasToJavaName(ALIAS_END_REASON, m_pgmEcDictionary, true);

        m_fieldProviderType = translateAliasToJavaName(ALIAS_PROVIDER_TYPE, true);
        m_fieldProviderIein = translateAliasToJavaName(ALIAS_PROVIDER_IEIN, true);
        m_fieldProviderStart = translateAliasToJavaName(ALIAS_PROVIDER_START, true);
        m_fieldProviderEnd = translateAliasToJavaName(ALIAS_PROVIDER_END, true);
        m_fieldProviderEndReason = translateAliasToJavaName(ALIAS_PROVIDER_END_REASON, true);

        m_fieldGatewaysCredential = translateAliasToJavaName(ALIAS_GATEWAYS_CREDENTIAL, true);
        m_fieldGatewaysId = translateAliasToJavaName(ALIAS_GATEWAYS_ID, true);

        loadGradeCodes();
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        Map<String, ReferenceCode> map = referenceTable.getCodeMap();
        List<String> codes = new ArrayList<String>();
        for (String code : map.keySet()) {
            ReferenceCode refCode = map.get(code);
            if ("14".equals(refCode.getStateCode())) {
                codes.add(code);
            }
        }
        m_referenceGradeCodeList = codes;
    }
}
