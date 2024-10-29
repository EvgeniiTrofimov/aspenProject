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
package com.x2dev.reports.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.reports.statereporting.ri.RIImmunizationSummary.ImmunizationAccumulator.StudentInfo;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationGroup;
import com.x2dev.sis.model.beans.HealthImmunizationGroupOverride;
import com.x2dev.sis.model.beans.HealthImmunizationRuleAttributes;
import com.x2dev.sis.model.beans.HealthImmunizationRuleInstance;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.model.business.health.ImmunizationRuleEngine;
import com.x2dev.sis.web.health.ImmunizationRuleException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class RIImmunizationSummary.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class RIImmunizationSummary extends ReportJavaSourceNet {
    /**
     * The Enum ImmunizationStatus. THe order is critical becuase the ordinal is used to specify
     * replace order when multiple series are found.
     */
    enum ImmunizationStatus {
        NO_RECORD("No Doses"),

        NOT_COMPLIANT("Not Compliant"),

        MEDICAL("Medical Exemption"),

        RELIGIOUS("Religious Exemption"),

        TEMPORARY("Temporary Exemption"),

        COMPLIANT("Compliant");

        private String m_name;

        /**
         * Instantiates a new immunization status.
         *
         * @param name String
         */
        ImmunizationStatus(String name) {
            m_name = name;
        }

        /**
         * Gets the name.
         *
         * @return String
         */
        public String getName() {
            return m_name;
        }
    }

    /**
     * Sort the students by name view
     *
     * @param list List<StudentInfo>
     */
    public static void sort(List<StudentInfo> list) {
        Collections.sort(list, new Comparator<StudentInfo>() {
            @Override
            public int compare(StudentInfo o1, StudentInfo o2) {
                int value = o1.getStudent().getNameView().compareTo(o2.getStudent().getNameView());
                if (value == 0) {
                    value = o1.getStudent().getLocalId().compareTo(o2.getStudent().getLocalId());
                }
                return value;
            }
        });
    }

    private static final String ALIAS_HIG_IMMUNIZATION_TYPE = "all-hig-ImmunizationType";
    private static final String ALIAS_HIM_IMMUNIZATION_TYPE = "all-him-ImmunizationType";
    private static final String ALIAS_HIO_MEDICAL_EXEMPTION = "all-hio-MedicalExemption";
    private static final String ALIAS_HIO_RELIGIOUS_EXEMPTION = "all-hio-ReligiousExemption";
    private static final String ALIAS_HIO_TEMPORARY_EXEMPTION = "all-hio-TemporaryExemption";
    private static final String ALIAS_HIS_MEDICAL_EXEMPTION = "all-his-MedicalExemption";
    private static final String ALIAS_HIS_RELIGIOUS_EXEMPTION = "all-his-ReligiousExemption";
    private static final String ALIAS_HIS_TEMPORARY_EXEMPTION = "all-his-TemporaryExemption";

    private static final List EMPTY_LIST = new ArrayList();

    private static final String FIELD_NAME = "fieldName";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_STUDENT_LOCAL_ID = "studentLocalId";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_VALUE = "fieldValue";

    private static final String IMMUNIZATION_TYPE_ALL = "ALL";
    private static final String IMMUNIZATION_TYPE_UNDUPLICATED = "UNDUPLICATED";

    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_DETAILS = "details";
    private static final String INPUT_PARAM_GRADE_LEVEL = "gradeLevel";
    private static final String INPUT_PARAM_IMMUNIZATION_TYPES = "immunizationTypes";
    private static final String INPUT_PARAM_Q_A_HEADING = "headingQA";
    private static final String INPUT_PARAM_Q_B_HEADING = "headingQB";
    private static final String INPUT_PARAM_Q_C_HEADING = "headingQC";
    private static final String INPUT_PARAM_Q_C1_HEADING = "headingQC1";
    private static final String INPUT_PARAM_Q_C1_TOT_HEADING = "headingQC1Tot";
    private static final String INPUT_PARAM_Q_C2_HEADING = "headingQC2";
    private static final String INPUT_PARAM_Q_C2_TOT_HEADING = "headingQC2Tot";
    private static final String INPUT_PARAM_Q_D_HEADING = "headingQD";
    private static final String INPUT_PARAM_Q_D1_HEADING = "headingQD1";
    private static final String INPUT_PARAM_Q_E_HEADING = "headingQE";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";

    /**
     * The Class ImmunizationAccumulator.
     */
    class ImmunizationAccumulator {

        /**
         * The Class StudentInfo.
         */
        class StudentInfo {
            private SisStudent m_student;
            private Map<String, ImmunizationStatus> m_statusMap = new HashMap();

            /**
             * Instantiates a new student info.
             *
             * @param student SisStudent
             */
            public StudentInfo(SisStudent student) {
                m_student = student;
            }

            /**
             * Adds the status.
             *
             * @param immunizationType String
             * @param isCompliant boolean
             * @param ruleInstance HealthImmunizationRuleInstance
             * @param doses List<HealthImmunizationDose>
             */
            public void addStatus(String immunizationType,
                                  boolean isCompliant,
                                  HealthImmunizationRuleInstance ruleInstance,
                                  List<HealthImmunizationDose> doses) {
                ImmunizationStatus status = null;
                if (isCompliant) {
                    status = ImmunizationStatus.COMPLIANT;
                } else if (isExempt(ruleInstance, ALIAS_HIS_TEMPORARY_EXEMPTION, ALIAS_HIO_TEMPORARY_EXEMPTION)) {
                    status = ImmunizationStatus.TEMPORARY;
                } else if (isExempt(ruleInstance, ALIAS_HIS_RELIGIOUS_EXEMPTION, ALIAS_HIO_RELIGIOUS_EXEMPTION)) {
                    status = ImmunizationStatus.RELIGIOUS;
                } else if (isExempt(ruleInstance, ALIAS_HIS_MEDICAL_EXEMPTION, ALIAS_HIO_MEDICAL_EXEMPTION)) {
                    status = ImmunizationStatus.MEDICAL;
                } else if (doses != null && !doses.isEmpty()) {
                    status = ImmunizationStatus.NOT_COMPLIANT;
                } else {
                    status = ImmunizationStatus.NO_RECORD;
                }
                ImmunizationStatus current = getImmunizationStatus(immunizationType);
                if (current == null || current.ordinal() <= status.ordinal()) {
                    setImmunizationStatus(immunizationType, status);
                }
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
                StudentInfo other = (StudentInfo) obj;
                if (!getOuterType().equals(other.getOuterType())) {
                    return false;
                }
                if (m_student == null) {
                    if (other.m_student != null) {
                        return false;
                    }
                } else if (!m_student.equals(other.m_student)) {
                    return false;
                }
                return true;
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
                result = prime * result + ((m_student == null) ? 0 : m_student.hashCode());
                return result;
            }

            /**
             * Gets the immunization status.
             *
             * @param immunizationType String
             * @return Immunization status
             */
            public ImmunizationStatus getImmunizationStatus(String immunizationType) {
                return m_statusMap.get(immunizationType);
            }

            /**
             * Gets the student.
             *
             * @return SisStudent student
             */
            public SisStudent getStudent() {
                return m_student;
            }

            /**
             * Checks if is exempt.
             *
             * @param ruleInstance HealthImmunizationRuleInstance
             * @param aliasHis String
             * @param aliasHio String
             * @return true, if is exempt
             */
            private boolean isExempt(HealthImmunizationRuleInstance ruleInstance, String aliasHis, String aliasHio) {
                boolean exempt = false;
                if (ruleInstance instanceof HealthImmunizationSeries && isTrue((X2BaseBean) ruleInstance, aliasHis)) {
                    exempt = true;
                } else if (ruleInstance instanceof HealthImmunizationGroupOverride
                        && isTrue((X2BaseBean) ruleInstance, aliasHio)) {
                    exempt = true;
                }
                return exempt;
            }

            /**
             * Checks if alias field is true (logical or UDF Logical field).
             *
             * @param bean X2BaseBean
             * @param alias String
             * @return true, if is true
             */
            private boolean isTrue(X2BaseBean bean, String alias) {
                boolean value = false;
                DataDictionaryField field = getAliasDataDictionaryField(alias);
                Object fieldValue = bean.getFieldValueByBeanPath(field.getJavaName());
                if (fieldValue instanceof String) {
                    SystemStringConverter converter = getConverter(field);
                    if (converter != null) {
                        fieldValue = converter.parseSystemString((String) fieldValue);
                    }
                }
                if (fieldValue instanceof Boolean) {
                    value = ((Boolean) fieldValue).booleanValue();
                }
                return value;
            }

            /**
             * Sets the immunization status.
             *
             * @param immunizationType String
             * @param status ImmunizationStatus
             */
            private void setImmunizationStatus(String immunizationType, ImmunizationStatus status) {
                m_statusMap.put(immunizationType, status);
            }

            /**
             * Gets the outer type.
             *
             * @return Immunization accumulator
             */
            private ImmunizationAccumulator getOuterType() {
                return ImmunizationAccumulator.this;
            }

        }

        private Map<ImmunizationStatus, Map<String, Set<StudentInfo>>> m_accumulations = new HashMap();
        private Set<StudentInfo> m_allStudents = new HashSet();
        private Map<String, Collection<HealthImmunizationDefinition>> m_definitionCache;
        private Collection<HealthImmunizationRuleAttributes> m_definitions;
        private Map<String, Map<String, List<HealthImmunizationDose>>> m_doses;
        private Map<String, ImmunizationRuleEngine> m_engines = new HashMap();
        private GradeLevelHistory m_history;
        private Map<String, String> m_immunizationCodeMap = new HashMap();
        private List<String> m_immunizationTypes;
        private SisSchool m_school;
        private Map<String, Map<String, HealthImmunizationRuleInstance>> m_series;

        /**
         * Instantiates a new immunization accumulator.
         *
         * @param school SisSchool
         */
        public ImmunizationAccumulator(SisSchool school) {
            m_school = school;
        }

        /**
         * Adds the.
         *
         * @param student SisStudent
         * @throws ImmunizationRuleException exception
         */
        public void add(SisStudent student) throws ImmunizationRuleException {
            StudentInfo info = new StudentInfo(student);
            for (HealthImmunizationRuleAttributes ruleAttributes : getRuleAttributes()) {
                // Get state code - skip series that are not on state list
                String code = getImmunizationCode(ruleAttributes);
                if (StringUtils.isEmpty(code) || StringUtils.isEmpty(ruleAttributes.getRuleDefinition())) {
                    continue;
                }
                // get series and doses
                HealthImmunizationRuleInstance ruleInstance = getRuleInstance(ruleAttributes, student);
                List<HealthImmunizationDose> doses = getDoses(ruleAttributes, student);

                // Get compliance status.
                ImmunizationRuleEngine engine = getImmunizationRuleEngine(ruleAttributes);
                boolean isCompliant = engine.evaluateCompliance(student, doses) ||
                        (ruleInstance != null && ruleInstance.getComplianceOverrideIndicator());
                info.addStatus(code, isCompliant, ruleInstance, doses);
            }
            accumulate(info);
        }

        /**
         * Gets the immunization types.
         *
         * @return List
         */
        public List<String> getImmunizationTypes() {
            if (m_immunizationTypes == null) {
                m_immunizationTypes = getInputList(INPUT_PARAM_IMMUNIZATION_TYPES);
            }
            return m_immunizationTypes;
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        public String getSchoolName() {
            return m_school.getName();
        }

        /**
         * Status count.
         *
         * @param status ImmunizationStatus
         * @param immunizationType String
         * @return Integer
         */
        public Integer statusCount(ImmunizationStatus status, String immunizationType) {
            int count = 0;
            Map<String, Set<StudentInfo>> typeAccumulator = m_accumulations.get(status);
            if (typeAccumulator != null) {
                Set<StudentInfo> students = typeAccumulator.get(immunizationType);
                if (students != null) {
                    count = students.size();
                }
            }
            return Integer.valueOf(count);
        }

        /**
         * Status list.
         *
         * @param status ImmunizationStatus
         * @param immunizationType String
         * @return List
         */
        public Collection<StudentInfo> statusList(ImmunizationStatus status, String immunizationType) {
            List<StudentInfo> list = EMPTY_LIST;
            Map<String, Set<StudentInfo>> typeAccumulator = m_accumulations.get(status);
            if (typeAccumulator != null) {
                Set<StudentInfo> students = typeAccumulator.get(immunizationType);
                if (students != null) {
                    list = new ArrayList(students);
                    RIImmunizationSummary.sort(list);
                }
            }
            return list;
        }

        /**
         * Total count.
         *
         * @return Integer
         */
        public Integer totalCount() {
            return Integer.valueOf(m_allStudents.size());
        }

        /**
         * Accumulate.
         *
         * @param info StudentInfo
         */
        private void accumulate(StudentInfo info) {
            boolean hasNoRecords = true;
            for (String immunizationType : getImmunizationTypes()) {
                ImmunizationStatus status = info.getImmunizationStatus(immunizationType);
                if (status != null && !ImmunizationStatus.NO_RECORD.equals(status)) {
                    hasNoRecords = false;
                }
            }

            m_allStudents.add(info);
            Boolean first = true;
            ImmunizationStatus allStatuses = null;
            for (String immunizationType : getImmunizationTypes()) {
                ImmunizationStatus status = info.getImmunizationStatus(immunizationType);
                if (status == null || status.equals(ImmunizationStatus.NO_RECORD)) {
                    status = ImmunizationStatus.NOT_COMPLIANT;
                }
                if (first) {
                    allStatuses = status;
                    first = false;
                } else if (allStatuses != null && !allStatuses.equals(status)) {
                    allStatuses = null;
                }
                accumulateStatus(status, immunizationType, info);
                accumulateStatus(status, IMMUNIZATION_TYPE_UNDUPLICATED, info);
            }
            if (hasNoRecords) {
                accumulateStatus(ImmunizationStatus.NO_RECORD, IMMUNIZATION_TYPE_ALL, info);
            } else if (allStatuses != null) {
                accumulateStatus(allStatuses, IMMUNIZATION_TYPE_ALL, info);
            }

        }

        /**
         * Accumulate status.
         *
         * @param status ImmunizationStatus
         * @param immunizationType String
         * @param info StudentInfo
         */
        private void accumulateStatus(ImmunizationStatus status, String immunizationType, StudentInfo info) {
            Map<String, Set<StudentInfo>> typeAccumulator = m_accumulations.get(status);
            if (typeAccumulator == null) {
                typeAccumulator = new HashMap();
                m_accumulations.put(status, typeAccumulator);
            }

            Set<StudentInfo> students = typeAccumulator.get(immunizationType);
            if (students == null) {
                students = new HashSet();
                typeAccumulator.put(immunizationType, students);
            }

            students.add(info);
        }

        /**
         * Gets the doses.
         *
         * @param ruleAttributes HealthImmunizationRuleAttributes
         * @param student SisStudent
         * @return List
         */
        private List<HealthImmunizationDose> getDoses(HealthImmunizationRuleAttributes ruleAttributes,
                                                      SisStudent student) {
            if (m_doses == null) {
                SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
                Criteria doseCriteria = new Criteria();
                doseCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, studentSubQuery);

                QueryByCriteria doseQuery =
                        new QueryByCriteria(HealthImmunizationDose.class, doseCriteria);
                doseQuery.addOrderByAscending(HealthImmunizationDose.COL_DATE);

                String[] columns = new String[] {HealthImmunizationDose.REL_IMMUNIZATION_SERIES + "."
                        + HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                        HealthImmunizationSeries.COL_STUDENT_OID};
                int[] sizes = new int[] {16, 128};

                m_doses = getBroker().getGroupedCollectionByQuery(doseQuery, columns, sizes);
            }

            List<HealthImmunizationDose> dosesList = new LinkedList<>();

            if (ruleAttributes != null) {
                for (HealthImmunizationDefinition definition : getImmunizationDefinitions(ruleAttributes)) {
                    Map<String, List<HealthImmunizationDose>> doseMap = m_doses.get(definition.getOid());
                    if (doseMap != null) {
                        List<HealthImmunizationDose> dosesForStudent = doseMap.get(student.getOid());
                        if (dosesForStudent != null) {
                            dosesList.addAll(dosesForStudent);
                        }
                    }
                }
            }

            // The doses list must be sorted by date before passing into the compliance engine
            Comparator comparator = new Comparator<HealthImmunizationDose>() {
                @Override
                public int compare(HealthImmunizationDose o1, HealthImmunizationDose o2) {
                    if (o1.getDate() == null) {
                        return -1;
                    } else if (o2.getDate() == null) {
                        return 1;
                    } else {
                        return o1.getDate().compareTo(o2.getDate());
                    }
                }
            };

            Collections.sort(dosesList, comparator);

            return dosesList;
        }

        /**
         * Gets the grade level history.
         *
         * @return Grade level history
         */
        private GradeLevelHistory getGradeLevelHistory() {
            if (m_history == null) {
                m_history = new GradeLevelHistory(getStudentCriteria(), 20,
                        OrganizationManager.getRootOrganization(getOrganization()), getBroker());
            }
            return m_history;
        }

        /**
         * Gets the immunization code.
         *
         * @param ruleInstance HealthImmunizationRuleAttributes
         * @return String
         */
        private String getImmunizationCode(HealthImmunizationRuleAttributes ruleInstance) {
            String oid = ruleInstance.getOid();
            String code = m_immunizationCodeMap.get(oid);

            if (code == null) {
                if (ruleInstance instanceof HealthImmunizationDefinition) {
                    HealthImmunizationDefinition defn = (HealthImmunizationDefinition) ruleInstance;
                    code = (String) defn.getFieldValueByBeanPath(getAliasField(ALIAS_HIM_IMMUNIZATION_TYPE));
                } else if (ruleInstance instanceof HealthImmunizationGroup) {
                    HealthImmunizationGroup grp = (HealthImmunizationGroup) ruleInstance;
                    code = (String) grp.getFieldValueByBeanPath(getAliasField(ALIAS_HIM_IMMUNIZATION_TYPE));
                }
                m_immunizationCodeMap.put(oid, code == null ? "" : code);
            }
            return code;
        }

        /**
         * Gets the immunization definitions.
         *
         * @param ruleAttributes HealthImmunizationRuleAttributes
         * @return Collection
         */
        private Collection<HealthImmunizationDefinition> getImmunizationDefinitions(HealthImmunizationRuleAttributes ruleAttributes) {
            if (m_definitionCache == null) {
                m_definitionCache = new HashMap();
            }
            Collection<HealthImmunizationDefinition> defns = m_definitionCache.get(ruleAttributes.getOid());
            if (defns == null) {
                defns = ruleAttributes.getImmunizationDefinitions(getBroker());
                m_definitionCache.put(ruleAttributes.getOid(), defns);
            }
            return defns;
        }

        /**
         * Gets the immunization rule engine.
         *
         * @param ruleAttributes HealthImmunizationRuleAttributes
         * @return Immunization rule engine
         * @throws ImmunizationRuleException exception
         */
        private ImmunizationRuleEngine getImmunizationRuleEngine(HealthImmunizationRuleAttributes ruleAttributes)
                throws ImmunizationRuleException {
            ImmunizationRuleEngine engine = m_engines.get(ruleAttributes.getOid());
            if (engine == null && !m_engines.containsKey(ruleAttributes.getOid())) {
                engine = new ImmunizationRuleEngine(ruleAttributes, getGradeLevelHistory(), getBroker());
                m_engines.put(ruleAttributes.getOid(), engine);
            }
            return engine;
        }

        /**
         * Gets the rule attributes.
         *
         * @return Collection
         */
        private Collection<HealthImmunizationRuleAttributes> getRuleAttributes() {
            if (m_definitions == null) {
                X2Criteria groupCriteria = new X2Criteria();
                groupCriteria.addIn(getAliasField(ALIAS_HIG_IMMUNIZATION_TYPE),
                        getImmunizationTypes());
                groupCriteria.addEqualTo(HealthImmunizationGroup.COL_REQUIRED_INDICATOR, Boolean.TRUE);

                BeanQuery groupQuery = new BeanQuery(HealthImmunizationGroup.class, groupCriteria);
                m_definitions = getBroker().getCollectionByQuery(groupQuery);

                X2Criteria defnCriteria = new X2Criteria();
                defnCriteria.addIn(getAliasField(ALIAS_HIM_IMMUNIZATION_TYPE),
                        getImmunizationTypes());
                defnCriteria.addEqualTo(HealthImmunizationDefinition.COL_REQUIRED_INDICATOR, Boolean.TRUE);

                BeanQuery defnQuery = new BeanQuery(HealthImmunizationDefinition.class, defnCriteria);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(defnQuery)) {
                    while (iterator.hasNext()) {
                        HealthImmunizationDefinition defn = (HealthImmunizationDefinition) iterator.next();
                        if (defn.getImmunizationGroupMembers().isEmpty()) {
                            m_definitions.add(defn);
                        }
                    }
                }
            }
            return m_definitions;
        }

        /**
         * Gets the rule instance.
         *
         * @param ruleAttributes HealthImmunizationRuleAttributes
         * @param student SisStudent
         * @return Health immunization rule instance
         */
        private HealthImmunizationRuleInstance getRuleInstance(HealthImmunizationRuleAttributes ruleAttributes,
                                                               SisStudent student) {
            if (m_series == null) {
                SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
                // series
                X2Criteria seriesMapCriteria = new X2Criteria();
                seriesMapCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, studentSubQuery);
                seriesMapCriteria.addIn(
                        HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + ModelProperty.PATH_DELIMITER
                                + getAliasField(ALIAS_HIM_IMMUNIZATION_TYPE),
                        getImmunizationTypes());
                seriesMapCriteria.addEqualTo(
                        HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + ModelProperty.PATH_DELIMITER +
                                HealthImmunizationDefinition.COL_REQUIRED_INDICATOR,
                        Boolean.TRUE);

                BeanQuery seriesQuery = new BeanQuery(HealthImmunizationSeries.class, seriesMapCriteria);
                seriesQuery.addOrderBy(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, true);
                seriesQuery.addOrderBy(HealthImmunizationSeries.COL_STUDENT_OID, true);
                m_series = getBroker().getNestedMapByQuery(seriesQuery,
                        HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                        HealthImmunizationSeries.COL_STUDENT_OID,
                        16,
                        128);

                // group immunizations
                Criteria overridesCriteria = new Criteria();
                overridesCriteria.addIn(HealthImmunizationGroupOverride.COL_STUDENT_OID, studentSubQuery);
                seriesMapCriteria.addIn(
                        HealthImmunizationGroupOverride.REL_IMMUNIZATION_GROUP + ModelProperty.PATH_DELIMITER
                                + getAliasField(ALIAS_HIG_IMMUNIZATION_TYPE),
                        getImmunizationTypes());
                seriesMapCriteria.addEqualTo(
                        HealthImmunizationGroupOverride.REL_IMMUNIZATION_GROUP + ModelProperty.PATH_DELIMITER +
                                HealthImmunizationGroup.COL_REQUIRED_INDICATOR,
                        Boolean.TRUE);

                QueryByCriteria overridesQuery =
                        new QueryByCriteria(HealthImmunizationGroupOverride.class, overridesCriteria);

                m_series.putAll(getBroker().getNestedMapByQuery(overridesQuery,
                        HealthImmunizationGroupOverride.COL_IMMUNIZATION_GROUP_OID,
                        HealthImmunizationGroupOverride.COL_STUDENT_OID,
                        16,
                        128));
            }

            HealthImmunizationRuleInstance ruleInstance = null;
            Map<String, HealthImmunizationRuleInstance> ruleInstanceMap = m_series.get(ruleAttributes.getOid());
            if (ruleInstanceMap != null) {
                ruleInstance = ruleInstanceMap.get(student.getOid());
            }
            return ruleInstance;
        }

    }

    /**
     * The Class ImmunizationGrid.
     */
    class ImmunizationGrid extends ReportDataGrid {

        /**
         * Adds the row.
         *
         * @param accumulator ImmunizationAccumulator
         * @param fieldName String
         * @param value String
         * @param collection Collection<StudentInfo>
         */
        public void addRow(ImmunizationAccumulator accumulator,
                           String fieldName,
                           String value,
                           Collection<StudentInfo> collection) {
            if (collection == null || collection.isEmpty() || !includeDetails()) {
                append();
                set(FIELD_SCHOOL_NAME, accumulator.getSchoolName());
                set(FIELD_NAME, fieldName);
                set(FIELD_VALUE, value);
            } else {
                for (StudentInfo info : collection) {
                    append();
                    set(FIELD_SCHOOL_NAME, accumulator.getSchoolName());
                    set(FIELD_NAME, fieldName);
                    set(FIELD_VALUE, value);
                    set(FIELD_STUDENT_NAME, info.getStudent().getNameView());
                    set(FIELD_STUDENT_LOCAL_ID, info.getStudent().getLocalId());
                }
            }
        }
        //

        /**
         * Adds the type rows.
         *
         * @param accumulator ImmunizationAccumulator
         * @param fieldName String
         * @param status ImmunizationStatus
         */
        public void addTypeRows(ImmunizationAccumulator accumulator, String fieldName, ImmunizationStatus status) {
            int cnt = 0;
            for (String type : accumulator.getImmunizationTypes()) {
                String formattedFieldName = String.format(fieldName, ++cnt);
                addRow(accumulator, formattedFieldName + type, accumulator.statusCount(status, type).toString(),
                        accumulator.statusList(status, type));
            }
        }
    }

    private Map<String, DataDictionaryField> m_aliasMap = new HashMap();
    private HashMap<DataDictionaryField, SystemStringConverter> m_converterMap;
    private Boolean m_details;
    private Map<String, ImmunizationAccumulator> m_schoolAccumulators = new TreeMap();
    private X2Criteria m_studentCriteria;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        X2Criteria criteria = getStudentCriteria();
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                getImmunizationAccumulator(student.getSchool()).add(student);
            }
        }

        ImmunizationGrid grid = new ImmunizationGrid();

        for (Entry<String, ImmunizationAccumulator> item : m_schoolAccumulators.entrySet()) {
            ImmunizationAccumulator accumulator = item.getValue();
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_A_HEADING))) {
                grid.addRow(accumulator, "Q A. - " + getParameter(INPUT_PARAM_Q_A_HEADING),
                        accumulator.totalCount().toString(), null);
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_B_HEADING))) {
                grid.addRow(accumulator, "Q B. - " + getParameter(INPUT_PARAM_Q_B_HEADING),
                        accumulator.statusCount(ImmunizationStatus.COMPLIANT, IMMUNIZATION_TYPE_ALL).toString(), null);
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_C_HEADING))) {
                List<StudentInfo> list = null;
                if (accumulator.getImmunizationTypes().size() == 1) {
                    list = new ArrayList();
                    list.addAll(accumulator.statusList(ImmunizationStatus.NOT_COMPLIANT,
                            accumulator.getImmunizationTypes().get(0)));
                    list.addAll(accumulator.statusList(ImmunizationStatus.MEDICAL,
                            accumulator.getImmunizationTypes().get(0)));
                    list.addAll(accumulator.statusList(ImmunizationStatus.RELIGIOUS,
                            accumulator.getImmunizationTypes().get(0)));
                    list.addAll(accumulator.statusList(ImmunizationStatus.TEMPORARY,
                            accumulator.getImmunizationTypes().get(0)));
                    sort(list);
                }
                grid.addRow(accumulator, "Q C. - " + getParameter(INPUT_PARAM_Q_C_HEADING),
                        Integer.toString(accumulator.totalCount().intValue() - accumulator
                                .statusCount(ImmunizationStatus.COMPLIANT, IMMUNIZATION_TYPE_ALL).intValue()),
                        list);
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_C1_TOT_HEADING))) {
                grid.addRow(accumulator, "Q C1 - " + getParameter(INPUT_PARAM_Q_C1_TOT_HEADING),
                        accumulator.statusCount(ImmunizationStatus.MEDICAL, IMMUNIZATION_TYPE_UNDUPLICATED).toString(),
                        accumulator.statusList(ImmunizationStatus.MEDICAL, IMMUNIZATION_TYPE_UNDUPLICATED));
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_C1_HEADING))) {
                grid.addTypeRows(accumulator, "Q C1  - " + getParameter(INPUT_PARAM_Q_C1_HEADING),
                        ImmunizationStatus.MEDICAL);
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_C2_TOT_HEADING))) {
                grid.addRow(accumulator, "Q C2 - " + getParameter(INPUT_PARAM_Q_C2_TOT_HEADING),
                        accumulator.statusCount(ImmunizationStatus.RELIGIOUS, IMMUNIZATION_TYPE_UNDUPLICATED)
                                .toString(),
                        accumulator.statusList(ImmunizationStatus.RELIGIOUS, IMMUNIZATION_TYPE_UNDUPLICATED));
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_C2_HEADING))) {
                grid.addTypeRows(accumulator, "Q C2 - " + getParameter(INPUT_PARAM_Q_C2_HEADING),
                        ImmunizationStatus.RELIGIOUS);
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_D_HEADING))) {
                grid.addRow(accumulator, "Q D - " + getParameter(INPUT_PARAM_Q_D_HEADING),
                        accumulator.statusCount(ImmunizationStatus.NOT_COMPLIANT, IMMUNIZATION_TYPE_UNDUPLICATED)
                                .toString(),
                        accumulator.statusList(ImmunizationStatus.NOT_COMPLIANT, IMMUNIZATION_TYPE_UNDUPLICATED));
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_D1_HEADING))) {
                grid.addRow(accumulator, (String) getParameter(INPUT_PARAM_Q_D1_HEADING),
                        accumulator.statusCount(ImmunizationStatus.NO_RECORD, IMMUNIZATION_TYPE_ALL).toString(),
                        accumulator.statusList(ImmunizationStatus.NO_RECORD, IMMUNIZATION_TYPE_ALL));
            }
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_Q_E_HEADING))) {
                grid.addTypeRows(accumulator, "Q E%d - " + getParameter(INPUT_PARAM_Q_E_HEADING),
                        ImmunizationStatus.NOT_COMPLIANT);
            }
        }
        grid.beforeTop();

        return grid;
    }

    /**
     * Gets the alias field.
     *
     * @param alias String
     * @return String
     */
    protected DataDictionaryField getAliasDataDictionaryField(String alias) {
        if (!m_aliasMap.containsKey(alias)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                throw new IllegalStateException("Alias " + alias + " is required");
            }
            m_aliasMap.put(alias, field);
        }
        return m_aliasMap.get(alias);
    }

    /**
     * Gets the alias field.
     *
     * @param alias String
     * @return String
     */
    protected String getAliasField(String alias) {
        if (!m_aliasMap.containsKey(alias)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                throw new IllegalStateException("Alias " + alias + " is required");
            }
            m_aliasMap.put(alias, field);
        }
        return m_aliasMap.get(alias).getJavaName();
    }

    /**
     * For a DataDictionaryField, find a SystemStringConverter appropriate for the field.
     * If no converter is appropriate, return null.
     * Use a map to maintain a cache of converters.
     *
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
                        LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
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
     * Include details.
     *
     * @return true, if successful
     */
    protected boolean includeDetails() {
        if (m_details == null) {
            Object param = getParameter(INPUT_PARAM_DETAILS);
            if (param != null && param instanceof Boolean) {
                m_details = ((Boolean) param).booleanValue();
            } else {
                m_details = Boolean.FALSE;
            }
        }
        return m_details.booleanValue();
    }

    /**
     * Gets the immunization accumulator.
     *
     * @param school SisSchool
     * @return Immunization accumulator
     */
    private ImmunizationAccumulator getImmunizationAccumulator(SisSchool school) {
        ImmunizationAccumulator accumulator = null;
        String key = school.getName() + school.getOid();
        accumulator = m_schoolAccumulators.get(key);
        if (accumulator == null) {
            accumulator = new ImmunizationAccumulator(school);
            m_schoolAccumulators.put(key, accumulator);
        }
        return accumulator;
    }

    /**
     * Gets the input list.
     *
     * @param parameter String
     * @return List
     */
    private List<String> getInputList(String parameter) {
        List<String> schoolOids = null;
        Object param = getParameter(parameter);
        if (param != null && param instanceof String && !StringUtils.isEmpty((String) param)) {
            schoolOids = Arrays.asList(((String) param).split(","));
        } else {
            schoolOids = new ArrayList();
            schoolOids.add("--dummyvalue--");
        }
        return schoolOids;
    }

    /**
     * Gets the student criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentCriteria() {
        if (m_studentCriteria == null) {
            m_studentCriteria = new X2Criteria();
            m_studentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    Student.COL_ENROLLMENT_STATUS));
            if (processAllSchools()) {
                m_studentCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                m_studentCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            } else {
                m_studentCriteria.addIn(Student.COL_SCHOOL_OID, getInputList(INPUT_PARAM_SCHOOL_OIDS));
            }
            String gradeLevel = (String) getParameter(INPUT_PARAM_GRADE_LEVEL);
            m_studentCriteria.addEqualTo(SisStudent.COL_GRADE_LEVEL, gradeLevel);
        }
        return m_studentCriteria;
    }

    /**
     * Process all schools.
     *
     * @return true, if successful
     */
    private boolean processAllSchools() {
        boolean value = false;
        if (getParameter(INPUT_PARAM_ALL_SCHOOLS) != null && getParameter(INPUT_PARAM_ALL_SCHOOLS) instanceof Boolean) {
            value = ((Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS)).booleanValue();
        }
        return value;
    }

}
