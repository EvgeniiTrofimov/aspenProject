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

package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Copy of {@link com.x2dev.procedures.statereporting.PARCCAssessmentCreationProcedure
 * PARCCAssessmentCreationProcedure} for MD
 *
 * @author Follett Software Company
 *
 */
public class PARCCAssessmentCreationProcedureMD extends ProcedureJavaSource {
    private static final String ALIAS_CRS_PARCC_TEST_CODE = "all-crs-PARCCTestCode";
    private static final String ALIAS_PARCC_TESTCODE = "PARCCTSTCODE";
    private static final String ALIAS_PARCC_TESTDATE = "PARCCTSTDATE";
    private static final String ALIAS_PARCC_TESTFORMAT = "PARCCTSTFORMAT";
    private static final String ALIAS_PARCC_TESTYEAR = "PARCCTSTYEAR";
    private static final String ALIAS_PARCC_TESTPERIOD = "PARCCTSTPERIOD";

    private static final String INPUT_PARAM_ASD_OID = "asdOid";
    private static final String INPUT_PARAM_ASM_DATE = "asmDate";
    private static final String INPUT_PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String INPUT_PARAM_CREATE_BASED_ON = "createBasedOn";
    private static final String INPUT_PARAM_CTX = "schoolYearContext";
    private static final String INPUT_PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    private static final String INPUT_PARAM_CYCLE = "cycle";
    private static final String INPUT_EXCLUDE_SPED_ALIAS = "excludeSpedAlias";
    private static final String INPUT_PARAM_GRADES = "gradeList";
    private static final String INPUT_PARAM_QUERY_BY_FIELD = "queryBy";
    private static final String INPUT_PARAM_QUERY_BY_CRITERIA = "queryString";
    private static final String INPUT_PARAM_SECTIONS = "courseSections";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_CODE = "testCode";
    private static final String INPUT_PARAM_TEST_FORMAT = "testFormat";

    /**
     * Other constants.
     */
    private static final String COURSE = "Course";
    private static final String EMPTY_STRING = "";
    private static final String[] VALID_GRADE_LIST = {"03", "04", "05", "06", "07", "08", "09", "10", "11"};

    /**
     * Class members
     */
    private AssessmentDefinition m_asd;
    private PlainDate m_asmDate;
    private Map<String, Collection<StudentAssessment>> m_asmMap;
    private String m_assessmentTestCodeField;
    private String m_assessmentTestDateField;
    private String m_assessmentTestFormatField;
    private String m_assessmentTestPeriodField;
    private String m_assessmentTestYearField;
    private int m_countCreated;
    private int m_countSkipped;
    private String m_cycle;
    private String m_createBasedOn;
    private String m_ctxForParccValue;
    private DateAsStringConverter m_dateConverter;
    private List<String> m_detailMessages = new LinkedList();
    private DataDictionary m_dictionary;
    private String m_fieldCrsTestCode;
    private X2Criteria m_sscCriteria;
    private X2Criteria m_stdCriteria;
    private Map<String, Collection<StudentSchedule>> m_sscMap;
    private Collection<SisStudent> m_stdCollection;
    private Set<String> m_testCodes = new HashSet();
    private String m_testFormat;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (m_sscMap != null) {
            for (Entry<String, Collection<StudentSchedule>> entry : m_sscMap.entrySet()) {
                String stdOid = entry.getKey();
                Collection<StudentSchedule> sscList = entry.getValue();
                Collection<StudentAssessment> asmList = m_asmMap.get(stdOid);
                HashSet<String> testCodes = new HashSet<String>();
                if (!sscList.isEmpty()) {
                    for (StudentSchedule ssc : sscList) {
                        String testCode =
                                (String) ssc.getSection().getSchoolCourse().getCourse()
                                        .getFieldValueByBeanPath(m_fieldCrsTestCode);
                        if (testCodes.add(testCode)) {
                            createNewAsm(ssc, asmList, testCode);
                        }
                    }
                }
            }
        }
        if (m_stdCollection != null) {
            for (SisStudent std : m_stdCollection) {
                String stdOid = std.getOid();
                Collection<StudentAssessment> asmList = m_asmMap.get(stdOid);
                for (String testCode : m_testCodes) {
                    createNewAsm(std, asmList, testCode);
                }
            }
        }
        if (m_countCreated > 0 || m_countSkipped > 0) {
            logMessage("Student Assessments created is " + m_countCreated);
            logMessage("Student Assessments already existing is  " + m_countSkipped);
            logMessage("");
        }
        for (String detail : m_detailMessages) {
            logMessage(detail);
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                getLocale(), true);

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_cycle = (String) getParameter(INPUT_PARAM_CYCLE);
        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByOid(DistrictSchoolYearContext.class, (String) getParameter(INPUT_PARAM_CTX));
        m_createBasedOn = (String) getParameter(INPUT_PARAM_CREATE_BASED_ON);
        m_ctxForParccValue = (String) ctx.getFieldValueByBeanPath((String) getParameter(INPUT_PARAM_CTX_BEAN_PATH));
        m_asmDate = (PlainDate) getParameter(INPUT_PARAM_ASM_DATE);

        if (m_asmDate.after(ctx.getEndDate())) {
            m_asmDate = ctx.getEndDate();
        } else if (m_asmDate.before(ctx.getStartDate())) {
            m_asmDate = ctx.getStartDate();
        }
        String testCodeOids = (String) getParameter(INPUT_PARAM_TEST_CODE);
        if (!StringUtils.isEmpty(testCodeOids)) {
            for (String codeOid : testCodeOids.split(",")) {
                ReferenceCode code = getBroker().getBeanByOid(ReferenceCode.class, codeOid);
                if (code != null) {
                    m_testCodes.add(code.getCode());
                }
            }
        }
        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;
        m_fieldCrsTestCode = getFieldJavaNameByAliasForCode(m_dictionary, ALIAS_CRS_PARCC_TEST_CODE);

        if (!StringUtils.isEmpty(m_fieldCrsTestCode)) {
            if (initializeASDById()) {
                if (COURSE.equals(m_createBasedOn)) {
                    populateSSCMap();
                } else {
                    populateSTDCollection();
                }
                populateASMMap();
            }
        }
    }

    /**
     * Apply the standard user input criteria from input template to a Criteria object.
     *
     * <p>
     * Multiple criteria supported.
     * <br>
     * Iterate through enumerated values of PARAM_QUERY_BY_FIELD + "#" as "#" increments from 1
     * to n.
     *
     * <p>
     * The prefix path is the relationship bean path from to apply to the current bean to take it
     * to the
     * expected base bean of the criteria.
     * <br>
     * EX: If the user input selection criteria is for the Student table, but the criteria is
     * for a related
     * table StudentEnrollment, the prefix path would be "student" (or
     * StudentEnrollment.REL_STUDENT)
     * to get from the StudentEnrollment to the Student table.
     *
     * @param criteria Criteria
     * @param prefixPath String
     */
    private void applyInputCriteria(Criteria criteria, String prefixPath) {
        String fullPrefixPath = EMPTY_STRING;
        if (!StringUtils.isEmpty(prefixPath)) {
            fullPrefixPath = prefixPath + ModelProperty.PATH_DELIMITER;
        }
        String queryBy = (String) getParameter(INPUT_PARAM_QUERY_BY_FIELD);
        String queryString = (String) getParameter(INPUT_PARAM_QUERY_BY_CRITERIA);
        if (!StringUtils.isEmpty(queryBy) && !StringUtils.isEmpty(queryString)) {
            if (queryBy.equals("##all")) {
                // Do nothing, select all.
            } else if (queryBy.equals("##snapshot")) {
                SubQuery recordSetSubquery = ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
                Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);
                criteria.addIn(fullPrefixPath + X2BaseBean.COL_OID, objectOids);
            } else if (queryBy.startsWith("a:")) {
                String resolvedAliasBeanPath = getResolvedAliasBeanPath(queryBy);
                if (resolvedAliasBeanPath != null) {
                    criteria.addEqualTo(fullPrefixPath + resolvedAliasBeanPath, queryString);
                }
            } else {
                criteria.addEqualTo(fullPrefixPath + queryBy, queryString);
            }
        }
    }

    /**
     * Creates the new asm.
     *
     * @param std SisStudent
     * @param asmList Collection<StudentAssessment>
     * @param testCode String
     */
    private void createNewAsm(SisStudent std, Collection<StudentAssessment> asmList, String testCode) {
        boolean isAsmAlreadyCreated = false;

        if (asmList != null) {
            for (StudentAssessment asm : asmList) {
                String testedTestCode = (String) asm.getFieldValueByBeanPath(m_assessmentTestCodeField);
                String testedTestPeriod = (String) asm.getFieldValueByBeanPath(m_assessmentTestPeriodField);
                String testedTestContextValue = (String) asm.getFieldValueByBeanPath(m_assessmentTestYearField);

                if (testCode.equals(testedTestCode) && m_cycle != null && m_cycle.equals(testedTestPeriod) &&
                        m_ctxForParccValue != null && m_ctxForParccValue.equals(testedTestContextValue)) {
                    isAsmAlreadyCreated = true;
                    ++m_countSkipped;
                    logDetail("Student Assessment already exists: Student: " + std.getNameView() + ", MD Test Code: "
                            + testCode + ", Period: " + m_cycle);
                    break;
                }
            }
        }

        if (!isAsmAlreadyCreated) {
            saveNewAsm(std, testCode);
        }
    }

    /**
     * Check if it is needed to save candidate ASM to the DB.
     *
     * @param ssc StudentSchedule
     * @param asmList Collection<StudentAssessment>
     * @param testCode String
     */
    private void createNewAsm(StudentSchedule ssc, Collection<StudentAssessment> asmList, String testCode) {
        boolean isAsmAlreadyCreated = false;

        if (asmList != null) {
            for (StudentAssessment asm : asmList) {
                String testedTestCode = (String) asm.getFieldValueByBeanPath(m_assessmentTestCodeField);
                String testedTestPeriod = (String) asm.getFieldValueByBeanPath(m_assessmentTestPeriodField);
                String testedTestContextValue = (String) asm.getFieldValueByBeanPath(m_assessmentTestYearField);

                if (testCode.equals(testedTestCode) && m_cycle != null && m_cycle.equals(testedTestPeriod) &&
                        m_ctxForParccValue != null && m_ctxForParccValue.equals(testedTestContextValue)) {
                    isAsmAlreadyCreated = true;
                    ++m_countSkipped;
                    logDetail("Student Assessment already exists: Student: " + ssc.getStudent().getNameView()
                            + ", PARCC Test Code: " + testCode + ", Period: " + m_cycle);
                    break;
                }
            }
        }

        if (!isAsmAlreadyCreated) {
            saveNewAsm(ssc);
        }
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @param isRequired boolean
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary, boolean isRequired) {
        String javaName = null;

        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        } else if (isRequired) {
            logMessage("Assessment column for " + alias + " is not defined");
        }

        return javaName;
    }

    /**
     * Check if alias define correct field.
     *
     * @param dictionary DataDictionary
     * @param alias String
     * @return String
     */
    private String getFieldJavaNameByAliasForCode(DataDictionary dictionary, String alias) {
        DataDictionaryField ddField = null;
        String value = null;
        if (dictionary != null && (ddField = dictionary.findDataDictionaryFieldByAlias(alias)) != null) {
            if (alias.split("-")[1].toUpperCase().equals(ddField.getDataTable().getObjectPrefix())) {
                value = ddField.getJavaName();
            } else {
                logMessage("Alias " + alias + " is defined on the wrong table");
            }
        } else {
            logMessage("Alias " + alias + " is not defined");
        }

        return value;
    }

    /**
     * Returns the resolved bean path of the passed alias field.
     * <p>
     * Detect if the path contains an alias request. And alias request can be either:
     * <ul>
     * <li>prefix a:</li>
     * <li>last field in square brackets []</li>
     * </ul>
     * <p>
     * Returns the bean path if there is not an alias requested.
     * <p>
     * Returns the translated bean path (with a: and [] removed) with the alias translated to the
     * proper bean path.
     * <p>
     * Returns NULL if there is an alias request and the alias cannot be resolved.
     *
     * @param beanPath - sort by field for an alias
     *
     * @return String
     */
    private String getResolvedAliasBeanPath(String beanPath) {
        String resolvedAliasBeanPath = null;
        boolean isAliasRequest = false;

        if (beanPath.startsWith("a:")) {
            beanPath = beanPath.substring(2).trim();
            isAliasRequest = true;
        }

        String[] relationshipPaths = beanPath.split("\\.");

        String lastField = relationshipPaths[(relationshipPaths.length) - 1];
        int position = beanPath.indexOf(lastField);
        if (lastField.startsWith("[") && lastField.endsWith("]")) {
            lastField = lastField.substring(0, lastField.length() - 1).substring(1);
            isAliasRequest = true;
        }

        if (isAliasRequest) {
            DataDictionaryField aliasField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryFieldByAlias(lastField);

            if (aliasField != null) {
                if (position == 0) {
                    resolvedAliasBeanPath = aliasField.getJavaName();
                } else {
                    resolvedAliasBeanPath = beanPath.substring(0, position) + aliasField.getJavaName();
                }
            }
        } else {
            resolvedAliasBeanPath = beanPath;
        }

        return resolvedAliasBeanPath;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Map<String, SisSchool> getSchools() {
        Map<String, SisSchool> schools = new LinkedHashMap<String, SisSchool>();
        Object objSchools = getParameter(INPUT_PARAM_SCHOOLS);
        String schoolOids = objSchools == null ? "" : (String) objSchools;

        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

        if (!StringUtils.isEmpty(schoolOids)) {
            List<String> oids = Arrays.asList(schoolOids.split(","));
            schoolCriteria.addIn(X2BaseBean.COL_OID, oids);
        }

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
        schoolQuery.addOrderByAscending(SisSchool.COL_SCHOOL_LEVEL_CODE);
        schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
        schools = getBroker().getGroupedCollectionByQuery(schoolQuery, X2BaseBean.COL_OID, 1024);

        return schools;
    }

    /**
     * Find proper ASD by asdId.
     *
     * @return true, if successful
     */
    private boolean initializeASDById() {
        boolean retValue = false;
        String asdOid = (String) getParameter(INPUT_PARAM_ASD_OID);
        if (!StringUtils.isEmpty(asdOid)) {
            m_asd = (AssessmentDefinition) getBroker().getBeanByOid(AssessmentDefinition.class, asdOid);
        } else {
            X2Criteria asdCriteria = new X2Criteria();
            asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, getParameter(INPUT_PARAM_ASM_DEF_ID));

            m_asd = (AssessmentDefinition) getBroker()
                    .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));
        }

        if (m_asd == null) {
            logMessage("Required assessment definition with ID = " + getParameter(INPUT_PARAM_ASM_DEF_ID)
                    + " could not be found");
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(m_asd, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                logMessage("Extended Dictinary for PARCC could not be loaded");
            } else {
                m_assessmentTestCodeField = getAsmJavaName(ALIAS_PARCC_TESTCODE, dataDictionary, true);
                m_assessmentTestDateField = getAsmJavaName(ALIAS_PARCC_TESTDATE, dataDictionary, false);
                m_assessmentTestFormatField = getAsmJavaName(ALIAS_PARCC_TESTFORMAT, dataDictionary, true);
                m_assessmentTestPeriodField = getAsmJavaName(ALIAS_PARCC_TESTPERIOD, dataDictionary, true);
                m_assessmentTestYearField = getAsmJavaName(ALIAS_PARCC_TESTYEAR, dataDictionary, true);
                if (!StringUtils.isEmpty(m_assessmentTestCodeField) &&
                        !StringUtils.isEmpty(m_assessmentTestPeriodField) &&
                        !StringUtils.isEmpty(m_assessmentTestFormatField) &&
                        !StringUtils.isEmpty(m_assessmentTestYearField)) {
                    retValue = true;
                }
            }
        }
        return retValue;
    }

    /**
     * Get a list of the grade codes selected.
     *
     * @return List
     */
    private List<String> inputGrades() {
        List<String> grades = new LinkedList();
        if (StringUtils.isEmpty((String) getParameter(INPUT_PARAM_GRADES))) {
            grades.addAll(Arrays.asList(VALID_GRADE_LIST));
        } else {
            String[] rcdOids = ((String) getParameter(INPUT_PARAM_GRADES)).split(",");
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(rcdOids));

            ReportQueryByCriteria query =
                    new ReportQueryByCriteria(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    grades.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }
        }
        return grades;
    }

    /**
     * Add detail message that will be appended after other messages.
     *
     * @param key String
     */
    private void logDetail(String key) {
        m_detailMessages.add(key);
    }

    /**
     * Method to populate ASM map keyed on asmStdOid.
     */
    private void populateASMMap() {
        X2Criteria asmCriteria = new X2Criteria();

        asmCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_asd.getOid());
        asmCriteria.addEqualTo(m_assessmentTestPeriodField, m_cycle);
        asmCriteria.addEqualTo(m_assessmentTestYearField, m_ctxForParccValue);
        if (m_sscCriteria != null) {
            asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                    new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, m_sscCriteria));
        }
        if (m_stdCriteria != null) {
            asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_stdCriteria));
        }

        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);

        m_asmMap = getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID, 1024);
    }

    /**
     * Method to populate SSC map keyed on sscStdOid.
     */
    private void populateSSCMap() {
        m_sscMap = null;
        String[] mstOids = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SECTIONS))
                ? ((String) getParameter(INPUT_PARAM_SECTIONS)).split(",")
                : null;
        String ctxOid = (String) getParameter(INPUT_PARAM_CTX);

        m_sscCriteria = new X2Criteria();

        m_sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, ctxOid);

        m_sscCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldCrsTestCode, m_testCodes);

        if (mstOids != null) {
            m_sscCriteria.addIn(StudentSchedule.COL_SECTION_OID, Arrays.asList(mstOids));
        }

        m_sscCriteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_GRADE_LEVEL, inputGrades());

        m_sscCriteria.addIn(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                getSchools().keySet());

        Collection<String> activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization());
        m_sscCriteria.addIn(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.COL_ENROLLMENT_STATUS, activeStudentCodes);
        if (getParameter(INPUT_EXCLUDE_SPED_ALIAS) != null
                && !StringUtils.isEmpty((String) getParameter(INPUT_EXCLUDE_SPED_ALIAS))) {
            DataDictionaryField dataField =
                    m_dictionary.findDataDictionaryFieldByAlias((String) getParameter(INPUT_EXCLUDE_SPED_ALIAS));
            if (dataField != null) {
                m_sscCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + dataField.getJavaName(),
                        BooleanAsStringConverter.TRUE);
            }
        }
        if (getParameter(INPUT_PARAM_QUERY_BY_FIELD) != null) {
            applyInputCriteria(m_sscCriteria, StudentSchedule.REL_STUDENT);
        }
        QueryByCriteria sscQuery = new QueryByCriteria(StudentSchedule.class, m_sscCriteria);

        sscQuery.addOrderBy(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_START_DATE,
                true);

        m_sscMap = getBroker().getGroupedCollectionByQuery(sscQuery, StudentSchedule.COL_STUDENT_OID, 1024);

    }


    /**
     * Populate STD collection.
     */
    private void populateSTDCollection() {
        m_stdCriteria = new X2Criteria();
        m_stdCriteria.addIn(SisStudent.COL_GRADE_LEVEL, inputGrades());
        m_stdCriteria.addIn(SisStudent.COL_SCHOOL_OID, getSchools().keySet());
        Collection<String> activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization());
        m_stdCriteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, activeStudentCodes);
        if (getParameter(INPUT_EXCLUDE_SPED_ALIAS) != null
                && !StringUtils.isEmpty((String) getParameter(INPUT_EXCLUDE_SPED_ALIAS))) {
            DataDictionaryField dataField =
                    m_dictionary.findDataDictionaryFieldByAlias((String) getParameter(INPUT_EXCLUDE_SPED_ALIAS));
            if (dataField != null) {
                m_stdCriteria.addNotEqualTo(dataField.getJavaName(), BooleanAsStringConverter.TRUE);
            }
        }
        if (getParameter(INPUT_PARAM_QUERY_BY_FIELD) != null) {
            applyInputCriteria(m_stdCriteria, null);
        }
        QueryByCriteria stdQuery = new QueryByCriteria(SisStudent.class, m_stdCriteria);
        m_stdCollection = getBroker().getCollectionByQuery(stdQuery);
    }

    /**
     * Save new asm.
     *
     * @param std SisStudent
     * @param testCode String
     */
    private void saveNewAsm(SisStudent std, String testCode) {
        StudentAssessment newAsm = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        newAsm.setAssessmentDefinitionOid(m_asd.getOid());
        newAsm.setDate(m_asmDate);
        newAsm.setGradeLevelCode(std.getGradeLevel());
        newAsm.setStudentOid(std.getOid());
        newAsm.setSchoolOid(std.getSchoolOid());
        newAsm.setFieldValueByBeanPath(m_assessmentTestCodeField, testCode);
        if (!StringUtils.isEmpty(m_assessmentTestDateField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestDateField, m_dateConverter.getSystemString(m_asmDate));
        }
        newAsm.setFieldValueByBeanPath(m_assessmentTestFormatField, m_testFormat);
        if (!StringUtils.isEmpty(m_assessmentTestPeriodField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestPeriodField, m_cycle);
        }
        if (!StringUtils.isEmpty(m_assessmentTestYearField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestYearField, m_ctxForParccValue);
        }

        getBroker().saveBeanForced(newAsm);

        ++m_countCreated;
        logDetail("Student Assessment was created for: Student: " + std.getNameView() + ", MD Test Code: " + testCode
                + ", Period: " + m_cycle);
    }

    /**
     * Save ASM to DB.
     *
     * @param ssc StudentSchedule
     */
    private void saveNewAsm(StudentSchedule ssc) {
        String testCode =
                (String) ssc.getSection().getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldCrsTestCode);

        StudentAssessment newAsm = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        newAsm.setAssessmentDefinitionOid(m_asd.getOid());
        newAsm.setDate(m_asmDate);
        newAsm.setGradeLevelCode(ssc.getStudent().getGradeLevel());
        newAsm.setMasterScheduleOid(ssc.getSectionOid());
        newAsm.setStudentOid(ssc.getStudentOid());
        newAsm.setSchoolOid(ssc.getSchedule().getSchoolOid());
        newAsm.setSchoolCourseOid(ssc.getSection().getSchoolCourseOid());
        newAsm.setFieldValueByBeanPath(m_assessmentTestCodeField, testCode);
        if (!StringUtils.isEmpty(m_assessmentTestDateField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestDateField, m_dateConverter.getSystemString(m_asmDate));
        }
        newAsm.setFieldValueByBeanPath(m_assessmentTestFormatField, m_testFormat);
        if (!StringUtils.isEmpty(m_assessmentTestPeriodField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestPeriodField, m_cycle);
        }
        if (!StringUtils.isEmpty(m_assessmentTestYearField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestYearField, m_ctxForParccValue);
        }

        getBroker().saveBeanForced(newAsm);

        ++m_countCreated;
        logDetail("Student Assessment was created for: Student: " + ssc.getStudent().getNameView() + ", Section: "
                + ssc.getSection().getDescription());

    }

}
