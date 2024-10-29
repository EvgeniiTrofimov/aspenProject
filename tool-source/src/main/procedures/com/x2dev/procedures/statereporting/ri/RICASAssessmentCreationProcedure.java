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
package com.x2dev.procedures.statereporting.ri;

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
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class RICASAssessmentCreationProcedure.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class RICASAssessmentCreationProcedure extends ProcedureJavaSource {
    private static final String ALIAS_CRS_RICAS_TEST_CODE = "ri-crs-RICASTestCode";
    private static final String ALIAS_RICAS_SESSION_NAME = "RICASSESSIONNAME";
    private static final String ALIAS_RICAS_TEST_CODE = "RICASTSTCODE";
    private static final String ALIAS_RICAS_TEST_DATE = "RICASTSTDATE";
    private static final String ALIAS_RICAS_TEST_FORMAT = "RICASTSTFORMAT";
    private static final String ALIAS_RICAS_TEST_YEAR = "RICASTSTYEAR";
    private static final String ALIAS_RICAS_TEST_PERIOD = "RICASTSTPERIOD";

    private static final String EMPTY_STRING = "";

    private static final String INPUT_PARAM_ASM_DATE = "asmDate";
    private static final String INPUT_PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String INPUT_PARAM_CTX = "schoolYearContext";
    private static final String INPUT_PARAM_GRADES = "gradeList";
    private static final String INPUT_PARAM_QUERY_BY_FIELD = "queryBy";
    private static final String INPUT_PARAM_QUERY_BY_CRITERIA = "queryString";
    private static final String INPUT_PARAM_SECTIONS = "courseSections";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_FORMAT = "testFormat";
    private static final String INPUT_PARAM_TEST_PERIOD = "testPeriod";

    private AssessmentDefinition m_asd;
    private PlainDate m_asmDate;
    private Map<String, Collection<StudentAssessment>> m_asmMap;
    private String m_assessmentSessionNameField;
    private String m_assessmentTestCodeField;
    private String m_assessmentTestDateField;
    private String m_assessmentTestFormatField;
    private String m_assessmentTestPeriodField;
    private String m_assessmentTestYearField;
    private int m_countCreated;
    private int m_countSkipped;
    private DistrictSchoolYearContext m_ctx;
    private DateAsStringConverter m_dateConverter;
    private List<String> m_detailMessages = new LinkedList();
    private String m_fieldCrsTestCode;
    private X2Criteria m_sscCriteria;
    private Map<String, Collection<StudentSchedule>> m_sscMap;
    private String m_testFormat;
    private String m_testPeriod;
    private String m_testYear;

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

                // List of test codes added during this procedure
                Set<String> testCodes = new HashSet<String>();

                // List of codes from existing student assessments
                Set<String> asmCodes = getAsmCodes(stdOid);

                Collection<StudentSchedule> sscList = entry.getValue();
                if (!sscList.isEmpty()) {
                    for (StudentSchedule ssc : sscList) {
                        String testCode =
                                (String) ssc.getSection().getSchoolCourse().getCourse()
                                        .getFieldValueByBeanPath(m_fieldCrsTestCode);
                        if (testCodes.add(testCode)) {
                            if (asmCodes.contains(testCode)) {
                                ++m_countSkipped;
                                logDetail(
                                        "Student Assessment already exists: Student: " + ssc.getStudent().getNameView()
                                                + ", PARCC Test Code: " + testCode + ", Period: " + m_testPeriod);
                            } else {
                                saveNewAsm(ssc);
                            }
                        }
                    }
                }
            }

            if (m_countCreated > 0 || m_countSkipped > 0) {
                logMessage("Number of Student Assessments created is " + m_countCreated);
                logMessage("Number of Student Assessments already existing is  " + m_countSkipped);
                logMessage("");
            }

            // Ouptut detail messages
            for (String detail : m_detailMessages) {
                logMessage(detail);
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                getLocale(), true);

        String rcdTestPeriodOid = (String) getParameter(INPUT_PARAM_TEST_PERIOD);
        ReferenceCode rcdTestPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestPeriodOid);
        m_testPeriod = rcdTestPeriod != null ? rcdTestPeriod.getStateCode() : null;

        m_ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByOid(DistrictSchoolYearContext.class, (String) getParameter(INPUT_PARAM_CTX));

        m_testYear = m_ctx.getContextId();

        m_asmDate = (PlainDate) getParameter(INPUT_PARAM_ASM_DATE);
        if (m_asmDate.after(m_ctx.getEndDate())) {
            m_asmDate = m_ctx.getEndDate();
        } else if (m_asmDate.before(m_ctx.getStartDate())) {
            m_asmDate = m_ctx.getStartDate();
        }

        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldCrsTestCode = getFieldJavaNameByAliasForCode(dictionary, ALIAS_CRS_RICAS_TEST_CODE);

        if (!StringUtils.isEmpty(m_fieldCrsTestCode)) {
            if (initializeASDById()) {
                populateSSCMap();
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
     * Gets the asm codes.
     *
     * @param stdOid String
     * @return Sets the
     */
    private Set<String> getAsmCodes(String stdOid) {
        Set<String> codes = new HashSet();
        Collection<StudentAssessment> list = m_asmMap.get(stdOid);
        if (list != null) {
            for (StudentAssessment item : list) {
                String code = (String) item.getFieldValueByBeanPath(m_assessmentTestCodeField);
                if (!StringUtils.isEmpty(code)) {
                    codes.add(code);
                }
            }
        }
        return codes;
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
     * Gets the school oids.
     *
     * @return List
     */
    private List<String> getSchoolOids() {
        List<String> oids = null;
        Object objSchools = getParameter(INPUT_PARAM_SCHOOLS);
        String schoolOids = objSchools == null ? "" : (String) objSchools;

        if (!StringUtils.isEmpty(schoolOids)) {
            oids = Arrays.asList(schoolOids.split(","));
        }

        return oids;
    }

    /**
     * Gets the session name.
     *
     * @param ssc StudentSchedule
     * @return String
     */
    private String getSessionName(StudentSchedule ssc) {
        StringBuilder output = new StringBuilder();
        output.append(ssc.getSection().getSchoolCourse().getCourse().getNumber());
        output.append("-");
        output.append(ssc.getSection().getSectionNumber());
        output.append("-");
        SisStaff staff = ssc.getSection().getPrimaryStaff();
        if (staff != null) {
            output.append(staff.getPerson().getLastName());
        }
        return output.toString();
    }

    /**
     * Initialize ASD by id.
     *
     * @return true, if successful
     */
    private boolean initializeASDById() {
        boolean retValue = false;
        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, getParameter(INPUT_PARAM_ASM_DEF_ID));

        m_asd = (AssessmentDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));

        if (m_asd == null) {
            logMessage("Required assessment definition with ID = " + getParameter(INPUT_PARAM_ASM_DEF_ID)
                    + " could not be found");
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(m_asd, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                logMessage("Extended Dictinary for assessment could not be loaded");
            } else {
                m_assessmentSessionNameField = getAsmJavaName(ALIAS_RICAS_SESSION_NAME, dataDictionary, true);
                m_assessmentTestCodeField = getAsmJavaName(ALIAS_RICAS_TEST_CODE, dataDictionary, true);
                m_assessmentTestDateField = getAsmJavaName(ALIAS_RICAS_TEST_DATE, dataDictionary, false);
                m_assessmentTestFormatField = getAsmJavaName(ALIAS_RICAS_TEST_FORMAT, dataDictionary, true);
                m_assessmentTestPeriodField = getAsmJavaName(ALIAS_RICAS_TEST_PERIOD, dataDictionary, true);
                m_assessmentTestYearField = getAsmJavaName(ALIAS_RICAS_TEST_YEAR, dataDictionary, false);
                if (!StringUtils.isEmpty(m_assessmentSessionNameField) &&
                        !StringUtils.isEmpty(m_assessmentTestCodeField) &&
                        !StringUtils.isEmpty(m_assessmentTestDateField) &&
                        !StringUtils.isEmpty(m_assessmentTestFormatField) &&
                        !StringUtils.isEmpty(m_assessmentTestPeriodField) &&
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
     * Populate ASM map.
     */
    private void populateASMMap() {
        X2Criteria asmCriteria = new X2Criteria();

        asmCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_asd.getOid());
        asmCriteria.addEqualTo(m_assessmentTestPeriodField, m_testPeriod);
        asmCriteria.addEqualTo(m_assessmentTestYearField, m_testYear);
        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, m_sscCriteria));

        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);

        m_asmMap = getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID, 1024);
    }

    /**
     * Populate SSC map.
     */
    private void populateSSCMap() {
        m_sscMap = null;
        String[] mstOids = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SECTIONS))
                ? ((String) getParameter(INPUT_PARAM_SECTIONS)).split(",") : null;

        m_sscCriteria = new X2Criteria();

        m_sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, m_ctx.getOid());

        m_sscCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldCrsTestCode, getBroker().getPersistenceKey());

        if (mstOids != null) {
            m_sscCriteria.addIn(StudentSchedule.COL_SECTION_OID, Arrays.asList(mstOids));
        }

        m_sscCriteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_GRADE_LEVEL, inputGrades());

        m_sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        m_sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        List<String> schoolOids = getSchoolOids();
        if (schoolOids != null && !schoolOids.isEmpty()) {
            m_sscCriteria.addIn(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, schoolOids);
        }

        Collection<String> activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization());
        m_sscCriteria.addIn(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.COL_ENROLLMENT_STATUS, activeStudentCodes);
        if (getParameter(INPUT_PARAM_QUERY_BY_FIELD) != null) {
            applyInputCriteria(m_sscCriteria, StudentSchedule.REL_STUDENT);
        }
        QueryByCriteria sscQuery = new QueryByCriteria(StudentSchedule.class, m_sscCriteria);

        sscQuery.addOrderBy(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_START_DATE,
                true);

        m_sscMap = getBroker().getGroupedCollectionByQuery(sscQuery, StudentSchedule.COL_STUDENT_OID, 1024);

    }

    /**
     * Save new asm.
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

        newAsm.setFieldValueByBeanPath(m_assessmentSessionNameField, getSessionName(ssc));
        newAsm.setFieldValueByBeanPath(m_assessmentTestCodeField, testCode);
        newAsm.setFieldValueByBeanPath(m_assessmentTestDateField, m_dateConverter.getSystemString(m_asmDate));
        newAsm.setFieldValueByBeanPath(m_assessmentTestFormatField, m_testFormat);
        newAsm.setFieldValueByBeanPath(m_assessmentTestPeriodField, m_testPeriod);
        newAsm.setFieldValueByBeanPath(m_assessmentTestYearField, m_testYear);

        getBroker().saveBeanForced(newAsm);

        ++m_countCreated;
        logDetail("Student Assessment was created for: Student: " + ssc.getStudent().getNameView() + ", Section: "
                + ssc.getSection().getDescription());

    }

}
