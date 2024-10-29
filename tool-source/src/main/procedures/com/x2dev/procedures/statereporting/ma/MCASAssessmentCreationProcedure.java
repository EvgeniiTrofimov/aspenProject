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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
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
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure that looks at the student schedule and auto creates a MCAS Assessment
 * for those courses that are selected in the procedure input,
 * for the cycle indicated and align to the test code selected in the procedure input.
 *
 * @author Follett Software Company
 *
 */
public class MCASAssessmentCreationProcedure extends ProcedureJavaSource {
    private static final String ALIAS_CRS_MCAS_TEST_CODE = "ma-crs-MCASTestCode";
    private static final String ALIAS_MCAS_SESSION_NAME = "MCASSessionName";
    private static final String ALIAS_MCAS_TESTCODE = "MCASTestCode";
    private static final String ALIAS_MCAS_TESTFORMAT = "MCASTestFormat";
    private static final String ALIAS_MCAS_TESTYEAR = "MCASTestYear";
    private static final String ALIAS_MCAS_TESTCYCLE = "MCASTestCycle";
    private static final String INPUT_PARAM_ASM_DATE = "asmDate";
    private static final String INPUT_PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String INPUT_PARAM_CHAR_DEL = "charDelimiter";
    private static final String INPUT_PARAM_CODES = "Codes";
    private static final String INPUT_PARAM_CTX = "schoolYearContext";
    private static final String INPUT_PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    private static final String INPUT_PARAM_CYCLE = "cycle";
    private static final String INPUT_PARAM_DONT_CREATE_SESSION_NAME = "notCreateSessionName";
    private static final String INPUT_PARAM_GRADES = "gradeList";
    private static final String INPUT_PARAM_SECTIONS = "courseSections";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_FORMAT = "testFormat";
    private static final String[] VALID_GRADE_LIST = {"09", "10", "11", "12"};

    /**
     * Class members
     */
    private AssessmentDefinition m_asd;
    private PlainDate m_asmDate;
    private Map<String, Collection<StudentAssessment>> m_asmMap;
    private String m_assessmentSessionNameField;
    private String m_assessmentTestCodeField;
    private String m_assessmentTestFormatField;
    private String m_assessmentTestPeriodField;
    private String m_assessmentTestYearField;
    private int m_countCreated;
    private int m_countSkipped;
    private String m_cycle;
    private String m_ctxForMcasValue;
    private List<String> m_detailMessages = new LinkedList();
    private String m_fieldCrsTestCode;
    private Boolean m_inputDontCreateSessionName;
    private X2Criteria m_sscCriteria;
    private Map<String, Collection<StudentSchedule>> m_sscMap;
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
                if (!sscList.isEmpty()) {
                    for (StudentSchedule ssc : sscList) {
                        createNewAsm(ssc, asmList);
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

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_cycle = (String) getParameter(INPUT_PARAM_CYCLE);
        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByOid(DistrictSchoolYearContext.class, (String) getParameter(INPUT_PARAM_CTX));
        m_ctxForMcasValue = (String) ctx.getFieldValueByBeanPath((String) getParameter(INPUT_PARAM_CTX_BEAN_PATH));
        m_asmDate = (PlainDate) getParameter(INPUT_PARAM_ASM_DATE);

        if (m_asmDate.after(ctx.getEndDate())) {
            m_asmDate = ctx.getEndDate();
        } else if (m_asmDate.before(ctx.getStartDate())) {
            m_asmDate = ctx.getStartDate();
        }

        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;
        m_fieldCrsTestCode = getFieldJavaNameByAliasForCode(dictionary, ALIAS_CRS_MCAS_TEST_CODE);

        if (!StringUtils.isEmpty(m_fieldCrsTestCode)) {
            if (initializeASDById()) {
                populateSSCMap();
                populateASMMap();
            }
        }
        m_inputDontCreateSessionName = (Boolean) getParameter(INPUT_PARAM_DONT_CREATE_SESSION_NAME);
    }

    /**
     * Check if it is needed to save candidate ASM to the DB.
     *
     * @param ssc StudentSchedule
     * @param asmList Collection<StudentAssessment>
     */
    private void createNewAsm(StudentSchedule ssc, Collection<StudentAssessment> asmList) {
        boolean isAsmAlreadyCreated = false;

        String testCode =
                (String) ssc.getSection().getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldCrsTestCode);

        if (asmList != null) {
            for (StudentAssessment asm : asmList) {
                String testedTestCode = (String) asm.getFieldValueByBeanPath(m_assessmentTestCodeField);
                String testedTestPeriod = (String) asm.getFieldValueByBeanPath(m_assessmentTestPeriodField);
                String testedTestContextValue = (String) asm.getFieldValueByBeanPath(m_assessmentTestYearField);

                if (testCode.equals(testedTestCode) && m_cycle != null && m_cycle.equals(testedTestPeriod) &&
                        m_ctxForMcasValue != null && m_ctxForMcasValue.equals(testedTestContextValue)) {
                    isAsmAlreadyCreated = true;
                    ++m_countSkipped;
                    logDetail("Student Assessment already exists: Student: " + ssc.getStudent().getNameView()
                            + ", MCAS Test Code: " + testCode + ", Period: " + m_cycle);
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
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary) {
        String javaName = null;

        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        } else {
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
                logMessage("Extended Dictinary for MCAS could not be loaded");
            } else {
                m_assessmentTestCodeField = getAsmJavaName(ALIAS_MCAS_TESTCODE, dataDictionary);
                m_assessmentTestFormatField = getAsmJavaName(ALIAS_MCAS_TESTFORMAT, dataDictionary);
                m_assessmentTestPeriodField = getAsmJavaName(ALIAS_MCAS_TESTCYCLE, dataDictionary);
                m_assessmentTestYearField = getAsmJavaName(ALIAS_MCAS_TESTYEAR, dataDictionary);
                m_assessmentSessionNameField = getAsmJavaName(ALIAS_MCAS_SESSION_NAME, dataDictionary);
                if (!StringUtils.isEmpty(m_assessmentTestCodeField) &&
                        !StringUtils.isEmpty(m_assessmentTestPeriodField) &&
                        !StringUtils.isEmpty(m_assessmentTestFormatField)) {
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
        String[] rcdOids = !(StringUtils.isEmpty((String) Arrays.asList(getParameter(INPUT_PARAM_GRADES)).get(0)))
                ? ((String) getParameter(INPUT_PARAM_GRADES)).split(",")
                : VALID_GRADE_LIST;
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
     * Method to populate ASM map keyed on asmStdOid.
     */
    private void populateASMMap() {
        X2Criteria asmCriteria = new X2Criteria();

        asmCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_asd.getOid());
        asmCriteria.addEqualTo(m_assessmentTestPeriodField, m_cycle);
        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, m_sscCriteria));

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

        String inputParamCodes = getParameter(INPUT_PARAM_CYCLE) + INPUT_PARAM_CODES;
        String[] codes = ((String) getParameter(inputParamCodes)).split((String) getParameter(INPUT_PARAM_CHAR_DEL));

        m_sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, ctxOid);
        m_sscCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldCrsTestCode, Arrays.asList(codes));

        if (mstOids != null) {
            m_sscCriteria.addIn(StudentSchedule.COL_SECTION_OID, Arrays.asList(mstOids));
        }

        m_sscCriteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_GRADE_LEVEL, inputGrades());

        m_sscCriteria.addIn(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                getSchools().keySet());

        QueryByCriteria sscQuery = new QueryByCriteria(StudentSchedule.class, m_sscCriteria);

        m_sscMap = getBroker().getGroupedCollectionByQuery(sscQuery, StudentSchedule.COL_STUDENT_OID, 1024);

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
        if (!m_inputDontCreateSessionName.booleanValue()) {
            newAsm.setFieldValueByBeanPath(m_assessmentSessionNameField, getSessionName(ssc));
        }
        newAsm.setFieldValueByBeanPath(m_assessmentTestCodeField, testCode);
        newAsm.setFieldValueByBeanPath(m_assessmentTestFormatField, m_testFormat);
        if (!StringUtils.isEmpty(m_assessmentTestPeriodField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestPeriodField, m_cycle);
        }
        if (!StringUtils.isEmpty(m_assessmentTestYearField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestYearField, m_ctxForMcasValue);
        }

        getBroker().saveBeanForced(newAsm);

        ++m_countCreated;
        logDetail("Student Assessment was created for: Student: " + ssc.getStudent().getNameView() + ", Section: "
                + ssc.getSection().getDescription());

    }

}
