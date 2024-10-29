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

package com.x2dev.procedures.statereporting.nj;

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
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure that looks at the student schedule and auto creates a PARCC Assessment
 * for those courses that are selected in the procedure input,
 * for the cycle indicated and align to the test code selected in the procedure input.
 *
 * @author Follett Software Company
 *
 */
public class NJAccessCreationProcedure extends ProcedureJavaSource {

    /**
     * Aliases
     */
    // private static final String ALIAS_CRS_PARCC_TEST_CODE = "all-crs-PARCCTestCode";
    private static final String ALIAS_PARCC_TESTCODE = "PARCCTSTCODE";
    private static final String ALIAS_PARCC_TESTDATE = "PARCCTSTDATE";
    private static final String ALIAS_PARCC_TESTFORMAT = "PARCCTSTFORMAT";
    private static final String ALIAS_PARCC_TESTYEAR = "PARCCTSTYEAR";
    private static final String ALIAS_PARCC_TESTPERIOD = "PARCCTSTPERIOD";

    /**
     * Input parameters.
     */
    private static final String INPUT_PARAM_ASD_OID = "asdOid";
    private static final String INPUT_PARAM_ASM_DATE = "asmDate";
    private static final String INPUT_PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String INPUT_PARAM_CTX = "schoolYearContext";
    private static final String INPUT_PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    private static final String INPUT_PARAM_CYCLE = "cycle";
    private static final String INPUT_PARAM_GRADES = "gradeList";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_FORMAT = "testFormat";

    /**
     * Other constants.
     */
    private static final Collection VALID_STATE_GRADE_LIST =
            Arrays.asList("DH", "DF", "KH", "KF", "PF", "01", "02", "12");

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
    private DistrictSchoolYearContext m_ctx;
    private String m_ctxForParccValue;
    private DateAsStringConverter m_dateConverter;
    private List<String> m_detailMessages = new LinkedList();
    private DataDictionary m_dictionary;
    private Collection m_gradesByState12 = new ArrayList<>();
    private X2Criteria m_pgmCriteria;
    private Collection<StudentProgramParticipation> m_pgms = new ArrayList<StudentProgramParticipation>();
    private String m_testFormat;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (!m_pgms.isEmpty()) {
            for (StudentProgramParticipation pgm : m_pgms) {
                String stdOid = pgm.getStudentOid();
                Collection<StudentAssessment> asmList = m_asmMap.get(stdOid);
                if (m_gradesByState12.contains(pgm.getStudent().getGradeLevel())) {
                    createNewAsm(pgm, asmList, "EXELA", true);
                } else {
                    createNewAsm(pgm, asmList, "EXEK2", false);
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
        m_ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByOid(DistrictSchoolYearContext.class, (String) getParameter(INPUT_PARAM_CTX));
        m_ctxForParccValue = (String) m_ctx.getFieldValueByBeanPath((String) getParameter(INPUT_PARAM_CTX_BEAN_PATH));
        m_asmDate = (PlainDate) getParameter(INPUT_PARAM_ASM_DATE);
        if (m_asmDate.after(m_ctx.getEndDate())) {
            m_asmDate = m_ctx.getEndDate();
        } else if (m_asmDate.before(m_ctx.getStartDate())) {
            m_asmDate = m_ctx.getStartDate();
        }
        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;
        if (initializeASDById()) {
            populateGradesByState12();
            populatePgmsMap();
            populateASMMap();
        }
    }

    /**
     * Check if it is needed to save candidate ASM to the DB.
     *
     * @param ssc StudentSchedule
     * @param asmList Collection<StudentAssessment>
     * @param testCode String
     */
    private void createNewAsm(StudentProgramParticipation pgm,
                              Collection<StudentAssessment> asmList,
                              String testCode,
                              boolean is12Grade) {
        boolean isAsmAlreadyCreated = false;
        if (asmList != null) {
            for (StudentAssessment asm : asmList) {
                String testedTestCode = (String) asm.getFieldValueByBeanPath(m_assessmentTestCodeField);
                String testedTestPeriod = (String) asm.getFieldValueByBeanPath(m_assessmentTestPeriodField);
                String testedTestContextValue = (String) asm.getFieldValueByBeanPath(m_assessmentTestYearField);
                if (m_cycle != null && m_cycle.equals(testedTestPeriod) &&
                        m_ctxForParccValue != null && m_ctxForParccValue.equals(testedTestContextValue)) {
                    if (is12Grade || testCode.equals(testedTestCode)) {
                        isAsmAlreadyCreated = true;
                        ++m_countSkipped;
                        logDetail("Student Assessment already exists: Student: " + pgm.getStudent().getNameView()
                                + ", PARCC Test Code: " + testCode + ", Period: " + m_cycle);
                        break;
                    }
                }
            }
        }
        if (!isAsmAlreadyCreated) {
            saveNewAsm(pgm, testCode);
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
     * Get collection of program codes corresponding to given state reference code.
     *
     * @param beanClass Class
     * @param column String
     * @param stateCodes Collection<String>
     * @return Collection
     */
    private Collection<String> getCodesByState(Class beanClass, String column, Collection<String> stateCodes) {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField field = getDataDictionaryField(beanClass, column);
        if (field != null && field.hasReferenceTable()) {
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        } else {
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "___dummy__");
        }
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
        criteria.addIn(ReferenceCode.COL_STATE_CODE, stateCodes);
        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);
        List<String> result = new ArrayList<String>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                result.add(code);
            }
        } finally {
            iterator.close();
        }
        return result;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     * @param beanClass Class
     * @param path String
     * @return Data dictionary field
     */
    private DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
        ModelProperty prop = new ModelProperty(beanClass, path, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = m_dictionary.findDataDictionaryField(prop.getFieldId());
        return dictionaryField;
    }

    /**
     * Get a list of the grade codes selected.
     *
     * @return List
     */
    private List<String> getInputGrades() {
        List<String> grades = new LinkedList();
        if (StringUtils.isEmpty((String) getParameter(INPUT_PARAM_GRADES))) {
            grades.addAll(getCodesByState(SisStudent.class, SisStudent.COL_GRADE_LEVEL, VALID_STATE_GRADE_LIST));
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
                m_assessmentTestYearField = getAsmJavaName(ALIAS_PARCC_TESTYEAR, dataDictionary, false);
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
                new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID,
                        m_pgmCriteria));
        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);
        m_asmMap = getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID, 1024);
    }

    /**
     * Method to populate SSC map keyed on sscStdOid.
     */
    private void populateGradesByState12() {
        m_gradesByState12.addAll(getCodesByState(SisStudent.class, SisStudent.COL_GRADE_LEVEL, Arrays.asList("12")));
    }

    /**
     * Method to populate students' programs.
     */
    private void populatePgmsMap() {
        m_pgmCriteria = new X2Criteria();
        m_pgmCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
        m_pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getCodesByState(
                StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE, Arrays.asList("LEP")));
        m_pgmCriteria.addIn(
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_GRADE_LEVEL,
                getInputGrades());
        m_pgmCriteria.addIn(
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchools().keySet());
        Collection<String> activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization());
        m_pgmCriteria.addIn(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.COL_ENROLLMENT_STATUS, activeStudentCodes);
        m_pgms.addAll(getBroker()
                .getCollectionByQuery(new QueryByCriteria(StudentProgramParticipation.class, m_pgmCriteria)));
    }

    /**
     * Save ASM to DB.
     *
     * @param pgm StudentProgramParticipation
     * @param testCode String
     */
    private void saveNewAsm(StudentProgramParticipation pgm, String testCode) {
        StudentAssessment newAsm = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        newAsm.setAssessmentDefinitionOid(m_asd.getOid());
        newAsm.setDate(m_asmDate);
        newAsm.setGradeLevelCode(pgm.getStudent().getGradeLevel());
        newAsm.setStudentOid(pgm.getStudentOid());
        newAsm.setSchoolOid(pgm.getStudent().getSchoolOid());
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
        logDetail("Student Assessment was created for: Student: " + pgm.getStudent().getNameView());
    }
}
