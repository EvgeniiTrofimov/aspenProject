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
package com.x2dev.procedures.statereporting;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure that looks at the student schedule and auto creates a NJSLA Assessment
 * for those courses that are selected in the procedure input,
 * for the cycle indicated and align to the test code selected in the procedure input.
 *
 * @author Follett Software Company
 *
 */
public class NJSLASProcedure extends ProcedureJavaSource {
    private static final String ALIAS_NJSLA_TESTCODE = "NJSLA-STSTCODE";
    private static final String ALIAS_NJSLA_TESTDATE = "NJSLA-STSTDATE";
    private static final String ALIAS_NJSLA_TESTFORMAT = "NJSLA-STSTFORMAT";
    private static final String ALIAS_NJSLA_TESTYEAR = "NJSLA-STSTYEAR";
    private static final String ALIAS_NJSLA_TESTPERIOD = "NJSLA-STSTCYCLE";

    private static final String INPUT_PARAM_ASM_DATE = "asmDate";
    private static final String INPUT_PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String INPUT_PARAM_CTX = "schoolYearContext";
    private static final String INPUT_PARAM_CYCLE = "cycle";
    private static final String INPUT_PARAM_GRADES = "gradeList";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_FORMAT = "testFormat";

    private static final String[] VALID_GRADE_LIST = {"04", "05", "06", "07", "08", "09", "10", "11", "12"};

    private static final HashMap<String, String> grade = new HashMap();

    static {
        grade.put("04", "SC04");
        grade.put("05", "SC05");
        grade.put("06", "SC06");
        grade.put("07", "SC07");
        grade.put("08", "SC08");
        grade.put("09", "SC09");
        grade.put("10", "SC10");
        grade.put("11", "SC11");
        grade.put("12", "SC12");
    }

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
    private String m_ctxForNJSLAValue;
    private DateAsStringConverter m_dateConverter;
    private List<String> m_detailMessages = new LinkedList();
    private X2Criteria m_stdCriteria;
    private String m_testFormat;

    private Collection<SisStudent> m_activeStudents;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        if (m_activeStudents != null) {
            for (SisStudent entry : m_activeStudents) {
                Collection<StudentAssessment> asmList = m_asmMap.get(entry.getOid());
                createNewAsm(entry, asmList);
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

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                getLocale(), true);

        m_cycle = (String) getParameter(INPUT_PARAM_CYCLE);
        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByOid(DistrictSchoolYearContext.class, (String) getParameter(INPUT_PARAM_CTX));
        m_ctxForNJSLAValue = (String) ctx.getFieldValueByBeanPath(DistrictSchoolYearContext.COL_CONTEXT_ID);
        m_asmDate = (PlainDate) getParameter(INPUT_PARAM_ASM_DATE);

        if (m_asmDate.after(ctx.getEndDate())) {
            m_asmDate = ctx.getEndDate();
        } else if (m_asmDate.before(ctx.getStartDate())) {
            m_asmDate = ctx.getStartDate();
        }

        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;

        if (initializeASDById()) {
            populateSTD();
            populateAssMap();
        }
    }


    /**
     * Check if it is needed to save candidate ASM to the DB.
     *
     * @param std SisStudent
     * @param asmList Collection<StudentAssessment>
     */
    private void createNewAsm(SisStudent std, Collection<StudentAssessment> asmList) {
        boolean isAsmAlreadyCreated = false;

        if (asmList != null) {
            for (StudentAssessment asm : asmList) {
                String testedTestPeriod = (String) asm.getFieldValueByBeanPath(m_assessmentTestPeriodField);
                String testedTestContextValue = (String) asm.getFieldValueByBeanPath(m_assessmentTestYearField);
                if (m_cycle != null && m_cycle.equals(testedTestPeriod) &&
                        m_ctxForNJSLAValue != null && m_ctxForNJSLAValue.equals(testedTestContextValue)) {
                    isAsmAlreadyCreated = true;
                    ++m_countSkipped;
                    logDetail("Student Assessment already exists: Student: " + std.getNameView()
                            + ", Period: " + m_cycle);
                    break;
                }
            }
        }

        if (!isAsmAlreadyCreated) {
            saveNewAsm(std);
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
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private String getSchools() {
        Object objSchools = getParameter(INPUT_PARAM_SCHOOLS);
        String schoolOids = objSchools == null ? "" : (String) objSchools;
        return schoolOids;
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
                logMessage("Extended Dictinary for NJSLA could not be loaded");
            } else {
                m_assessmentTestCodeField = getAsmJavaName(ALIAS_NJSLA_TESTCODE, dataDictionary, true);
                m_assessmentTestDateField = getAsmJavaName(ALIAS_NJSLA_TESTDATE, dataDictionary, true);// ----------------false
                m_assessmentTestFormatField = getAsmJavaName(ALIAS_NJSLA_TESTFORMAT, dataDictionary, true);
                m_assessmentTestPeriodField = getAsmJavaName(ALIAS_NJSLA_TESTPERIOD, dataDictionary, true);
                m_assessmentTestYearField = getAsmJavaName(ALIAS_NJSLA_TESTYEAR, dataDictionary, false);
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
     * Method to populate active Student.
     */
    private void populateSTD() {
        String schoolOids = getSchools();
        m_stdCriteria = new X2Criteria();

        if (!StringUtils.isEmpty(schoolOids)) {
            List<String> oids = Arrays.asList(schoolOids.split(","));
            m_stdCriteria.addIn(SisStudent.COL_SCHOOL_OID, oids);
        }
        m_stdCriteria.addIn(SisStudent.COL_GRADE_LEVEL, inputGrades());
        m_stdCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_stdCriteria);
        query.addOrderBy(SisStudent.COL_NAME_VIEW, true);
        m_activeStudents = getBroker().getCollectionByQuery(query);

    }

    /**
     * Method to populate Ass map keyed on stdAssessmentOid.
     */
    private void populateAssMap() {

        X2Criteria asmCriteria = new X2Criteria();
        asmCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_asd.getOid());
        asmCriteria.addEqualTo(m_assessmentTestPeriodField, m_cycle);
        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_stdCriteria));
        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);
        m_asmMap =
                getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID,
                        m_activeStudents.size());
    }

    /**
     * Save ASM to DB.
     *
     * @param std SisStudent
     */
    private void saveNewAsm(SisStudent std) {

        StudentAssessment newAsm = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        newAsm.setAssessmentDefinitionOid(m_asd.getOid());
        newAsm.setDate(m_asmDate);

        newAsm.setGradeLevelCode(std.getGradeLevel());
        newAsm.setStudentOid(std.getOid());
        newAsm.setSchoolOid(std.getSchoolOid());
        if (!StringUtils.isEmpty(m_assessmentTestDateField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestDateField, m_dateConverter.getSystemString(m_asmDate));
        }
        newAsm.setFieldValueByBeanPath(m_assessmentTestFormatField, m_testFormat);
        if (!StringUtils.isEmpty(m_assessmentTestPeriodField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestPeriodField, m_cycle);
        }
        if (!StringUtils.isEmpty(m_assessmentTestYearField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestYearField, m_ctxForNJSLAValue);
        }
        if (!StringUtils.isEmpty(m_assessmentTestCodeField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestCodeField, grade.get(newAsm.getGradeLevelCode()));
        }

        getBroker().saveBeanForced(newAsm);

        ++m_countCreated;
        logDetail("Student Assessment was created for: Student: " + std.getNameView() + ", Grade: "
                + std.getGradeLevel());

    }
}
