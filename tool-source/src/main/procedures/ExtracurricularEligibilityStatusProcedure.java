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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ExcParticipationActivity;
import com.x2dev.sis.model.beans.ExtracurricularCriteria;
import com.x2dev.sis.model.beans.ExtracurricularProgram;
import com.x2dev.sis.model.beans.ExtracurricularSchoolProgram;
import com.x2dev.sis.model.beans.ExtracurricularStudent;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.sis.model.business.criteriaCategory.CriteriaCategory;
import com.x2dev.sis.model.business.extracurricular.ExtracurricularCodeHelper;
import com.x2dev.sis.model.business.extracurricular.ExtracurricularCodeHelper.ExtracurricularLocalCode;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.PersistenceBrokerSQLException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for changing students to eligibile for an extracurricular.
 */
public class ExtracurricularEligibilityStatusProcedure extends ProcedureJavaSource {

    // protected for testing
    protected static String PARAM_PROGRAM_TYPE = "programType";
    protected static String PARAM_PROGRAM_SUB_TYPE = "programSubType";
    protected static String PARAM_QUERY_BY = "queryBy";
    protected final List<ExtracurricularSchoolProgram> extracurrSchoolProgs =
            new ArrayList<ExtracurricularSchoolProgram>();
    protected Class dataClass = null;
    protected UserDataContainer m_userData;

    /**
     * Main execution for running program eligibility for students.
     *
     * @throws Exception exception
     */
    @Override
    protected void execute() throws Exception {

        String eligibleCode = ExtracurricularCodeHelper.getStudentEligibilityStatusCodeForLocalCode(getBroker(),
                ExtracurricularLocalCode.ELIGIBILITY_ELIGIBLE);
        String ineligibleCode = ExtracurricularCodeHelper.getStudentEligibilityStatusCodeForLocalCode(getBroker(),
                ExtracurricularLocalCode.ELIGIBILITY_INELIGIBLE);


        populateSchoolProgramList();

        for (ExtracurricularSchoolProgram schoolProgram : extracurrSchoolProgs) {
            runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, schoolProgram);
        }

    }

    /**
     * Test method to get messages. The method is protected and not accessible from the tests
     *
     * @return
     */
    protected List<String> getProcedureMessages() {
        return this.getMessages();
    }

    /**
     * Test method to reset messages. The method is protected and not accessible from the tests
     */
    protected void clearProcedureMessages() {
        this.getMessages().clear();
    }

    /**
     * Test method to reset parameters. The method is protected and not accessible from the tests
     */
    protected void clearProcedureParameters() {
        this.getParameters().clear();
    }

    /**
     * Gets the student criteria either from the current node if its the right object, or creates
     * one with a path to the extracurricular student.
     *
     * @param eligibleCode String
     * @param schoolProgram ExtracurricularSchoolProgram
     * @return X2Criteria
     *
     *         visibility for testing
     */
    protected X2Criteria getStudentsCriteria(String eligibleCode, ExtracurricularSchoolProgram schoolProgram) {
        X2Criteria studentsCriteria = new X2Criteria();

        if (schoolProgram == null) {
            logMessage("Invalid location to run procedure.");
            return null;
        }

        if (dataClass == ExtracurricularStudent.class) {
            studentsCriteria = getCurrentCriteria();
        }

        if (studentsCriteria == null) {
            logMessage("Invalid location to run procedure.");
            return null;
        }

        studentsCriteria.addEqualTo(ExtracurricularStudent.COL_EXTRACURRICULAR_SCHOOL_PROG_OID,
                schoolProgram.getOid());
        studentsCriteria.addNotEqualTo(ExtracurricularStudent.COL_ELIGIBILITY_STATUS,
                eligibleCode);

        return studentsCriteria;
    }

    /**
     * Mark eligibility. This will mark the students either eligible or ineligible based on the
     * value that's passed in
     *
     * @param studentsCriteria X2Criteria
     * @param eligibilityStatus String
     * @param eligibilityMessage String
     * @param ineligibleOids Collection<String>
     * @param eligibilityStatusMessages Map<String, List<String>>
     * @param ineligibleCode String
     *
     *        visibility for testing
     */
    protected void markEligibility(X2Criteria studentsCriteria,
                                   String eligibilityStatus,
                                   String eligibilityMessage,
                                   Collection<String> ineligibleOids,
                                   Map<String, List<String>> eligibilityStatusMessages,
                                   String ineligibleCode) {
        QueryByCriteria query = new QueryByCriteria(ExtracurricularStudent.class, studentsCriteria);

        List<ValidationError> errors = new ArrayList<ValidationError>();

        try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                ExtracurricularStudent extracurricularStudent = (ExtracurricularStudent) iterator.next();
                extracurricularStudent.setEligibilityStatus(eligibilityStatus);
                List<ValidationError> beanErrors = (getBroker().saveBean(extracurricularStudent));
                if (beanErrors.isEmpty()) {
                    String studentNameView = extracurricularStudent.getStudent().getNameView();
                    String message = "Student: " + studentNameView + " became "
                            + eligibilityStatus
                            + (ineligibleCode.equals(eligibilityStatus) ? ". " + eligibilityMessage : ".");
                    if (!eligibilityStatusMessages.containsKey(studentNameView)) {
                        eligibilityStatusMessages.put(studentNameView, new ArrayList<String>(Arrays.asList(message)));
                    } else {
                        eligibilityStatusMessages.get(studentNameView).add(message);
                    }
                    addParticipationActivity(errors, extracurricularStudent, eligibilityStatus, eligibilityMessage);
                    if (ineligibleOids != null) {
                        ineligibleOids.add(extracurricularStudent.getStudentOid());
                    }
                }
                errors.addAll(beanErrors);
            }
        } catch (PersistenceBrokerSQLException ex) {
            addCustomErrorMessage(
                    "Invalid criteria. Check that the criteria on the district program was created correctly.");
            throw ex;
        }
    }

    /**
     * Populate the school programs we want to run against. Its either a single program, and we are
     * in the detail context already. Or we will query for the school programs based on the inputs.
     */
    protected void populateSchoolProgramList() {
        if (!extracurrSchoolProgs.isEmpty()) {
            // We are in the context of a single program, no need to query on programs
            return;
        }

        int queryBy = 0;
        if (getParameter(PARAM_QUERY_BY) != null) {
            queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        }
        String programType = (String) getParameter(PARAM_PROGRAM_TYPE);
        String programSubType = (String) getParameter(PARAM_PROGRAM_SUB_TYPE);

        X2Criteria schoolProgramCriteria = new X2Criteria();

        switch (queryBy) {
            case 1: // Current selection
                schoolProgramCriteria = getCurrentCriteria();
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        if (schoolProgramCriteria == null) {
            return;
        }

        if (getSchool() != null) {
            schoolProgramCriteria.addEqualTo(ExtracurricularSchoolProgram.COL_SCHOOL_OID, getSchool().getOid());
        }

        setCodeForProgram(programType, ExtracurricularProgram.COL_TYPE, schoolProgramCriteria);
        setCodeForProgram(programSubType, ExtracurricularProgram.COL_SUB_TYPE, schoolProgramCriteria);

        // Now, get the list of schoolPrograms and add them to the program list
        QueryByCriteria query = new QueryByCriteria(ExtracurricularSchoolProgram.class, schoolProgramCriteria);
        if (getBroker().getCount(query) < 1) {
            return;
        }
        extracurrSchoolProgs.addAll(getBroker().getCollectionByQuery(query));

    }

    /**
     *
     * @param myType
     * @param column
     * @param schoolProgramCriteria
     */
    private void setCodeForProgram(String myType, String column, X2Criteria schoolProgramCriteria) {

        if (StringUtils.isBlank(myType)) {
            return;
        }

        List<String> codeOidsList = StringUtils.convertDelimitedStringToList(myType, ",");
        List<String> codeValue =
                ReferenceManager.getCodesByOids(getBroker(), codeOidsList, ExtracurricularProgram.class,
                        column, ReferenceCodeRetriever.EXCLUDE_DISABLED_CODES);

        if (codeOidsList.size() != codeValue.size()) {
            schoolProgramCriteria.addEqualTo("0", "1");
            logMessage("Invalid reference codes chosen.");
            return;
        }
        schoolProgramCriteria.addIn(ExtracurricularSchoolProgram.REL_EXTRACURRICULAR_PROGRAM + "."
                + column, codeValue);
    }

    /**
     * Runs eligibility for a single school program
     *
     * @param eligibleCode
     * @param ineligibleCode
     * @param schoolProgram
     * @throws Exception
     */
    protected void runEligibilityForSchoolProgram(String eligibleCode,
                                                  String ineligibleCode,
                                                  ExtracurricularSchoolProgram schoolProgram)
            throws Exception {

        if (schoolProgram == null) {
            return;
        }

        X2Criteria studentBaseCriteria = getStudentsCriteria(eligibleCode, schoolProgram);
        if (studentBaseCriteria == null) {
            return;
        }

        Map<String, List<String>> eligibilityStatusMessages = new TreeMap<>();
        List<String> ineligibleOids = new ArrayList();

        ExtracurricularProgram districtProgram = schoolProgram.getExtracurricularProgram();

        logMessage("-- START -- Evaluating eligibility for program: " + districtProgram.getName() + " at school "
                + schoolProgram.getSchool().getName());
        for (ExtracurricularCriteria extracurricularCriteria : districtProgram.getExtracurricularCriteria()) {

            CriteriaCategory criteriaCategory = CriteriaCategory.create(extracurricularCriteria, m_userData);

            X2Criteria ineligibleCriteria = studentBaseCriteria.copy();
            ineligibleCriteria.addNotIn(SisBeanPaths.EXTRACURRICULAR_STUDENT.studentOid().getPath(),
                    criteriaCategory.buildQuery(getBroker()));

            markEligibility(ineligibleCriteria, ineligibleCode, extracurricularCriteria.getMessage(),
                    ineligibleOids, eligibilityStatusMessages, ineligibleCode);
        }

        X2Criteria eligibleCriteria = studentBaseCriteria.copy();
        eligibleCriteria.addNotIn(SisBeanPaths.EXTRACURRICULAR_STUDENT.studentOid().getPath(), ineligibleOids);
        markEligibility(eligibleCriteria, eligibleCode, eligibleCode, null, eligibilityStatusMessages, ineligibleCode);

        for (Map.Entry<String, List<String>> statusMessages : eligibilityStatusMessages.entrySet()) {
            for (String statusMessage : statusMessages.getValue()) {
                logMessage(statusMessage);
            }
        }

        logMessage("-- END -- Evaluating eligibility for program: " + districtProgram.getName() + " at school "
                + schoolProgram.getSchool().getName());
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        extracurrSchoolProgs.clear();

        ExtracurricularSchoolProgram currentExtracurrSchoolProg =
                userData.getCurrentRecord(ExtracurricularSchoolProgram.class);
        if (currentExtracurrSchoolProg != null) {
            extracurrSchoolProgs.add(currentExtracurrSchoolProg);
        }
        dataClass = userData.getCurrentNode().getDataClass();
        m_userData = userData;
    }

    /**
     * Adds the participation activity.
     *
     * @param errors List<ValidationError>
     * @param exStudent ExtracurricularStudent
     * @param eligibilityStatus String
     * @param participationComment String
     */
    private void addParticipationActivity(List<ValidationError> errors,
                                          ExtracurricularStudent exStudent,
                                          String eligibilityStatus,
                                          String participationComment) {
        ExcParticipationActivity activity = new ExcParticipationActivity(getBroker().getPersistenceKey());
        activity.setAction(eligibilityStatus);
        activity.setComment(participationComment);
        activity.setCreatedByUserOid(null);
        activity.setActionDate(new PlainDate(System.currentTimeMillis()));
        activity.setExcStudentMembershipOid(exStudent.getOid());
        errors.addAll(getBroker().saveBean(activity));
    }
}
