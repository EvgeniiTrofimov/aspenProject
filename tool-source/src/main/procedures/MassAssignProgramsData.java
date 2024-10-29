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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;

/**
 * Procedure to assign multiple students to a program.
 *
 * @author X2 Development Corporation
 */
public class MassAssignProgramsData extends ProcedureJavaSource {
    // Input parameters
    private static final String ACTIVE_STUDENT_PARAM = "activeOnly";
    private static final String END_DATE_PARAM = "endDate";
    private static final String PROGRAM_CODE_PARAM = "programCode";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String SCHOOL_PARAM = "school";
    private static final String START_DATE_PARAM = "startDate";
    private static final String PROGRAM_DEFINTION_PARAM = "extendedDictionaryOid";

    private ModelBroker m_modelBroker; // Broker that used to save StudentProgramParticipation
                                       // records

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int count = 0;
        int skipped = 0;

        ReferenceCode programCode = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class,
                (String) getParameter(PROGRAM_CODE_PARAM));
        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);
        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        SisSchool school = (SisSchool) getParameter(SCHOOL_PARAM);
        boolean activeStudentsOnly = ((Boolean) getParameter(ACTIVE_STUDENT_PARAM)).booleanValue();

        if (!queryBy.startsWith(SELECTION_SPECIAL_CASE_PREFIX)) {
            queryBy = StudentProgramParticipation.REL_STUDENT + "." + queryBy;
        }

        String queryString =
                (String) getParameter(QUERY_STRING_PARAM) == null ? "" : (String) getParameter(QUERY_STRING_PARAM);

        Criteria programCriteria = new Criteria();
        Criteria studentCriteria = new Criteria();

        if (activeStudentsOnly) {
            programCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    StudentProgramParticipation.REL_STUDENT + "." + SisStudent.COL_ENROLLMENT_STATUS));
            studentCriteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        }

        addUserCriteria(programCriteria, queryBy, queryString, StudentProgramParticipation.class, SisStudent.class,
                StudentProgramParticipation.COL_STUDENT_OID);
        programCriteria.addAndCriteria(getSchoolCriteria(StudentProgramParticipation.REL_STUDENT + ".", school));

        BeanQuery programQuery = new BeanQuery(StudentProgramParticipation.class, programCriteria, true, false);
        programQuery.addOrderByAscending(StudentProgramParticipation.COL_START_DATE);
        Map<String, Map<String, StudentProgramParticipation>> studentProgramsMultiMap =
                getBroker().getNestedMapByQuery(programQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, StudentProgramParticipation.COL_PROGRAM_CODE, 100,
                        10);

        addUserCriteria(studentCriteria, (String) getParameter(QUERY_BY_PARAM), queryString, SisStudent.class,
                SisStudent.class, X2BaseBean.COL_OID);
        studentCriteria.addAndCriteria(getSchoolCriteria("", school));

        BeanQuery studentQuery = new BeanQuery(SisStudent.class, studentCriteria, true, false);
        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);

        logMessage("Students skipped due to existing program within date range:");
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                if (studentProgramsMultiMap.containsKey(student.getOid())) {
                    StudentProgramParticipation program =
                            studentProgramsMultiMap.get(student.getOid()).get(programCode.getCode());

                    if (program != null && (program.getEndDate() == null || !startDate.after(program.getEndDate()))) {
                        skipped++;
                        logMessage(student.getNameView() + " - existing program.");
                        continue;
                    }
                }

                StudentProgramParticipation spp = new StudentProgramParticipation(m_modelBroker.getPersistenceKey());
                spp.setEndDate(endDate);
                spp.setProgramCode(programCode.getCode());
                spp.setStartDate(startDate);
                spp.setStudentOid(student.getOid());
                spp.setExtendedDataDictionaryOid((String) getParameter(PROGRAM_DEFINTION_PARAM));

                DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                DataDictionaryField ddField = dictionary.findDataDictionaryFieldByAlias("pgm-school-number");

                if (ddField != null) {
                    spp.setFieldValueByBeanPath(ddField.getJavaName(), school.getSchoolId());
                }
                List errors = m_modelBroker.saveBean(spp);
                if (errors.isEmpty()) {
                    count++;
                } else {
                    skipped++;
                    logMessage(student.getNameView() + " - validation error.");
                }
            }
        } finally {
            iterator.close();
        }

        logMessage(count + " student program record" + (count != 1 ? "" : "s") + " created.");
        logMessage(skipped + " student program record" + (skipped != 1 ? "" : "s") + " skipped.");
    }

    /**
     * Added to allow for Secondary students to be picked up by the procedure.
     *
     * @param relPrefix String
     * @param school SisSchool
     * @return Criteria
     */
    private Criteria getSchoolCriteria(String relPrefix, SisSchool school) {
        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addEqualTo(relPrefix + SisStudent.COL_SCHOOL_OID, school.getOid());
        schoolCriteria
                .addOrCriteria(StudentManager.getSecondaryStudentCriteria(relPrefix + Student.REL_STUDENT_SCHOOLS + ".",
                        getCurrentContext().getOid(), school.getOid(), null, new PlainDate(),
                        getBroker().getPersistenceKey()));

        return schoolCriteria;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_modelBroker = new ModelBroker(userData);
    }
}
