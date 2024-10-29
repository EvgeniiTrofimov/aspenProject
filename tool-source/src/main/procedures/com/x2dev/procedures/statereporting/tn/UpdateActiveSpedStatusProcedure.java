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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to update "Active" to "Inactive" SPED status.
 *
 * @author X2 Development Corporation
 */
public class UpdateActiveSpedStatusProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String ALIAS_STD_PRIMARY_OPTION = "EasyIEP Primary Option";
    private final static String CATEGORY_DISABILITY = "Disability";
    private final static String CATEGORY_OPTIONS = "Options";
    private static final String FLAG_INACTIVE = "Inactive";
    private final static String INPUT_AS_OF_DATE = "asOfDate";

    private Collection<String> m_allCodes;
    private PlainDate m_asOfDate;
    private ModelBroker m_modelBroker;
    private String m_primaryOptionField;
    private Collection<SisStudent> m_studentToUpdate = new ArrayList<SisStudent>();

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (!m_studentToUpdate.isEmpty()) {
            for (SisStudent student : m_studentToUpdate) {
                updateStudentSped(student);
            }
        } else {
            logMessage("There are no students to update SPED Status.");
        }
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        return m_modelBroker;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#isRunOnApplicationServer()
     */
    @Override
    protected boolean isRunOnApplicationServer() {
        return true;
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

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_asOfDate = (PlainDate) getParameter(INPUT_AS_OF_DATE);
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField primaryOptionField = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_PRIMARY_OPTION);
        m_primaryOptionField = primaryOptionField != null ? primaryOptionField.getJavaName() : null;

        loadDisabilityAndOptionsCodes();
        loadStudentsWithClosedSpedPrograms();
    }

    /**
     * Loads all codes that are associated with the 'Disability' and 'Options' category.
     */
    protected void loadDisabilityAndOptionsCodes() {
        m_allCodes = new HashSet<String>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        Criteria categoryCriteria = new Criteria();
        categoryCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, CATEGORY_DISABILITY);
        Criteria optionsCriteria = new Criteria();
        optionsCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, CATEGORY_OPTIONS);
        categoryCriteria.addOrCriteria(optionsCriteria);
        criteria.addAndCriteria(categoryCriteria);

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);

        ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (queryItr.hasNext()) {
                Object[] row = (Object[]) queryItr.next();
                m_allCodes.add((String) row[0]);
            }
        } finally {
            queryItr.close();
        }
    }

    /**
     * Method loads students without open SPED programs but with active SPED status.
     *
     * @return students to update
     */
    private void loadStudentsWithClosedSpedPrograms() {
        String spedActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);

        Criteria criteria = new Criteria();
        criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, m_allCodes);
        criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.COL_SPED_STATUS_CODE, spedActiveCode);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        Map<String, Collection<StudentProgramParticipation>> pgmsMap = m_modelBroker.getGroupedCollectionByQuery(query,
                StudentProgramParticipation.COL_STUDENT_OID,
                1024);

        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addEqualTo(SisStudent.COL_SPED_STATUS_CODE, spedActiveCode);

        Collection<SisStudent> students =
                m_modelBroker.getCollectionByQuery(new QueryByCriteria(SisStudent.class, stdCriteria));

        for (SisStudent student : students) {
            boolean needToUpdate = true;
            Collection<StudentProgramParticipation> pgms = pgmsMap.get(student.getOid());
            if (pgms != null) {
                for (StudentProgramParticipation pgm : pgms) {
                    if (pgm.getEndDate() == null || !pgm.getEndDate().before(m_asOfDate)) {
                        needToUpdate = false;
                        break;
                    }
                }
            }

            if (needToUpdate) {
                m_studentToUpdate.add(student);
            }
        }
    }

    /**
     * Updates the student SPED_STATUS to 'Inactive' if there are still sped status is "Active" but
     * no SPED programs.
     *
     * @param student SisStudent
     */
    private void updateStudentSped(SisStudent student) {
        logMessage("Sped status was set to Inactive for student with LASID = " + student.getLocalId() +
                ", Student Name = " + student.getNameView() +
                ", Current School = " + student.getSchool().getName());

        student.setSpedStatusCode(FLAG_INACTIVE);
        if (m_primaryOptionField != null) {
            student.setFieldValueByBeanPath(m_primaryOptionField, null);
        }
        if (student.isDirty()) {
            getBroker().saveBeanForced(student);
        }
    }
}
