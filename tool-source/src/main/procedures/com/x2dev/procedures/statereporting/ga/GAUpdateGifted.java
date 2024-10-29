/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class GAUpdateGifted extends ProcedureJavaSource {

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends StateReportData {
        protected StudentHistoryHelper m_helper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            getCodeMaps();
        }
    }

    protected static final String GIFTED_PROGRAM_CODE = "G";
    protected static final String NOT_REFERRED_CODE = "9";

    protected static final String ALIAS_GIFT_REFERRAL = "DOE Gift Referral";
    protected static final String ALIAS_GIFT_ELGIBILE = "DOE Gift Elig";
    protected static final String ALIAS_SERVICE_REASON = "all-pgm-ServiceStateReason";
    protected static final String ALIAS_ELIGIBLE_REASON = "all-pgm-EligibleReason";

    private EnrollmentStatistics m_data;
    private Map<String, String> serviceStartReasons = new HashMap<String, String>();
    private Map<String, String> elgibileReasons = new HashMap<String, String>();
    private Map<String, String> giftReferrals = new HashMap<String, String>();
    private Map<String, String> giftElgibility = new HashMap<String, String>();

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int rowsRead = 0;
        int rowsGifted = 0;
        int rowsNonReferred = 0;

        setUpEnrollmentStatistics();
        Criteria criteria = m_data.m_helper.getStudentCriteria();

        Collection<String> codes = getStateReportableCodes(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE, GIFTED_PROGRAM_CODE);

        Criteria studentCriteria = new Criteria();
        studentCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes);
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, criteria);
        studentCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, studentCriteria);
        Map<String, Collection<StudentProgramParticipation>> programs =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 100);
        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);

        try {
            while (iterator.hasNext()) {
                rowsRead++;
                Student student = (Student) iterator.next();
                Collection<StudentProgramParticipation> studentProgram = programs.get(student.getOid());
                if (studentProgram == null) {
                    rowsNonReferred++;
                    String giftReferral = giftReferrals.get(NOT_REFERRED_CODE);
                    student.setFieldValueByAlias(ALIAS_GIFT_REFERRAL, giftReferral);
                    String elgibileGiftReferral = giftElgibility.get(NOT_REFERRED_CODE);
                    student.setFieldValueByAlias(ALIAS_GIFT_ELGIBILE, elgibileGiftReferral);
                } else {
                    rowsGifted++;
                    StudentProgramParticipation program = studentProgram.iterator().next();
                    String startReason = (String) program.getFieldValueByAlias(ALIAS_SERVICE_REASON);
                    String stateCode = serviceStartReasons.get(startReason);
                    String giftReferral = giftReferrals.get(stateCode);
                    student.setFieldValueByAlias(ALIAS_GIFT_REFERRAL, giftReferral);

                    String elgibileReason = (String) program.getFieldValueByAlias(ALIAS_ELIGIBLE_REASON);
                    String elgibileStateCode = elgibileReasons.get(elgibileReason);
                    String elgibileGiftReferral = giftElgibility.get(elgibileStateCode);
                    student.setFieldValueByAlias(ALIAS_GIFT_ELGIBILE, elgibileGiftReferral);
                }

                getBroker().saveBean(student);
            }
        } finally {
            iterator.close();
        }

        logMessage("Total records read: " + rowsRead);
        logMessage("Total gifted records read: " + rowsGifted);
        logMessage("Total non-referred records read: " + rowsNonReferred);
    }

    /**
     * Setup EnrollmentStatistics for the StudentHistoryHelper.
     *
     * @throws X2BaseException exception
     */
    private void setUpEnrollmentStatistics() throws X2BaseException {
        m_data = new EnrollmentStatistics();
        m_data.setBroker(getBroker());
        m_data.setCurrentContext(getCurrentContext());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(false);
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
    }

    /**
     * Returns state reportable codes.
     *
     * @param string
     * @param m_fieldEll2
     * @param class1
     *
     * @return Collection<String> codes
     */
    private Collection<String> getStateReportableCodes(Class beanClass,
                                                       String beanPath,
                                                       String matchValue) {
        Set<String> codes = new HashSet<String>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(beanClass.getName(), beanPath);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, matchValue);

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                codes.add(code);
            }
        } finally {
            iterator.close();
        }
        if (codes.isEmpty()) {
            codes.add("--No-Match--");
        }
        return codes;
    }

    private void getCodeMaps() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SERVICE_REASON);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

        String[] columns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                String stateCode = (String) record[1];
                serviceStartReasons.put(code, stateCode);
            }
        } finally {
            iterator.close();
        }

        DataDictionaryField reasonField = dictionary.findDataDictionaryFieldByAlias(ALIAS_ELIGIBLE_REASON);
        X2Criteria reasonCriteria = new X2Criteria();
        reasonCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, reasonField.getReferenceTableOid());

        String[] reasonColumns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        ReportQueryByCriteria reasonQuery =
                new ReportQueryByCriteria(ReferenceCode.class, reasonColumns, reasonCriteria);
        ReportQueryIterator reasonIterator = getBroker().getReportQueryIteratorByQuery(reasonQuery);
        try {
            while (reasonIterator.hasNext()) {
                Object[] record = (Object[]) reasonIterator.next();
                String code = (String) record[0];
                String stateCode = (String) record[1];
                elgibileReasons.put(code, stateCode);
            }
        } finally {
            reasonIterator.close();
        }

        DataDictionaryField referralField = dictionary.findDataDictionaryFieldByAlias(ALIAS_GIFT_REFERRAL);
        X2Criteria referralCriteria = new X2Criteria();
        referralCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referralField.getReferenceTableOid());

        String[] referralColumns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        ReportQueryByCriteria referralQuery =
                new ReportQueryByCriteria(ReferenceCode.class, referralColumns, referralCriteria);
        ReportQueryIterator referralIterator = getBroker().getReportQueryIteratorByQuery(referralQuery);
        try {
            while (referralIterator.hasNext()) {
                Object[] record = (Object[]) referralIterator.next();
                String code = (String) record[0];
                String stateCode = (String) record[1];
                giftReferrals.put(stateCode, code);
            }
        } finally {
            iterator.close();
        }

        DataDictionaryField elgibilityField = dictionary.findDataDictionaryFieldByAlias(ALIAS_GIFT_ELGIBILE);
        X2Criteria elgibilityCriteria = new X2Criteria();
        elgibilityCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, elgibilityField.getReferenceTableOid());

        String[] elgibilityColumns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        ReportQueryByCriteria elgibilityQuery =
                new ReportQueryByCriteria(ReferenceCode.class, elgibilityColumns, elgibilityCriteria);
        ReportQueryIterator elgibilityIiterator = getBroker().getReportQueryIteratorByQuery(elgibilityQuery);
        try {
            while (elgibilityIiterator.hasNext()) {
                Object[] record = (Object[]) elgibilityIiterator.next();
                String code = (String) record[0];
                String stateCode = (String) record[1];
                giftElgibility.put(stateCode, code);
            }
        } finally {
            iterator.close();
        }
    }
}
