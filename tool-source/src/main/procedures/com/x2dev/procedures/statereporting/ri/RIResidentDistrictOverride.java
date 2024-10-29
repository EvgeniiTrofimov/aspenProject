/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

/**
 * Procedure to create an additional W and E enrollment records.
 *
 * @author X2 Development Corporation
 */
public class RIResidentDistrictOverride extends ProcedureJavaSource {

    public static final String ALIAS_ACTIVE_OP = "Active Outside Placement";
    public static final String ALIAS_HOME_SKL = "all-enr-HomeSchoolCode";
    public static final String ALIAS_OP_CLASSIFICATION = "OP Classification";
    public static final String ALIAS_OP_LOCATION_CODE = "OP Location Code";
    public static final String ALIAS_STATE_EXCLUSION = "DOE EXCLUDE ENR";
    public static final String PARAM_AS_OF_DATE = "asOfDate";
    public static final String STATE_CODE_DISTRICT_CHANGE = "33";
    public static final String STATUS_INACTIVE = "Inactive";
    public static final long TIME_BETWEEN_SAVE = 5;

    private PlainDate m_asOfDate;
    private ModelBroker m_broker;
    private ReferenceCode m_changeDistrictCode;
    private SisStudent m_student;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_changeDistrictCode = getChangeDistrictRefCode();
        if (m_student == null) {
            logMessage("This can only be performed for a single student");
        } else {
            Collection<StudentEnrollment> enrollments = loadEnrollments(m_student);
            if (m_changeDistrictCode != null) {
                processEnrollments(enrollments);
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    /*
     * (non-Javadoc)
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     * UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_asOfDate = (PlainDate) getParameter(PARAM_AS_OF_DATE);
        m_broker = new ModelBroker(userData.getPrivilegeSet());
        m_student = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Returns the reference code object for district change code
     *
     *
     * @return ReferenceCode
     */
    private ReferenceCode getChangeDistrictRefCode() {
        ReferenceCode codeChangeDistrict = null;
        String referenceTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        if (referenceTableOid != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, STATE_CODE_DISTRICT_CHANGE);
            criteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
            BeanQuery query = new BeanQuery(ReferenceCode.class, criteria, false);
            codeChangeDistrict = getBroker().getBeanByQuery(query);
        }
        if (codeChangeDistrict == null) {
            logMessage(String.format("Code with state value %s cannot be found", STATE_CODE_DISTRICT_CHANGE));
        }
        return codeChangeDistrict;

    }

    /**
     * Returns the collection of student enrollments
     *
     * @param student SisStudent
     * @return Collection
     */
    private Collection<StudentEnrollment> loadEnrollments(SisStudent student) {
        Collection<StudentEnrollment> enrollments = Collections.EMPTY_LIST;
        if (m_asOfDate != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_asOfDate);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            BeanQuery enrollmentQuery = new BeanQuery(StudentEnrollment.class, enrollmentCriteria);
            enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            enrollments = m_broker.getCollectionByQuery(enrollmentQuery);
        }
        return enrollments;
    }

    /**
     * Creates an additional W and E enrollment records.
     *
     * @param enrollments Collection<StudentEnrollment>
     */
    private void processEnrollments(Collection<StudentEnrollment> enrollments) {
        StudentEnrollment enrollment = null;
        if (enrollments != null) {
            for (StudentEnrollment enr : enrollments) {
                if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType())) {
                    enrollment = enr;
                    break;
                }
                if (StudentEnrollment.WITHDRAWAL.equals(enr.getEnrollmentType())) {
                    break;
                }
            }
        }
        if (enrollment != null) {

            StudentEnrollment withdrawal =
                    X2BaseBean.newInstance(StudentEnrollment.class, m_broker.getPersistenceKey());
            withdrawal.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
            withdrawal.setEnrollmentDate(DateUtils.add(m_asOfDate, -1));
            withdrawal.setEnrollmentCode(m_changeDistrictCode.getCode());
            withdrawal.setStatusCode(STATUS_INACTIVE);
            withdrawal.setYog(enrollment.getYog());
            withdrawal.setSchoolOid(enrollment.getSchoolOid());
            withdrawal.setStudentGuid(enrollment.getStudentGuid());
            withdrawal.setStudentOid(enrollment.getStudentOid());

            StudentEnrollment entry = X2BaseBean.newInstance(StudentEnrollment.class, m_broker.getPersistenceKey());
            entry.setEnrollmentType(StudentEnrollment.ENTRY);
            entry.setEnrollmentDate(m_asOfDate);
            entry.setEnrollmentCode(enrollment.getEnrollmentCode());
            entry.setStatusCode(enrollment.getStatusCode());
            entry.setYog(enrollment.getYog());
            entry.setFieldValueByAlias(ALIAS_ACTIVE_OP, enrollment.getFieldValueByAlias(ALIAS_ACTIVE_OP));
            entry.setFieldValueByAlias(ALIAS_HOME_SKL, enrollment.getFieldValueByAlias(ALIAS_HOME_SKL));
            entry.setFieldValueByAlias(ALIAS_OP_CLASSIFICATION,
                    enrollment.getFieldValueByAlias(ALIAS_OP_CLASSIFICATION));
            entry.setFieldValueByAlias(ALIAS_OP_LOCATION_CODE, enrollment.getFieldValueByAlias(ALIAS_OP_LOCATION_CODE));
            entry.setFieldValueByAlias(ALIAS_STATE_EXCLUSION, enrollment.getFieldValueByAlias(ALIAS_STATE_EXCLUSION));
            entry.setSchoolOid(enrollment.getSchoolOid());
            entry.setStudentGuid(enrollment.getStudentGuid());
            entry.setStudentOid(enrollment.getStudentOid());

            m_broker.beginTransaction();
            String wErrors =
                    m_broker.saveBean(withdrawal).stream().map(err -> err.toString()).collect(Collectors.joining("\n"));
            String eErrors =
                    m_broker.saveBean(entry).stream().map(err -> err.toString()).collect(Collectors.joining("\n"));

            if (StringUtils.isEmpty(wErrors) && StringUtils.isEmpty(eErrors)) {
                logMessage("Enrollment records created" + wErrors);
                m_broker.commitTransaction();
            } else {
                if (!StringUtils.isEmpty(wErrors)) {
                    logMessage("W record errors: \n" + wErrors);
                }
                if (!StringUtils.isEmpty(eErrors)) {
                    logMessage("E record errors: \n" + eErrors);
                }
                m_broker.rollbackTransaction();
            }

        } else {
            logMessage("Not found E record for student or found W record prior to E record");
        }
    }
}
