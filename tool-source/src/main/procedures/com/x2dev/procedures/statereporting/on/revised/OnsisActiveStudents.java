/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2017 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * State report data module for the SIF SchoolInfo object, according to the MA DESE SIF 2.7 profile.
 * This data module is rooted at the School bean.
 *
 * @author Follett Software Company
 */
public class OnsisActiveStudents extends OnsisStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static class OnsisActiveStudentsEntity extends OnsisStateReportEntity {
        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnStudent student = (OnStudent) getBean();
            String name = student.getNameView();

            return name;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public OnStudent getStudent() {
            return (OnStudent) getBean();
        }
    }

    private DictionaryExtractor m_dictExtractor;
    private List<String> m_schoolOids;


    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());
        ToolBean.clearAllCachedToolBeans(OnStudent.class);
        setSchoolContext(true);

        X2Criteria studentCriteria = buildStudentCriteria();
        studentCriteria.addNotEmpty(OnStudent.FIELD_OEN.resolve(getDictionaryExtractor()),
                getBroker().getPersistenceKey());
        QueryByCriteria studentQuery = new BeanQuery(SisStudent.class, studentCriteria);
        List<OnStudent> students = FilterableFactory.create(getBroker(), getDictExtractor(),
                OnStudent.class, studentCriteria, null).extract().stream()
                .sorted((stu0, stu1) -> {
                    int res = getStudentLegalName(stu0).compareTo(getStudentLegalName(stu1));
                    if (res == 0) {
                        res = stu0.getOen().compareTo(stu1.getOen());
                    }
                    return res;
                })
                .collect(Collectors.toList());

        setBeans(students);
    }

    /**
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisActiveStudentsEntity.class);
    }

    /**
     * Gets the criteria for active students.
     *
     * @return X2Criteria
     */
    protected X2Criteria buildStudentCriteriaForActiveStudents() {
        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                Student.COL_ENROLLMENT_STATUS));
        activeCriteria.addIn(Student.COL_SCHOOL_OID, getSchoolOids());
        return activeCriteria;
    }

    /**
     * Gets the criteria for active student enrollments.
     *
     * @param beginDate PlainDate
     * @return X2Criteria
     */
    protected X2Criteria buildStudentCriteriaForEnrollments(PlainDate beginDate) {
        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, beginDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, getSchoolOids());
        return enrollmentCriteria;
    }

    /**
     * Gets the criteria for students.
     *
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria() {
        PlainDate beginDate = getCurrentContext().getStartDate();

        X2Criteria criteria = new X2Criteria();
        X2Criteria enrollmentCriteria = buildStudentCriteriaForEnrollments(beginDate);

        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        X2Criteria orCriteria = new X2Criteria();

        X2Criteria activeCriteria = buildStudentCriteriaForActiveStudents();

        // join the two criteria in an OR.
        orCriteria.addOrCriteria(enrCriteria);
        orCriteria.addOrCriteria(activeCriteria);

        // Build the final student criteria, including user criteria and exclude criteria.
        criteria.addAndCriteria(orCriteria);

        // Apply exclude criteria.
        criteria.addNotEqualTo(OnStudent.FIELD_EXCLUDE_FROM_REPORTING.resolve(getDictionaryExtractor()), BooleanAsStringConverter.TRUE);

        return criteria;
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the school oids.
     *
     * @return Collection
     */
    public List<String> getSchoolOids() {
        OnsisStateReportData reportData = this;
        if (getParentReportData() != null) {
            reportData = getParentReportData();
        }
        List<List<OnSchool>> schools = reportData.getGlobalData().getSchools();
        if (m_schoolOids == null) {
            Integer size = getGlobalData().getSchools().stream().collect(Collectors.summingInt(list -> list.size()));
            m_schoolOids = new ArrayList(size);
            getGlobalData().getSchools().stream().forEach(list -> list.forEach(skl -> m_schoolOids.add(skl.getOid())));
        }
        return m_schoolOids;
    }

    /**
     * Gets the legal name.
     *
     * @param student OnStudent
     * @return String
     */
    private String getStudentLegalName(OnStudent student) {
        String firstName = student.getLegalFirstName();
        String lastName = student.getLegalLastName();
        return lastName + ", " + firstName;
    }
}
