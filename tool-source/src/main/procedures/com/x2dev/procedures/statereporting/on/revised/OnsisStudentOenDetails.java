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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.ValidationError;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * State report data module for the SIF SchoolInfo object, according to the MA DESE SIF 2.7 profile.
 * This data module is rooted at the School bean.
 *
 * @author Follett Software Company
 */
public class OnsisStudentOenDetails extends OnsisStateReportData {

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static class OnsisStudentOenDetailsEntity extends OnsisStateReportEntity {
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
         * Gets the report data.
         *
         * @return Onsis state report data
         */
        @Override
        public OnsisStudentOenDetails getReportData() {
            return (OnsisStudentOenDetails) getData();
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnStudent student = (OnStudent) bean;
            if (!hasValidSpan(student)) {
                setRowCount(0);
            }
        }

        /**
         *
         * @param student
         * @return
         */
        protected boolean hasValidSpan(OnStudent student) {

            List<ValidationError> errors = new ArrayList();
            List<AnnualSpan> annualSpans = student.getEnrollmentSpans(getBroker(), true, true);

            boolean hasValidSpan = false;
            if (annualSpans != null && !annualSpans.isEmpty()) {
                for (AnnualSpan annualSpan : annualSpans) {
                    PlainDate lastActive = annualSpan.getLastActiveInSessionDate();
                    PlainDate firstActive = annualSpan.getFirstActiveInSessionDate();
                    if (firstActive != null &&
                            (lastActive == null || !lastActive.before(firstActive))) {
                        hasValidSpan = true;
                        break;
                    }
                }
            }

            return hasValidSpan;
        }
    }

    public static final String ALIAS_PSN_LEGAL_FIRST_NAME = "all-psn-LegalFirstName";
    public static final String ALIAS_PSN_LEGAL_SURENAME = "all-psn-LegalLastName";

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {

        ToolBean.setBroker(getBroker());

        ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getGlobalData().getContext().getStartDate());

        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnSchool.class);

        ToolBean.clearAllCachedToolBeans(OnEnrollment.class);
        ToolBean.clearAllCachedToolBeans(OnStudent.class);

        List<ValidationError> errors = new ArrayList<>();
        List<String> schoolOids =
                this.getGlobalData().getCurrentSchool().stream().map(ToolBean::getOid).collect(Collectors.toList());

        X2Criteria studentLimitingCriteria = new X2Criteria();
        studentLimitingCriteria.addEmpty(OnStudent.FIELD_OEN.resolve(getDictExtractor()),
                getBroker().getPersistenceKey());
        studentLimitingCriteria.addNotEmpty(OnStudent.FIELD_LEGAL_FIRST_NAME.resolve(getDictExtractor()),
                getBroker().getPersistenceKey());
        studentLimitingCriteria.addNotEmpty(OnStudent.FIELD_LEGAL_LAST_NAME.resolve(getDictExtractor()),
                getBroker().getPersistenceKey());
        studentLimitingCriteria.addNotNull(OnStudent.FIELD_DOB.resolve(getDictExtractor()));
        EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                .setSchoolOids(schoolOids)
                .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING)
                .setCurrentContext(getCurrentContext())
                .setIncludeSecondarySpans(false)
                .setStudentLimitingCriteria(studentLimitingCriteria);
        X2Criteria candidateCriteria =
                CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
        // load students with filterable
        FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class, candidateCriteria,
                Arrays.asList(ToolBean.FIELD_OID));
        // load enrollments and student school
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                OnStudent.CHILD_STUDENT_ENROLLMENTS);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                OnStudent.CHILD_STUDENT_SCHOOLS);

        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));

        Set<String> allStudentsOids = new HashSet();
        ToolBean.getCachedToolBeans(OnStudent.class).stream()
                .filter(student -> {
                    List<ValidationError> errList = new ArrayList<>();
                    List<AnnualSpan> spans = student.getEnrollmentSpans(getBroker(), true, true);
                    return spans.stream().anyMatch(span -> {
                        String arrivalStatus = null;
                        OnEnrollment enrollment = (OnEnrollment) span.getRecentEnrollment();
                        if (enrollment != null) {
                            arrivalStatus = enrollment.getArrivalStatus();
                        }
                        return OnsisConstants.ARRIVAL_STATUSES_ARRIVED.contains(arrivalStatus);
                    });
                })
                .map(student -> student.getEnrollmentSpans(getBroker(), true, true))
                .flatMap(List::stream)
                .filter(span -> {
                    // test school
                    if (!schoolOids.contains(span.getSchool().getOid())) {
                        return false;
                    }
                    // test date range
                    PlainDate spanStartDate = span.getSpanStartDate();
                    PlainDate spanEndDate = span.getSpanEndDate();
                    boolean overlaps = !getCurrentContext().getEndDate().before(spanStartDate)
                            && (spanEndDate == null || !getCurrentContext().getStartDate().after(spanEndDate));
                    if (!overlaps) {
                        return false;
                    }

                    return true;
                })
                .forEach(span -> {
                    allStudentsOids.add(span.getStudent().getOid());
                });

        ToolBean.filterCachedToolBeans(OnStudent.class,
                student -> allStudentsOids.contains(student.getOid()));

        setBeans(ToolBean.getCachedToolBeans(OnStudent.class).stream()
                .sorted((s1, s2) -> s1.getOid().compareTo(s2.getOid()))
                .collect(Collectors.toList()));
    }

    /**
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisStudentOenDetailsEntity.class);
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        return getGlobalData().getDictionaryExtractor();
    }
}
