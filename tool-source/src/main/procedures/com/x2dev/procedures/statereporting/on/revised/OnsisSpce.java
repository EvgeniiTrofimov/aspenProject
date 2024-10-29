/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnGraduationStudentProgram;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisSpce.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSpce extends OnsisStateReportData {

    /**
     * The Class OnsisSpceEntity.
     */
    public static class OnsisSpceEntity extends OnsisStateReportEntity {

        private static final String SPC_CODING_VALUE = "2";
        private static final String SPC_ICE_VALUE = "1";
        private static final String SPC_MATH_LITERACY_VALUE = "3";

        private boolean m_isUnderEnrolment = true;
        private List<String> m_spceCodes;

        /**
         * Gets the spce type.
         *
         * @return String
         */
        public String getSpceType() {
            return m_spceCodes.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean); // data.getSchool()

            OnsisStateReportEntity parentEntity = getReportData().getParentEntity();
            OnsisStudent student = (OnsisStudent) getBean();
            m_isUnderEnrolment = (parentEntity instanceof OnsisStudentEnrollmentEntity);

            /*
             * Secondary Enrolment spans should not should export SPCE
             */
            if (m_isUnderEnrolment) {
                OnsisAnnualSpan span = ((OnsisStudentEnrollmentEntity) parentEntity).getSpan();
                if (span == null || span.isSecondary()) {
                    setRowCount(0);
                    return;
                }
            }


            /*
             * Get SPCE records issued during the submission period
             */
            List<OnGraduationStudentProgram> graduationPrograms = student.getProgramStudies(getBroker()).stream()
                    .filter(gsr -> gsr.getIssuedDate() != null
                            && getGlobalData().getDateRange().contains(gsr.getIssuedDate())
                            && !StringUtils.isEmpty(gsr.getDiplomaType()))
                    .collect(Collectors.toList());

            /*
             * If the GSR record has a BSID, skip the GSR if it doesn't match this school
             */
            String thisBSID = deepGetFieldValueByFieldName(OnsisSchoolData.FIELD_SCHOOL_NUMBER);
            graduationPrograms = graduationPrograms
                    .stream()
                    .filter(gsr -> {
                        // keep GSR's with a blank or matching BSID
                        String issuedByBSID = gsr.getBsidDiplomaEarned();
                        return (StringUtils.isBlank(issuedByBSID)
                                || StringUtils.isEqual(issuedByBSID, thisBSID));
                    }).collect(Collectors.toList());

            /*
             * An Enrolment span (as opposed to a non-enrolment)
             * should only export SPCE within its date range
             */
            if (m_isUnderEnrolment) {
                // Get the span start/end dates
                PlainDate spanStartDate =
                        ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentStartDate();
                PlainDate spanEndDate = ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentEndDate();
                Range<Date> spanDateRange = Range.of(spanStartDate, spanEndDate);

                graduationPrograms = graduationPrograms
                        .stream()
                        .filter(gsr -> {
                            if (spanStartDate == null) {
                                return false;
                            }

                            PlainDate issuedDate = gsr.getIssuedDate();
                            if (issuedDate == null) {
                                return false;
                            }
                            return spanDateRange.contains(issuedDate);
                        }).collect(Collectors.toList());
            }

            /*
             * A NonEnrolment should only export SPCE
             * that aren't in any Enrolment span date range
             */
            if (!m_isUnderEnrolment) {
                Element parentElement = getReportData().getParentElement();
                Element studentElement = (Element) parentElement.getParentNode();
                List<Element> schoolEnrolments =
                        getChildElements(OnsisStudentSchoolEnrollment.ELEMENT_STUDENT_SCHOOL_ENROLMENT, studentElement);
                graduationPrograms = graduationPrograms
                        .stream()
                        .filter(gsr -> {
                            PlainDate issuedDate = gsr.getIssuedDate();

                            if (issuedDate == null) {
                                return true;
                            }

                            String issuedByBSID = gsr.getBsidDiplomaEarned();
                            Set<String> limitingSchoolsOrNull =
                                    getGlobalData().getSchoolByBsid(issuedByBSID).stream()
                                            .map(skl -> skl.getOid())
                                            .collect(Collectors.toSet());

                            boolean foundSpan = student.getEnrollmentSpans(getBroker(), false, false).stream()
                                    .filter(span -> limitingSchoolsOrNull.isEmpty()
                                            || limitingSchoolsOrNull.contains(span.getSchool().getOid()))
                                    .filter(span -> span.getDateRange().contains(issuedDate))
                                    .anyMatch(span -> !span.isSecondary());

                            if (foundSpan) {
                                return false;
                            }

                            boolean foundEnrollment = schoolEnrolments.stream().anyMatch(element -> {
                                Range<Date> elementRange = Range.of(getDateFromXml(
                                        OnsisStudentSchoolEnrollment.ELEMENT_ENROLMENT_START_DATE, element),
                                        getDateFromXml(
                                                OnsisStudentSchoolEnrollment.ELEMENT_ENROLMENT_END_DATE, element));
                                return elementRange.contains(issuedDate);
                            });
                            if (foundEnrollment) {
                                return false;
                            }

                            return true;
                        }).collect(Collectors.toList());
            }

            /*
             * Determine the SPCE code value.
             */
            Set<String> spceCodes = new TreeSet<>();
            for (OnGraduationStudentProgram graduationProgram : graduationPrograms) {
                PlainDate issuedDate = graduationProgram.getIssuedDate();

                if (issuedDate == null) {
                    continue;
                }

                if (issuedDate.after(AUG_31_2017_Date)) {
                    if (graduationProgram.getSpcCodingIndicator()) {
                        spceCodes.add(SPC_CODING_VALUE);
                    }
                    if (graduationProgram.getSpcIceIndicator()) {
                        spceCodes.add(SPC_ICE_VALUE);
                    }
                    if (graduationProgram.getSpcMathLiteracyIndicator()) {
                        spceCodes.add(SPC_MATH_LITERACY_VALUE);
                    }
                } else {
                    String spceTypeOSP = graduationProgram.getOntarioSkillsPassport();
                    if (!StringUtils.isEmpty(spceTypeOSP)) {
                        spceCodes.add(spceTypeOSP);
                    }
                }
            }

            m_spceCodes = new ArrayList<>(spceCodes);
            setRowCount(m_spceCodes.size());
        }
    }


    private static PlainDate AUG_31_2017_Date;
    private static final int YEAR_2017 = 2017;

    static {
        Calendar aug312017Cal = Calendar.getInstance();

        aug312017Cal.set(Calendar.YEAR, YEAR_2017);
        aug312017Cal.set(Calendar.MONTH, Calendar.AUGUST);
        aug312017Cal.set(Calendar.DAY_OF_MONTH, 31);

        AUG_31_2017_Date = new PlainDate(aug312017Cal.getTime());
    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSpceEntity.class);
    }
}
