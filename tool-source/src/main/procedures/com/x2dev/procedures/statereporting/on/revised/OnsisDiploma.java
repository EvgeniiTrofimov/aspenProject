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
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnGraduationStudentProgram;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisDiploma.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisDiploma extends OnsisStateReportData {
    /**
     * The Class OnsisDiplomaEntity.
     */
    public static class OnsisDiplomaEntity extends OnsisStateReportEntity {
        public static final String ELEMENT_CERTIFICATE_ISSUED = "CERTIFICATE_ISSUED";
        public static final String ELEMENT_TYPE = "TYPE";

        private OnsisStudent m_student;
        private List<OnGraduationStudentProgram> m_studentPrograms;

        /**
         * Gets the report data.
         *
         * @return Onsis diploma
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisDiploma getReportData() {
            return (OnsisDiploma) super.getReportData();
        }

        /**
         * Gets the student.
         *
         * @return Onsis student
         */
        public OnsisStudent getStudent() {
            return m_student;
        }

        /**
         * Gets the program.
         *
         * @return Graduation student program
         */
        public OnGraduationStudentProgram getStudentProgram() {
            return m_studentPrograms.get(getCurrentRow());
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

            OnsisStateReportEntity parentEntity = getReportData().getParentEntity();
            boolean isUnderEnrolment = (parentEntity instanceof OnsisStudentEnrollmentEntity);
            m_student = (OnsisStudent) bean;

            /*
             * Get diplomas issued during the submission period
             */
            String thisBSID = deepGetFieldValueByFieldName(OnsisSchoolData.FIELD_SCHOOL_NUMBER);
            Range<Date> dateRange = Range.of(null, null);
            if (isUnderEnrolment) {
                dateRange = Range.of(((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentStartDate(),
                        getGlobalData().getEndDate());
            }
            List<OnGraduationStudentProgram> diplomas =
                    m_student.getProgramStudies(getGlobalData().getDateRange(), thisBSID, isUnderEnrolment, dateRange);
            getReportData().log("Diplomas - DateRange: " + dateRange + " School: " + thisBSID + " isUnderEnrollment: "
                    + isUnderEnrolment + " count: " + diplomas.size());
            /*
             * Check exported STUDENT_SCHOOL_ENROLMENT
             * have overlapping range with diploma issue date
             */
            if (!isUnderEnrolment) {
                Element parentElement = getReportData().getParentElement();
                Element studentElement = (Element) parentElement.getParentNode();
                List<Element> schoolEnrolments =
                        getChildElements(OnsisStudentSchoolEnrollment.ELEMENT_STUDENT_SCHOOL_ENROLMENT, studentElement);
                diplomas = diplomas.stream()
                        .filter(gsr -> {
                            PlainDate issuedDate = gsr.getIssuedDate();
                            if (issuedDate == null) {
                                return true;
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
                        })
                        .collect(Collectors.toList());
            }

            m_studentPrograms =
                    diplomas.stream().sorted(Comparator.comparing(ToolBean::getOid)).collect(Collectors.toList());
            setRowCount(m_studentPrograms.size());
        }

        /**
         * After render row fields.
         *
         * @param entityElement Element
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#afterRenderRowFields(org.w3c.dom.Element)
         */
        @Override
        protected void afterRenderRowFields(Element entityElement) {
            super.afterRenderRowFields(entityElement);

            /*
             * Determine DIPLOMA_TYPE_OSSD_SHSM issue date to update subsequent diploma type
             */
            Optional<PlainDate> completedDate = getStudent().getProgramStudies(getBroker()).stream()
                    .filter(gsr -> {
                        PlainDate issuedDate = gsr.getIssuedDate();
                        if (issuedDate == null) {
                            return false;
                        }

                        String diplomaType = gsr.getDiplomaType();
                        if (OnGraduationStudentProgram.DIPLOMA_TYPE_OSSD_SHSM.equals(diplomaType)) {
                            return true;
                        }

                        return false;
                    })
                    .map(gsr -> gsr.getIssuedDate())
                    .reduce(BinaryOperator.maxBy(Comparator.nullsFirst(Comparator.naturalOrder())));

            if (!completedDate.isPresent()) {
                return;
            }

            Element diplomaType = OnsisStateReportData.getChildElement(ELEMENT_TYPE, entityElement);
            String textContent = diplomaType.getTextContent();
            if (!StringUtils.isEqual(textContent, OnGraduationStudentProgram.DIPLOMA_TYPE_COLLEGE_PREP)) {
                return;
            }

            /*
             * Change the Diploma type to DIPLOMA_TYPE_OSSD_SHSM
             * if completed on or before issue date
             */
            PlainDate dateFromXml = OnsisStateReportData.getDateFromXml(ELEMENT_CERTIFICATE_ISSUED, entityElement);
            if (completedDate.isPresent() && dateFromXml.compareTo(completedDate.get()) >= 0) {
                diplomaType.setTextContent(OnGraduationStudentProgram.DIPLOMA_TYPE_OSSD_SHSM);
            }
        }
    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        List<ToolBean> beans = Arrays.asList(getParentEntity().getBean());
        if (getParentEntity() instanceof OnsisStudentEnrollmentEntity) {
            OnsisStudentEnrollmentEntity parentEntity = (OnsisStudentEnrollmentEntity) getParentEntity();
            if (!parentEntity.isMainSchool().booleanValue()) {
                beans = Collections.EMPTY_LIST;
            }
        }
        log("Diploma Bean Count: " + beans.size());
        setBeans(beans);
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisDiplomaEntity.class);
    }

}
