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
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisShsmProgram.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisShsmProgram extends OnsisStateReportData {
    /**
     * The Class OnsisShsmProgramEntity.
     */
    public static class OnsisShsmProgramEntity extends OnsisStateReportEntity {

        private boolean m_isUnderEnrolment = true;
        private List<OnGraduationStudentProgram> m_studentPrograms;

        /**
         * Gets the all requirements met date.
         *
         * @return Plain date
         */
        public PlainDate getAllRequirementsMetDate() {
            return getAllRequirementsMetDate(getStudentProgram());
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisStudent student = (OnsisStudent) getBean();
            String name = student.getNameView();
            return name;
        }

        /**
         * Gets the student program.
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
            OnsisStudent student = (OnsisStudent) getBean();
            m_isUnderEnrolment = (parentEntity instanceof OnsisStudentEnrollmentEntity);

            /*
             * Secondary Enrolment spans should not should export SHSM_PROGRAM
             */
            if (parentEntity instanceof OnsisStudentEnrollmentEntity) {
                OnsisAnnualSpan span = ((OnsisStudentEnrollmentEntity) parentEntity).getSpan();
                if (span == null || span.isSecondary()) {
                    setRowCount(0);
                    return;
                }
            }

            /*
             * export SHSM Program if:
             *
             * 1. (GsrEndDate is empty)
             * OR (GsrEndDate is on/after ReportEndDate)
             * OR (GsrIssuedDate is in SubmissionPeriod)
             *
             * AND
             *
             * 2. GsrStartDate is on/before ReportEndDate
             */
            String thisBSID = deepGetFieldValueByFieldName(OnsisSchoolData.FIELD_SCHOOL_NUMBER);
            List<OnGraduationStudentProgram> studentPrograms = student.getProgramStudies(getBroker()).stream()
                    .filter(gsr -> gsr.getEndDate() == null || !gsr.getEndDate().before(getGlobalData().getEndDate())
                            || (gsr.getIssuedDate() != null
                                    && getGlobalData().getDateRange().contains(gsr.getIssuedDate())))
                    .filter(gsr -> gsr.getStartDate() != null
                            && !gsr.getStartDate().after(getGlobalData().getEndDate()))
                    .filter(gsr -> !StringUtils.isEmpty(gsr.getShsmProgramType()))
                    .filter(gsr -> {
                        // keep GSR's with a blank or matching BSID
                        String issuedByBSID = gsr.getBsidDiplomaEarned();
                        return (StringUtils.isBlank(issuedByBSID) || StringUtils.isEqual(issuedByBSID, thisBSID));
                    })
                    .collect(Collectors.toList());

            PlainDate reportStartDate = getGlobalData().getStartDate();
            PlainDate reportEndDate = getGlobalData().getEndDate();

            /*
             * An Enrolment span should only export SHSM_PROGRAM
             * 1. if there is no AllReqMetDate
             * or
             * 2. the AllReqMetDate is inside the span's date range
             */
            if (m_isUnderEnrolment) {
                OnsisStudentEnrollmentEntity studentEnrollmentEntity = (OnsisStudentEnrollmentEntity) parentEntity;
                PlainDate spanStartDate = ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentStartDate();
                PlainDate spanEndDate = ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentEndDate();

                PlainDate limitStartDate = (spanStartDate == null || spanStartDate.before(reportStartDate))
                        ? reportStartDate
                        : spanStartDate;
                PlainDate limitEndDate = spanEndDate;
                Range<Date> limitRange = Range.of(limitStartDate, limitEndDate);

                boolean isMainSchool = studentEnrollmentEntity.isMainSchool();

                m_studentPrograms = studentPrograms.stream().filter(gsr -> {
                    PlainDate allRequirementsMetDate = getAllRequirementsMetDate(gsr);
                    return (allRequirementsMetDate == null && isMainSchool)
                            || (allRequirementsMetDate != null && limitRange.contains(allRequirementsMetDate));
                }).collect(Collectors.toList());
            }

            /*
             * A NonEnrolment should only export SHSM_PROGRAM
             * 1. if there is no AllReqMetDate and no Primary span anywhere during the period
             * or
             * 2. if there is an AllReqMetDate during the period
             * and no Primary span anywhere contains AllReqMetDate
             */
            if (!m_isUnderEnrolment) {
                m_studentPrograms = studentPrograms.stream().filter(gsr -> {
                    PlainDate allRequirementsMetDate = getAllRequirementsMetDate(gsr);

                    /*
                     * Exclude Issued Date outside reporting period
                     */
                    if (allRequirementsMetDate != null) {
                        if (!getGlobalData().getDateRange().contains(allRequirementsMetDate)) {
                            return false;
                        }
                    }

                    /*
                     * Exclude unmatched BSID (if present)
                     */
                    String issuedByBSID = gsr.getBsidDiplomaEarned();
                    Set<String> limitingSchoolsOrNull =
                            getGlobalData().getSchoolByBsid(issuedByBSID).stream()
                                    .map(skl -> skl.getBsid())
                                    .collect(Collectors.toSet());

                    if (!StringUtils.isBlank(issuedByBSID) && !limitingSchoolsOrNull.isEmpty()) {
                        for (String limitingBSID : limitingSchoolsOrNull) {
                            if (!StringUtils.isBlank(limitingBSID)) {
                                if (!StringUtils.isEqual(issuedByBSID, limitingBSID, true)) {
                                    return false;
                                }
                            }
                        }
                    }

                    /*
                     * If allRequirementsMetDate is null:
                     * skip if there are any Enrolment spans in this reporting period
                     * regardless of school.
                     *
                     * Note: hasOverlappingEnrolmentSpan won't be aware of a generated demit span
                     * (an Enrolment found in the CSV but not exported by Aspen).
                     *
                     * Further checks for demit spans and child Certificate Dates
                     * will be performed later in isRowCanceled()
                     */
                    PlainDate startDate = reportStartDate;
                    PlainDate endDate = reportEndDate;
                    if (allRequirementsMetDate != null) {
                        startDate = allRequirementsMetDate;
                        endDate = allRequirementsMetDate;
                    }
                    Range<Date> dateRange = Range.of(startDate, endDate);

                    boolean foundSpan = student.getEnrollmentSpans(getBroker(), false, false).stream()
                            .filter(span -> limitingSchoolsOrNull.isEmpty()
                                    || limitingSchoolsOrNull.contains(((OnSchool) span.getSchool()).getBsid()))
                            .filter(span -> dateRange.isOverlap(span.getDateRange()))
                            .anyMatch(span -> !span.isSecondary());

                    if (foundSpan) {
                        return false;
                    }

                    Element parentElement = getReportData().getParentElement();
                    Element studentElement = (Element) parentElement.getParentNode();
                    List<Element> schoolEnrolments = getChildElements(
                            OnsisStudentSchoolEnrollment.ELEMENT_STUDENT_SCHOOL_ENROLMENT, studentElement);

                    boolean foundEnrollment = schoolEnrolments.stream().anyMatch(element -> {
                        Range<Date> elementRange = Range.of(getDateFromXml(
                                OnsisStudentSchoolEnrollment.ELEMENT_ENROLMENT_START_DATE, element),
                                getDateFromXml(
                                        OnsisStudentSchoolEnrollment.ELEMENT_ENROLMENT_END_DATE, element));
                        return elementRange.isOverlap(dateRange);
                    });
                    if (foundEnrollment) {
                        return false;
                    }

                    return true;
                }).collect(Collectors.toList());
            }

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
             * Don't send <SHSM_CERTIFICATION> that have Action=Add/Update,
             * with dates outside the reporting period.
             */
            List<Element> certificationsAddUpdates =
                    getElementsWithChildValue(ELEMENT_SHSM_CERTIFICATION, entityElement,
                            OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_ADD);
            certificationsAddUpdates.addAll(getElementsWithChildValue(ELEMENT_SHSM_CERTIFICATION, entityElement,
                    OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_UPDATE));
            for (Element certificationElement : certificationsAddUpdates) {
                String shsmDateText = OnsisStateReportData.getChildText(ELEMENT_DATE_COMPLETED, certificationElement);
                PlainDate shsmDate = OnsisConstants.parseDate(shsmDateText, SHSM_DATE_FORMATTER);

                boolean areOverlappedWithSubmissionPeriod =
                        shsmDate != null && getGlobalData().getDateRange().contains(shsmDate);
                if (!areOverlappedWithSubmissionPeriod) {
                    entityElement.removeChild(certificationElement);
                }

            }
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement Element
         * @param parentElement Element
         * @return true, if is row canceled
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.w3c.dom.Element,
         *      org.w3c.dom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            /*
             * This method cancels SHSM_PROGRAMs that are under NON_ENROLMENT
             */
            if (m_isUnderEnrolment) {
                return false;
            }

            /*
             * A NonEnrolment should only export SHSM_PROGRAM
             * 1. if there is no (AllReqMetDate or certificate date)
             * and no Primary span anywhere during the period
             *
             * OR
             *
             * 2. if there is an (AllReqMetDate or certificate date) during the period
             * and no Primary span anywhere contains that date
             */
            boolean isNoDateAndNoSpan = false;
            boolean isDateOutsideSpan = false;

            /*
             * Collect issued date
             */
            Set<PlainDate> relevantShsmDates = new HashSet<>();
            PlainDate shsmAllReqsMetDate = getDateFromXml(ELEMENT_ALL_REQUIREMENTS_MET_DATE, entityElement);
            if (shsmAllReqsMetDate != null) {
                relevantShsmDates.add(shsmAllReqsMetDate);
            }

            /*
             * Collect any child SHSM_CERTIFICATION/DATE_COMPLETED dates
             */
            List<Element> certificationElements = getChildElements(ELEMENT_SHSM_CERTIFICATION, entityElement);
            for (Element certificationElement : certificationElements) {
                PlainDate shsmCertDate = getDateFromXml(ELEMENT_DATE_COMPLETED, certificationElement);
                if (shsmCertDate != null) {
                    relevantShsmDates.add(shsmCertDate);
                }
            }

            /*
             * Collect Enrolment span dates
             */
            Element studentElement = (Element) parentElement.getParentNode();
            Element firstEnrolmentElement =
                    OnsisStateReportData.getChildElement(OnsisStudentSchoolEnrollment.ELEMENT_STUDENT_SCHOOL_ENROLMENT,
                            studentElement);

            /*
             * Evaluate retain condition #1
             * 1. if there is no (AllReqMetDate or certificate date)
             * and no Primary span anywhere during the period
             */
            isNoDateAndNoSpan = relevantShsmDates.isEmpty() && firstEnrolmentElement == null;

            /*
             * Evaluate retain condition #2
             * 2. if there is an (AllReqMetDate or certificate date) during the period
             * and no Primary span anywhere contains that date
             */
            PlainDate spanStartDate = null;
            PlainDate spanEndDate = null;
            if (firstEnrolmentElement != null) {
                spanStartDate = getDateFromXml(OnsisStudentSchoolEnrollment.ELEMENT_ENROLMENT_START_DATE,
                        firstEnrolmentElement);
                spanEndDate =
                        getDateFromXml(OnsisStudentSchoolEnrollment.ELEMENT_ENROLMENT_END_DATE, firstEnrolmentElement);
            }
            Range<Date> spanDateRange = Range.of(spanStartDate, spanEndDate);

            for (PlainDate shsmDate : relevantShsmDates) {
                if (!getGlobalData().getDateRange().contains(shsmDate)) {
                    continue;
                }

                isDateOutsideSpan = (firstEnrolmentElement == null) || !spanDateRange.contains(shsmDate);
                if (isDateOutsideSpan) {
                    break;
                }
            }

            boolean retain = isNoDateAndNoSpan || isDateOutsideSpan;
            return !retain;
        }

        /**
         * Gets the all requirements met date.
         *
         * @param shsmProgram OnsisGraduationStudentProgram
         * @return Plain date
         */
        private PlainDate getAllRequirementsMetDate(OnGraduationStudentProgram shsmProgram) {
            PlainDate allRequirementsMetDate = shsmProgram.getIssuedDate();
            if (allRequirementsMetDate == null || allRequirementsMetDate.after(getGlobalData().getEndDate())) {
                return null;
            }
            return allRequirementsMetDate;
        }
    }

    public static final String ELEMENT_ALL_REQUIREMENTS_MET_DATE = "ALL_REQUIREMENTS_MET_DATE";
    public static final String ELEMENT_DATE_COMPLETED = "DATE_COMPLETED";
    public static final String ELEMENT_SHSM_CERTIFICATION = "SHSM_CERTIFICATION";

    private static final SimpleDateFormat SHSM_DATE_FORMATTER = new SimpleDateFormat("yyyy/MM");

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
     * Generate deletes.
     *
     * @param parentElement Element
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateDeletes(org.w3c.dom.Element)
     */
    @Override
    protected void generateDeletes(Element parentElement) {
        OnsisStateReportEntity parentEntity = getParentEntity();
        boolean isUnderEnrolment = (parentEntity instanceof OnsisStudentEnrollmentEntity);
        if (isUnderEnrolment) {
            // Avoid generating DELETE for under non-Enrolment
            super.generateDeletes(parentElement);
        }
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisShsmProgramEntity.class);
    }

}
