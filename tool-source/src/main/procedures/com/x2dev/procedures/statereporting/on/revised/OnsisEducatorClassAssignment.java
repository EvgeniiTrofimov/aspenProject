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
import com.x2dev.procedures.statereporting.common.ToolBean.ToolScheduleTeacher;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStaff;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Pair;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher.CoreScheduledMode;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaffPosition;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolEducatorAssignment.OnsisSchoolEducatorAssignmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.w3c.dom.Element;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisEducatorClassAssignment extends OnsisStateReportData {

    /**
     * The Class OnsisEducatorClassAssignmentEntity.
     */
    public static class OnsisEducatorClassAssignmentEntity extends OnsisStateReportEntity {

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisSchoolEducatorAssignmentEntity parentEntity =
                    (OnsisSchoolEducatorAssignmentEntity) getReportData().getParentEntity();
            OnStaffPosition staffPosition = parentEntity.getEducatorAssignment().getStaffPosition();

            String name = staffPosition.toString();
            ToolStaff staff = staffPosition.getStaff(getBroker());
            if (staff != null) {
                name = staff.getNameView();
            }
            PlainDate startDate = staffPosition.getStartDate();
            PlainDate endDate = staffPosition.getEndDate();

            name = name + "[" + startDate + " " + endDate + "]";

            OnSection section = (OnSection) getBean();
            name = name + " " + section.getCourseView();

            return name;
        }

        /**
         * Gets the id.
         *
         * @return String
         */
        public String getId() {
            OnSection section = (OnSection) getBean();
            String classCode = getReportData().getClassCode(section);
            return classCode;
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisEducatorClassAssignment getReportData() {
            return (OnsisEducatorClassAssignment) super.getReportData();
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

            OnsisEducatorClassAssignment reportData = (OnsisEducatorClassAssignment) data;

            OnSection section = (OnSection) bean;

            OnsisSchoolEducatorAssignmentEntity educatorAssignmentEntity =
                    (OnsisSchoolEducatorAssignmentEntity) getReportData().getParentEntity();

            PlainDate assignmentStartDate = OnsisConstants.parseDate(educatorAssignmentEntity.getAssignmentStartDate(),
                    OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES);
            PlainDate assignmentEndDate = OnsisConstants.parseDate(educatorAssignmentEntity.getAssignmentEndDate(),
                    OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES);
            Range<Date> sfpDateRange = Range.of(assignmentStartDate, assignmentEndDate);


            boolean isNotApplicable = OnsisSchoolEducatorAssignmentEntity.TEACHING_TYPE_NOT_APPLICABLE
                    .equals(educatorAssignmentEntity.getTeachingType());
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            PlainDate periodStartDate = submissionType.getPeriodStartDate();

            /*
             * Skip this section if it starts after the report end date,
             * or if it doesn't overlap the StaffPosition dates,
             * or if Educator has teaching type N\A and section doesn't start with submission period
             */
            boolean startDateOk = false;
            boolean overlapsSfp = false;
            startDateOk = section.getScheduleTermDates(getBroker()).stream()
                    .anyMatch(termDate -> termDate.getStartDate() != null
                            && !termDate.getStartDate().after(getGlobalData().getEndDate())
                            && (!isNotApplicable || !termDate.getStartDate().before(periodStartDate)));
            overlapsSfp = section.getScheduleTermDates(getBroker()).stream()
                    .anyMatch(termDate -> termDate.getRange().isOverlap(sfpDateRange));

            if (!startDateOk || !overlapsSfp) {
                getReportData()
                        .log("Skip " + section.getCourseView() + ": startDateOk=" + startDateOk + ", overlapsSfp="
                                + overlapsSfp);
                setRowCount(0);
                return;
            }
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#afterRenderRowFields(org.w3c.dom.Element)
         */
        @Override
        protected void afterRenderRowFields(Element entityElement) {
            super.afterRenderRowFields(entityElement);

            /*
             * If the parent Action is Add,
             * replace child Update with Add
             */
            Element myActionElement =
                    OnsisStateReportData.getChildElement(OnsisRetrieverAction.ELEMENT_NAME_ACTION, entityElement);
            if (myActionElement == null) {
                return;
            }

            String myAction = myActionElement.getTextContent();
            String parentEducAssignmentAction =
                    getReportData().getParentEntity().getFieldValue(OnsisRetrieverAction.FIELD_ACTION);
            if (OnsisRetrieverAction.ACTION_ADD.equals(parentEducAssignmentAction)
                    && OnsisRetrieverAction.ACTION_UPDATE.equals(myAction)) {
                myActionElement.setTextContent(OnsisRetrieverAction.ACTION_ADD);
            }
        }
    }

    private Set<String> m_processedScheduleClassOids = null;

    /**
     * Builds the beans.
     *
     * 20200506 The spec for sections to include is:
     * 1. Include Class
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        OnsisSchoolEducatorAssignmentEntity parentEntity =
                (OnsisSchoolEducatorAssignmentEntity) getParentEntity();
        OnStaffPosition sfp = parentEntity.getEducatorAssignment().getStaffPosition();
        List<OnStaffPosition> allPositions = parentEntity.getReportablePositions();

        StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
        if (debugOutput != null) {
            debugOutput.append("OnsisEducatorClassAssignment.buildBeans - SFP: " + sfp + "\n");
        }

        // Validate the SFP is for this teacher and in the timeframe
        if (!(sfp != null && parentEntity.isTeacher(sfp)
                && (sfp.getEndDate() == null || !sfp.getEndDate().before(getGlobalData().getStartDate()))
                && (sfp.getStartDate() == null || !sfp.getStartDate().after(getGlobalData().getEndDate())))) {
            setBeans(Collections.EMPTY_LIST);
            if (debugOutput != null) {
                this.log(debugOutput.toString());
            }
            return;
        }

        boolean isTeaching =
                OnsisSchoolEducatorAssignmentEntity.TEACHING_TYPE_TEACHING.equals(parentEntity.getTeachingType());
        boolean isNotApplicable =
                OnsisSchoolEducatorAssignmentEntity.TEACHING_TYPE_NOT_APPLICABLE.equals(parentEntity.getTeachingType());

        SubmissionSchoolType schoolType = getGlobalData().getSchoolType();
        List<Pair<OnsisScheduleTeacher, OnSection>> sections;

        if (debugOutput != null) {
            debugOutput.append("isTeaching: " + isTeaching + "\n");
            debugOutput.append("All sections: " + sfp.getStaff(getBroker()).getScheduleTeachers(getBroker()).stream()
                    .map(Object::toString).collect(Collectors.joining("\n")) + "\n");
        }
        if (!parentEntity.isEarlyChildhoodEducator(sfp)) {
            sections = sfp.getStaff(getBroker()).getScheduleTeachers(getBroker()).stream()
                    .map(mtc -> (OnsisScheduleTeacher) mtc)
                    .filter(mtc -> mtc.isCoreSchedule(getBroker(), getGlobalData().getEndDate(),
                            CoreScheduledMode.SCHEDULED_BEFORE_DATE))
                    .map(mtc -> Pair.of(mtc, (OnSection) mtc.getSection(getBroker())))
                    .filter(pair -> schoolType.isClassAssignment(pair.getRight()))
                    .collect(Collectors.toList());
            if (debugOutput != null) {
                debugOutput.append("Regular sections: "
                        + sections.stream().map(Object::toString).collect(Collectors.joining("\n")) + "\n");
            }
        } else {
            sections = sfp.getStaff(getBroker()).getScheduleTeachers(getBroker()).stream()
                    .map(mtc -> (OnsisScheduleTeacher) mtc)
                    .filter(mtc -> {
                        String translatedRole =
                                getDictionaryExtractor().getStateValue(mtc, ToolScheduleTeacher.FIELD_ROLE);
                        if (mtc.getStartDate2() != null && !mtc.getStartDate2().after(getGlobalData().getEndDate())) {
                            translatedRole =
                                    getDictionaryExtractor().getStateValue(mtc, OnScheduleTeacher.FIELD_ROLE_2);
                        }
                        return !OnScheduleTeacher.MTC_TEACHING_ROLE_EXCLUDE.equals(translatedRole);
                    })
                    .map(mtc -> Pair.of(mtc, (OnSection) mtc.getSection(getBroker())))
                    .filter(pair -> {
                        String currentClassType = pair.getRight().getClassType();
                        return !OnSection.CLASS_TYPE_EXTERNAL_EDUCATOR.equals(currentClassType);
                    })
                    .filter(pair -> OnSection.COURSE_CODE_TYPE_HOMEROOM.equals(pair.getRight().getCourseCodeType()))
                    .filter(pair -> {
                        Range<Date> dateRangeSection = pair.getRight().getDateRange(getBroker());
                        return pair.getLeft().getDateIntervals(getBroker()).stream()
                                .filter(Objects::nonNull)
                                .anyMatch(dateRange -> dateRange.contains(getGlobalData().getEndDate())
                                        || dateRange.contains(dateRangeSection.getEnd()));
                    })
                    .collect(Collectors.toList());
            if (debugOutput != null) {
                debugOutput.append("Early Childhood sections: "
                        + sections.stream().map(Object::toString).collect(Collectors.joining("\n")) + "\n");
            }
        }

        Stream<OnSection> standalone = sections.stream()
                .filter(pair -> {
                    boolean isEmpty = StringUtils.isEmpty(pair.getRight().getSectionClassOid());
                    if (!isEmpty) {
                        return false;
                    }
                    return pair.getRight().isStudentEnrolled(getBroker(), getGlobalData().getDateRange(), true);
                })
                .filter(pair -> isTeaching
                        || sfp.getOid().equals(getBestPosition(allPositions, Arrays.asList(pair)).getOid()))
                .map(pair -> pair.getRight());

        Stream<OnSection> classBased = sections.stream()
                .filter(pair -> {
                    boolean isEmpty = StringUtils.isEmpty(pair.getRight().getSectionClassOid());
                    if (isEmpty) {
                        return false;
                    }
                    return pair.getRight().isStudentEnrolled(getBroker(), getGlobalData().getDateRange(), true);
                })
                .collect(Collectors.groupingBy(pair -> pair.getRight().getSectionClassOid()))
                .values().stream()
                .filter(pairs -> isTeaching || sfp.getOid().equals(getBestPosition(allPositions, pairs).getOid()))
                .map((list -> list.get(0).getRight()));

        // Concat and sort by Class Code
        List<OnSection> sectionsToReport = Stream.concat(standalone, classBased)
                .filter(mst -> {
                    if (!isNotApplicable) {
                        return true;
                    }
                    Range<Date> range = mst.getDateRange(getBroker());
                    Range<Date> intersection = range.intersection(getGlobalData().getDateRange());
                    if (range.equals(intersection)) {
                        return true;
                    }
                    return false;
                })
                .collect(Collectors.toList());

        if (sectionsToReport.size() > 1) {
            Collections.sort(sectionsToReport, new Comparator<OnSection>() {
                @Override
                public int compare(OnSection o1, OnSection o2) {
                    return OnsisHelper.compareTo(getClassCode(o1), getClassCode(o2));
                }
            });
        }

        if (debugOutput != null) {
            debugOutput.append("Reportable sections: "
                    + sectionsToReport.stream().map(Object::toString).collect(Collectors.joining("\n")) + "\n");
        }
        setBeans(sectionsToReport);
        if (debugOutput != null) {
            this.log(debugOutput.toString());
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#close()
     */
    @Override
    public void close() {
        super.close();

        if (m_processedScheduleClassOids != null) {
            m_processedScheduleClassOids.clear();
        }
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.follett.fsc.aspensif.framework.SifStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * See OnsisStateReportData.CalculatorClass.getCalculatedValue
     *
     * @param section
     * @return
     */
    public String getClassCode(OnSection section) {
        return fixupClassCode(getGlobalData(), section.getClassCode(getBroker()), getBroker());
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateDeletes(org.w3c.dom.Element)
     */
    @Override
    protected void generateDeletes(Element parentElement) {
        /*
         * 2020-12-03: Never send DELETE for an Educator Class Assignment
         * on Secondary submissions
         */
        if (getGlobalData().getSubmissionType().isSecondarySubmission()) {
            return;
        }

        super.generateDeletes(parentElement);
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisEducatorClassAssignmentEntity.class);
    }

    /**
     * Gets the best position.
     *
     * @param allPositions the all positions
     * @param pairs2
     * @param pairs the pairs
     * @return the best position
     */
    public OnStaffPosition getBestPosition(List<OnStaffPosition> allPositions,
                                           List<Pair<OnsisScheduleTeacher, OnSection>> pairs) {
        List<OnStaffPosition> newPositions = allPositions.stream()
                .sorted(new Comparator<OnStaffPosition>() {

                    @Override
                    public int compare(OnStaffPosition sfp1, OnStaffPosition sfp2) {
                        // first comparison is the number of mtc records with matching
                        Integer positionScore1 = pairs.stream()
                                .map(pair -> {
                                    if (StringUtils.isEmpty(pair.getLeft().getPosition())) {
                                        return 0;
                                    } else if (pair.getLeft().getPosition().equals(sfp1.getPositionType())) {
                                        return 1;
                                    } else {
                                        return -1;
                                    }
                                })
                                .collect(Collectors.summingInt(Integer::intValue));
                        Integer positionScore2 = pairs.stream()
                                .map(pair -> {
                                    if (StringUtils.isEmpty(pair.getLeft().getPosition())) {
                                        return 0;
                                    } else if (pair.getLeft().getPosition().equals(sfp2.getPositionType())) {
                                        return 1;
                                    } else {
                                        return -1;
                                    }
                                })
                                .collect(Collectors.summingInt(Integer::intValue));
                        // reverse sort position match count
                        int value = positionScore2.compareTo(positionScore1);



                        if (value == 0) {
                            Boolean isTeaching1 = sfp1.isTeacher();
                            Boolean isTeaching2 = sfp2.isTeacher();
                            // reverse sort teaching status
                            value = isTeaching2.compareTo(isTeaching1);
                        }

                        if (value == 0) {
                            Integer numDates1 = pairs.stream()
                                    .map(pair -> pair.getRight().getCalendarDates(null, null, m_broker)
                                            .stream()
                                            .filter(csd -> sfp1.isActiveOn(csd.getDate()))
                                            .count())
                                    .collect(Collectors.summingInt(Long::intValue));
                            Integer numDates2 = pairs.stream()
                                    .map(pair -> pair.getRight().getCalendarDates(null, null, m_broker)
                                            .stream()
                                            .filter(csd -> sfp2.isActiveOn(csd.getDate()))
                                            .count())
                                    .collect(Collectors.summingInt(Long::intValue));

                            // reverse sort on # days
                            value = numDates2.compareTo(numDates1);
                        }

                        if (value == 0) {
                            // reverse sort on fte
                            value = sfp2.getFte().compareTo(sfp1.getFte());
                        }

                        if (value == 0) {
                            // reverse sort on oid
                            value = sfp2.getOid().compareTo(sfp1.getOid());
                        }

                        return value;
                    }
                })
                .collect(Collectors.toList());
        return newPositions.get(0);
    }
}
