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

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher.CoreScheduledMode;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaff;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaffPosition;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolEducatorAssignment.OnsisSchoolEducatorAssignmentEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisValidations.ValidationRule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisAssignedSubject.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisAssignedSubject extends OnsisStateReportData {

    /**
     * The Class OnsisAssignedSubjectEntity.
     */
    public static class OnsisAssignedSubjectEntity extends OnsisStateReportEntity {
        public static final String DEFAULT_GRADE_FLAG = "G";

        private static final ValidationRule<OnsisStateReportEntity> s_validateMtcWithBadPosition =
                new ValidationRule<OnsisStateReportEntity>() {

                    @Override
                    public String getCustomMessage(OnsisStateReportEntity entity) {
                        OnsisAssignedSubjectEntity assignedSubjectEntity = (OnsisAssignedSubjectEntity) entity;

                        Set<String> staffPositionCodes = assignedSubjectEntity.getStaffPositionCodes();
                        StringBuilder message = new StringBuilder();
                        message.append(
                                " Correct staff position codes are: [" + String.join(", ", staffPositionCodes) + "].");
                        List<OnsisScheduleTeacher> schedulesWithBadPositions =
                                assignedSubjectEntity.getSchedulesBadPositions();
                        List<String> badSchedulesInfos = schedulesWithBadPositions.stream()
                                .map(scheduleTeacher -> {
                                    String schedulePositionCode = scheduleTeacher.getSchedulePositionCode(
                                            entity.getBroker(), entity.getGlobalData().getDateRange());
                                    return "[" + scheduleTeacher.getOid() + ": " + (schedulePositionCode == null
                                            ? "NO CODE"
                                            : schedulePositionCode) + "]";
                                })
                                .collect(Collectors.toList());
                        message.append(" Schedules with bad position codes: " + String.join(", ", badSchedulesInfos));
                        return message.toString();
                    }

                    @Override
                    public ValidationErrorType getErrorType() {
                        return ValidationErrorType.CRITICAL_ERROR;
                    }

                    @Override
                    public String getFieldName() {
                        return null;
                    }

                    @Override
                    public String getFieldValue(OnsisStateReportEntity entity) {
                        return null;
                    }

                    @Override
                    public String getGroupName() {
                        return "OnsisAssignedSubject";
                    }

                    @Override
                    public String getMessageCode() {
                        return "MtcBadPosition";
                    }

                    @Override
                    public String getMessageEn() {
                        return getMessage();
                    }

                    @Override
                    public String getMessageFr() {
                        return getMessage();
                    }

                    @Override
                    public boolean isValid(OnsisStateReportEntity entity) throws Exception {
                        return false;
                    }

                    private String getMessage() {
                        return "Staff has schedules with incorrect position types.";
                    }
                };

        private static final ValidationRule<OnsisStateReportEntity> s_validateMultiplePositionWithSameType =
                new ValidationRule<OnsisStateReportEntity>() {

                    @Override
                    public String getCustomMessage(OnsisStateReportEntity entity) {
                        OnsisAssignedSubjectEntity assignedSubjectEntity = (OnsisAssignedSubjectEntity) entity;
                        return " Duplicated position types are: ["
                                + String.join(", ", assignedSubjectEntity.getDuplicatedPositions()) + "]";
                    }

                    @Override
                    public ValidationErrorType getErrorType() {
                        return ValidationErrorType.CRITICAL_ERROR;
                    }

                    @Override
                    public String getFieldName() {
                        return null;
                    }

                    @Override
                    public String getFieldValue(OnsisStateReportEntity entity) {
                        return null;
                    }

                    @Override
                    public String getGroupName() {
                        return "OnsisAssignedSubject";
                    }

                    @Override
                    public String getMessageCode() {
                        return "SameType";
                    }

                    @Override
                    public String getMessageEn() {
                        return getMessage();
                    }

                    @Override
                    public String getMessageFr() {
                        return getMessage();
                    }

                    @Override
                    public boolean isValid(OnsisStateReportEntity entity) throws Exception {
                        return false;
                    }

                    private String getMessage() {
                        return "Staff has multiple positions of same type.";
                    }
                };


        /**
         * Gets the reference number from csv.
         *
         * @param csvRecord OnsisCsvDataRecord
         * @return String
         */
        public static String getReferenceNumberFromCsv(OnsisCsvDataRecord csvRecord) {
            /*
             * Get from CSV only.
             * Don't create if not in CSV
             * because Onsis wants to generate this key on an Add.
             */
            return (csvRecord != null)
                    ? csvRecord.getSingleFieldValue(OnsisExtractHelper.CsvField.REFERENCE_NUMBER)
                    : null;
        }

        private List<String> m_duplicatedPositions = new ArrayList<>();
        private OnsisAssignedSubject m_reportData;
        private List<OnsisScheduleTeacher> m_schedules;
        private List<OnsisScheduleTeacher> m_schedulesBadPositions = new ArrayList<>();
        private List<List<OnsisScheduleTeacher>> m_schedulesBySubject;
        private Set<String> m_staffPositionCodes = new HashSet<>();

        /**
         * Gets the duplicated positions.
         *
         * @return List
         */
        public List<String> getDuplicatedPositions() {
            return m_duplicatedPositions;
        }

        /**
         * Gets the grade flag.
         *
         * @return String
         */
        public String getGradeFlag() {
            return DEFAULT_GRADE_FLAG;
        }

        /**
         * Gets the language type.
         *
         * @return String
         */
        public String getLanguageType() {
            return ((OnSection) getCurrentSchedules().get(0).getSection(getBroker())).getLanguageOfInstruction();
        }

        /**
         * Gets the number of classes.
         *
         * @return String
         */
        public String getNumberOfClasses() {
            long numSections = getCurrentSchedules().stream()
                    .map(mtc -> (OnSection) mtc.getSection(getBroker()))
                    .filter(Objects::nonNull)
                    .filter(mst -> StringUtils.isEmpty(mst.getSectionClassOid()))
                    .count();
            long numClasses = getCurrentSchedules().stream()
                    .map(mtc -> (OnSection) mtc.getSection(getBroker()))
                    .filter(Objects::nonNull)
                    .map(mst -> mst.getSectionClassOid())
                    .filter(Objects::nonNull)
                    .distinct()
                    .count();
            return String.valueOf(numSections + numClasses);
        }

        /**
         * Gets the reference number.
         *
         * @return String
         */
        public String getReferenceNumber() {
            OnsisCsvDataRecord csvRecord = getCsvRecord();
            return getReferenceNumberFromCsv(csvRecord);
        }

        /**
         * Gets the schedules bad positions.
         *
         * @return List
         */
        public List<OnsisScheduleTeacher> getSchedulesBadPositions() {
            return m_schedulesBadPositions;
        }

        /**
         * Gets the staff position codes.
         *
         * @return Sets the
         */
        public Set<String> getStaffPositionCodes() {
            return m_staffPositionCodes;
        }

        /**
         * Gets the subject type.
         *
         * @return String
         */
        public String getSubjectType() {
            return ((OnSection) getCurrentSchedules().get(0).getSection(getBroker())).getElementarySubjectType();
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
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;

            m_reportData = (OnsisAssignedSubject) data;
            OnStaffPosition sfp = (OnStaffPosition) bean;
            OnStaff staff = (OnStaff) sfp.getStaff(getBroker());
            GlobalData globalData = getGlobalData();
            SubmissionSchoolType submissionSchoolType = globalData.getSchoolType();

            SubmissionType submissionType = globalData.getSubmissionType();
            PlainDate reportCountDate = submissionType.isJuneSubmission()
                    ? m_reportData.getLastInSesssionDateInPeriod()
                    : getGlobalData().getEndDate();

            m_schedulesBySubject = Collections.EMPTY_LIST;

            /*
             * Sections that teacher is core:
             * - Teacher is Primary or role is Primary/Co-Teach
             * - if Elementary only consider Homeroom sections
             */
            List<OnSection> coreSectionsAsOf = staff.getScheduleTeachers(getBroker()).stream()
                    .map(mtc -> (OnsisScheduleTeacher) mtc)
                    .filter(mtc -> mtc.isCoreSchedule(getBroker(), getGlobalData().getEndDate(),
                            CoreScheduledMode.SCHEDULED_ON_DATE))
                    .map(mtc -> mtc.getSection(getBroker()))
                    .map(mst -> (OnSection) mst)
                    .filter(mst -> submissionSchoolType.isClassAssignment(mst))
                    .collect(Collectors.toList());
            if (debugOutput != null) {
                debugOutput.append(
                        "OnsisAssignedSubjectEntity.intitialize - coreSectionsAsOf: " + coreSectionsAsOf + "\n");
            }


            /*
             * 2020-04-23 DevOps #8677
             *
             * Export ASSIGNED_SUBJECT's for teacher:
             * 1. IF teacher doesn't have any HR sections on which they are Primary,
             * then export all their ScheduleTeacher records
             * that qualify;
             *
             * 2. ELSE (IF teacher DOES have any HR sections on which they are Primary),
             * export all ScheduleTeacher with [all-mtc-ReportSpecialtySubject]=TRUE
             * (that qualify)
             */

            // Get the ScheduleTeacher records for this school/staff in date range
            List<OnsisScheduleTeacher> teacherSchedulesToReport = staff.getScheduleTeachers(getBroker()).stream()
                    .filter(mtc -> getGlobalData().getCurrentSchoolOids()
                            .contains(mtc.getSection(getBroker()).getSchedule(getBroker()).getSchoolOid()))
                    .map(mtc -> (OnsisScheduleTeacher) mtc)
                    .collect(Collectors.toList());
            if (debugOutput != null) {
                debugOutput.append("OnsisAssignedSubjectEntity.intitialize - teacherSchedulesToReport: "
                        + teacherSchedulesToReport + "\n");
            }

            // getReportData().log(
            // "Initial teacherSchedulesToReport " + teacherSchedulesToReport.extract().size());

            // If this is a core teacher (Primary/Co-Teach, plus Homeroom if Elem),
            // only publish ScheduleTeacher with [all-mtc-ReportSpecialtySubject] = true
            if (!coreSectionsAsOf.isEmpty()) {
                // getReportData().log("this is a core (Homeroom) teacher because
                // !primarySections.extract().isEmpty()");
                teacherSchedulesToReport =
                        teacherSchedulesToReport.stream().filter(mtc -> {
                            List<Range<Date>> dateIntervals = mtc.getDateIntervals(getBroker());
                            if (debugOutput != null) {
                                debugOutput.append("OnsisAssignedSubjectEntity.intitialize - dateIntervals: "
                                        + dateIntervals + "\n");
                            }

                            PlainDate span1Start = mtc.getStartDate();
                            if (debugOutput != null) {
                                debugOutput.append(
                                        "OnsisAssignedSubjectEntity.intitialize - span1Start: " + span1Start + "\n");
                            }
                            boolean shouldCheckSpecialtySubject1 = (span1Start == null)
                                    || (dateIntervals.get(0) != null && dateIntervals.get(0).contains(span1Start));
                            if (debugOutput != null) {
                                debugOutput.append(
                                        "OnsisAssignedSubjectEntity.intitialize - shouldCheckSpecialtySubject1: "
                                                + shouldCheckSpecialtySubject1 + "\n");
                            }
                            if (shouldCheckSpecialtySubject1 && mtc.getReportSpecialtySubject()) {
                                return true;
                            }

                            if (dateIntervals.get(1) != null && dateIntervals.get(1).contains(span1Start)
                                    && mtc.getReportSpecialtySubject2()) {
                                return true;
                            }

                            return false;
                        }).collect(Collectors.toList());
                // getReportData().log(
                // "Filtered by Specialty flag: teacherSchedulesToReport "
                // + teacherSchedulesToReport.extract().size());
            } else {
                /*
                 * Filter ScheduleTeacher records by date: Must overlap report count date
                 */
                teacherSchedulesToReport = teacherSchedulesToReport.stream()
                        .filter(mtc -> mtc.getDateIntervals(getBroker()).stream()
                                .anyMatch(range -> range != null && range.contains(reportCountDate)))
                        .collect(Collectors.toList());
            }
            if (debugOutput != null) {
                debugOutput.append("OnsisAssignedSubjectEntity.intitialize - teacherSchedulesToReport(final): "
                        + teacherSchedulesToReport + "\n");
            }

            if (!teacherSchedulesToReport.isEmpty()) {
                m_schedules = teacherSchedulesToReport;
                List<OnStaffPosition> allPositions = staff.getStaffPositions(getBroker()).stream()
                        .map(pos -> (OnStaffPosition) pos)
                        .filter(pos -> !staff.getScheduleTeachers(getBroker()).isEmpty()
                                || ((OnsisSchoolEducatorAssignment) m_reportData.getParentReportData())
                                        .getValidPositionTypeCodes().contains(pos.getJobCode()))
                        .collect(Collectors.toList());
                if (debugOutput != null) {
                    debugOutput.append("OnsisAssignedSubjectEntity.intitialize - allPositions: " + allPositions + "\n");
                }

                if (allPositions.size() > 1) {
                    // Get all PositionTypes from allPositions
                    m_staffPositionCodes.addAll(allPositions.stream()
                            .filter(pos -> ((OnsisSchoolEducatorAssignment) m_reportData.getParentReportData())
                                    .getValidPositionTypeCodes().contains(pos.getJobCode()))
                            .map(pos -> pos.getPositionType())
                            .collect(Collectors.toList()));
                    if (debugOutput != null) {
                        debugOutput.append("OnsisAssignedSubjectEntity.intitialize - m_staffPositionCodes: "
                                + m_staffPositionCodes + "\n");
                    }

                    // Which PositionType values are duplicated
                    if (m_staffPositionCodes.size() < allPositions.size()) {
                        List<String> duplicatedPositions = allPositions.stream()
                                .collect(Collectors.groupingBy(
                                        position -> position.getPositionType()))
                                .entrySet().stream().filter(entry -> entry.getValue().size() > 1)
                                .map(entry -> entry.getKey()).collect(Collectors.toList());
                        if (debugOutput != null) {
                            debugOutput.append("OnsisAssignedSubjectEntity.intitialize - duplicatedPositions: "
                                    + duplicatedPositions + "\n");
                        }
                        m_duplicatedPositions.addAll(duplicatedPositions);
                    }
                    // getReportData().log("m_staffPositionCodes " + m_staffPositionCodes);

                    String staffPostionCode = sfp.getPositionType();
                    // getReportData().log("staffPostionCode " + staffPostionCode);
                    m_schedules.stream()
                            .filter(mtc -> {
                                String schedulePositionCode =
                                        mtc.getSchedulePositionCode(getBroker(), globalData.getDateRange());
                                if (!m_staffPositionCodes.contains(schedulePositionCode)) {
                                    m_schedulesBadPositions.add(mtc);
                                }

                                // getReportData().log("schedulePositionCode " +
                                // schedulePositionCode);
                                return staffPostionCode.equals(schedulePositionCode)
                                        || !m_staffPositionCodes.contains(schedulePositionCode);
                            });
                }
                // getReportData().log("Intermediate m_schedules " + m_schedules.extract().size());

                List<OnsisScheduleTeacher> schedulesWithSubjTypeAndLang = m_schedules.stream()
                        .filter(schedule -> {
                            OnSection section = (OnSection) schedule.getSection(getBroker());
                            return section != null &&
                                    !StringUtils.isEmpty(section.getElementarySubjectType()) &&
                                    !StringUtils.isEmpty(section.getLanguageOfInstruction()) &&
                                    !StringUtils.isEmpty(section.getGradeLevel());
                        }).collect(Collectors.toList());
                if (debugOutput != null) {
                    debugOutput.append("OnsisAssignedSubjectEntity.intitialize - schedulesWithSubjTypeAndLang: "
                            + schedulesWithSubjTypeAndLang + "\n");
                }

                m_schedulesBySubject = new ArrayList(schedulesWithSubjTypeAndLang.stream()
                        .collect(Collectors.groupingBy(new Function<OnsisScheduleTeacher, String>() {

                            @Override
                            public String apply(OnsisScheduleTeacher mtc) {
                                OnSection section = (OnSection) mtc.getSection(getBroker());
                                return section.getElementarySubjectType() + ":" + section.getLanguageOfInstruction();
                            }
                        })).values());

                // Sort by subject type and LOI key for consistent XML
                if (m_schedulesBySubject.size() > 1) {
                    Collections.sort(m_schedulesBySubject, new Comparator<List<OnsisScheduleTeacher>>() {
                        @Override
                        public int compare(List<OnsisScheduleTeacher> o1, List<OnsisScheduleTeacher> o2) {
                            OnSection mst1 = (OnSection) o1.get(0).getSection(getBroker());
                            OnSection mst2 = (OnSection) o2.get(0).getSection(getBroker());

                            int compareTo =
                                    mst1.getElementarySubjectTypeRaw().compareTo(mst2.getElementarySubjectTypeRaw());

                            if (compareTo == 0) {
                                compareTo = mst1.getLanguageOfInstruction().compareTo(mst2.getLanguageOfInstruction());
                            }
                            return compareTo;
                        }
                    });
                }
            }

            if (debugOutput != null) {
                debugOutput.append("OnsisAssignedSubjectEntity.intitialize - m_schedulesBySubject: "
                        + m_schedulesBySubject + "\n");
                m_reportData.log(debugOutput.toString());
            }
            setRowCount(m_schedulesBySubject.size());

            if (m_schedulesBySubject.size() > 0) {
                // TODO: Implement when validator is implemented
                // if (!m_duplicatedPositions.isEmpty()) {
                // globalData.getValidator().validateEntity(this,
                // s_validateMultiplePositionWithSameType);
                // }
                // if (!m_schedulesBadPositions.isEmpty()) {
                // globalData.getValidator().validateEntity(this, s_validateMtcWithBadPosition);
                // }
            }
        }

        /**
         * After render row fields.
         *
         * @param entityElement Element
         */
        @Override
        protected void afterRenderRowFields(Element entityElement) {
            /*
             * Don't send <ASSIGNED_GRADE> that have Action=Update.
             * Because ASSIGNED_GRADE is an all-key object
             * so it's either Add or Delete but never changed via Update.
             *
             * Updates are removed here by the parent
             * after call to OnsisAssignedGrade.generateDeletes()
             * so that Deletes won't be generated
             * for the removed AssignedGrade Updates.
             */
            List<Element> assignedGradeUpdates =
                    getElementsWithChildValue(OnsisAssignedGrade.ELEMENT_ASSIGNED_GRADE, entityElement,
                            OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_UPDATE);
            for (Element assignedGradeUpdate : assignedGradeUpdates) {
                entityElement.removeChild(assignedGradeUpdate);
            }
        }

        /**
         * Gets the grades.
         *
         * @return String
         */
        protected Set<String> getGradesPrimaryDuringSubmission() {
            Set<String> grades = new TreeSet<>(new Comparator<String>() {
                @Override
                public int compare(String grade1, String grade2) {
                    return m_reportData.getGradeNumeric(grade1).compareTo(m_reportData.getGradeNumeric(grade2));
                }
            });

            for (OnsisScheduleTeacher schedule : getCurrentSchedules()) {
                String grade = schedule.getGradeLevel();
                grades.add(grade);
            }
            return grades;
        }

        /**
         * Gets the csv record.
         *
         * @return Onsis csv data record
         */
        private OnsisCsvDataRecord getCsvRecord() {
            OnsisExtractRecords matcher = getGlobalData().getExtractHelper()
                    .getMatcherByExtractType(OnsisStateReportData.EXTRACT_TYPE_ASSIGNED_SUBJECT);

            OnsisCsvDataRecord csvRecord = matcher == null ? null
                    : matcher.findRecord(
                            Arrays.asList(
                                    OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                                    OnsisExtractHelper.CsvField.MEN.toString(),
                                    OnsisExtractHelper.CsvField.ELEMENTARY_SUBJECT_TYPE.toString(),
                                    OnsisExtractHelper.CsvField.LANGUAGE_TYPE.toString()),
                            Arrays.asList(
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString()),
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.MEN.toString()),
                                    deepGetFieldValueByFieldName(
                                            OnsisExtractHelper.CsvField.ELEMENTARY_SUBJECT_TYPE.toString()),
                                    deepGetFieldValueByFieldName(
                                            OnsisExtractHelper.CsvField.LANGUAGE_TYPE.toString())));
            return csvRecord;
        }

        /**
         * Gets the current schedules.
         *
         * @return List
         */
        private List<OnsisScheduleTeacher> getCurrentSchedules() {
            return m_schedulesBySubject.get(getCurrentRow());
        }
    }

    public static final String ELEMENT_NAME_REFERENCE_NUMBER = "REFERENCE_NUMBER";

    private static final String ALIAS_RCD_GRADE_LEVEL_NUMERIC = "NumericGradeLevel";
    private static final String DDX_ID_GRADE_LEVELS = "REF-GRADE-LEVELS";

    private PlainDate m_lastInSesssionDateInPeriod;

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        OnsisSchoolEducatorAssignmentEntity parentEntity =
                (OnsisSchoolEducatorAssignmentEntity) getParentEntity();
        OnStaffPosition sfp = parentEntity.getEducatorAssignment().getStaffPosition();

        if (sfp == null) {
            setBeans(Collections.EMPTY_LIST);
            return;
        }

        /// Must be isTeacher and not ECE
        boolean includeSfp = (sfp != null);
        includeSfp &= parentEntity.isTeacher(sfp);
        includeSfp &= !parentEntity.isEarlyChildhoodEducator(sfp);

        // SFP must be active in report timeframe
        includeSfp &= Range.of(sfp.getStartDate(), sfp.getEndDate()).contains(getGlobalData().getEndDate());

        if (includeSfp) {
            setBeans(Arrays.asList(sfp));
        } else {
            setBeans(Collections.EMPTY_LIST);
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
     * Generate and append delete.
     *
     * @param record OnsisCsvDataRecord
     * @param currentEntityKeySet List<String>
     * @param currentEntityValueSet List<String>
     * @param parentElement Element
     * @return Element
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateAndAppendDelete(com.x2dev.procedures.statereporting.on.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord,
     *      java.util.List, java.util.List, org.w3c.dom.Element)
     */
    @Override
    protected Element generateAndAppendDelete(OnsisCsvDataRecord record,
                                              List<String> currentEntityKeySet,
                                              List<String> currentEntityValueSet,
                                              Element parentElement) {
        Element thisElement =
                super.generateAndAppendDelete(record, currentEntityKeySet, currentEntityValueSet, parentElement);

        // Get additional required fields from the CSV and add onto this Element
        if (record == null) {
            return thisElement;
        }

        String referenceNum = OnsisAssignedSubjectEntity.getReferenceNumberFromCsv(record);
        if (!referenceNum.contains(":")) {
            appendTextElement(ELEMENT_NAME_REFERENCE_NUMBER, "", thisElement);
            OnsisSchoolEducatorAssignmentEntity parentEntity =
                    (OnsisSchoolEducatorAssignmentEntity) getParentEntity();
            OnStaffPosition sfp = parentEntity.getEducatorAssignment().getStaffPosition();
            String men = ((OnStaff) sfp.getStaff(getBroker())).getMEN();
            throw new IllegalStateException("The CSV file for " + men + " subject type "
                    + OnsisStateReportData.getChildText("ELEMENTARY_SUBJECT_TYPE", thisElement)
                    + " contains an illegal reference number " + referenceNum
                    + " that cannot be processed by Onsis."
                    + " This is normally caused by a lack of grade levels on the previous submission.");
        }

        appendTextElement(ELEMENT_NAME_REFERENCE_NUMBER, referenceNum, thisElement);

        return thisElement;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisAssignedSubjectEntity.class);
    }


    /**
     * Gets the grade numeric.
     *
     * @param grade String
     * @return Integer
     */
    private Integer getGradeNumeric(String grade) {
        int value = Integer.MAX_VALUE;
        DictionaryExtractor extractor = getDictionaryExtractor();
        DataDictionaryField gradeLevelField = OnsisScheduleTeacher.FIELD_GRADE_LEVEL.getField(extractor);
        ReferenceCode code = extractor.getRefCodesWithStateValue(gradeLevelField, Arrays.asList(grade)).stream()
                .findAny().orElse(null);
        try {
            value = Integer.valueOf((String) code.getFieldValueByAlias(ALIAS_RCD_GRADE_LEVEL_NUMERIC,
                    extractor.getDictionary(DDX_ID_GRADE_LEVELS)));
        } catch (Exception e) {
            // no action
        }

        return Integer.valueOf(value);
    }

    /**
     * Gets the last in sesssion date in period.
     *
     * @return Plain date
     */
    private PlainDate getLastInSesssionDateInPeriod() {
        if (m_lastInSesssionDateInPeriod == null) {
            PlainDate periodEndDate = getGlobalData().getEndDate();
            m_lastInSesssionDateInPeriod = getGlobalData().getCurrentSchool().stream()
                    .map(skl -> skl.getMostCommonCalendar(getBroker(), getGlobalData().getCurrentContext().getOid()))
                    .map(cas -> cas.findFirstInSessionDate(getBroker(), periodEndDate, false))
                    .max(new Comparator<PlainDate>() {

                        @Override
                        public int compare(PlainDate o1, PlainDate o2) {
                            if (o1 == null) {
                                return o2 == null ? 0 : -1;
                            } else if (o2 == null) {
                                return 1;
                            }
                            return o1.compareTo(o2);
                        }
                    }).orElse(null);
        }
        return m_lastInSesssionDateInPeriod;
    }

}
