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

import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStaff;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStaffPosition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher.CoreScheduledMode;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaff;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaffPosition;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.CsvField;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisSchoolEducatorAssignment.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSchoolEducatorAssignment extends OnsisStateReportData {

    /**
     * The Class EducatorAssignment.
     */
    public static class EducatorAssignment {
        OnStaffPosition m_sfp;
        PlainDate m_endDate;
        OnStaff m_stf;
        OnsisCsvDataRecord m_csvRecord;

        /**
         * Instantiates a new educator assignment.
         *
         * @param broker X2Broker
         * @param sfp StaffPosition
         * @param record OnsisCsvDataRecord
         */
        public EducatorAssignment(X2Broker broker, OnStaffPosition sfp, OnsisCsvDataRecord record) {
            m_sfp = sfp;
            m_stf = (OnStaff) sfp.getStaff(broker);
            m_csvRecord = record;
            m_endDate = sfp.getEndDate();
        }

        /**
         * Instantiates a new educator assignment.
         *
         * @param record OnsisCsvDataRecord
         * @param stf SisStaff
         * @param endDate PlainDate
         */
        public EducatorAssignment(OnsisCsvDataRecord record, OnStaff stf, PlainDate endDate) {
            m_sfp = null;
            m_stf = stf;
            m_csvRecord = record;
            m_endDate = endDate;
        }

        /**
         * Instantiates a new educator assignment.
         *
         * @param broker X2Broker
         * @param sfp StaffPosition
         */
        public EducatorAssignment(X2Broker broker, OnStaffPosition sfp) {
            m_sfp = sfp;
            m_stf = (OnStaff) sfp.getStaff(broker);
            m_csvRecord = null;
            m_endDate = sfp.getEndDate();
        }

        /**
         * Gets the action code.
         *
         * @return String
         */
        public String getActionCode() {
            String result = null;
            if (m_sfp == null) {
                result = m_endDate == null ? OnsisRetrieverAction.ACTION_DELETE
                        : OnsisRetrieverAction.ACTION_UPDATE;
            } else {
                result = m_csvRecord == null ? OnsisRetrieverAction.ACTION_ADD : OnsisRetrieverAction.ACTION_UPDATE;
            }
            return result;
        }

        /**
         * Gets the csv record.
         *
         * @return Onsis csv data record
         */
        public OnsisCsvDataRecord getCsvRecord() {
            return m_csvRecord;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Gets the staff.
         *
         * @return Sis staff
         */
        public OnStaff getStaff() {
            return m_stf;
        }

        /**
         * Gets the staff position.
         *
         * @return Staff position
         */
        public OnStaffPosition getStaffPosition() {
            return m_sfp;
        }
    }

    /**
     * The Class OnsisSchoolEducatorAssignmentEntity.
     */
    public static class OnsisSchoolEducatorAssignmentEntity extends OnsisStateReportEntity {
        static final String FIELD_ACTION = "Action";
        static final String FIELD_ASSIGNMENT_END_DATE = "AssignmentEndDate";
        static final String FIELD_LEAVE_TYPE = "Leave type code";
        static final String FIELD_NEW_LEAVE_TYPE = "NewEducatorLeaveType";
        static final String FIELD_NEW_WITHDRAWAL_TYPE = "NwAssignmentWthdType";
        static final String FIELD_TEACHING_TYPE = "TeachingType";
        static final String FIELD_WITHDRAWAL_TYPE = "Withdrawal Type";

        static final String TEACHING_TYPE_NOT_APPLICABLE = "N/A";
        static final String TEACHING_TYPE_TEACHING = "T";
        static final List<String> TEACHING_TYPES_TEACHING_LEADING = Arrays.asList("B", "T");

        private static final String ELEMENT_ASSIGNED_SUBJECT = "ASSIGNED_SUBJECT";
        private static final String ELEMENT_ASSIGNMENT_WTHD_TYPE = "ASSIGNMENT_WTHD_TYPE";
        private static final String ELEMENT_CLASS_ASSIGNMENT = "CLASS_ASSIGNMENT";
        private static final String ELEMENT_CLASS_CODE = "CLASS_CODE";
        private static final String ELEMENT_CORE_FLAG = "CORE_FLAG";
        private static final String ELEMENT_EDUCATOR_LEAVE_TYPE = "EDUCATOR_LEAVE_TYPE";
        private static final String ELEMENT_MEN = "MEN";
        private static final String ELEMENT_POSITION_TYPE = "POSITION_TYPE";

        private static final String MTC_TEACHING_ROLE_PRIMARY = "Primary";
        private static final String MTC_TEACHING_ROLE_CO_TEACH = "Co-Teach";


        private static final String[] UPDATE_FIELDS = {OnsisRetrieverAction.ELEMENT_NAME_ACTION,
                ELEMENT_MEN, ELEMENT_POSITION_TYPE, ELEMENT_EDUCATOR_LEAVE_TYPE, ELEMENT_ASSIGNMENT_WTHD_TYPE,
                ELEMENT_CORE_FLAG,
                ELEMENT_ASSIGNED_SUBJECT, ELEMENT_CLASS_ASSIGNMENT};
        private static final Set<String> UPDATE_FIELD_SET = new HashSet<>(Arrays.asList(UPDATE_FIELDS));

        private static final String VALUE_ASSIGNMENT_WITHDRAWAL_TYPE_TRANSFER = "TRA";

        private List<EducatorAssignment> m_educatorAssignments = new ArrayList();
        private OnsisSchoolEducatorAssignment m_reportData;

        /**
         * Gets the action code.
         *
         * @return String
         */
        public String getActionCode() {
            return getEducatorAssignment().getActionCode();
        }

        /**
         * Gets the assignment end date.
         *
         * @return String
         */
        public String getAssignmentEndDate() {
            PlainDate endDate = getEducatorAssignment().getEndDate();
            return endDate == null || endDate.after(getGlobalData().getEndDate())
                    ? null
                    : OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.format(endDate);
        }

        /**
         * Gets the assignment start date.
         *
         * @return String
         */
        public String getAssignmentStartDate() {
            if (getEducatorAssignment().getCsvRecord() != null) {
                return getEducatorAssignment().getCsvRecord()
                        .getSingleFieldValue(OnsisExtractHelper.CsvField.ASSIGNMENT_START_DATE);
            }

            SubmissionType submissionType = getGlobalData().getSubmissionType();
            PlainDate startDate = getEducatorAssignment().getStaffPosition().getStartDate();

            if (startDate != null && startDate.after(getGlobalData().getEndDate())) {
                startDate = getGlobalData().getEndDate();
            }

            if (submissionType.isElementarySubmission() && submissionType.isOctoberSubmission()) {
                /*
                 * S-70867 - for OCTELEM - SFP_START_DATE should be reported as it is in Aspen
                 * but not earlier than November 1 of previous school year
                 */
                PlainDate dateNovember1 = getGlobalData().getDateNovember1PrevYear();
                if (startDate.before(dateNovember1)) {
                    startDate = dateNovember1;
                }
            } else {
                /*
                 * If this is an ADD, make sure assignment start date
                 * is advanced to start of reporting period.
                 *
                 * This is because the Ministry combines FTE in the CSV
                 * so we might be sending an UPDATE with Withdrawal for their old CSV record
                 * and an ADD(s) for the real SFP records
                 * that published last time and were totalled into the CSV record.
                 */
                if (OnsisRetrieverAction.ACTION_ADD.equals(getAction())
                        && startDate != null && startDate.before(getGlobalData().getStartDate())) {
                    startDate = getGlobalData().getStartDate();
                }
            }

            return startDate != null
                    ? OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.format(startDate)
                    : null;
        }

        /**
         * Gets the assignment withdrawal type.
         *
         * @return String
         */
        public String getAssignmentWithdrawalType() {
            String value = null;
            if (getEducatorAssignment().getCsvRecord() != null) {
                value = getEducatorAssignment().getCsvRecord()
                        .getSingleFieldValue(OnsisExtractHelper.CsvField.ASSIGNMENT_WITHDRAWAL_TYPE);
            } else if (!StringUtils.isEmpty(getFieldValue(FIELD_ASSIGNMENT_END_DATE))) {
                value = getDictionaryExtractor().getStateValue(getEducatorAssignment().getStaffPosition(),
                        OnStaffPosition.FIELD_WITHDRAWAL_TYPE);
            }
            return value;
        }

        /**
         * Gets the core flag.
         *
         * @return String
         */
        public String getCoreFlag() {
            OnStaffPosition sfp = getEducatorAssignment().getStaffPosition();
            if (sfp == null) {
                return "F";
            }

            return getCoreFlag(sfp);
        }

        /**
         * Gets the department code.
         *
         * @return String
         */
        public String getDepartmentCode() {
            OnStaffPosition sfp = getEducatorAssignment().getStaffPosition();
            return sfp == null ? null
                    : getDictionaryExtractor().getStateValue(sfp, OnStaffPosition.FIELD_DEPARTMENT);
        }

        /**
         * Gets the educator assignment.
         *
         * @return Educator assignment
         */
        public EducatorAssignment getEducatorAssignment() {
            return m_educatorAssignments.get(getCurrentRow());
        }

        /**
         * Gets the educator leave type.
         *
         * @return String
         */
        public String getEducatorLeaveType() {
            String value = null;

            String withdrawalType = getFieldValue(FIELD_WITHDRAWAL_TYPE);
            if (StringUtils.isEmpty(withdrawalType)) {
                if (getEducatorAssignment().getCsvRecord() != null) {
                    value = getEducatorAssignment().getCsvRecord()
                            .getSingleFieldValue(OnsisExtractHelper.CsvField.EDUCATOR_LEAVE_TYPE);
                } else {
                    value = getLeaveType(getEducatorAssignment().getStaffPosition());
                }
            }
            return value;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = "";
            ToolStaff staff = getEducatorAssignment().getStaff();
            if (staff != null) {
                name = staff.getNameView();
            }
            ToolStaffPosition staffPosition = getEducatorAssignment().getStaffPosition();
            if (staffPosition != null) {
                PlainDate startDate = staffPosition.getStartDate();
                PlainDate endDate = staffPosition.getEndDate();

                name = staffPosition.toString() + name + "[" + startDate + " " + endDate + "]";
            }

            return name;
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            BigDecimal result = null;
            if (getEducatorAssignment().getStaffPosition() == null) {
                result = new BigDecimal(
                        getEducatorAssignment().getCsvRecord().getSingleFieldValue(CsvField.EDUCATOR_FTE));
            } else {
                result = getFteSafe(getEducatorAssignment().getStaffPosition());
            }
            return result;
        }

        /**
         * Gets the gender type.
         *
         * @return String
         */
        public String getGenderType() {
            return getDictionaryExtractor().getStateValue(getStaff(), OnStaff.FIELD_GENDER_CODE);
        }

        /**
         * Gets the letter permission.
         *
         * @return String
         */
        public String getLetterPermission() {
            OnStaffPosition sfp = getEducatorAssignment().getStaffPosition();
            return sfp == null ? null : sfp.getLetterOfPermission();
        }

        /**
         * Gets the new assignment withdrawal type.
         *
         * @return String
         */
        public String getNewAssignmentWithdrawalType() {
            String value = null;
            if (getEducatorAssignment().getCsvRecord() != null
                    && !StringUtils.isEmpty(getFieldValue(FIELD_ASSIGNMENT_END_DATE))) {
                if (getEducatorAssignment().getStaffPosition() == null) {
                    value = VALUE_ASSIGNMENT_WITHDRAWAL_TYPE_TRANSFER;
                } else {
                    value = getDictionaryExtractor().getStateValue(getEducatorAssignment().getStaffPosition(),
                            OnStaffPosition.FIELD_WITHDRAWAL_TYPE);
                }
            }
            return value;
        }

        /**
         * Gets the new educator leave type.
         *
         * @return String
         */
        public String getNewEducatorLeaveType() {
            String withdrawalType = getFieldValue(FIELD_NEW_WITHDRAWAL_TYPE);
            if (StringUtils.isEmpty(withdrawalType) && getEducatorAssignment().getCsvRecord() != null
                    && getEducatorAssignment().getStaffPosition() != null) {
                return getLeaveType(getEducatorAssignment().getStaffPosition());
            }
            return null;
        }

        /**
         * Gets the NTIP status type.
         *
         * @return Boolean
         * @throws X2BaseException exception
         */
        public Boolean getNTIPStatusType() throws X2BaseException {
            OnStaffPosition sfp = getEducatorAssignment().getStaffPosition();
            return sfp == null ? Boolean.FALSE : Boolean.valueOf(sfp.getNTIPStatus());
        }

        /**
         * Gets the position type.
         *
         * @return String
         */
        public String getPositionType() {
            return getEducatorAssignment().getStaffPosition() == null
                    ? getEducatorAssignment().getCsvRecord()
                            .getSingleFieldValue(OnsisExtractHelper.CsvField.POSITION_TYPE_CODE)
                    : getEducatorAssignment().getStaffPosition().getPositionType();
        }

        /**
         * Gets the reportable positions.
         *
         * @return the reportable positions
         */
        public List<OnStaffPosition> getReportablePositions() {
            return m_educatorAssignments.stream().map(EducatorAssignment::getStaffPosition)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        }

        /**
         * Gets the staff.
         *
         * @return Sis staff
         */
        public OnStaff getStaff() {
            return getEducatorAssignment().getStaff();
        }

        /**
         * Gets the teaching type.
         *
         * @return String
         */
        public String getTeachingType() {
            OnStaffPosition sfp = null;
            String leaveType = getFieldValue(FIELD_LEAVE_TYPE);
            String withdrawalType = getFieldValue(FIELD_WITHDRAWAL_TYPE);
            String leaveTypeNew = getFieldValue(FIELD_NEW_LEAVE_TYPE);
            String withdrawalTypeNew = getFieldValue(FIELD_NEW_WITHDRAWAL_TYPE);
            if (StringUtils.isEmpty(leaveType) && StringUtils.isEmpty(withdrawalType)
                    && StringUtils.isEmpty(leaveTypeNew) && StringUtils.isEmpty(withdrawalTypeNew)) {
                sfp = getEducatorAssignment().getStaffPosition();
            }
            return sfp == null ? TEACHING_TYPE_NOT_APPLICABLE : getTeachingType(sfp);
        }

        /**
         * Gets the temporary letter approval.
         *
         * @return String
         */
        public String getTemporaryLetterApproval() {
            OnStaffPosition sfp = getEducatorAssignment().getStaffPosition();
            return sfp == null ? null : sfp.getTemporaryLetterOfApproval();
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean ToolBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity#intitialize(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_reportData = (OnsisSchoolEducatorAssignment) data;
            OnStaff staff = (OnStaff) bean;

            SubmissionType submissionType = getGlobalData().getSubmissionType();
            PlainDate periodStartDate = submissionType.getPeriodStartDate();
            PlainDate periodEndDate = submissionType.getPeriodEndDate();

            List<OnStaffPosition> staffPositionList = staff.getStaffPositions(getBroker()).stream()
                    .map(sfp -> (OnStaffPosition) sfp)
                    .filter(sfp -> !staff.getScheduleTeachers(getBroker()).isEmpty()
                            || m_reportData.getValidPositionTypeCodes().contains(sfp.getJobCode()))
                    .collect(Collectors.groupingBy(new Function<OnStaffPosition, String>() {

                        @Override
                        public String apply(OnStaffPosition sfp) {
                            return sfp.getPositionType() + StringUtils.emptyIfNull(sfp.getLeaveType());
                        }
                    })).values().stream()
                    .map(positions -> positions.stream()
                            .sorted(new Comparator<OnStaffPosition>() {
                                @Override
                                public int compare(OnStaffPosition o1, OnStaffPosition o2) {
                                    // reverse sort
                                    int cmp = -OnsisConstants.compareDates(o1.getStartDate(), o2.getStartDate(), true);
                                    if (cmp == 0) {
                                        cmp = -OnsisConstants.compareDates(o1.getEndDate(), o2.getEndDate(), false);
                                    }
                                    return cmp;
                                }
                            })
                            .findFirst()
                            .orElse(null))
                    .sorted(new Comparator<OnStaffPosition>() {
                        @Override
                        public int compare(OnStaffPosition sfp1, OnStaffPosition sfp2) {
                            // PositionType
                            int compareTo = OnsisHelper.compareTo(sfp1.getPositionType(),
                                    sfp2.getPositionType());
                            if (compareTo == 0) {
                                // earliest end date
                                compareTo = OnsisHelper.compareEndDates(sfp1.getEndDate(), sfp2.getEndDate());
                            }
                            if (compareTo == 0) {
                                // sort - without leave type first
                                int val1 = StringUtils.isEmpty(sfp1.getLeaveType()) ? 0 : 1;
                                int val2 = StringUtils.isEmpty(sfp2.getLeaveType()) ? 0 : 1;
                                compareTo = val1 - val2;
                            }
                            if (compareTo == 0) {
                                // reverse sort - high value first
                                compareTo = getFteSafe(sfp2).compareTo(getFteSafe(sfp1));
                            }
                            if (compareTo == 0) {
                                compareTo = OnsisHelper.compareTo(sfp1.getLeaveType(), sfp2.getLeaveType());
                            }
                            return compareTo;
                        }
                    })
                    .collect(Collectors.toList());
            m_reportData.log("latestSfpsForEachPosTypeFte: " + staffPositionList.toString());

            /*
             * Get all CSV records for the SKL-STF.
             * CSV records are keyed by (MEN + PosType + FTE)
             */
            String sklNumber = deepGetFieldValueByFieldName(OnsisSchoolData.FIELD_SCHOOL_NUMBER);
            String men = staff.getMEN();
            Collection<OnsisCsvDataRecord> csvRecordsForStaff = getCsvRecords(sklNumber, men);
            m_reportData.log("csvRecordsForStaff(" + sklNumber + "," + men + "): "
                    + (csvRecordsForStaff == null ? "null" : csvRecordsForStaff.toString()));

            /*
             * Analyze each CSV record to build an EducatorAssignment.
             * If matched to a latestSfp, remove from latestSfps.
             */
            if (csvRecordsForStaff != null && !csvRecordsForStaff.isEmpty()) {
                for (OnsisCsvDataRecord record : csvRecordsForStaff) {
                    /*
                     * Find latestSfp "continuation" match
                     * where this CSV finds first latestSfp with:
                     * - sfpPos==csvPos
                     * - sfpFte >= csvFte
                     * - sfpEndDate in future (or never)
                     */
                    String csvPosition = record.getSingleFieldValue(CsvField.POSITION_TYPE_CODE);
                    BigDecimal csvFte = new BigDecimal(record.getSingleFieldValue(CsvField.EDUCATOR_FTE));
                    Optional<OnStaffPosition> sfpContinuation = staffPositionList.stream()
                            .filter(sfp -> csvPosition.equals(sfp.getPositionType()) && isStaffPositionValid(sfp))
                            .findFirst();
                    if (sfpContinuation.isPresent()) {
                        m_reportData.log("m_educatorAssignments.add1: " + sfpContinuation.get() + "|" + record);
                        m_educatorAssignments
                                .add(new EducatorAssignment(getBroker(), sfpContinuation.get(), record));
                        staffPositionList.remove(sfpContinuation.get());
                        continue;
                    }

                    /*
                     * Else find latestSfp best match (non-continuation)
                     * where this CSV finds a latestSfp with:
                     * - sfpPos==csvPos
                     *
                     * Selecting the SFP by priority order:
                     * 1. Prefer SFP1 if sfpFte1 >= csvFte and sfpFte2 < csvFte)
                     * 2. Next prefer SFP with earliest end date
                     * 3. Next prefer SFP with highest FTE
                     * 4. Next prefer SFP without a Leave Type
                     * 5. Finally prefer oldest SfpOid for consistency
                     */
                    Optional<OnStaffPosition> sfpBestMatch = staffPositionList.stream()
                            .filter(sfp -> csvPosition
                                    .equals(sfp.getPositionType()))
                            .sorted(new Comparator<OnStaffPosition>() {
                                @Override
                                public int compare(OnStaffPosition sfp1, OnStaffPosition sfp2) {
                                    int compareTo = 0;

                                    // 1. Prefer SFP1 if sfpFte1 >= csvFte and sfpFte2 < csvFte)
                                    if (csvFte.compareTo(getFteSafe(sfp1)) <= 0) {
                                        if (csvFte.compareTo(getFteSafe(sfp2)) > 0) {
                                            compareTo = -1;
                                        }
                                    } else if (csvFte.compareTo(getFteSafe(sfp2)) <= 0) {
                                        compareTo = 1;
                                    }

                                    // 2. Next prefer SFP with earliest end date
                                    if (compareTo == 0) {
                                        compareTo = OnsisHelper.compareEndDates(sfp1.getEndDate(), sfp2.getEndDate());
                                    }

                                    // 3. Next prefer SFP with highest FTE
                                    if (compareTo == 0) {
                                        // reverse sort - high value first
                                        compareTo = getFteSafe(sfp2).compareTo(getFteSafe(sfp1));
                                    }

                                    // 4. Next prefer SFP without a Leave Type
                                    if (compareTo == 0) {
                                        String leaveType1 = sfp1.getLeaveType();
                                        String leaveType2 = sfp2.getLeaveType();
                                        compareTo = OnsisHelper.compareTo(leaveType1, leaveType2);
                                    }

                                    // 5. Finally prefer oldest SfpOid for consistency
                                    if (compareTo == 0) {
                                        compareTo = sfp1.getOid().compareTo(sfp2.getOid());
                                    }
                                    return compareTo;
                                }
                            })
                            .findFirst();
                    if (sfpBestMatch.isPresent()) {
                        if (sfpBestMatch.get().getEndDate() == null
                                || sfpBestMatch.get().getEndDate().after(periodEndDate)
                                || isStaffPositionValid(sfpBestMatch.get())) {
                            PlainDate endDate = sfpBestMatch.get().getStartDate().after(getGlobalData().getStartDate())
                                    ? sfpBestMatch.get().getStartDate()
                                    : getGlobalData().getStartDate();
                            m_reportData.log("m_educatorAssignments.add2: " + staff + "|" + endDate);
                            m_educatorAssignments
                                    .add(new EducatorAssignment(record, staff, endDate));
                        } else {
                            m_reportData.log("m_educatorAssignments.add3: " + sfpBestMatch.get() + "|" + record);
                            m_educatorAssignments
                                    .add(new EducatorAssignment(getBroker(), sfpBestMatch.get(), record));
                            staffPositionList.remove(sfpBestMatch.get());
                        }
                    } else {
                        m_reportData.log("m_educatorAssignments.add4: " + staff + "|" + record);
                        m_educatorAssignments.add(new EducatorAssignment(record, staff, null));
                    }

                } // for each CSV record
            } // if CSV records for Staff

            /*
             * Build EducatorAssignments for remaining latestSfps that didn't match CSV
             * and extend beyond the report end date
             */
            staffPositionList.stream()
                    .filter(sfp -> sfp.getEndDate() == null || sfp.getEndDate().after(periodEndDate) ||
                            (sfp.getStartDate() != null && !sfp.getStartDate().before(periodStartDate)))
                    .forEach(sfp -> {
                        m_reportData.log("m_educatorAssignments.add5: " + sfp);
                        m_educatorAssignments.add(new EducatorAssignment(getBroker(), sfp));
                    });

            // Sort by MEN, descending Start/End date, and SFP Oid for consistent XML
            Collections.sort(m_educatorAssignments, new Comparator<EducatorAssignment>() {
                @Override
                public int compare(EducatorAssignment o1, EducatorAssignment o2) {
                    int compareTo = OnsisHelper.compareTo(o1.getStaff().getMEN(), o2.getStaff().getMEN());
                    if (compareTo == 0) {
                        PlainDate startDate1 =
                                (o1.getStaffPosition() == null) ? null : o1.getStaffPosition().getStartDate();
                        PlainDate startDate2 =
                                (o2.getStaffPosition() == null) ? null : o2.getStaffPosition().getStartDate();
                        compareTo = -OnsisHelper.compareStartDates(startDate1, startDate2);
                    }
                    if (compareTo == 0) {
                        PlainDate endDate1 =
                                (o1.getStaffPosition() == null) ? null : o1.getStaffPosition().getEndDate();
                        PlainDate endDate2 =
                                (o2.getStaffPosition() == null) ? null : o2.getStaffPosition().getEndDate();
                        compareTo = -OnsisHelper.compareEndDates(endDate1, endDate2);
                    }
                    if (compareTo == 0) {
                        String sfpOid1 = (o1.getStaffPosition() == null) ? "" : o1.getStaffPosition().getOid();
                        String sfpOid2 = (o2.getStaffPosition() == null) ? "" : o2.getStaffPosition().getOid();
                        compareTo = OnsisHelper.compareTo(sfpOid1, sfpOid2);
                    }
                    return compareTo;
                }
            });
            setRowCount(m_educatorAssignments.size());
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#afterRenderRowFields(org.w3c.dom.Element)
         */
        @Override
        protected void afterRenderRowFields(Element entityElement) {
            super.afterRenderRowFields(entityElement);

            String actionCode = getActionCode();
            String csvTeachingType = getEducatorAssignment().getCsvRecord() == null ? ""
                    : getEducatorAssignment().getCsvRecord()
                            .getSingleFieldValue(OnsisExtractHelper.CsvField.TEACHING_TYPE);
            OnStaffPosition sfp = getEducatorAssignment().getStaffPosition();
            String teachingType = sfp == null ? TEACHING_TYPE_NOT_APPLICABLE
                    : getDictionaryExtractor().getStateValue(sfp, OnStaffPosition.FIELD_TEACHING_TYPE);

            m_reportData.log("afterRenderRowFields for " +
                    "SCHOOL_EDUCATOR_ASSIGNMENT: POSITION_TYPE = "
                    + OnsisStateReportData.getChildText("POSITION_TYPE", entityElement) +
                    ", ASSIGNMENT_START_DATE = "
                    + OnsisStateReportData.getChildText("ASSIGNMENT_START_DATE", entityElement) +
                    ", FTE = " + OnsisStateReportData.getChildText("FTE", entityElement) +
                    "\nOnStaffPosition = " + sfp +
                    "\nactionCode = " + actionCode +
                    ", csvTeachingType = " + csvTeachingType +
                    ", teachingType = " + teachingType);
            /*
             * S-56334: Don't send <CLASS_ASSIGNMENT> that have Action=Update.
             *
             * Updates are removed here by the parent
             * after call to ClassAssignment.generateDeletes()
             * rather than in ClassAssignment.intitialize()
             * so that Deletes won't be generated
             * for ClassAssignments that were Updates.
             */
            List<Element> classAssignmentUpdates = getElementsWithChildValue(ELEMENT_CLASS_ASSIGNMENT, entityElement,
                    OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_UPDATE);
            for (Element classAssignmentUpdate : classAssignmentUpdates) {
                m_reportData.log("remove update " + ELEMENT_CLASS_ASSIGNMENT + " with " + ELEMENT_CLASS_CODE + " = "
                        + OnsisStateReportData.getChildText(ELEMENT_CLASS_CODE, classAssignmentUpdate));
                entityElement.removeChild(classAssignmentUpdate);
            }

            /*
             * We cannot have delete <CLASS_ASSIGNMENT> when the parent has ACTION = ADD
             */
            if (OnsisRetrieverAction.ACTION_ADD.equals(actionCode)) {
                List<Element> classAssignmentDeletes =
                        getElementsWithChildValue(ELEMENT_CLASS_ASSIGNMENT, entityElement,
                                OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_DELETE);
                for (Element classAssignmentDelete : classAssignmentDeletes) {
                    m_reportData.log("remove deletes from parent add " + ELEMENT_CLASS_ASSIGNMENT + " with "
                            + ELEMENT_CLASS_CODE + " = "
                            + OnsisStateReportData.getChildText(ELEMENT_CLASS_CODE, classAssignmentDelete));
                    entityElement.removeChild(classAssignmentDelete);
                }
            }

            /*
             * Don't send <CLASS_ASSIGNMENT>
             * (including Action=Deletes)
             * if staff position eaching type is N/A
             *
             * Elements are removed here by the parent
             * after call to ClassAssignment.generateDeletes().
             */
            if (TEACHING_TYPE_NOT_APPLICABLE.equals(teachingType)) {
                List<Element> classAssignments = getChildElements(ELEMENT_CLASS_ASSIGNMENT, entityElement);
                for (Element classAssignment : classAssignments) {
                    m_reportData.log("test NA rule: actionCode = " + actionCode + ", childAction = "
                            + getChildText(OnsisRetrieverAction.ELEMENT_NAME_ACTION, classAssignment)
                            + ", csvTeachingType = " + csvTeachingType + " for " + ELEMENT_CLASS_ASSIGNMENT + " with "
                            + ELEMENT_CLASS_CODE + " = "
                            + OnsisStateReportData.getChildText(ELEMENT_CLASS_CODE, classAssignment));
                    if (!OnsisRetrieverAction.ACTION_UPDATE.equals(actionCode) ||
                            !OnsisRetrieverAction.ACTION_DELETE
                                    .equals(getChildText(OnsisRetrieverAction.ELEMENT_NAME_ACTION, classAssignment))
                            ||
                            !TEACHING_TYPES_TEACHING_LEADING.contains(csvTeachingType)) {
                        m_reportData.log("remove NA rule based " + ELEMENT_CLASS_ASSIGNMENT + " with "
                                + ELEMENT_CLASS_CODE + " = "
                                + OnsisStateReportData.getChildText(ELEMENT_CLASS_CODE, classAssignment));
                        entityElement.removeChild(classAssignment);
                    }
                }

                List<Element> assignedSubjects = getChildElements(ELEMENT_ASSIGNED_SUBJECT, entityElement);
                for (Element assignedSubject : assignedSubjects) {
                    entityElement.removeChild(assignedSubject);
                }
            }

            /*
             * Don't send <ASSIGNED_SUBJECT>
             * (including Action=Deletes)
             * if parent Educator's TEACHING_TYPE=N/A
             *
             * Elements are removed here by the parent
             * after call to ClassAssignment.generateDeletes().
             */
            if (TEACHING_TYPE_NOT_APPLICABLE.equals(getFieldValue(FIELD_TEACHING_TYPE))) {
                List<Element> assignedSubjects = getChildElements(ELEMENT_ASSIGNED_SUBJECT, entityElement);
                for (Element assignedSubject : assignedSubjects) {
                    entityElement.removeChild(assignedSubject);
                }
            }
            /*
             * S-58712: Support partial update to educator assignment
             */
            if (!getGlobalData().getSubmissionType().doValidateSeaAddUpdate()) {
                List<Element> allChildElements = getChildElements("*", entityElement);
                for (Element allChildElement : allChildElements) {
                    if (!UPDATE_FIELD_SET.contains(allChildElement.getTagName())) {
                        entityElement.removeChild(allChildElement);
                    }
                }
            }
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.w3c.dom.Element,
         *      org.w3c.dom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            if (!getGlobalData().getSubmissionType().doValidateSeaAddUpdate()
                    && OnsisRetrieverAction.ACTION_DELETE.equals(getActionCode())) {
                return true;
            }
            if (!getGlobalData().getSubmissionType().doValidateSeaAddUpdate()
                    && OnsisRetrieverAction.ACTION_ADD.equals(getActionCode())) {
                return true;
            }
            if (getEducatorAssignment().getCsvRecord() != null) {
                return false;
            }

            if (!getGlobalData().getSubmissionType().doValidateSeaAddUpdate()) {
                boolean isEmpty = true;
                List<Element> classAssignments = getChildElements(ELEMENT_CLASS_ASSIGNMENT, entityElement);
                if (classAssignments.size() > 0) {
                    isEmpty = false;
                }

                if (isEmpty) {
                    List<Element> assignedSubjects = getChildElements(ELEMENT_ASSIGNED_SUBJECT, entityElement);
                    if (assignedSubjects.size() > 0) {
                        isEmpty = false;
                    }
                }

                if (isEmpty) {
                    return true;
                }
            }

            return false;
        }

        /**
         * Checks if is teacher.
         *
         * @param sfp StaffPosition
         * @return true, if is teacher
         */
        boolean isEarlyChildhoodEducator(OnStaffPosition sfp) {
            if (sfp == null) {
                return false;
            }

            return m_reportData.getEcePositionTypeCodes().contains(sfp.getJobCode());
        }

        /**
         * Checks if is teacher.
         *
         * @param sfp StaffPosition
         * @return true, if is teacher
         */
        boolean isTeacher(OnStaffPosition sfp) {
            return sfp == null ? false : sfp.isTeacher();
        }

        /**
         * Gets the action.
         *
         * @return String
         */
        private String getAction() {
            return getFieldValue(FIELD_ACTION);
        }

        /**
         * Gets the core flag.
         *
         * @param sfp StaffPosition
         * @return String
         */
        private String getCoreFlag(OnStaffPosition sfp) {
            SubmissionSchoolType schoolType = getGlobalData().getSchoolType();

            /*
             * Must be isTeacher
             */
            boolean isCore = isTeacher(sfp);
            isCore &= !isEarlyChildhoodEducator(sfp);

            /*
             * SFP must still be active
             */
            isCore &= (sfp.getEndDate() == null || !sfp.getEndDate().before(getGlobalData().getEndDate()));

            /*
             * SFP must not be a future assignment
             */
            isCore &= (sfp.getStartDate() == null || !sfp.getStartDate().after(getGlobalData().getEndDate()));

            /*
             * S-70559 Teaching type should not be "N/A"
             */
            isCore &= !getTeachingType(sfp).equals(TEACHING_TYPE_NOT_APPLICABLE);

            /*
             * Must have at least one section of interest
             * (filtered by CourseCodeType=Homeroom for elementary, else all sections)
             * for which teacher isPrimary
             */
            if (isCore) {
                isCore &= sfp.getStaff(getBroker()).getScheduleTeachers(getBroker()).stream()
                        .map(mtc -> (OnsisScheduleTeacher) mtc)
                        .filter(mtc -> mtc.isCoreSchedule(getBroker(), getGlobalData().getEndDate(),
                                CoreScheduledMode.SCHEDULED_ON_DATE))
                        .map(mtc -> mtc.getSection(getBroker()))
                        .map(mst -> (OnSection) mst)
                        .anyMatch(section -> section == null ? false : schoolType.isClassAssignment(section));
            }

            /*
             * has class assignments
             * Commented out because redundant with sectionsOfInterest
             */
            // isCore &= schoolType.getClassAssignments(OnsisStateReportData.this, staffOid).count()
            // > 0;

            return isCore ? "T" : "F";
        }

        /**
         * Gets the csv record.
         *
         * @param sklNumber String
         * @param men String
         * @return Onsis csv data record
         */
        private Collection<OnsisCsvDataRecord> getCsvRecords(String sklNumber, String men) {
            return m_reportData.getMatcher().findRecords(
                    Arrays.asList(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                            OnsisExtractHelper.CsvField.MEN.toString()),
                    Arrays.asList(sklNumber, men));
        }

        /**
         * Gets the fte safe.
         *
         * @param sfp OnsisStaffPosition
         * @return Big decimal
         */
        private BigDecimal getFteSafe(OnStaffPosition sfp) {
            BigDecimal fte = null;

            try {
                fte = sfp.getFte();
            } catch (Exception ex) {
                //
            }

            if (fte == null) {
                return BigDecimal.valueOf(0.0d);
            }

            return fte;
        }

        /**
         * Gets the leave type.
         *
         * @param sfp StaffPosition
         * @return String
         */
        private String getLeaveType(OnStaffPosition sfp) {
            String result = null;
            Date leaveStartDate = sfp.getLeaveStartDate();
            Date leaveEndDate = sfp.getLeaveEndDate();
            if (leaveEndDate != null && leaveStartDate != null && leaveEndDate.before(leaveStartDate)) {
                throw new ValidationException() {

                    @Override
                    public ValidationErrorType getErrorType() {
                        return ValidationErrorType.CRITICAL_ERROR;
                    }

                    @Override
                    public String getGroupName() {
                        return "Staff Position Data Error";
                    }

                    @Override
                    public String getMessageCode() {
                        return "Staff Position Data Error";
                    }

                    @Override
                    public String getMessageEn() {
                        return getCommonMessage();
                    }

                    @Override
                    public String getMessageFr() {
                        return getCommonMessage();
                    }

                    private String getCommonMessage() {
                        return "Staff Position Leave Start Date [" + leaveStartDate
                                + "] is before Staff Position Leave End Date [" + leaveEndDate + "]";
                    }
                };
            }
            Range<Date> dateRange = Range.of(leaveStartDate, leaveEndDate);
            if (leaveStartDate != null && dateRange.contains(getGlobalData().getEndDate())) {
                result = getDictionaryExtractor().getStateValue(sfp, OnStaffPosition.FIELD_LEAVE_TYPE);
            }

            return result;
        }

        /**
         * Gets the teaching type.
         *
         * @param sfp StaffPosition
         * @return String
         */
        private String getTeachingType(OnStaffPosition sfp) {
            if (!StringUtils.isEmpty(getLeaveType(sfp)) ||
                    (sfp.getEndDate() != null && !sfp.getEndDate().after(getGlobalData().getEndDate()) &&
                            (sfp.getStartDate() == null
                                    || sfp.getStartDate().before(getGlobalData().getStartDate()))
                            &&
                            !StringUtils.isEmpty(getDictionaryExtractor().getStateValue(sfp,
                                    OnStaffPosition.FIELD_WITHDRAWAL_TYPE)))) {
                return TEACHING_TYPE_NOT_APPLICABLE; // MSED061
            }
            return getDictionaryExtractor().getStateValue(sfp, OnStaffPosition.FIELD_TEACHING_TYPE);
        }

        /**
         * Checks if is staff position is valid for reporting.
         *
         * @param sfp StaffPosition
         * @return true, if valid
         */
        private boolean isStaffPositionValid(OnStaffPosition sfp) {
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            PlainDate periodStartDate = submissionType.getPeriodStartDate();
            PlainDate sfpEndDate = sfp.getEndDate();
            boolean isValid = sfpEndDate == null || !sfpEndDate.before(periodStartDate);
            if (!isValid && submissionType.isElementarySubmission()) {
                isValid = isValid || (!sfpEndDate.before(getGlobalData().getDateNovember1PrevYear())
                        && !sfpEndDate.after(periodStartDate));
            }
            return isValid;
        }

    }

    static private final List<String> VALID_ECE_POSITION_TYPE_CODES = Arrays.asList("ECE", "LCE");

    private List<String> m_ecePositionTypeCodes;
    private List<String> m_validPositionTypeCodes;


    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(ToolBean.getCachedToolBeans(OnStaff.class));
    }

    /**
     * Gets the Early Childhood Educator position type codes.
     *
     * @return List
     */
    public List<String> getEcePositionTypeCodes() {
        if (m_ecePositionTypeCodes == null) {
            DataDictionaryField positionTypeField = ToolStaffPosition.FIELD_JOB_CODE.getField(getDictionaryExtractor());
            m_ecePositionTypeCodes = getDictionaryExtractor().getRefCodesWithStateValue(positionTypeField).stream()
                    .filter(code -> VALID_ECE_POSITION_TYPE_CODES.contains(code.getStateCode()))
                    .map(code -> code.getCode()).collect(Collectors.toList());
        }
        return m_ecePositionTypeCodes;
    }

    /**
     * Gets the valid position type codes.
     *
     * @return List
     */
    public List<String> getValidPositionTypeCodes() {
        if (m_validPositionTypeCodes == null) {
            DataDictionaryField positionTypeField = ToolStaffPosition.FIELD_JOB_CODE.getField(getDictionaryExtractor());
            m_validPositionTypeCodes = getDictionaryExtractor().getRefCodesWithStateValue(positionTypeField).stream()
                    .map(code -> code.getCode()).collect(Collectors.toList());
        }
        return m_validPositionTypeCodes;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSchoolEducatorAssignmentEntity.class);
    }

    /**
     * Generate deletes.
     *
     * @param parentElement Element
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateDeletes(org.w3c.dom.Element)
     */
    @Override
    protected void generateDeletes(Element parentElement) {
        /*
         * Educator Assigments don't generate deletes.
         * See getActionCode
         */
        return;
    }
}
