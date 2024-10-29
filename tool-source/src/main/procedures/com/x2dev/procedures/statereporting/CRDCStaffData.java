/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.CRDCStaffData.StaffCRDCEntity.CRDCStaffPosition;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data set for Staff data to be used in final CRDC export.
 *
 * @author X2 Development Corporation
 */
public class CRDCStaffData extends CRDCReportData {
    /**
     * Helper class to get all needed information for staf.
     *
     * @author X2 Development Corporation
     */
    public static class CRDCStaffHelper {

        protected static final String ALIAS_MA_SFP_JOB_CLASS = "WA07-SFP";
        protected static final String ALIAS_MA_SFP_PRIM_ASSIGN = "SIF SFP Primary Assignment";
        private static final Set<String> SFC_CERTIFICATION_CODES = new HashSet(Arrays.asList("Certification"));
        private static final int STAFF_DEFAULT_SIZE = 100;

        /**
         * An X2Broker for performing initialization and the data query.
         */
        private CRDCStaffData m_data = null;
        private DistrictSchoolYearContext m_districtContext;
        private Set<PlainDate> m_insessionDates;
        private PlainDate m_snapshotDate;
        private Criteria m_staffCriteria;

        /**
         * Maps
         */
        private Map<String, Map<String, List<StaffAttendance>>> m_staffAbsentMap = null;
        private Map<String, Integer> m_staffInsessionCountMap = new HashMap();
        private Map<String, List<StaffCertification>> m_staffLicenseMap = null;
        private Map<String, List<StaffPosition>> m_staffPositionMap = null;
        private Map<String, List<ScheduleTeacher>> m_staffScheduleMap = null;

        /**
         * Constructor.
         *
         * @param data CRDCReportData
         * @param staffCriteria Criteria
         * @param context DistrictSchoolYearContext
         * @param snapshotDate PlainDate
         */
        public CRDCStaffHelper(CRDCStaffData data, Criteria staffCriteria, DistrictSchoolYearContext context,
                PlainDate snapshotDate) {
            super();
            m_data = data;
            m_districtContext = context;
            m_staffCriteria = staffCriteria;
            m_snapshotDate = snapshotDate;
        }

        /**
         * Returns map of staff's absences keyed on staff Oid.
         *
         * @param staffOid String
         * @return Map
         */
        public Map<String, List<StaffAttendance>> getStaffAbsent(String staffOid) {
            Map<String, List<StaffAttendance>> mapStaffAttendance = getStaffAbsentMap().get(staffOid);
            if (mapStaffAttendance == null) {
                mapStaffAttendance = new HashMap<String, List<StaffAttendance>>();
            }
            return mapStaffAttendance;
        }

        /**
         * Gets the staff fte based on hire date and exit date
         *
         * @param staff SisStaff
         * @return double
         */
        public double getStaffFte(SisStaff staff) {
            double fte = 1.0;
            PlainDate beginDate = staff.getHireDate();
            PlainDate endDate = m_data.getStaffExitDate(staff);
            if ((beginDate != null && beginDate.after(getCurrentContext().getStartDate())) ||
                    (endDate != null && endDate.before(getCurrentContext().getEndDate()))) {
                int cnt = 0;
                Set<PlainDate> dates = getInsessionDates();
                for (PlainDate date : dates) {
                    if (beginDate != null && !date.before(beginDate) && (endDate == null || !date.after(endDate))) {
                        ++cnt;
                    }
                }
                fte = (double) cnt / (double) dates.size();
            }
            return fte;
        }

        /**
         * Returns staff's certifications.
         *
         * @param staffOid String
         * @return List
         */
        public List<StaffCertification> getStaffLicense(String staffOid) {
            List<StaffCertification> licenses = getStaffLicenseMap().get(staffOid);
            if (licenses == null) {
                licenses = new ArrayList<StaffCertification>();
            }
            return licenses;
        }

        /**
         * Gets the staff position fte.
         *
         * @param sfp StaffPosition
         * @return double
         */
        public double getStaffPositionFte(StaffPosition sfp) {
            double fte = 0;
            Set<PlainDate> dates = getInsessionDates();
            int dayCount = getStaffPositionInSessionCount(sfp);
            if (sfp.getFte() != null) {
                fte = sfp.getFte().doubleValue();
                if (dates.size() > 0) {
                    fte *= (double) dayCount / (double) dates.size();
                }
            }
            return fte;
        }

        /**
         * Gets the staff position in session count.
         *
         * @param sfp StaffPosition
         * @return int
         */
        public int getStaffPositionInSessionCount(StaffPosition sfp) {
            int cnt = 0;
            if (m_staffInsessionCountMap.containsKey(sfp.getOid())) {
                cnt = m_staffInsessionCountMap.get(sfp.getOid()).intValue();
            } else {
                PlainDate endDate = m_data.getStaffExitDate(sfp.getStaff());
                if (endDate == null ||
                        (sfp.getEndDate() != null && sfp.getEndDate().before(endDate))) {
                    endDate = sfp.getEndDate();
                }
                Set<PlainDate> dates = getInsessionDates();
                for (PlainDate date : dates) {
                    if (sfp.getStartDate() != null &&
                            !date.before(sfp.getStartDate()) &&
                            (endDate == null || !date.after(endDate))) {
                        ++cnt;
                    }
                }
                m_staffInsessionCountMap.put(sfp.getOid(), Integer.valueOf(cnt));
            }
            return cnt;

        }

        /**
         * Returns staff's positions.
         *
         * @param staffOid String
         * @return List
         */
        public List<StaffPosition> getStaffPositions(String staffOid) {
            List<StaffPosition> listOfStaffPostion = getStaffPositionMap().get(staffOid);
            if (listOfStaffPostion == null) {
                listOfStaffPostion = new ArrayList();
                getStaffPositionMap().put(staffOid, listOfStaffPostion);
            }
            return listOfStaffPostion;
        }

        /**
         * Returns staff's schedules.
         *
         * @param staffOid String
         * @return List
         */
        public List<ScheduleTeacher> getStaffSchedules(String staffOid) {
            List<ScheduleTeacher> listOfStaffPostion = getStaffScheduleMap().get(staffOid);
            if (listOfStaffPostion == null) {
                listOfStaffPostion = new ArrayList();
                getStaffScheduleMap().put(staffOid, listOfStaffPostion);
            }
            return listOfStaffPostion;
        }

        /**
         * Checks if is staff attendance used.
         *
         * @return true, if is staff attendance used
         */
        public boolean isStaffAttendanceUsed() {
            return !getStaffAbsentMap().isEmpty();
        }

        /**
         * Populate common map of maps of staffs' absences keyed on staff oid and school oid.
         */
        private void calculateAbsent() {
            Map<String, RefAttendanceStaff> mapRefAttendanceStaff =
                    getBroker().getMapByQuery(new QueryByCriteria(RefAttendanceStaff.class),
                            RefAttendanceStaff.COL_ATTENDANCE_CODE, 16);

            DistrictSchoolYearContext currentCtx = getCurrentContext();
            Criteria absentCriteria = new Criteria();
            absentCriteria.addGreaterOrEqualThan(StaffAttendance.COL_DATE, currentCtx.getStartDate());
            absentCriteria.addLessOrEqualThan(StaffAttendance.COL_DATE, currentCtx.getEndDate());
            absentCriteria.addIn(StaffAttendance.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_staffCriteria));


            QueryByCriteria byCriteria = new QueryByCriteria(StaffAttendance.class, absentCriteria);
            String[] columns = new String[] {StaffAttendance.COL_STAFF_OID,
                    StaffAttendance.COL_SCHOOL_OID};
            int[] sizes = new int[] {STAFF_DEFAULT_SIZE, 10};
            m_staffAbsentMap = getBroker().getGroupedCollectionByQuery(byCriteria, columns, sizes);
            if (!m_staffAbsentMap.isEmpty()) {
                for (Map<String, List<StaffAttendance>> value : m_staffAbsentMap.values()) {
                    for (List<StaffAttendance> sfaList : value.values()) {
                        Iterator<StaffAttendance> iter = sfaList.iterator();
                        while (iter.hasNext()) {
                            StaffAttendance sfa = iter.next();
                            RefAttendanceStaff ras = sfa.getReferenceAttendance();
                            if (ras == null) {
                                ras = mapRefAttendanceStaff.get(sfa.getCode());
                            }
                            if (ras == null || !ras.getAbsentIndicator()) {
                                iter.remove();
                            }
                        }
                    }
                }
            }
        }

        /**
         * Populate map of staffs' licenses keyed on staff oid.
         */
        private void calculateLicense() {
            Set<String> licenseTypes =
                    m_data.getCodesForCRDCValue(StaffCertification.class, StaffCertification.COL_TYPE,
                            SFC_CERTIFICATION_CODES);
            Criteria licenseCriteria = new Criteria();
            licenseCriteria.addIn(StaffCertification.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_staffCriteria));

            Criteria orExpDateFirstCriteria = new Criteria();
            orExpDateFirstCriteria.addGreaterOrEqualThan(StaffCertification.COL_EXPIRATION_DATE, getSnapshotDate());
            Criteria orExpDateSecondCriteria = new Criteria();
            orExpDateSecondCriteria.addIsNull(StaffCertification.COL_EXPIRATION_DATE);
            orExpDateFirstCriteria.addOrCriteria(orExpDateSecondCriteria);
            licenseCriteria.addAndCriteria(orExpDateFirstCriteria);

            Criteria orIssDateFirstCriteria = new Criteria();
            orIssDateFirstCriteria.addLessOrEqualThan(StaffCertification.COL_ISSUE_DATE, getSnapshotDate());
            Criteria orIssDateSecondCriteria = new Criteria();
            orIssDateSecondCriteria.addIsNull(StaffCertification.COL_ISSUE_DATE);
            orIssDateFirstCriteria.addOrCriteria(orIssDateSecondCriteria);
            licenseCriteria.addAndCriteria(orIssDateFirstCriteria);

            licenseCriteria.addIn(StaffCertification.COL_TYPE, licenseTypes);
            QueryByCriteria byCriteria = new QueryByCriteria(StaffCertification.class, licenseCriteria);
            m_staffLicenseMap = getBroker().getGroupedCollectionByQuery(byCriteria, StaffCertification.COL_STAFF_OID,
                    STAFF_DEFAULT_SIZE);
        }

        /**
         * Populate map of staffs' positions keyed on staff oid.
         */
        private void calculateStaffPosition() {
            DistrictSchoolYearContext currentCtx = getCurrentContext();
            Criteria positionCriteria = new Criteria();
            positionCriteria.addIn(StaffPosition.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_staffCriteria));
            Criteria orCriteriaFC = new Criteria();
            orCriteriaFC.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, currentCtx.getStartDate());
            Criteria orCriteriaSC = new Criteria();
            orCriteriaSC.addIsNull(StaffPosition.COL_END_DATE);
            orCriteriaFC.addOrCriteria(orCriteriaSC);
            positionCriteria.addAndCriteria(orCriteriaFC);
            positionCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, currentCtx.getEndDate());
            DataDictionaryField maJobClassDDField =
                    m_data.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MA_SFP_JOB_CLASS);
            if (maJobClassDDField != null
                    && StaffPosition.DATABASE_NAME.equals(maJobClassDDField.getDataTable().getDatabaseName())
                    && maJobClassDDField.hasReferenceTable()) {

                Map<String, ReferenceCode> codesMap = maJobClassDDField.getReferenceTable().getCodeMap();
                Set<String> codesToCriteria = new HashSet<>();
                for (String code : codesMap.keySet()) {
                    if (codesMap.get(code).getFieldValueByBeanPath(m_data.getFieldCRDCCode()) != null) {
                        codesToCriteria.add(code);
                    }
                }
                if (codesToCriteria.isEmpty()) {
                    m_data.addSetupError("Configuration Error",
                            "No CRDC codes found for Staff Position Field [WA07-SFP]");
                    m_staffPositionMap = new HashMap();
                    return;
                }
                positionCriteria.addIn(maJobClassDDField.getJavaName(), codesToCriteria);
            }
            QueryByCriteria byCriteria = new QueryByCriteria(StaffPosition.class, positionCriteria);
            m_staffPositionMap =
                    getBroker().getGroupedCollectionByQuery(byCriteria, StaffPosition.COL_STAFF_OID,
                            STAFF_DEFAULT_SIZE);
        }

        /**
         * Populate map of staffs' schedules keyed on staff oid.
         */
        private void calculateStaffSchedule() {
            X2Criteria mtcCriteria = new X2Criteria();
            mtcCriteria.addIn(ScheduleTeacher.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_staffCriteria));
            mtcCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());

            mtcCriteria.addLessOrEqualThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_START_DATE, getSnapshotDate());

            mtcCriteria.addGreaterOrEqualThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_END_DATE, getSnapshotDate());

            QueryByCriteria byCriteria = new QueryByCriteria(ScheduleTeacher.class, mtcCriteria);
            m_staffScheduleMap =
                    getBroker().getGroupedCollectionByQuery(byCriteria, ScheduleTeacher.COL_STAFF_OID,
                            STAFF_DEFAULT_SIZE);
        }

        /**
         * Gets the broker.
         *
         * @return X2Broker
         */
        private X2Broker getBroker() {
            return m_data.getBroker();
        }

        /**
         * Gets the current context.
         *
         * @return DistrictSchoolYearContext
         */
        private DistrictSchoolYearContext getCurrentContext() {
            return m_districtContext;
        }

        /**
         * Gets the insession dates.
         *
         * @return Sets the
         */
        private Set<PlainDate> getInsessionDates() {
            if (m_insessionDates == null) {
                m_insessionDates = new HashSet();
                for (DistrictCalendar calendarDay : m_districtContext.getCalendars()) {
                    if (calendarDay.getInSessionIndicator()) {
                        m_insessionDates.add(calendarDay.getDate());
                    }
                }
            }
            return m_insessionDates;
        }

        /**
         * Returns snapshot date.
         *
         * @return PlainDate
         */
        private PlainDate getSnapshotDate() {
            return m_snapshotDate;
        }

        /**
         * Gets the staff absent map.
         *
         * @return Map
         */
        private Map<String, Map<String, List<StaffAttendance>>> getStaffAbsentMap() {
            if (m_staffAbsentMap == null) {
                calculateAbsent();
            }
            return m_staffAbsentMap;
        }

        /**
         * Gets the staff license map.
         *
         * @return Map
         */
        private Map<String, List<StaffCertification>> getStaffLicenseMap() {
            if (m_staffLicenseMap == null) {
                calculateLicense();
            }
            return m_staffLicenseMap;
        }

        /**
         * Gets the staff position map.
         *
         * @return Map
         */
        private Map<String, List<StaffPosition>> getStaffPositionMap() {
            if (m_staffPositionMap == null) {
                calculateStaffPosition();
            }
            return m_staffPositionMap;
        }

        /**
         * Gets the staff schedule map.
         *
         * @return Map
         */
        private Map<String, List<ScheduleTeacher>> getStaffScheduleMap() {
            if (m_staffScheduleMap == null) {
                calculateStaffSchedule();
            }
            return m_staffScheduleMap;
        }

    }

    /**
     * Retriever to determine absent time.
     *
     * @author X2 Development Corporation
     *
     */
    public class AbsentCRtrvr implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            double absent = 0;
            StaffCRDCEntity staffEntity = (StaffCRDCEntity) entity;
            for (StaffAttendance attendance : staffEntity.getCurrentStaffAbsent()) {
                absent += attendance.getTimeAbsent() == null ? 0 : attendance.getTimeAbsent().doubleValue();
            }
            return Double.valueOf(absent);
        }
    }

    /**
     * Retriever for MA to determine absent time.
     *
     * @author X2 Development Corporation
     *
     */
    public class AbsentMARtrvr implements FieldRetriever {
        private static final String ALIAS_STF_DAYS_EXPECTED = "SR37-STF";
        private static final String ALIAS_STF_DAYS_EXPECTED_ADJUSTED = "SR37-STF ADJUSTED";
        private static final String ALIAS_STF_DAYS_PRESENT = "SR36-STF";
        private static final String ALIAS_STF_DAYS_PRESENT_ADJUSTED = "SR36-STF ADJUSTED";

        String m_fieldDaysExpected;
        String m_fieldDaysExpectedAdjusted;
        String m_fieldDaysPresent;
        String m_fieldDaysPresentAdjusted;

        /**
         * Instantiates a new absent MA rtrvr.
         */
        public AbsentMARtrvr() {
            super();

            m_fieldDaysExpected = CRDCStaffData.this.translateAliasToJavaName(ALIAS_STF_DAYS_EXPECTED, true);
            m_fieldDaysExpectedAdjusted =
                    CRDCStaffData.this.translateAliasToJavaName(ALIAS_STF_DAYS_EXPECTED_ADJUSTED, true);
            m_fieldDaysPresent = CRDCStaffData.this.translateAliasToJavaName(ALIAS_STF_DAYS_PRESENT, true);
            m_fieldDaysPresentAdjusted =
                    CRDCStaffData.this.translateAliasToJavaName(ALIAS_STF_DAYS_PRESENT_ADJUSTED, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            double absent = 0;

            SisStaff staff = (SisStaff) entity.getBean();
            CRDCStaffData staffData = (CRDCStaffData) data;
            if (staffData.m_crdcStaffHelper.isStaffAttendanceUsed()) {
                StaffCRDCEntity staffEntity = (StaffCRDCEntity) entity;
                for (StaffAttendance attendance : staffEntity.getCurrentStaffAbsent()) {
                    absent += attendance.getTimeAbsent() == null ? 0 : attendance.getTimeAbsent().doubleValue();
                }
            } else {
                try {
                    String expected = (String) staff.getFieldValueByBeanPath(m_fieldDaysExpectedAdjusted);
                    if (StringUtils.isEmpty(expected) || Double.parseDouble(expected) == 0d) {
                        expected = (String) staff.getFieldValueByBeanPath(m_fieldDaysExpected);
                    }

                    String present = (String) staff.getFieldValueByBeanPath(m_fieldDaysPresentAdjusted);
                    if (StringUtils.isEmpty(present) || Double.parseDouble(present) == 0d) {
                        present = (String) staff.getFieldValueByBeanPath(m_fieldDaysPresent);
                    }

                    if (expected != null && present != null) {
                        absent = Double.parseDouble(expected) - Double.parseDouble(present);
                    }
                } catch (NumberFormatException e) {
                    // Leave at 0
                }
            }

            return Double.valueOf(absent);
        }
    }

    /**
     * Retriever to determine if a staff has license.
     *
     * @author X2 Development Corporation
     *
     */
    public class CertifiedCRtrvr implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StaffCRDCEntity staffEntity = (StaffCRDCEntity) entity;
            boolean isCertified = staffEntity.getCurrentStaffLicenses().size() > 0 ? true : false;
            return Boolean.valueOf(isCertified);
        }
    }

    /**
     * Retriever to determine staff position.
     *
     * @author X2 Development Corporation
     *
     */
    public class PositionCRtrvr implements FieldRetriever {
        private static final String CALC_PARAM_FTE = "FTE";
        private static final String CALC_PARAM_TEACHER_TYPE = "TEACHER_TYPE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object returnObject = null;
            StaffCRDCEntity staffEntity = (StaffCRDCEntity) entity;

            String type = (String) field.getParameter();
            CRDCStaffPosition position = staffEntity.getCurrentStaffPosition();
            if (type.equals(CALC_PARAM_FTE)) {
                double fte = position.getFte();
                returnObject = Double.valueOf(fte);
            } else if (type.equals(CALC_PARAM_TEACHER_TYPE)) {
                returnObject = position.getJobCode();
            }
            return returnObject;
        }
    }

    /**
     * Retriever to determine staff position (MA version).
     *
     * @author X2 Development Corporation
     *
     */
    public class PositionMARtrvr implements FieldRetriever {
        private static final String ALIAS_STF_FTE = "Overall FTE";
        private static final String CALC_PARAM_FTE = "FTE";
        private static final String CALC_PARAM_TEACHER_TYPE = "TEACHER_TYPE";

        private String m_fieldJobClassSfp;

        /**
         * Instantiates a new position MA rtrvr.
         */
        public PositionMARtrvr() {
            m_fieldJobClassSfp = translateAliasToJavaName(CRDCStaffHelper.ALIAS_MA_SFP_JOB_CLASS, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            StaffCRDCEntity stfEntity = (StaffCRDCEntity) entity;
            StaffPosition sfp = stfEntity.getCurrentStaffPosition().getStaffPosition();
            if (CALC_PARAM_TEACHER_TYPE.equals(param)) {
                if (sfp != null) {
                    String jobClass = (String) sfp.getFieldValueByBeanPath(m_fieldJobClassSfp);
                    if (!StringUtils.isEmpty(jobClass)) {
                        value = lookupCRDCCodeByBeanPath(StaffPosition.class, m_fieldJobClassSfp, jobClass);
                    }
                }
                if (value == null) {
                    value = stfEntity.getCurrentStaffPosition().getJobCode();
                }
            } else if (CALC_PARAM_FTE.equals(param)) {
                Double fte = null;
                if (fte == null || Double.valueOf(0d).compareTo(fte) == 0) {
                    fte = Double.valueOf(stfEntity.getCurrentStaffPosition().getFte());
                }
                if (fte == null || Double.valueOf(0d).compareTo(fte) == 0) {
                    SisStaff stf = (SisStaff) entity.getBean();
                    if (stf.getFieldValueByAlias(ALIAS_STF_FTE) instanceof String) {
                        try {
                            fte = Double.parseDouble((String) stf.getFieldValueByAlias(ALIAS_STF_FTE));
                        } catch (Exception e) {
                            fte = Double.valueOf(0d);
                        }
                    } else if (stf.getFieldValueByAlias(ALIAS_STF_FTE) instanceof Number) {
                        fte = Double.valueOf(((BigDecimal) stf.getFieldValueByAlias(ALIAS_STF_FTE)).doubleValue());
                    }
                }
                if (fte == null || Double.valueOf(0d).compareTo(fte) == 0) {
                    fte = Double.valueOf(1d);
                }
                BigDecimal computedFte = new BigDecimal(fte.doubleValue());
                computedFte = computedFte.setScale(5, RoundingMode.HALF_EVEN);

                value = computedFte;
            }

            return value;
        }
    }

    /**
     * Retriever to determine staff's current school.
     *
     * @author X2 Development Corporation
     *
     */
    public class SchoolCRtrvr implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StaffCRDCEntity staffEntity = (StaffCRDCEntity) entity;
            return staffEntity.getCurrentStaffPosition().getSchoolOid();
        }
    }

    /**
     * Implementation of StateReportEntity to be used for Staff CRDC Data export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StaffCRDCEntity extends StateReportEntity {

        /**
         * The Class CRDCStaffPosition.
         */
        public class CRDCStaffPosition {
            private double m_fte = 0;
            private String m_jobCode;
            private StaffPosition m_position;
            private String m_schoolOid;

            /**
             * Instantiates a new CRDC staff position.
             *
             * @param position StaffPosition
             * @param schoolOid String
             * @param jobCode String
             * @param fte double
             */
            public CRDCStaffPosition(StaffPosition position, String schoolOid, String jobCode, double fte) {
                if (position != null) {
                    m_position = position;
                }
                m_fte = fte;
                m_jobCode = jobCode;
                m_schoolOid = schoolOid;
            }

            /**
             * Gets the fte.
             *
             * @return double
             */
            public double getFte() {
                return m_fte;
            }

            /**
             * Gets the job code.
             *
             * @return String
             */
            public String getJobCode() {
                return m_jobCode;
            }

            /**
             * Gets the school oid.
             *
             * @return String
             */
            public String getSchoolOid() {
                return m_schoolOid;
            }

            /**
             * Gets the staff position.
             *
             * @return Staff position
             */
            public StaffPosition getStaffPosition() {
                return m_position;
            }
        }

        /**
         * Instantiates a new staff CRDC entity.
         */
        public StaffCRDCEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        private CRDCStaffData m_crdcData;
        private List<CRDCStaffPosition> m_positions = new ArrayList();
        private SisStaff m_staff;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = "[SASID: " + m_staff.getStateId() +
                    ", LASID: " + m_staff.getLocalId() +
                    "] " + m_staff.getNameView();

            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_staff = (SisStaff) bean;
            m_crdcData = (CRDCStaffData) data;

            List<StaffPosition> stfPositions = m_crdcData.m_crdcStaffHelper.getStaffPositions(m_staff.getOid());

            if (stfPositions.isEmpty()) {
                String jobCode =
                        m_crdcData.lookupCRDCCodeByBeanPath(Staff.class, Staff.COL_STAFF_TYPE, m_staff.getStaffType());
                if (!StringUtils.isEmpty(jobCode)) {
                    double totalFte = m_crdcData.m_crdcStaffHelper.getStaffFte(m_staff);
                    List<ScheduleTeacher> stfSchedules =
                            m_crdcData.m_crdcStaffHelper.getStaffSchedules(m_staff.getOid());
                    if (stfSchedules.isEmpty()) {
                        if (m_staff.getSchool() != null &&
                                (!m_crdcData.isSchoolContext()
                                        || m_staff.getSchool().getOid().equals(m_crdcData.getSchool().getOid()))) {
                            m_positions
                                    .add(new CRDCStaffPosition(null, m_staff.getSchool().getOid(), jobCode, totalFte));
                        }
                    } else {
                        Map<String, Integer> scheduleCounts = new HashMap();
                        int count = 0;

                        for (ScheduleTeacher schedule : stfSchedules) {
                            if (schedule.getSection() != null &&
                                    schedule.getSection().getSchedule() != null &&
                                    schedule.getSection().getSchedule().getSchool() != null) {
                                String schoolOid = schedule.getSection().getSchedule().getSchoolOid();
                                count++;
                                Integer schoolCount = scheduleCounts.get(schoolOid);
                                schoolCount =
                                        schoolCount == null ? Integer.valueOf(1)
                                                : Integer.valueOf(schoolCount.intValue() + 1);
                                scheduleCounts.put(schoolOid, schoolCount);
                            }
                        }
                        for (Entry<String, Integer> item : scheduleCounts.entrySet()) {
                            if (!m_crdcData.isSchoolContext()
                                    || item.getKey().equals(m_crdcData.getSchool().getOid())) {
                                double fte = (double) item.getValue().intValue() / (double) count;
                                fte *= totalFte;
                                m_positions.add(new CRDCStaffPosition(null, item.getKey(), jobCode, fte));
                            }
                        }
                    }
                }
            } else {
                // get total fte
                double totalFte = 0;
                for (StaffPosition stfPosition : stfPositions) {
                    if (stfPosition.getSchool() != null) {
                        totalFte += (stfPosition.getFte() == null ? 0
                                : m_crdcData.m_crdcStaffHelper.getStaffPositionFte(stfPosition));
                    }
                }
                for (StaffPosition stfPosition : stfPositions) {
                    if (stfPosition.getSchool() != null &&
                            m_crdcData.m_crdcStaffHelper.getStaffPositionInSessionCount(stfPosition) > 0 &&
                            (!m_crdcData.isSchoolContext()
                                    || stfPosition.getSchool().getOid().equals(m_crdcData.getSchool().getOid()))) {
                        double fte = (stfPosition.getFte() == null ? 0
                                : m_crdcData.m_crdcStaffHelper.getStaffPositionFte(stfPosition));
                        if (fte == 0 && totalFte == 0) {
                            fte = 1.0 / stfPositions.size();
                        }

                        String jobCode = stfPosition.getJobCode();
                        if (!StringUtils.isEmpty(jobCode)) {
                            ReferenceCode rcdJobCode = m_crdcData.m_sfpAllJobCodes.get(jobCode);
                            if (rcdJobCode != null) {
                                jobCode = (String) rcdJobCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);
                            }
                        }
                        if (StringUtils.isEmpty(jobCode)) {
                            // default to staff
                            jobCode = m_crdcData.lookupCRDCCodeByBeanPath(Staff.class, Staff.COL_STAFF_TYPE,
                                    m_staff.getStaffType());
                        }
                        BigDecimal computedFte = new BigDecimal(fte);
                        computedFte = computedFte.setScale(5, RoundingMode.HALF_EVEN);
                        if (BigDecimal.ZERO.compareTo(computedFte) == -1) {
                            m_positions.add(
                                    new CRDCStaffPosition(stfPosition, stfPosition.getSchool().getOid(), jobCode, fte));
                        }
                    }
                }
            }
            setRowCount(m_positions.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Return staff absences for current staff bean.
         *
         * @return List of StaffAttendance
         */
        public List<StaffAttendance> getCurrentStaffAbsent() {
            List<StaffAttendance> stfAbsents = new ArrayList();
            Map<String, List<StaffAttendance>> stfSchAtt =
                    m_crdcData.m_crdcStaffHelper.getStaffAbsent(m_staff.getOid());
            if (stfSchAtt.get(getCurrentStaffPosition().getSchoolOid()) != null) {
                stfAbsents.addAll(stfSchAtt.get(getCurrentStaffPosition().getSchoolOid()));
            }
            if (stfSchAtt.get(null) != null) {
                stfAbsents.addAll(stfSchAtt.get(null));
            }
            return stfAbsents == null ? new ArrayList<StaffAttendance>() : stfAbsents;
        }

        /**
         * Return staff certifications for current staff bean.
         *
         * @return List of StaffCertification
         */
        public List<StaffCertification> getCurrentStaffLicenses() {
            return m_crdcData.m_crdcStaffHelper.getStaffLicense(m_staff.getOid());
        }

        /**
         * Gets the current staff position.
         *
         * @return CRDC staff position
         */
        public CRDCStaffPosition getCurrentStaffPosition() {
            return m_positions.get(getCurrentRow());
        }

        /**
         * Return FTE for current staff bean.
         *
         * @return double FTE
         */
        public double getTotalFteX() {
            double returnValue = 0.0;
            List<StaffPosition> staffPostions = m_crdcData.m_crdcStaffHelper.getStaffPositions(m_staff.getOid());
            for (StaffPosition staffPosition : staffPostions) {
                if (!m_crdcData.m_reportDate.before(staffPosition.getStartDate()) &&
                        (staffPosition.getEndDate() == null
                                || !m_crdcData.m_reportDate.after(staffPosition.getEndDate()))) {
                    returnValue += staffPosition.getFte() == null ? 0.0 : staffPosition.getFte().doubleValue();
                }
            }
            return returnValue;
        }
    }

    /**
     * Validate type of staff.
     *
     * @author X2 Development Corporation
     *
     */
    public class ValidationType implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!STF_TYPE_CODES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        STYLE_BOLD + value + STYLE_END + " is not valid Type.",
                        "Type = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Retriever to determine staff position.
     *
     * @author X2 Development Corporation
     *
     */
    public class WATeacherType implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return lookupCRDCCodeByBeanPath(Staff.class, Staff.COL_STAFF_TYPE,
                    ((Staff) entity.getBean()).getStaffType());
        }
    }

    /**
     * Retrieve teaching years of the staff.
     *
     * @author X2 Development Corporation
     *
     */
    public class YearTeachingCRtrvr implements FieldRetriever {
        public static final String ALIAS_STF_BEGAN_TEACH = "all-stf-CRDCYearBeganTeaching";

        private static final String YEAR_TEACH_1 = "1";
        private static final String YEAR_TEACH_2 = "2";
        private static final String YEAR_TEACH_MORE = "More";

        String m_stfYearStartTeachPath;
        DistrictSchoolYearContext m_ctx;

        /**
         * Instantiates a new year teaching C rtrvr.
         */
        public YearTeachingCRtrvr() {
            super();
            X2Criteria criteria = new X2Criteria();
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
            QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
            m_ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
            if (m_ctx == null) {
                addSetupError("School District context for report date is not found in YearTeachingCRtrvr",
                        m_reportDate.toString());
            }
            m_stfYearStartTeachPath = translateAliasToJavaName(ALIAS_STF_BEGAN_TEACH, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            int teachYear = 9999;

            if (m_stfYearStartTeachPath != null) {
                SisStaff staff = (SisStaff) entity.getBean();
                Object yearStartTeach = getPropertyAsJavaType(staff, m_stfYearStartTeachPath);

                if (yearStartTeach != null && !StringUtils.isEmpty(yearStartTeach.toString())) {
                    int currYear = m_ctx.getSchoolYear() - 1;

                    if (yearStartTeach instanceof String) {
                        try {
                            teachYear = Integer.valueOf((String) yearStartTeach).intValue();
                        } catch (NumberFormatException nfe) {
                            // Do nothing - bad string value.
                        }
                    } else if (yearStartTeach instanceof Number) {
                        teachYear = ((Number) yearStartTeach).intValue();
                    }

                    if (currYear - teachYear == 0) {
                        value = YEAR_TEACH_1;
                    } else if (currYear - teachYear == 1) {
                        value = YEAR_TEACH_2;
                    } else if (teachYear == 0 || currYear - teachYear > 1) {
                        value = YEAR_TEACH_MORE;
                    }
                } else {
                    value = YEAR_TEACH_MORE;
                }
            }
            return value;
        }
    }

    private static final String ALIAS_STF_EXIT_DATE = "SR27";
    private static final String ALIAS_DOE_EXCLUDE_STF = "DOE EXCLUDE STF";
    public static final List SFC_CERT_TYPES = Arrays.asList("Certification");
    public static final List STF_TYPE_CODES = Arrays.asList("Teacher",
            "School Counselor",
            "Sworn Law Enforcement Officer",
            "Security Guard",
            "Nurse",
            "Psychologist",
            "Social Worker");
    private static final String PARAM_LICENSE_TYPES = "licenseTypes";
    private static final String SPLIT = ";";
    private static final String PROC_ID_PRIOR_YEAR = "EXPDATA-CRDC-PRYRSTF";
    private static final String PROC_ID_CURRENT_YEAR = "EXPDATA-CRDC-STF";

    // For MA use only
    private static final String PARAM_RUN_MA = "doRunMA";

    CRDCStaffHelper m_crdcStaffHelper;
    boolean m_doRunMA = false;
    String m_excludeStaffField = null;
    String m_fieldSfpPrimAssignMA;
    DataDictionaryField m_fieldStfHireDate;
    PlainDate m_reportDate;
    Map<String, FieldRetriever> m_retrieversMap;
    Map<String, ReferenceCode> m_sfpAllJobCodes = new HashMap();
    Map<String, BigDecimal> m_staffTotalFte = new HashMap();

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.CRDCReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {

        super.initialize();
        initializeCtxAndReportDate();
        initializeFields();
        loadAllSfpJobCodesMap();

        if (getSetupErrors().isEmpty()) {
            X2Criteria staffCriteria = getStaffCriteria();
            QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);
            staffQuery.addOrderBy(SisStaff.COL_NAME_VIEW, true);
            setQuery(staffQuery);
            setEntityClass(StaffCRDCEntity.class);
            m_crdcStaffHelper = new CRDCStaffHelper(this, staffCriteria, getCurrentContext(), m_reportDate);
            CRDCDataHelper crdcHelper = new CRDCDataHelper(this);
            m_retrieversMap = crdcHelper.getUsedRetrievers();
            addCalcs(m_retrieversMap);
            addValidators(crdcHelper.getUsedValidators());
            // run with empty value to check for setup errors
            m_crdcStaffHelper.getStaffPositions("");
        }
    }

    /**
     * Return staff criteria.
     *
     * @return X2Criteria
     */
    public X2Criteria getStaffCriteria() {
        X2Criteria staffCriteria = new X2Criteria();
        // Find the staff with included position records
        X2Criteria positionCriteria = new X2Criteria();
        X2Criteria positionCriteria2 = new X2Criteria();
        X2Criteria positionCriteria3 = new X2Criteria();
        positionCriteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, getCurrentContext().getStartDate());
        positionCriteria3.addEmpty(StaffPosition.COL_END_DATE, getBroker().getPersistenceKey());
        positionCriteria2.addOrCriteria(positionCriteria3);
        positionCriteria.addAndCriteria(positionCriteria2);
        positionCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, getCurrentContext().getEndDate());
        if (isSchoolContext()) {
            positionCriteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getSchool().getOid());
        }
        Set<String> crdcCodeLookup =
                getCodesForCRDCValue(StaffPosition.class, StaffPosition.COL_JOB_CODE, STF_TYPE_CODES);
        X2Criteria jobTypeAndCriteria = new X2Criteria();
        jobTypeAndCriteria.addIn(StaffPosition.COL_JOB_CODE, crdcCodeLookup);
        if (!StringUtils.isEmpty(m_excludeStaffField)) {
            positionCriteria.addNotEqualTo(StaffPosition.REL_STAFF + ModelProperty.PATH_DELIMITER + m_excludeStaffField,
                    BooleanAsStringConverter.TRUE);
        }
        DataDictionaryField maJobClassDDField =
                getDataDictionary().findDataDictionaryFieldByAlias(CRDCStaffHelper.ALIAS_MA_SFP_JOB_CLASS);
        if (maJobClassDDField != null
                && StaffPosition.DATABASE_NAME.equals(maJobClassDDField.getDataTable().getDatabaseName())
                && maJobClassDDField.hasReferenceTable()) {
            X2Criteria maJobTypeOrCriteria = new X2Criteria();
            Map<String, ReferenceCode> codesMap = maJobClassDDField.getReferenceTable().getCodeMap();
            Set<String> codesToCriteria = new HashSet<>();
            for (String code : codesMap.keySet()) {
                if (codesMap.get(code).getFieldValueByBeanPath(getFieldCRDCCode()) != null) {
                    codesToCriteria.add(code);
                }
            }
            maJobTypeOrCriteria.addIn(maJobClassDDField.getJavaName(), codesToCriteria);
            jobTypeAndCriteria.addOrCriteria(maJobTypeOrCriteria);
        }
        positionCriteria.addAndCriteria(jobTypeAndCriteria);
        SubQuery sfpSubQuery = new SubQuery(StaffPosition.class, StaffPosition.COL_STAFF_OID, positionCriteria);

        // Include staff with included schedules

        X2Criteria mtcCriteria = new X2Criteria();
        mtcCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        if (isCurrentYearExport().booleanValue()) {
            mtcCriteria.addLessOrEqualThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_START_DATE, m_reportDate);

            mtcCriteria.addGreaterOrEqualThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_END_DATE, m_reportDate);
        }
        if (isSchoolContext()) {
            mtcCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }
        crdcCodeLookup = getCodesForCRDCValue(SisStaff.class, SisStaff.COL_STAFF_TYPE, STF_TYPE_CODES);
        mtcCriteria.addIn(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + SisStaff.COL_STAFF_TYPE, crdcCodeLookup);
        if (!StringUtils.isEmpty(m_excludeStaffField)) {
            mtcCriteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + m_excludeStaffField,
                    BooleanAsStringConverter.TRUE);
        }
        SubQuery mtcSubQuery = new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_STAFF_OID, mtcCriteria);
        if (isCurrentYearExport().booleanValue()) {
            X2Criteria staffCriteria1 = new X2Criteria();
            staffCriteria1.addNotEqualTo(SisStaff.COL_ARCHIVED_INDICATOR, BooleanAsStringConverter.TRUE);
            crdcCodeLookup = getCodesForCRDCValue(SisStaff.class, SisStaff.COL_STAFF_TYPE, STF_TYPE_CODES);
            staffCriteria1.addIn(SisStaff.COL_STAFF_TYPE, crdcCodeLookup);
            String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

            staffCriteria1.addEqualTo(SisStaff.COL_STATUS, activeStatus);
            if (isSchoolContext()) {
                staffCriteria1.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
            }
            if (!StringUtils.isEmpty(m_excludeStaffField)) {
                staffCriteria1.addNotEqualTo(m_excludeStaffField, BooleanAsStringConverter.TRUE);
            }
            X2Criteria staffHireDateCriteria = new X2Criteria();
            staffHireDateCriteria.addLessOrEqualThan(SisStaff.COL_HIRE_DATE, m_reportDate);
            X2Criteria staffHireDateEmptyCriteria = new X2Criteria();
            staffHireDateEmptyCriteria.addEmpty(SisStaff.COL_HIRE_DATE, getBroker().getPersistenceKey());
            staffHireDateCriteria.addOrCriteria(staffHireDateEmptyCriteria);
            staffCriteria1.addAndCriteria(staffHireDateCriteria);
            staffCriteria.addOrCriteria(staffCriteria1);
        }

        X2Criteria staffCriteria2 = new X2Criteria();
        staffCriteria2.addIn(X2BaseBean.COL_OID, sfpSubQuery);
        X2Criteria staffCriteria3 = new X2Criteria();
        staffCriteria3.addIn(X2BaseBean.COL_OID, mtcSubQuery);

        staffCriteria.addOrCriteria(staffCriteria2);
        staffCriteria.addOrCriteria(staffCriteria3);
        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STF_EXIT_DATE);
        if (field != null && field.isEnabled() && SisStaff.class.equals(field.getTable().getBeanClass())) {
            m_fieldStfHireDate = field;
            X2Criteria staffExitDateCriteria1 = new X2Criteria();
            staffExitDateCriteria1.addGreaterOrEqualThan(field.getJavaName(), getCurrentContext().getStartDate());
            X2Criteria staffExitDateEmptyCriteria1 = new X2Criteria();
            staffExitDateEmptyCriteria1.addEmpty(field.getJavaName(), getBroker().getPersistenceKey());
            staffExitDateCriteria1.addOrCriteria(staffExitDateEmptyCriteria1);
            staffCriteria.addAndCriteria(staffExitDateCriteria1);
        }
        return staffCriteria;
    }

    /**
     * Gets the staff exit date.
     *
     * @param stf SisStaff
     * @return Plain date
     */
    public PlainDate getStaffExitDate(SisStaff stf) {
        PlainDate exitDate = null;
        if (stf != null && m_fieldStfHireDate != null) {
            try {
                Object property = getPropertyAsJavaType(stf, m_fieldStfHireDate.getJavaName());
                if (property instanceof PlainDate) {
                    exitDate = (PlainDate) property;
                }
            } catch (X2BaseException e) {
                // return null if property cannot be converted
            }
        }
        return exitDate;
    }

    /**
     * Next.
     *
     * @return StateReportEntity
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#next()
     */
    @Override
    public StateReportEntity next() throws X2BaseException {
        StateReportEntity entity = super.next();
        if (entity != null) {
            StaffPosition sfp = ((StaffCRDCEntity) entity).getCurrentStaffPosition().getStaffPosition();
            if (m_doRunMA && m_fieldSfpPrimAssignMA != null && sfp != null
                    && !BooleanAsStringConverter.TRUE.equals(sfp.getFieldValueByBeanPath(m_fieldSfpPrimAssignMA))) {
                return entity;
            }
            Double fte = (((StaffCRDCEntity) entity).getCurrentStaffPosition().getFte());

            BigDecimal computedFte = new BigDecimal(fte.doubleValue());
            computedFte = computedFte.setScale(5, RoundingMode.HALF_EVEN);
            BigDecimal sum = BigDecimal.ZERO;

            BigDecimal currentFte = m_staffTotalFte.get(entity.getBean().getOid());
            if (currentFte == null) {
                sum = computedFte;
                m_staffTotalFte.put(entity.getBean().getOid(), computedFte);
            } else {
                sum = computedFte.add(currentFte);
                m_staffTotalFte.put(entity.getBean().getOid(), computedFte);
            }

            sum = sum.setScale(5, RoundingMode.HALF_EVEN);
            if (BigDecimal.ONE.compareTo(sum) == -1) {
                FieldDefinition field = getFieldDefinition("FTE");
                StateReportValidationError error = null;
                if (field != null) {
                    error = new StateReportValidationError(entity, field,
                            "Total FTE Exceeds 1.0. Confirm FTE is correct",
                            sum.toString());
                } else {
                    error = new StateReportValidationError(entity.getEntityName(), "FTE",
                            "Total FTE Exceeds 1.0. Confirm FTE is correct",
                            sum.toString());
                }
                entity.addRetrievalError("FTE", error);
            }
        }
        return entity;
    }

    /**
     * Gets the previous context.
     *
     * @param currentContext DistrictSchoolYearContext
     * @return District school year context
     */
    private DistrictSchoolYearContext getPreviousContext(DistrictSchoolYearContext currentContext) {

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(currentContext.getSchoolYear() - 1));

        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        DistrictSchoolYearContext previousContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
        return previousContext;
    }

    /**
     * Checks if is current year export.
     *
     * @return Boolean
     */
    private Boolean isCurrentYearExport() {
        Boolean isCurrent = null;
        String currentId = getProcedureId();
        if (currentId != null && PROC_ID_CURRENT_YEAR.equals(currentId)) {
            isCurrent = Boolean.TRUE;
        } else if (currentId != null && PROC_ID_PRIOR_YEAR.equals(currentId)) {
            isCurrent = Boolean.FALSE;
        }
        return isCurrent;
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_excludeStaffField = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_STF, false);
        m_fieldSfpPrimAssignMA = translateAliasToJavaName(CRDCStaffHelper.ALIAS_MA_SFP_PRIM_ASSIGN, false);
        if (getParameter(PARAM_RUN_MA) != null) {
            m_doRunMA = true;
        }

        ModelProperty prop = new ModelProperty(StaffCertification.class, StaffCertification.COL_TYPE,
                getBroker().getPersistenceKey());
        DataDictionaryField typeField = prop.getField();
        if (typeField != null) {
            ReferenceTable refTable = typeField.getReferenceTable();
            if (refTable != null) {
                List<String> refCodes = new ArrayList<String>();
                for (ReferenceCode refCode : refTable.getReferenceCodes()) {
                    refCodes.add(refCode.getCode());
                }
                String types = (String) getParameter(PARAM_LICENSE_TYPES);
                if (!StringUtils.isEmpty(types)) {
                    List<String> licenseTypes = new ArrayList<String>(Arrays.asList(types.split(SPLIT)));
                    for (String type : licenseTypes) {
                        if (!refCodes.contains(type)) {
                            String refCodesMsg = "Reference table must have " + licenseTypes.toString() + " codes";
                            addSetupError(refCodesMsg, StaffCertification.COL_TYPE);
                        }
                    }
                }
            } else {
                addSetupError(StaffCertification.COL_TYPE + "must have reference table", StaffCertification.COL_TYPE);
            }
        }
    }

    /**
     * Initialize previous CTX and report date by "reportDatePart1" input param.
     */
    private void initializeCtxAndReportDate() {

        m_reportDate = (PlainDate) getParameter(PARAM_PART_1_DATE);
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));

        if (!isCurrentYearExport().booleanValue()) {
            context = getPreviousContext(context);
            m_reportDate = context.getEndDate();
        }

        setCurrentContext(context);
    }

    /**
     * Load staff positions' job codes including disabled.
     */
    private void loadAllSfpJobCodesMap() {
        DataDictionaryField dictionaryField =
                getDataDictionary().findDataDictionaryField(StaffPosition.class.getName(), StaffPosition.COL_JOB_CODE);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            X2Criteria rcdCriteria = new X2Criteria();
            rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, dictionaryField.getReferenceTableOid());
            m_sfpAllJobCodes = getBroker().getMapByQuery(
                    new QueryByCriteria(ReferenceCode.class, rcdCriteria), ReferenceCode.COL_CODE, 512);
        }
    }
}
