/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for EPIMS Work Assignment export.
 * This class implements the data export for Mass EPIMS work assignment export.
 *
 * NOTE:
 * Modified to include extended fields for the MA Sims expansion for 2011.
 * Changes include:
 * Two new fields: course term (WA16), term status (WA17)
 * Rework FTE calculation to be term based.
 *
 * @author X2 Development Corporation
 */
public class EpimsWork extends StateReportData {

    /**
     * The Class EpimsEntity.
     */
    public static class EpimsEntity extends StateReportEntity {
        /**
         * Entity name.
         */
        private String m_entityName;

        /**
         * A list of staff positions and teacher schedules from the data module.
         */
        private List<X2BaseBean> m_assignments;

        /**
         * FTE for TeacherSchedule entries in assignments.
         */
        private Map<String, BigDecimal> m_scheduleFte = null;
        private Map<String, TeacherInfo> m_scheduleTeacherInfo = null;

        /*
         * Placeholders for calculated unmapped fields. These can be written
         * back to the database in postProcess if update flag is set.
         * Also, holds some calcualted values that have been overridden with
         * default or related values.
         *
         * Map key should be field alias constant.
         */
        private Map<String, Object> m_updateValues = null;

        /**
         * Filter out staff where the hire date is empty.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            SisStaff staff = (SisStaff) getBean();
            if (staff.getHireDate() == null) {
                error = new StateReportValidationError(getEntityName(), "Hire Date", "Hire date is empty", "");
            }
            return error;
        }

        /**
         * Returns the display name identifying this entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            if (m_entityName == null) {
                SisStaff staff = (SisStaff) getBean();
                String name = staff.getNameView();
                String localId = staff.getLocalId();
                String stateId = staff.getStateId();
                SisSchool skl = staff.getSchool();
                String sklName = skl != null ? skl.getName() : "No school";
                boolean isSkl = getData().isSchoolContext();
                X2BaseBean assignment = getAssignment();

                m_entityName = (name != null ? name : "empty") +
                        " [" +
                        "ID: " + (localId != null ? localId : "empty") + ", " +
                        "MEPID: " + (stateId != null ? stateId : "empty") +
                        (isSkl ? ", SCHOOL: " + (sklName != null ? sklName : "empty") : "");

                // Identify the job or section of this assignment.
                if (assignment instanceof StaffPosition) {
                    String classification = getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
                    m_entityName += ", Job: " + classification;
                } else if (assignment instanceof ScheduleTeacher) {
                    String section = getFieldValue(WA_11_SFP_SECTION);
                    m_entityName += ", Section: " + section;
                }
                m_entityName += "]\n";
            }
            return m_entityName;
        }

        /**
         * Returns the accumulated total FTE as gathered by the FTE retriever.
         *
         * @param teacherScheduleOid String
         * @return total fte.
         */
        public BigDecimal getCalculatedFte(String teacherScheduleOid) {
            BigDecimal fte = m_scheduleFte.get(teacherScheduleOid);
            if (fte == null) {
                fte = new BigDecimal(0);
            }
            return fte;

        }

        /**
         * Returns the copied Highly Qualified Status if available.
         *
         * @param teacherScheduleOid String
         * @return TeacherInfo
         */
        public TeacherInfo getCopiedTeacherInfo(String teacherScheduleOid) {
            return m_scheduleTeacherInfo.get(teacherScheduleOid);
        }

        /**
         * Returns a field value saved before mapping.
         *
         * Certain calculated data fields can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @return Object
         */
        public Object getUpdateValue(String doeId) {
            Object value = null;
            if (m_updateValues != null) {
                value = m_updateValues.get(doeId);
            }
            return value;
        }

        /**
         * Initialize the entity. Get all staff work assignment information
         * and get a count of exportable records.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStaff staff = (SisStaff) bean;

            EpimsWork workData = (EpimsWork) data;
            Collection positions = workData.getPositions(staff.getOid());
            List<ScheduleTeacher> schedules = (List<ScheduleTeacher>) workData.getSchedules(staff.getOid());
            m_assignments = new ArrayList();
            double totalFte = 0.0;
            Boolean excludeEmptyRoster = (Boolean) data.getParameter(EXCLUDE_EMPTY_ROSTERS_PARAM);

            // Validate elements in the collections,
            // Add those that qualify to a list of exportable assignments.
            if (positions != null) {
                Iterator iterator = positions.iterator();
                while (iterator.hasNext()) {
                    StaffPosition position = (StaffPosition) iterator.next();
                    if (validateAssignment(position)) {

                        // accumulate total FTE for positions.
                        BigDecimal fte =
                                (BigDecimal) data.getProperty(position, ((EpimsWork) data).m_wa12SfpFullTimeEquivalent);

                        if (fte != null) {
                            fte = fte.setScale(3, RoundingMode.HALF_UP);
                            totalFte += fte.doubleValue();
                        }

                        m_assignments.add(position);
                    }
                }
            }

            m_scheduleTeacherInfo = new HashMap<String, TeacherInfo>(m_assignments.size());
            if (schedules != null) {
                Iterator iterator = schedules.iterator();
                while (iterator.hasNext()) {
                    ScheduleTeacher schedule = (ScheduleTeacher) iterator.next();
                    boolean isRosterEmpty = false;
                    MasterSchedule section = schedule.getSection();
                    if (excludeEmptyRoster.booleanValue()) {
                        if (null != section && section.getStudentSections(getData().getBroker()).size() <= 0) {
                            isRosterEmpty = true;
                        }
                    }
                    if (validateAssignment(schedule)) {
                        // If copying Teacher Status from last year is selected,
                        // Look through last year staff schedule and pick up the teacher status
                        // values from
                        // matching classes.
                        if (workData.m_copyTeacherStatus.booleanValue() && workData.m_teacherLYSchedulesMap != null) {
                            TeacherInfo teacherStatus = null;
                            String schoolOid = section.getSchedule().getSchoolOid();
                            List<ScheduleTeacher> lySchedules = workData.m_teacherLYSchedulesMap.get(staff.getOid());
                            if (lySchedules != null) {
                                // Search for a matching school and school course number .
                                Iterator lyIterator = lySchedules.iterator();
                                while (lyIterator.hasNext()) {
                                    ScheduleTeacher lySchedule = (ScheduleTeacher) lyIterator.next();
                                    // check for matching school course number and school OID.
                                    if (lySchedule.getSection().getSchoolCourse().getNumber().equals(
                                            section.getSchoolCourse().getNumber()) &&
                                            schoolOid.equals(lySchedule.getSection().getSchedule().getSchoolOid())) {
                                        String hqStatus = (String) lySchedule
                                                .getFieldValueByBeanPath(workData.m_wa14MtcHighlyQualifiedStatus);
                                        String jcStatus = (String) lySchedule
                                                .getFieldValueByBeanPath(workData.m_wa07MtcJobClassification);
                                        String smStatus = (String) lySchedule
                                                .getFieldValueByBeanPath(workData.m_wa15MtcSubjectMatterCompetency);
                                        String ppStatus = (String) lySchedule
                                                .getFieldValueByBeanPath(workData.m_wa13MtcParaprofessional);
                                        String asStatus = (String) lySchedule
                                                .getFieldValueByBeanPath(workData.m_wa08MtcAssignment);
                                        if (!StringUtils.isEmpty(hqStatus) ||
                                                !StringUtils.isEmpty(jcStatus) ||
                                                !StringUtils.isEmpty(smStatus) ||
                                                !StringUtils.isEmpty(ppStatus) ||
                                                !StringUtils.isEmpty(asStatus)) {
                                            teacherStatus =
                                                    new TeacherInfo(hqStatus, jcStatus, asStatus, ppStatus, smStatus);
                                            break;
                                        }
                                    }
                                }
                                // If no matching school course number was found, check for matching
                                // district course number.
                                if (teacherStatus == null) {
                                    lyIterator = lySchedules.iterator();
                                    while (lyIterator.hasNext()) {
                                        ScheduleTeacher lySchedule = (ScheduleTeacher) lyIterator.next();
                                        // check for matching district course number.
                                        if (lySchedule.getSection().getSchoolCourse().getCourse().getNumber().equals(
                                                section.getSchoolCourse().getCourse().getNumber())) {
                                            String hqStatus = (String) lySchedule
                                                    .getFieldValueByBeanPath(workData.m_wa14MtcHighlyQualifiedStatus);
                                            String jcStatus = (String) lySchedule
                                                    .getFieldValueByBeanPath(workData.m_wa07MtcJobClassification);
                                            String smStatus = (String) lySchedule
                                                    .getFieldValueByBeanPath(workData.m_wa15MtcSubjectMatterCompetency);
                                            String ppStatus = (String) lySchedule
                                                    .getFieldValueByBeanPath(workData.m_wa13MtcParaprofessional);
                                            String asStatus = (String) lySchedule
                                                    .getFieldValueByBeanPath(workData.m_wa08MtcAssignment);
                                            if (!StringUtils.isEmpty(hqStatus) ||
                                                    !StringUtils.isEmpty(jcStatus) ||
                                                    !StringUtils.isEmpty(smStatus) ||
                                                    !StringUtils.isEmpty(ppStatus) ||
                                                    !StringUtils.isEmpty(asStatus)) {
                                                teacherStatus = new TeacherInfo(hqStatus, jcStatus, asStatus, ppStatus,
                                                        smStatus);
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                            if (teacherStatus != null) {
                                m_scheduleTeacherInfo.put(schedule.getOid(), teacherStatus);
                            }
                        }
                        if (!isRosterEmpty) {
                            m_assignments.add(schedule);
                        }
                    }
                }

                Boolean calcParam = (Boolean) data.getParameter(CALCULATE_FTES_PARAM);
                m_scheduleFte = new HashMap<String, BigDecimal>(m_assignments.size());

                // calculate average FTE from positions and schedules.
                if (calcParam.booleanValue()) {
                    Double overallFte = Double.valueOf(1.0);
                    Object objFte = staff.getFieldValueByAlias(OVERALL_FTE);

                    if (objFte != null && !StringUtils.isEmpty((String) objFte)) {
                        overallFte = Double.valueOf(Double.parseDouble((String) objFte));
                    } else {
                        FieldDefinition field = data.getFieldDefinition(WA_12_MTC_FTE);
                        StateReportValidationError error = new StateReportValidationError(this, field,
                                "Alias Overall FTE was not found.",
                                "Defaulting value to 1.0");
                        addRetrievalError(WA_12_MTC_FTE, error);
                    }

                    // Calculating FTE based on Overall FTE and # of SFP/MTC records
                    calculatedFte(totalFte, overallFte.doubleValue());
                }
            }

            setRowCount(m_assignments.size());
        }

        /**
         * If update teacher schedule is set, save new values into the bean.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            EpimsWork workData = (EpimsWork) getData();
            Boolean updateSchedules = (Boolean) getData().getParameter(UPDATE_SCHEDULE_RECORDS_PARAM);
            Boolean calcParam = (Boolean) getData().getParameter(CALCULATE_FTES_PARAM);
            Boolean copyTeacherStatus = workData.m_copyTeacherStatus;
            String copyTeacherMode = workData.m_copyTeacherMode;

            if (updateSchedules != null && updateSchedules.booleanValue()) {
                X2BaseBean assignment = getAssignment();
                if (assignment instanceof ScheduleTeacher) {
                    ScheduleTeacher teacherSchedule = (ScheduleTeacher) assignment;
                    boolean changed = false;
                    if (calcParam != null && calcParam.booleanValue()) {
                        String roundedFte = getFieldValue(WA_12_MTC_FTE);
                        String fieldPath = workData.m_wa12MtcFullTimeEquivalent;
                        if (!StringUtils.isEmpty(fieldPath) && !StringUtils.isEmpty(roundedFte)) {
                            teacherSchedule.setFieldValueByBeanPath(fieldPath, roundedFte);
                            changed = true;
                        }
                    }
                    if (copyTeacherStatus != null && copyTeacherStatus.booleanValue()) {
                        boolean overwrite = copyTeacherMode.equals("overwrite");

                        String aStatus = (String) getUpdateValue(WA_07_SFP_JOB_CLASSIFICATION);
                        String fieldPath = workData.m_wa07MtcJobClassification;
                        if (overwrite || (!StringUtils.isEmpty(fieldPath) && !overwrite)) {
                            teacherSchedule.setFieldValueByBeanPath(fieldPath, aStatus);
                            changed = true;
                        }
                        aStatus = (String) getUpdateValue(WA_08_SFP_ASSIGNMENT);
                        fieldPath = workData.m_wa08MtcAssignment;
                        if (overwrite || (!StringUtils.isEmpty(fieldPath) && !overwrite)) {
                            teacherSchedule.setFieldValueByBeanPath(fieldPath, aStatus);
                            changed = true;
                        }
                        aStatus = (String) getUpdateValue(WA_13_SFP_PARAPROFESSIONAL);
                        fieldPath = workData.m_wa13MtcParaprofessional;
                        if (overwrite || (!StringUtils.isEmpty(fieldPath) && !overwrite)) {
                            teacherSchedule.setFieldValueByBeanPath(fieldPath, aStatus);
                            changed = true;
                        }
                        aStatus = (String) getUpdateValue(WA_14_SFP_HIGHLY_QUALIFIED_STATUS);
                        fieldPath = workData.m_wa14MtcHighlyQualifiedStatus;
                        if (overwrite || (!StringUtils.isEmpty(fieldPath) && !overwrite)) {
                            teacherSchedule.setFieldValueByBeanPath(fieldPath, aStatus);
                            changed = true;
                        }
                        aStatus = (String) getUpdateValue(WA_15_SFP_SUBJECT_MATTER_COMPETENCY);
                        fieldPath = workData.m_wa15MtcSubjectMatterCompetency;
                        if (overwrite || (!StringUtils.isEmpty(fieldPath) && !overwrite)) {
                            teacherSchedule.setFieldValueByBeanPath(fieldPath, aStatus);
                            changed = true;
                        }
                    }
                    if (changed) {
                        getData().getBroker().saveBeanForced(teacherSchedule);
                    }
                }
            }
        }

        /**
         * When the row is incremented, reset all local variables to the new row.
         *
         *
         * @param currentRow void
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#setCurrentRow(int)
         */
        @Override
        public void setCurrentRow(int currentRow) {
            super.setCurrentRow(currentRow);
            m_entityName = null;
        }

        /**
         * Sets a field value before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @param value Object
         */
        public void setUpdateValue(String doeId, Object value) {
            if (m_updateValues == null) {
                m_updateValues = new HashMap<String, Object>();
            }

            m_updateValues.put(doeId, value);
        }

        /**
         * Returns the current assignment (position or schedule) based
         * on the entity staff record and the entity current row index.
         * The assignment comes from the List of assignments built in
         * the initialization phase.
         *
         * @return X 2 base bean
         */
        protected X2BaseBean getAssignment() {
            X2BaseBean assignment = null;
            int index = getCurrentRow();
            if (m_assignments != null && index >= 0 && index < m_assignments.size()) {
                assignment = m_assignments.get(index);
            }
            return assignment;
        }

        /**
         * Calculates the FTE for each (valid) assignment based on teacher schedules.
         *
         * NEW: New FTP calculation from SIMS Expansion:
         * FTP is by term. Only compare/sum values within the same term to the total.
         * Each term must add up to (overall - existing).
         * Across terms may add up to greater than 1.00 if teacher has sections in different terms.
         *
         * @param existingFte the existing FTE for the teacher coming from staff positions
         * @param overallFTE the total FTE value for the teacher coming from Staff
         * @return BigDecimal set to two decimal places, this value will be 0.00 if there are no
         *         valid
         *         assignments for the teacher
         * @throws X2BaseException exception
         */
        private void calculatedFte(double existingFte, double overallFTE)
                throws X2BaseException {
            int scheduleCountsLen = 0;
            Map<String, ScheduleTerm> termMap = ((EpimsWork) getData()).m_termMap;

            // Find out how many schedules are in each term from the all term cover map.
            for (X2BaseBean assignment : m_assignments) {
                if (assignment instanceof ScheduleTeacher) {
                    ScheduleTeacher teacherSchedule = (ScheduleTeacher) assignment;
                    ScheduleTerm term = termMap.get(teacherSchedule.getSection().getScheduleTermOid());
                    String terms = term.getUniversalTermMap();
                    if (terms.length() > scheduleCountsLen) {
                        scheduleCountsLen = terms.length();
                    }
                }
            }

            /*
             * For each term in the section term cover map,
             * 1. Go through each term, from most classes per term to least classes per term,
             * identify all sections in the term.
             * 2. Count those that have not yet been assigned a FTE in a previous term.
             * 3. Calculate those based on the remainder from previous assignment and unassigned
             * count.
             */
            int[] termCount = new int[scheduleCountsLen];
            for (int chpos = 0; chpos < scheduleCountsLen; chpos++) {
                termCount[chpos] = 0;
            }
            for (X2BaseBean assignment : m_assignments) {
                if (assignment instanceof ScheduleTeacher) {
                    ScheduleTeacher teacherSchedule = (ScheduleTeacher) assignment;
                    ScheduleTerm term = termMap.get(teacherSchedule.getSection().getScheduleTermOid());
                    String terms = term.getUniversalTermMap();

                    for (int chpos = 0; chpos < terms.length(); chpos++) {
                        char tchar = terms.charAt(chpos);
                        if (tchar == '1') {
                            termCount[chpos]++;
                        }
                    }
                }
            }

            // Go through the terms from the term that has the most classes to the term that has the
            // least classes.
            boolean found = true;
            while (found) {
                found = false;
                int mostTermPos = -1;
                int mostTermMax = 0;
                for (int chpos = 0; chpos < scheduleCountsLen; chpos++) {
                    if (mostTermMax < termCount[chpos]) {
                        mostTermPos = chpos;
                        mostTermMax = termCount[chpos];
                    }
                }
                if (mostTermPos > -1) {
                    found = true;
                    calcualteOneFte(mostTermPos, termMap, existingFte, overallFTE);
                    termCount[mostTermPos] = 0;
                }
            }
        }

        /**
         * For each term in the section term cover map,
         * 1. Identify all sections in the term.
         * 2. Count those that have not yet been assigned a FTE in a previous term.
         * 3. Calculate those based on the remainder from previous assignment and unassigned count.
         *
         * @param chpos int
         * @param termMap Map<String,ScheduleTerm>
         * @param existingFte double
         * @param overallFTE double
         */
        private void calcualteOneFte(int chpos,
                                     Map<String, ScheduleTerm> termMap,
                                     double existingFte,
                                     double overallFTE) {
            // unassigned is the count of sections in this term that have not yet been
            // assigned an FTE value from a previous term.
            int unassigned = 0;
            float previousSubTotal = 0;
            for (X2BaseBean assignment : m_assignments) {
                if (assignment instanceof ScheduleTeacher) {
                    ScheduleTeacher teacherSchedule = (ScheduleTeacher) assignment;
                    ScheduleTerm term = termMap.get(teacherSchedule.getSection().getScheduleTermOid());
                    String terms = term.getUniversalTermMap();
                    char tchar = '0';
                    if (terms != null && chpos < terms.length()) {
                        tchar = terms.charAt(chpos);
                    } else if (terms != null && chpos >= terms.length() && (terms.charAt(terms.length() - 1) == '1')) {
                        // If the term is shorter than the requested position but is active in its
                        // last position,
                        // count it as active in the requested position.
                        tchar = '1';
                    }
                    if (tchar == '1') {
                        if (!m_scheduleFte.containsKey(teacherSchedule.getOid())) {
                            unassigned++;
                        } else {
                            BigDecimal prevFte = m_scheduleFte.get(teacherSchedule.getOid());
                            previousSubTotal += prevFte.floatValue();
                        }
                    }
                }
            }
            if (unassigned != 0) {
                for (X2BaseBean assignment : m_assignments) {
                    if (assignment instanceof ScheduleTeacher) {
                        ScheduleTeacher teacherSchedule = (ScheduleTeacher) assignment;
                        ScheduleTerm term = termMap.get(teacherSchedule.getSection().getScheduleTermOid());
                        String terms = term.getUniversalTermMap();
                        char tchar = '0';
                        if (terms != null && chpos < terms.length()) {
                            tchar = terms.charAt(chpos);
                        }
                        if (tchar == '1' && !m_scheduleFte.containsKey(teacherSchedule.getOid())) {
                            BigDecimal newFte = new BigDecimal(
                                    String.valueOf((overallFTE - existingFte - previousSubTotal) / unassigned));
                            m_scheduleFte.put(teacherSchedule.getOid(), newFte);
                        }
                    }
                }
            }
        }

        /**
         * Determines whether or not the assignment is valid for the district running this export.
         * behavior.
         *
         * @param assignment X2BaseBean
         * @return true if the assignment is valid, false otherwise
         * @throws X2BaseException exception
         */
        private boolean validateAssignment(X2BaseBean assignment) throws X2BaseException {
            boolean validated = false;
            String schoolCode = ((EpimsWork) getData()).getSchoolCode(assignment);
            String districtId = ((EpimsWork) getData()).m_districtId;
            if (districtId != null && districtId.length() >= 4 && schoolCode != null) {
                validated = schoolCode.startsWith(districtId.substring(0, 4));
            }
            if (getData().isSchoolContext() && validated) {
                String code =
                        (String) getData().getProperty(getData().getSchool(), ((EpimsWork) getData()).m_wa06SchoolCode);
                if (schoolCode != null && !schoolCode.equals(code)) {
                    validated = false;
                }
            }

            return validated;
        }
    }

    /**
     * Inner class to hold teacher schedule information as copied from last year schedule.
     * These are for WA07, WA08, WA 13, WA 14, WA 15
     */
    protected static class TeacherInfo {
        private String m_assignment;
        private String m_HQStatus;
        private String m_jobClass;
        private String m_paraprof;
        private String m_subjectMatter;

        /**
         * Instantiates a new teacher info.
         *
         * @param HQStatus String
         * @param jobClass String
         * @param assignment String
         * @param paraprof String
         * @param subjectMatter String
         */
        public TeacherInfo(String HQStatus, String jobClass, String assignment, String paraprof, String subjectMatter) {
            m_assignment = assignment;
            m_HQStatus = HQStatus;
            m_jobClass = jobClass;
            m_paraprof = paraprof;
            m_subjectMatter = subjectMatter;
        }

        /**
         * Retrieve the WA 08 assignment.
         *
         * @return String
         */
        public String getWA08Assignment() {
            return m_assignment;
        }

        /**
         * Retrieve the WA 14 Highly Qualified Status.
         *
         * @return String
         */
        public String getWA14HQStatus() {
            return m_HQStatus;
        }

        /**
         * Retrieve the WA 07 Job Classification.
         *
         * @return String
         */
        public String getWA07JobClassification() {
            return m_jobClass;
        }

        /**
         * Retrieve the WA 13 Paraprofessional.
         *
         * @return String
         */
        public String getWA13Paraprofessional() {
            return m_paraprof;
        }

        /**
         * Retrieve the WA 15 Subject Matter Competency.
         *
         * @return String
         */
        public String getWA15SubjectMatterCompetency() {
            return m_subjectMatter;
        }
    }

    private static final String ADJUSTED_DISTRICT_ID = "Adjusted District ID";
    private static final String DOE_15_SCHOOL = "DOE 15";
    private static final String DOE_DISTRICT_ID = "DOE District ID";
    private static final String ALIAS_SKL_SIF_DISTRICT_ID = "skl-sif-district-id";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String EPIMS_SR09 = "SR09";
    private static final String OVERALL_FTE = "Overall FTE";
    private static final String SFP_EXCLUDE_STATUS = "EPIMS SFP Status";
    private static final String SMT_EXCLUDE_STATUS = "EPIMS SMT Status";
    private static final String CRS_EXCLUDE_STATUS = "EPIMS CRS Status";

    /*
     * Field alias constants for the SCHEDULE_MASTER_TEACHER and COURSE tables (WA10 is on the
     * COURSE table, the rest are on the SCHEDULE_MASTER_TEACHER table).
     */
    private static final String WA_01_MEPID = "WA01";
    private static final String WA_02_LOCALID = "WA02";
    private static final String WA_03_FIRST_NAME = "WA03";
    private static final String WA_04_MIDDLE_NAME = "WA04";
    private static final String WA_05_LAST_NAME = "WA05";
    private static final String WA_06_SCHOOL_CODE = "WA06";

    private static final String WA_07_MTC_JOB_CLASSIFICATION = "WA07-MTC";
    private static final String WA_08_MTC_ASSIGNMENT = "WA08-MTC";
    private static final String WA_10_MTC_SUBJECT_AREA = "WA10-MTC";
    private static final String WA_12_MTC_FTE = "WA12-MTC";
    private static final String WA_13_MTC_PARAPROFESSIONAL = "WA13-MTC";
    private static final String WA_14_MTC_HIGHLY_QUALIFIED_STATUS = "WA14-MTC";
    private static final String WA_15_MTC_SUBJECT_MATTER_COMPETENCY = "WA15-MTC";
    private static final String WA_16_MTC_SCHEDULE_TERM = "WA16-MTC";
    private static final String WA_17_MTC_SCHEDULE_STATUS = "WA17-MTC";

    /*
     * Field alias constants for the STAFF_POSITION table.
     */
    private static final String WA_07_SFP_JOB_CLASSIFICATION = "WA07-SFP";
    private static final String WA_08_SFP_ASSIGNMENT = "WA08-SFP";
    private static final String WA_09_SFP_GRADE = "WA09-SFP";
    private static final String WA_10_SFP_SUBJECT_AREA = "WA10-SFP";
    private static final String WA_11_SFP_SECTION = "WA11-SFP";
    // WA12-SFP is a non-user defined field StaffPosition.COL_FTE
    private static final String WA_13_SFP_PARAPROFESSIONAL = "WA13-SFP";
    private static final String WA_14_SFP_HIGHLY_QUALIFIED_STATUS = "WA14-SFP";
    private static final String WA_15_SFP_SUBJECT_MATTER_COMPETENCY = "WA15-SFP";

    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- X2-specific code below
    // ---------------------------------------------------------------------------------------------

    /**
     * Name for the "district ID" parameter. The value is an String.
     */
    public static final String DISTRICT_ID_PARAM = "districtId";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the "calculate fte's" parameter. The value is a Boolean.
     */
    public static final String CALCULATE_FTES_PARAM = "calculateFTEs";

    /**
     * Name for the "Exclude sections with empty rosters" parameter. The value is a Boolean.
     */
    public static final String EXCLUDE_EMPTY_ROSTERS_PARAM = "excludeEmptyRosters";

    /**
     * Name for the "copy highly qualified status" parameter. The value is a Boolean.
     */
    public static final String COPY_TEACHER_STATUS_PARAM = "copyTeacher";

    /**
     * Name for the copying teacher. The value is a String.
     */
    public static final String COPY_TEACHER_MODE_PARAM = "copyTeacherMode";

    /**
     * Name for the "update schedule records" parameter. The value is a Boolean.
     */
    public static final String UPDATE_SCHEDULE_RECORDS_PARAM = "updateScheduleRecords";

    public static final String PARAM_INCLUDE_SIF_SCHOOL = "includeSifSchoolId";

    /*
     * Other internal constants
     */
    private static final String DEFAULT_SECTION_CODE = "0";
    private static final String SEPARATOR_COMMA = ",";
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String NO_MIDDLE_NAME = "NMN";
    private static final String REGEX_ALPHANUMERIC = "[A-Za-z0123456789]*";
    private static final String REGEX_ALPHANUMERIC_HYPHEN = "[-A-Za-z0123456789]*";
    private static final String REGEX_NAME = "[-'. A-Za-z0123456789]*";
    private static final String REGEX_MNAME = "[-' A-Za-z0123456789]*";
    private static final String REGEX_NUMERIC = "[0123456789]*";
    private static final String REGEX_NUMERIC_DECIMAL = "[.0123456789]*";
    private static final String REGEX_SECTION = "[-. A-Za-z0123456789]*";
    private static final String REGEX_CORE_ACADEMIC_SUBJECT =
            "0[123456][A0123456789][0123456789]{2}|5[123456][0123456789]{3}|73[0123456789]{3}|99999";
    private static final String REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE = "5680[01]|0425[456789]";
    private static final String REGEX_NON_SECONDARY_SUBJECT = "[567][0123456789]{4}";
    private static final String REGEX_SECONDARY_SUBJECT =
            "99999|[012][0123456789][A0123456789][0123456789]{2}|C[0123456789]{6}";
    private static final String REGEX_ALL_GRADE_LEVELS = "PK|K|K01|0102|0[0123456789]|1[012]|88|99";

    protected String m_adjustedDistrictIdField;
    protected Boolean m_copyTeacherStatus;
    protected String m_copyTeacherMode;
    protected String m_districtId;
    private String m_epimsSR09;
    protected Pattern m_illegalNameCharacters;
    private Map<String, Collection<StaffPosition>> m_positionsMap;
    protected PlainDate m_reportDate;
    protected Map<String, List<ScheduleTeacher>> m_teacherSchedulesMap;
    protected Map<String, List<ScheduleTeacher>> m_teacherLYSchedulesMap;
    protected Map<String, ScheduleTerm> m_termMap;
    protected String m_wa06SchoolCode;
    protected String m_wa07MtcJobClassification;
    protected String m_wa07SfpJobClassification;
    protected String m_wa08MtcAssignment;
    protected String m_wa08SfpAssignment;
    protected String m_wa09MtcGrade;
    protected String m_wa09SfpGrade;
    protected String m_wa10MtcSubjectArea;
    protected String m_wa10SfpSubjectArea;
    protected String m_wa11MtcSection;
    protected String m_wa11SfpSection;
    protected String m_wa12MtcFullTimeEquivalent;
    protected String m_wa12SfpFullTimeEquivalent;
    protected String m_wa13MtcParaprofessional;
    protected String m_wa13SfpParaprofessional;
    protected String m_wa14MtcHighlyQualifiedStatus;
    protected String m_wa14SfpHighlyQualifiedStatus;
    protected String m_wa15MtcSubjectMatterCompetency;
    protected String m_wa15SfpSubjectMatterCompetency;
    private String m_sklDstrIdField = null;
    private List<String> m_includeSifSchoolIds = null;

    /**
     * Returns the student grade level of the assignments.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFTE implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String formattedFte = null;
            EpimsEntity epimsEntity = (EpimsEntity) entity;
            X2BaseBean assignment = epimsEntity.getAssignment();

            if (assignment instanceof StaffPosition) {
                formattedFte = "00";
                BigDecimal fte = (BigDecimal) getProperty(assignment, m_wa12SfpFullTimeEquivalent);

                if (fte != null) {
                    fte = fte.setScale(3, RoundingMode.HALF_UP);
                    formattedFte = fte.toString();
                }
            } else if (assignment instanceof ScheduleTeacher) {
                ScheduleTeacher teacherAssignment = (ScheduleTeacher) assignment;
                Boolean calcParam = (Boolean) getParameter(CALCULATE_FTES_PARAM);
                BigDecimal fte = epimsEntity.getCalculatedFte(teacherAssignment.getOid());
                if (!calcParam.booleanValue()) {
                    Object obj = getProperty(assignment, m_wa12MtcFullTimeEquivalent);

                    // We accept both BigDecimal or String here to account for differences
                    // in how the field is configured (User Type: Numeric vs. Character)
                    if (obj != null && obj.getClass().equals(BigDecimal.class)) {
                        fte = (BigDecimal) obj;
                    } else if (obj != null && obj.getClass().equals(String.class)
                            && !StringUtils.isEmpty((String) obj)) {
                        fte = new BigDecimal((String) obj);
                    } else {
                        fte = new BigDecimal(00);
                    }
                }

                formattedFte = fte.setScale(3, RoundingMode.HALF_UP).toString();
            }

            return formattedFte;
        }
    }

    /**
     * Returns the student grade level of the assignments.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLevel implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String gradeLevel = null;
            X2BaseBean assignment = ((EpimsEntity) entity).getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa09SfpGrade;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa09MtcGrade;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
            }
            if (path != null) {
                String value = (String) getProperty(assignment, path);
                if ((StringUtils.isEmpty(value))) {
                    // Only sfp gets a default.
                    if (assignment instanceof StaffPosition) {
                        gradeLevel = "00";
                    }
                } else {
                    gradeLevel = data.lookupStateValue(assignment.getClass(), path, value);
                }
            }

            return gradeLevel;
        }
    }

    /**
     * Returns the highly qualified status from the
     * assignment depending on whether the assignment is a staff position
     * or a teacher schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveHighlyQualifiedStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String qualifiedStatus = null;
            EpimsEntity epimsEntity = (EpimsEntity) entity;
            X2BaseBean assignment = epimsEntity.getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa14SfpHighlyQualifiedStatus;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
                qualifiedStatus = (String) getProperty(assignment, path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa14MtcHighlyQualifiedStatus;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
                if (m_copyTeacherStatus.booleanValue()) {
                    ScheduleTeacher teacherAssignment = (ScheduleTeacher) assignment;
                    TeacherInfo teacherInfo = epimsEntity.getCopiedTeacherInfo(teacherAssignment.getOid());
                    if (teacherInfo != null) {
                        qualifiedStatus = teacherInfo.getWA14HQStatus();
                    }
                } else {
                    qualifiedStatus = (String) getProperty(assignment, path);
                }
            }
            epimsEntity.setUpdateValue(field.getFieldId(), qualifiedStatus);

            return qualifiedStatus;
        }
    }

    /**
     * Returns the teacher / para-professional assignment from the
     * assignment depending on whether the assignment is a staff position
     * or a teacher schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveJobAssignment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String jobAssign = null;
            EpimsEntity epimsEntity = (EpimsEntity) entity;
            X2BaseBean assignment = epimsEntity.getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa08SfpAssignment;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
                jobAssign = (String) getProperty(assignment, path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa08MtcAssignment;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
                if (m_copyTeacherStatus.booleanValue()) {
                    ScheduleTeacher teacherAssignment = (ScheduleTeacher) assignment;
                    TeacherInfo teacherInfo = epimsEntity.getCopiedTeacherInfo(teacherAssignment.getOid());
                    if (teacherInfo != null) {
                        jobAssign = teacherInfo.getWA08Assignment();
                    }
                } else {
                    jobAssign = (String) getProperty(assignment, path);
                }
            }
            epimsEntity.setUpdateValue(field.getFieldId(), jobAssign);

            return jobAssign;
        }
    }

    /**
     * Returns the job classification from the
     * assignment depending on whether the assignment is a staff position
     * or a teacher schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveJobClassification implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String jobClass = null;
            EpimsEntity epimsEntity = (EpimsEntity) entity;
            X2BaseBean assignment = epimsEntity.getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa07SfpJobClassification;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
                jobClass = (String) getProperty(assignment, path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa07MtcJobClassification;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
                if (m_copyTeacherStatus.booleanValue()) {
                    ScheduleTeacher teacherAssignment = (ScheduleTeacher) assignment;
                    TeacherInfo teacherInfo = epimsEntity.getCopiedTeacherInfo(teacherAssignment.getOid());
                    if (teacherInfo != null) {
                        jobClass = teacherInfo.getWA07JobClassification();
                    }
                } else {
                    jobClass = (String) getProperty(assignment, path);
                }
            }
            epimsEntity.setUpdateValue(field.getFieldId(), jobClass);

            return jobClass;
        }
    }

    /**
     * Returns the NCLB instructional para-professional requirements from the
     * assignment depending on whether the assignment is a staff position
     * or a teacher schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveParaProfessional implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String paraProf = null;
            EpimsEntity epimsEntity = (EpimsEntity) entity;
            X2BaseBean assignment = epimsEntity.getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa13SfpParaprofessional;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
                paraProf = (String) getProperty(assignment, path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa13MtcParaprofessional;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
                if (m_copyTeacherStatus.booleanValue()) {
                    ScheduleTeacher teacherAssignment = (ScheduleTeacher) assignment;
                    TeacherInfo teacherInfo = epimsEntity.getCopiedTeacherInfo(teacherAssignment.getOid());
                    if (teacherInfo != null) {
                        paraProf = teacherInfo.getWA13Paraprofessional();
                    }
                } else {
                    paraProf = (String) getProperty(assignment, path);
                }
            }
            epimsEntity.setUpdateValue(field.getFieldId(), paraProf);

            return paraProf;
        }
    }

    /**
     * Retrieves the school code for the current assignment in the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            return getSchoolCode(((EpimsEntity) entity).getAssignment());
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * Returns the subject area of the assignments.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSubjectArea implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String subject = null;
            X2BaseBean assignment = ((EpimsEntity) entity).getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa10SfpSubjectArea;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa10MtcSubjectArea;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
            }
            if (path != null) {
                subject = (String) getProperty(assignment, path);
                if (assignment instanceof StaffPosition && StringUtils.isEmpty(subject)) {
                    // Default value only applies to StaffPosition, not to teacher schedule.
                    subject = "00000";
                }
            }

            return subject;
        }
    }

    /**
     * Returns the subject matter competency of the teacher for the subject area.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSubjectMatter implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String subject = null;
            EpimsEntity epimsEntity = (EpimsEntity) entity;
            X2BaseBean assignment = epimsEntity.getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa15SfpSubjectMatterCompetency;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
                subject = (String) getProperty(assignment, path);
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa15MtcSubjectMatterCompetency;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
                if (m_copyTeacherStatus.booleanValue()) {
                    ScheduleTeacher teacherAssignment = (ScheduleTeacher) assignment;
                    TeacherInfo teacherInfo = epimsEntity.getCopiedTeacherInfo(teacherAssignment.getOid());
                    if (teacherInfo != null) {
                        subject = teacherInfo.getWA15SubjectMatterCompetency();
                    }
                } else {
                    subject = (String) getProperty(assignment, path);
                }
            }
            epimsEntity.setUpdateValue(field.getFieldId(), subject);

            return subject;
        }
    }

    /**
     * Returns the course section of the assignment.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSection implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String section = null;
            X2BaseBean assignment = ((EpimsEntity) entity).getAssignment();
            String path = null;
            if (assignment instanceof StaffPosition) {
                path = m_wa11SfpSection;
                field.setBeanPath(SisStaff.REL_STAFF_POSITIONS + ModelProperty.PATH_DELIMITER + path);
                section = (String) data.getProperty(assignment, path);
                if (StringUtils.isEmpty(section)) {
                    section = DEFAULT_SECTION_CODE;
                }
            } else if (assignment instanceof ScheduleTeacher) {
                path = m_wa11MtcSection;
                field.setBeanPath(SisStaff.REL_SCHEDULE_TEACHERS + ModelProperty.PATH_DELIMITER + path);
                section = (String) data.getProperty(assignment, path);
            }

            return section;
        }
    }

    /**
     * Returns course term information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String termCode = null;
            X2BaseBean assignment = ((EpimsEntity) entity).getAssignment();

            // Default value (Full year) goes to staff positions.
            if (assignment instanceof ScheduleTeacher) {
                ScheduleTeacher schedule = (ScheduleTeacher) assignment;
                ScheduleTerm term = m_termMap.get(schedule.getSection().getScheduleTermOid());
                termCode = data.lookupStateValue(ScheduleTerm.class, ScheduleTerm.COL_CODE, term.getCode());
            }
            return termCode;
        }
    }

    /**
     * Returns course term information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            X2BaseBean assignment = ((EpimsEntity) entity).getAssignment();
            PlainDate reportDate = ((EpimsWork) data).m_reportDate;
            String status = null;

            // Default value (active) goes to staff positions.
            if (assignment instanceof ScheduleTeacher) {
                /*
                 * Identify if the scheduled class is in term or out of term
                 * based on term dates and report date.
                 */
                ScheduleTeacher schedule = (ScheduleTeacher) assignment;
                boolean inTerm = false;
                MasterSchedule section = schedule.getSection();
                for (ScheduleTermDate dates : section.getScheduleTerm().getScheduleTermDates()) {
                    if (!reportDate.after(dates.getEndDate()) && !reportDate.before(dates.getStartDate())) {
                        inTerm = true;
                        break;
                    }
                }

                /*
                 * Determine status from term.
                 * In term, between split terms or after end of term.
                 * (Before start of term should not be reported at all, would be a bug.)
                 */
                if (inTerm) {
                    status = "01"; // Active
                } else {
                    status = "02"; // Inactive
                }
            }

            return status;
        }
    }

    /**
     * Validate the assignment against classification, grade and subjects.
     * Combinations of Job Classification (WA 07) and Subject area (WA 10)
     * require specific ranges of assignmnet values.
     *
     * See DOE EPIMS errorlist.pdf
     * 
     *
     * @author X2 Development Corporation
     */
    protected class ValidateAssignment implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA07 = entity.getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
            String WA10 = entity.getFieldValue(WA_10_SFP_SUBJECT_AREA);
            // Rule 6129
            if ("2310".equals(WA07) && value != null && !(value.matches("005|006|012|014|020|215") ||
                    (value.compareTo("208") >= 0 && value.compareTo("304") <= 0))) {
                errors.add(new StateReportValidationError(entity, field,
                        "6129: Assignment WA08 invalid for Classification WA07",
                        "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA08=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6141
            if (WA07 != null && !WA07.matches("2305|2306|2307|2308|2310|2325|4100") && !"000".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6141: Classification WA07 requires Assignment WA08 to be " + STYLE_BOLD + "000" + STYLE_END,
                        "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA08=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6132
            if (WA07 != null && WA07.matches("2305|2306|2307|2308|2325") &&
                    WA10 != null && WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                    && !WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE)) {
                if (value != null && !value.matches("001|002|003|004|007|008|009|010|011|016|017|029")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6132: Classification WA07 and Subject WA10 require Assignment WA08",
                            "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END
                                    + ", WA08=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Rule 6133
            if (WA07 != null && WA07.matches("2305|2306|2307|2308|2325") &&
                    WA10 != null && (!WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                            || WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE))) {
                if (value != null && !value.matches("208|209|210|211|212|213|214|301|302|303|304")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6133: Classification WA07 and Subject WA10 require Assignment WA08",
                            "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END
                                    + ", WA08=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Rule 6136
            if ("2310".equals(WA07) && WA10 != null && WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                    && !WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE)) {
                if (value != null && !value.matches("005|006|014|020")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6136: Classification WA07 and Subject WA10 require Assignment WA08",
                            "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END
                                    + ", WA08=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate the course section is present for teacher assignments in k-12 grades.
     * Combinations of Job Classification (WA 07) (2305|2310|2325) and grade (WA 09)
     * require a course section.
     *
     * See DOE EPIMS errorlist.pdf
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCourseSection implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA07 = entity.getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
            String WA09 = entity.getFieldValue(WA_09_SFP_GRADE);
            if (WA07 != null && WA07.matches("2305|2310|2325") &&
                    !"00".equals(WA09) && "0".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Classification WA07 requires a Section WA11",
                        "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA11=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate FTE values from >0.0 to 1.0
     *
     * @author X2 Development Corporation
     */
    protected class ValidateFTE implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (value != null) {
                // Rule 6127
                float fte = Float.parseFloat(value);
                if (fte <= 0.0f || fte > 1.0f) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6127: FTE WA12 must be " + STYLE_BOLD + "1.0" + STYLE_END + " or less and greater than "
                                    + STYLE_BOLD + "0" + STYLE_END,
                            "WA12=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate grade level reported for job classification and assignment.
     * Grade level and subject area (WA 10) must exist or not exist together.
     * Classification WA07 (2305|2310|2325) require a grade level.
     * Classification WA07 (1305|1310|1320|3329|3330|3350|3351|3360|3361|3370|3371|5020|5021) cannot
     * have a grade level.
     * Grade level requires Assignments WA08.
     * 
     * See DOE EPIMS errorlist.pdf
     *
     * @author X2 Development Corporation
     */
    protected class ValidateGradeLevel implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA07 = entity.getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
            String WA08 = entity.getFieldValue(WA_08_SFP_ASSIGNMENT);
            String WA10 = entity.getFieldValue(WA_10_SFP_SUBJECT_AREA);
            // Rule 6101
            if (WA07 != null && WA07.matches("2305|2306|2307|2308|2310|2325") && "00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6101: Classification WA07 requires Grade level WA09",
                        "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA09=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6110
            if ((!"00".equals(value) && "00000".equals(WA10)) ||
                    ("00".equals(value) && !"00000".equals(WA10))) {
                errors.add(new StateReportValidationError(entity, field,
                        "6110: Grade level WA09 and Subject WA10 must be zero or not zero together.",
                        "WA09=" + STYLE_BOLD + value + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END));
            }
            // Rule 6111
            if (WA07 != null && WA07.matches("1305|1310|1320|3329|3330|3350|3351|3360|3361|3370|3371|5020|5021") &&
                    !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6111: Classification WA07 requires Grade level WA09 to be " + STYLE_BOLD + "00" + STYLE_END,
                        " WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA09=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6112
            if (WA07 != null && WA07.matches("1200|1201|1202|1205|1211|1212|1226") &&
                    !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6112: Classification WA07 requires WA09 to be " + STYLE_BOLD + "00" + STYLE_END,
                        "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA09=" + STYLE_BOLD + value + STYLE_END));
            }
            if ("000".equals(WA08) && !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Assignment WA08 requires Grade level WA09 to be " + STYLE_BOLD + "00" + STYLE_END,
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA09=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate job classification against school code and grade level, or assignment.
     * Combinations of school code (WA06), assignment (WA08), subject (WA10), qualification
     * and subject matter (WA14 and 15) require specific ranges of job classification.
     *
     * See DOE EPIMS errorlist.pdf
     *
     * @author X2 Development Corporation
     */
    protected class ValidateJobClassification implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA06 = entity.getFieldValue(WA_06_SCHOOL_CODE);
            String WA08 = entity.getFieldValue(WA_08_SFP_ASSIGNMENT);
            String WA10 = entity.getFieldValue(WA_10_SFP_SUBJECT_AREA);
            String WA14 = entity.getFieldValue(WA_14_SFP_HIGHLY_QUALIFIED_STATUS);
            String WA15 = entity.getFieldValue(WA_15_SFP_SUBJECT_MATTER_COMPETENCY);
            boolean isSchool = (WA06 != null && !WA06.substring(4).equals("0000"));
            // Rule 6105
            if (value != null
                    && value.matches(
                            "1305|1310|1320|2606|2307|2308|2310|2325|3329|3330|3350|3351|3360|3361|3370|3371|4100|5020|5021")
                    && !isSchool) {
                errors.add(new StateReportValidationError(entity, field,
                        "6105: Classification WA07 requires WA06 to be a School code",
                        "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA06=" + STYLE_BOLD + WA06 + STYLE_END));
            }
            // Rule 6112
            if (value != null && value.matches("1200|1201|1202|1205|1211|1212|1226") && isSchool) {
                errors.add(new StateReportValidationError(entity, field,
                        "6112: Classification WA07 requires WA06 to be a District code",
                        "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA06=" + STYLE_BOLD + WA06 + STYLE_END));
            }
            // Rule 6128
            if (value != null && value.matches("2305|2306|2307|2308|2310|2325|4100") && "000".equals(WA08)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6128: Classification WA07 invalid for Assignment WA08",
                        "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END));
            }
            // Rule 6138
            if ("2310".equals(value) && WA08 != null && WA08.matches("005|006|012")) {
                if ("00".equals(WA14) || !"00".equals(WA15)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6138: Classification WA07 and assignment WA08 require Qualified status WA14 and Subject matter WA15",
                            " WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END
                                    + ", WA14=" +
                                    STYLE_BOLD + WA14 + STYLE_END + ",\nWA15=" + STYLE_BOLD + WA15 + STYLE_END));
                }
            }
            // Rule 6139
            if ("2310".equals(value) && WA08 != null && !WA08.matches("005|006|012")) {
                if (!"00".equals(WA14) || !"00".equals(WA15)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6139: Classification WA07 and Assignment WA08 require Qualified status WA14 and Subject matter WA15",
                            " WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END
                                    + ", WA14=" +
                                    STYLE_BOLD + WA14 + STYLE_END + ",\nWA15=" + STYLE_BOLD + WA15 + STYLE_END));
                }
            }
            // Rule 6129
            if (!"2310".equals(value) && WA08 != null && WA08.matches("005|006|012|014|020|215")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6129: Classification WA07 invalid for Assignment WA08",
                        "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END));
            }
            // Rule 6130
            if (("4100".equals(value) && WA08 != null && !WA08.matches("40[123456]")) ||
                    (!"4100".equals(value) && WA08 != null && WA08.matches("40[123456]"))) {
                errors.add(new StateReportValidationError(entity, field,
                        "6130: Classification WA07 and Assignment WA08 required combination (4100/401-6)",
                        "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END));
            }
            // Rule 6140
            if ("4100".equals(value) && (!"00".equals(WA14) || !"00".equals(WA15))) {
                errors.add(new StateReportValidationError(entity, field,
                        "6140: Classification WA07 require Qualified status WA14 and Subject matter WA15 to be "
                                + STYLE_BOLD +
                                "00" + STYLE_END,
                        "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA14=" + STYLE_BOLD + WA14 + STYLE_END + ", WA15="
                                +
                                STYLE_BOLD + WA15 + STYLE_END));
            }
            // Rule 6131
            if (value != null && value.matches("2305|2306|2307|2308|2325") && WA08 != null &&
                    WA08.matches("001|002|003|004|007|008|009|010|011|016|017|029")) {
                if (WA10 != null && (!WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                        || WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6131: Classification WA07 and Assignment WA08 require Subject WA10 to be a core subject",
                            "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END +
                                    ", WA10=" + STYLE_BOLD + WA10 + STYLE_END));
                }
                if ("00".equals(WA14) || "00".equals(WA15)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6131: Classification WA07 and Assignment WA08 require Qualified status WA14 and Subject matter WA15",
                            "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END +
                                    ", WA14=" + STYLE_BOLD + WA14 + STYLE_END + ",\nWA15=" + STYLE_BOLD + WA15
                                    + STYLE_END));
                }
            }
            // Rule 6132
            if (value != null && value.matches("2305|2306|2307|2308|2325") &&
                    WA10 != null && WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                    && !WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE)) {
                if (!WA08.matches("001|002|003|004|007|008|009|010|011|016|017|029")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6132: Classification WA07 and Subject WA10 require Assignment WA08",
                            "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END + ", "
                                    +
                                    "WA08=" + STYLE_BOLD + WA08 + STYLE_END));
                }
                if ("00".equals(WA14) || "00".equals(WA15)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6132: Classification WA07 and Subject WA10 require Qualified status WA14 and Subject matter WA15",
                            "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END + ", "
                                    +
                                    "WA14=" + STYLE_BOLD + WA14 + STYLE_END + ",\n WA15=" + STYLE_BOLD + WA15
                                    + STYLE_END));
                }
            }
            // Rule 6133
            if (value != null && value.matches("2305|2306|2307|2308|2325") && WA08 != null &&
                    !WA08.matches("001|002|003|004|007|008|009|010|011|016|017|029")) {
                if (WA10 != null && WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                        && !WA10.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6133: Classification WA07 and Assignment WA08 require Subject WA10 to be non core subject"
                                    +
                                    STYLE_BOLD + "00" + STYLE_END,
                            "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END +
                                    ", WA10=" + STYLE_BOLD + WA10 + STYLE_END));
                }
                if (!"00".equals(WA14) || !"00".equals(WA15)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6133: Classification WA07 and Assignment WA08 require Qualified status WA14 and Subject matter WA15 to be "
                                    +
                                    STYLE_BOLD + "00" + STYLE_END,
                            "WA07=" + STYLE_BOLD + value + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END +
                                    ", WA14=" + STYLE_BOLD + WA14 + STYLE_END + ",\n WA15=" + STYLE_BOLD + WA15
                                    + STYLE_END));
                }
            }
            // Rule 6145
            if (WA08 != null && WA08.matches("012|215") && value != null && !value.matches("2310")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6145: Assignment WA08 requires Classification WA07 to be " + STYLE_BOLD + "2310" + STYLE_END,
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA07=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate para professional with job classification.
     * Classification WA07 and assignment WA08 determine requirements for para-prefessional
     * 
     * See DOE EPIMS errorlist.pdf
     *
     * @author X2 Development Corporation
     */
    protected class ValidateParaProfessional implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA07 = entity.getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
            String WA08 = entity.getFieldValue(WA_08_SFP_ASSIGNMENT);
            // Rule 6124
            if ((("4100".equals(WA07) && !"406".equals(WA08)) || "3323".equals(WA07)) && "00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6124: Para requirements WA13 must not be " + STYLE_BOLD + "00" + STYLE_END
                                + " for Classification WA07",
                        "WA07=" +
                                STYLE_BOLD + WA07 + STYLE_END + ", WA13=" + STYLE_BOLD + value + STYLE_END));
            }
            if (!"4100".equals(WA07) && !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Para requirements WA13 must be " + STYLE_BOLD + "00" + STYLE_END + " for Classification WA07",
                        "WA07=" +
                                STYLE_BOLD + WA07 + STYLE_END + ", WA13=" + STYLE_BOLD + value + STYLE_END));
            }
            if ("000".equals(WA08) && !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Assignment WA08 requires Para professional WA13 to be " + STYLE_BOLD + "00" + STYLE_END,
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA13=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate subject area code for grade level.
     * Combinations of classification (WA07), assignment (WA08), grade (WA09), qualification
     * and subject matter (WA14 and 15) require specific ranges of subject area.
     * 
     * See DOE EPIMS errorlist.pdf
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSubjectArea implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA07 = entity.getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
            String WA08 = entity.getFieldValue(WA_08_SFP_ASSIGNMENT);
            String WA09 = entity.getFieldValue(WA_09_SFP_GRADE);
            String WA13 = entity.getFieldValue(WA_13_SFP_PARAPROFESSIONAL);
            String WA14 = entity.getFieldValue(WA_14_SFP_HIGHLY_QUALIFIED_STATUS);
            String WA15 = entity.getFieldValue(WA_15_SFP_SUBJECT_MATTER_COMPETENCY);
            // Rule 6125
            if (WA09 != null && WA09.matches("PK|K|K01|0102|0[12345678]") && value != null
                    && !value.matches(REGEX_NON_SECONDARY_SUBJECT)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6125: Subject area WA10 must be non-secondary for Grade WA09",
                        "WA09=" + STYLE_BOLD + WA09 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6126
            if (WA09 != null && WA09.matches("09|1[0-2]") && value != null && !value.matches(REGEX_SECONDARY_SUBJECT)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6126: Subject area WA10 must be secondary for Grade WA09",
                        "WA09=" + STYLE_BOLD + WA09 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rules 6131, 6132
            if ((WA07 != null && WA07.matches("2305|2306|2307|2308|2325") &&
                    WA08 != null && WA08.matches("001|002|003|004|007|008|010|011|016|017|029")) ||
                    ("2310".equals(WA07) && WA08 != null && WA08.matches("005|006|012"))) {
                if (value != null && (!value.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                        || value.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6131: Classification WA07 and Assignment WA08 require Subject WA10 to be a core academic subject",
                            "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END
                                    + ", WA10=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Rule 6133
            if (WA07 != null && WA07.matches("2305|2306|2307|2308|2325") &&
                    WA08 != null && !WA08.matches("001|002|003|004|007|008|009|010|011|016|017|029")) {
                if (value != null && value.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                        && !value.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6133: Classification WA07 and Assignment WA08, Subject WA10 cannot be a core academic subject",
                            "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END
                                    + ", WA10=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Rule 6136
            if ("2310".equals(WA07) && WA08 != null && !WA08.matches("005|006|012|014|020")) {
                if (value != null && value.matches(REGEX_CORE_ACADEMIC_SUBJECT)
                        && !value.matches(REGEX_CORE_ACADEMIC_SUBJECT_EXCLUDE)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6136: Classification WA07 and Assignment WA08, Subject WA10 cannot be a core academic subject",
                            "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END
                                    + ", WA10=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Rule 6142
            if ("000".equals(WA08) && !WA07.equals("3323")) {
                if (!"00000".equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6142: Assignment WA08 requires Subject WA10 to be " + STYLE_BOLD + "00000" + STYLE_END,
                            "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
                }
                if (!"00".equals(WA13)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6142: Assignment WA08 requires Paraprofessional status WA13 to be " + STYLE_BOLD + "00"
                                    + STYLE_END,
                            "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA13=" + STYLE_BOLD + WA13 + STYLE_END));
                }
                if (!"00".equals(WA14)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6142: Assignment WA08 requires Qualified status WA14 to be " + STYLE_BOLD + "00"
                                    + STYLE_END,
                            "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA14=" + STYLE_BOLD + WA14 + STYLE_END));
                }
                if (!"00".equals(WA15)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "6142: Assignment WA08 requires Subject matter WA15 to be " + STYLE_BOLD + "00" + STYLE_END,
                            "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA15=" + STYLE_BOLD + WA15 + STYLE_END));
                }
            }
            // Rule 6144
            if (WA08 != null && WA08.matches("011") && value != null && !value.equals("01008")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6144: Assignment WA08 requires Subject WA10 to be " + STYLE_BOLD + "01008" + STYLE_END,
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6146
            if (WA08 != null && WA08.matches("010") && value != null && !value.matches("51008")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6146: Assignment WA08 requires Subject WA10 to be " + STYLE_BOLD + "51500" + STYLE_END,
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6148
            if (WA08 != null && WA08.matches("001|010|014|212|301") && value != null
                    && !value.matches(REGEX_NON_SECONDARY_SUBJECT)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6148: Assignment WA08 requires Subject WA10 to be a non-secondary course",
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6149
            if (WA08 != null && WA08.matches("002|011|020|208|302|303|213") && value != null
                    && !value.matches(REGEX_SECONDARY_SUBJECT)) {
                errors.add(new StateReportValidationError(entity, field,
                        "6149: Assignment WA08 requires Subject WA10 to be a secondary course",
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA10=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate subject matter competency for job classification and assignment and qualification.
     * Combinations of school code (WA06), assignment (WA08), subject (WA10), qualification
     * (WA14) require specific ranges of subject matter.
     * 
     * See DOE EPIMS errorlist.pdf
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSubjectMatter implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String WA07 = entity.getFieldValue(WA_07_SFP_JOB_CLASSIFICATION);
            String WA08 = entity.getFieldValue(WA_08_SFP_ASSIGNMENT);
            String WA10 = entity.getFieldValue(WA_10_SFP_SUBJECT_AREA);
            String WA14 = entity.getFieldValue(WA_14_SFP_HIGHLY_QUALIFIED_STATUS);
            // Rule 6109
            if ("01".equals(WA14) && !"2301".equals(WA07) && WA08 != null && !WA08.matches("00[56]") &&
                    value != null && !value.matches("0[234567]")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6109: Classification WA07 and Assignment WA08 require Subject WA15 to be " + STYLE_BOLD
                                + "02-07" + STYLE_END,
                        "WA07=" + STYLE_BOLD + WA07 + STYLE_END + ", WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA14="
                                +
                                STYLE_BOLD + WA14 + STYLE_END + ",\nWA15=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6109
            if (WA14 != null && !WA14.matches("0[12]") &&
                    value != null && value.matches("0[2-7]")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6109: Subject matter WA15 requires Qualified status WA14 to be " + STYLE_BOLD + "01-02"
                                + STYLE_END,
                        "WA14=" + STYLE_BOLD + WA14 + STYLE_END + ", WA15=" + STYLE_BOLD + value + STYLE_END));
            }
            // Rule 6113
            if (WA08 != null && WA08.matches("001") && WA10.matches("7302[89]|7303[012349]|73041") && value != null
                    && value.matches("0[3456]")) {
                errors.add(new StateReportValidationError(entity, field,
                        "6113: Assignment WA08 requires Subject matter WA15 must not be " + STYLE_BOLD + "03-06"
                                + STYLE_END,
                        "WA08=" + STYLE_BOLD + WA08 + STYLE_END + ", WA10=" + STYLE_BOLD + WA10 + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Returns heading text to include at the top of the export file.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // EX: EPIMS,WORK_ASSIGNMENT,02170000
        StringBuilder sb = new StringBuilder();
        sb.append("EPIMS,WORK_ASSIGNMENT,");

        String code = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        if (StringUtils.isEmpty(code)) {
            code = "[INSERT DISTRICT ID HERE]";
            addSetupError("Using a placeholder for the district ID.",
                    "Set the " + STYLE_BOLD + DOE_DISTRICT_ID + STYLE_END +
                            " alias in the Data Dictionary and update that field with the correct ID.");
        } else {
            sb.append(code);
        }

        sb.append("\n");

        return sb.toString();
    }

    /**
     * Gets the bean class.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getBeanClass()
     */
    @Override
    public Class getBeanClass() {
        return SisStaff.class;
    }

    /**
     * Gets the export title.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "EPIMS Work Assignment";
    }

    /**
     * Turn off header row in export file.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Turn off value wrappers in the export file.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueWrappers() {
        return false;
    }

    /**
     * Initialize.
     *
     * @see
     *      com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(com.follett.fsc.core.
     *      k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Initialize global variables.
         */
        initializeFields();

        /*
         * Set up formatters and reference lookup tables
         */
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        /*
         * Get core parameters
         * Retrieve the district DOE code or use the override value from the user input
         */
        m_districtId = (String) getParameter(DISTRICT_ID_PARAM);
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_copyTeacherStatus = (Boolean) getParameter(COPY_TEACHER_STATUS_PARAM);
        m_copyTeacherMode = (String) getParameter(COPY_TEACHER_MODE_PARAM);

        if (StringUtils.isEmpty(m_districtId)) {
            m_districtId = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        }

        /*
         * Define all export fields.
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(17);
        fieldDefinitions.add(getWA01_mepid());
        fieldDefinitions.add(getWA02_localId());
        fieldDefinitions.add(getWA03_firstName());
        fieldDefinitions.add(getWA04_middleName());
        fieldDefinitions.add(getWA05_lastName());
        fieldDefinitions.add(getWA06_schoolCode());
        fieldDefinitions.add(getWA07_jobClassification());
        fieldDefinitions.add(getWA08_assignment());
        fieldDefinitions.add(getWA09_gradeLevel());
        fieldDefinitions.add(getWA10_subjectArea());
        fieldDefinitions.add(getWA11_courseSection());
        fieldDefinitions.add(getWA12_fte());
        fieldDefinitions.add(getWA13_paraProfessional());
        fieldDefinitions.add(getWA14_qualifiedStatus());
        fieldDefinitions.add(getWA15_subjectMatter());
        fieldDefinitions.add(getWA16_scheduleTerm());
        fieldDefinitions.add(getWA17_scheduleStatus());
        setFieldDefinitions(fieldDefinitions);

        if (getSetupErrors().size() == 0) {
            /*
             * Build the query for the Staff records to export.
             */
            X2Criteria staffCriteria = getStaffCriteria();
            QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 1: // Local ID
                    staffQuery.addOrderByAscending(SisStaff.COL_LOCAL_ID);
                    break;

                case 2: // MEPID
                    staffQuery.addOrderByAscending(SisStaff.COL_STATE_ID);
                    break;

                default:
                    staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);
                    break;
            }

            // Set query and entity class for superclass.
            setQuery(staffQuery);
            setEntityClass(EpimsEntity.class);

            /*
             * Load auxilary data in maps keyed on staff OID for all staff included in the export.
             */
            int count = getBroker().getCount(staffQuery);
            SubQuery staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);

            /*
             * Build the staff positions and staff schedules to export.
             */
            loadPositions(staffSubQuery, count);
            loadTeacherSchedules(staffSubQuery, count);
        }
    }

    /**
     * After processing, handle global errors.
     * District ID validation.
     * School id validation.
     *
     * @return a Collection of StateReportValidationErrors
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

        if (StringUtils.isEmpty(m_districtId) || m_districtId.length() != 8) {
            errors.add(new StateReportValidationError("District",
                    "DOE MA",
                    "District Id is invalid.",
                    "The input 'DistrictID Override' needs " +
                            "to be the 8-digit DOE MA state code. If not using the 'DistrictId Override', " +
                            "alias 'DOE District ID' needs to reference the 8-digit state code"));
        }

        // Validating school id's are set up properly
        Collection<School> schools = getOrganization().getSchools();

        if (isSchoolContext()) {
            schools = new LinkedList<School>();
            schools.add(getSchool());
        }

        for (School school : schools) {
            if (!school.getInactiveIndicator() && !school.getArchiveIndicator()) {
                String schoolCode = (String) school.getFieldValueByAlias(DOE_15_SCHOOL);

                if (schoolCode == null || schoolCode.length() != 8 || !StringUtils.isNumeric(schoolCode)) {
                    errors.add(new StateReportValidationError(school.getName(),
                            "DOE 15",
                            "The state expects 8 character IDs.",
                            schoolCode));
                }
            }
        }
        return errors;
    }

    /**
     * Returns the collection of positions for a staff.
     *
     * @param staffOid String
     * @return a Collection of StaffPositions.
     */
    protected Collection getPositions(String staffOid) {
        return m_positionsMap.get(staffOid);
    }

    /**
     * Returns a collection of teacher schedules for a staff.
     *
     * @param staffOid String
     * @return a Collection of ScheduleTeacher
     */
    protected Collection getSchedules(String staffOid) {
        return m_teacherSchedulesMap.get(staffOid);
    }

    /**
     * Returns the state code of the school/district for the given assignment. The code is
     * determined in the following order:
     * <ol>
     * <li>The adjusted ID (StaffPosition assignments only)
     * <li>The state code of the school (if present)
     * <li>The district ID for this export
     * </ol>
     *
     * @param assignment X2BaseBean
     * @return String
     * @throws X2BaseException exception
     */
    protected String getSchoolCode(X2BaseBean assignment) throws X2BaseException {
        String code = null;
        SisSchool school = null;

        if (assignment instanceof StaffPosition) {
            code = (String) WebUtils.getProperty(assignment, m_adjustedDistrictIdField);
            school = ((StaffPosition) assignment).getSchool();
        } else {
            school = ((ScheduleTeacher) assignment).getSection().getSchedule().getSchool();
        }

        if (StringUtils.isEmpty(code)) {
            if (school == null) {
                code = m_districtId;
            } else {
                code = (String) getProperty(school, m_wa06SchoolCode);
            }
        }

        return code;
    }

    /**
     * Build Field definition for WA01, staff state identifier (MEPID).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA01_mepid() {
        FieldDefinition field = new FieldDefinition(WA_01_MEPID,
                SisStaff.COL_STATE_ID,
                null, false, 8, 8, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for WA02, staff local identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA02_localId() {
        FieldDefinition field = new FieldDefinition(WA_02_LOCALID,
                SisStaff.COL_LOCAL_ID,
                "0", false, 1, 20, REGEX_ALPHANUMERIC_HYPHEN,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for WA03, staff first name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA03_firstName() {
        FieldDefinition field = new FieldDefinition(WA_03_FIRST_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, false, 1, 30, REGEX_NAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for WA04, staff middle name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA04_middleName() {
        FieldDefinition field = new FieldDefinition(WA_04_MIDDLE_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME,
                NO_MIDDLE_NAME, false, 1, 30, REGEX_MNAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for WA05, staff last name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA05_lastName() {
        FieldDefinition field = new FieldDefinition(WA_05_LAST_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, false, 1, 30, REGEX_NAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for WA06, school code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA06_schoolCode() {
        FieldDefinition field = new FieldDefinition(WA_06_SCHOOL_CODE,
                LABEL_PREFIX_CHAR + "School Code",
                null, false, 8, 8, REGEX_ALPHANUMERIC,
                null, new RetrieveSchoolCode(), null, null);
        return field;
    }

    /**
     * Build Field definition for WA07, job classification.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA07_jobClassification() {
        FieldDefinition field = new FieldDefinition(WA_07_SFP_JOB_CLASSIFICATION,
                null,
                "2305", true, 4, 4, REGEX_ALPHANUMERIC,
                null, new RetrieveJobClassification(),
                new ValidateJobClassification(), null);
        return field;
    }

    /**
     * Build Field definition for WA08, teacher/para-professional assignment.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA08_assignment() {
        FieldDefinition field = new FieldDefinition(WA_08_SFP_ASSIGNMENT,
                null,
                "000", true, 3, 3, REGEX_ALPHANUMERIC,
                null, new RetrieveJobAssignment(), null, null);
        return field;
    }

    /**
     * Build Field definition for WA09, grade level of assignment.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA09_gradeLevel() {
        FieldDefinition field = new FieldDefinition(WA_09_SFP_GRADE,
                null,
                null, false, 1, 4, REGEX_ALL_GRADE_LEVELS,
                null, new RetrieveGradeLevel(), new ValidateGradeLevel(), null);
        return field;
    }

    /**
     * Build Field definition for WA10, subject area.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA10_subjectArea() {
        FieldDefinition field = new FieldDefinition(WA_10_SFP_SUBJECT_AREA,
                null,
                null, false, 5, 7, REGEX_ALPHANUMERIC,
                null, new RetrieveSubjectArea(),
                new ValidateSubjectArea(), null);
        return field;
    }

    /**
     * Build Field definition for WA11, course section.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA11_courseSection() {
        FieldDefinition field = new FieldDefinition(WA_11_SFP_SECTION,
                null,
                null, false, 1, 20, REGEX_SECTION,
                null, new RetrieveSection(),
                new ValidateCourseSection(), null);
        return field;
    }

    /**
     * Build Field definition for WA12, full time equivalent.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA12_fte() {
        FieldDefinition field = new FieldDefinition(WA_12_MTC_FTE,
                LABEL_PREFIX_CHAR + "Full Time Equivalency",
                null, false, 2, 5, REGEX_NUMERIC_DECIMAL,
                null, new RetrieveFTE(),
                new ValidateFTE(), null);
        return field;
    }

    /**
     * Build Field definition for WA13, NCLB para professional requirements.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA13_paraProfessional() {
        FieldDefinition field = new FieldDefinition(WA_13_SFP_PARAPROFESSIONAL,
                null,
                "00", true, 2, 2, "0[012345]",
                null, new RetrieveParaProfessional(),
                new ValidateParaProfessional(), null);
        return field;
    }

    /**
     * Build Field definition for WA14, highly qualified status.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA14_qualifiedStatus() {
        FieldDefinition field = new FieldDefinition(WA_14_SFP_HIGHLY_QUALIFIED_STATUS,
                null,
                "00", true, 2, 2, "0[012]",
                null, new RetrieveHighlyQualifiedStatus(), null, null);
        return field;
    }

    /**
     * Build Field definition for WA15, subject matter competency.
     * Bean path is null initially. It will be set in the retriever for each row.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA15_subjectMatter() {
        FieldDefinition field = new FieldDefinition(WA_15_SFP_SUBJECT_MATTER_COMPETENCY,
                null,
                "00", true, 2, 2, "0[01234567]",
                null, new RetrieveSubjectMatter(),
                new ValidateSubjectMatter(), null);
        return field;
    }

    /**
     * Build Field definition for WA16, schedule term.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA16_scheduleTerm() {
        FieldDefinition field = new FieldDefinition(WA_16_MTC_SCHEDULE_TERM,
                LABEL_PREFIX_CHAR + "Schedule Term",
                "01", false, 2, 2, "01|2[12]|3[1-5]|[45][1-6]|6[1-9]|7[89]|[89]0",
                null, new RetrieveTermCode(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for WA17, schedule status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getWA17_scheduleStatus() {
        FieldDefinition field = new FieldDefinition(WA_17_MTC_SCHEDULE_STATUS,
                LABEL_PREFIX_CHAR + "Schedule Status",
                "01", false, 2, 2, "0[12]",
                null, new RetrieveTermStatus(),
                null, null);
        return field;
    }

    /**
     * Returns the criteria that returns all staff members that should be included in the export.
     * <br>
     * For school selection, must consider secondary school schedules and secondary chool positions.
     *
     * @return X2Criteria
     */
    private X2Criteria getStaffCriteria() {
        X2Criteria criteria = new X2Criteria();

        // Teacher Schedules
        X2Criteria subCriteria = new X2Criteria();
        if (isSchoolContext()) {
            // If a school context is selected, include staff in that school
            // as well as staff with schedule or positions in the school.
            X2Criteria primaryCriteria = new X2Criteria();
            X2Criteria scheduleCriteria = new X2Criteria();
            X2Criteria positionCriteria = new X2Criteria();
            X2Criteria orCriteria = new X2Criteria();
            primaryCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());


            subCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
            subCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            MasterSchedule.COL_SCHEDULE_OID);
            subCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_CLASS);

            SubQuery subQuery = new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_STAFF_OID, subCriteria);
            scheduleCriteria.addIn(X2BaseBean.COL_OID, subQuery);

            // Staff positions
            subCriteria = new X2Criteria();

            Criteria endDateCriteria = new Criteria();
            Criteria endDateCriteria2 = new Criteria();
            endDateCriteria.addIsNull(StaffPosition.COL_END_DATE);
            endDateCriteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, m_reportDate);
            endDateCriteria.addOrCriteria(endDateCriteria2);

            Criteria startDateCriteria = new Criteria();
            Criteria startDateCriteria2 = new Criteria();
            startDateCriteria.addIsNull(StaffPosition.COL_START_DATE);
            startDateCriteria2.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_reportDate);
            startDateCriteria.addOrCriteria(startDateCriteria2);

            subCriteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getSchool().getOid());
            subCriteria.addAndCriteria(endDateCriteria);
            subCriteria.addAndCriteria(startDateCriteria);

            subQuery = new SubQuery(StaffPosition.class, StaffPosition.COL_STAFF_OID, subCriteria);
            positionCriteria.addIn(X2BaseBean.COL_OID, subQuery);

            orCriteria.addOrCriteria(primaryCriteria);
            orCriteria.addOrCriteria(scheduleCriteria);
            orCriteria.addOrCriteria(positionCriteria);
            criteria.addAndCriteria(orCriteria);
        } else if (m_includeSifSchoolIds != null) {
            criteria.addIn(StaffPosition.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + m_sklDstrIdField, m_includeSifSchoolIds);
        }

        X2Criteria sr09Criteria = new X2Criteria();
        sr09Criteria.addNotEmpty(m_epimsSR09, getBroker().getPersistenceKey());
        criteria.addAndCriteria(sr09Criteria);

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // Bargaining unit
                criteria.addEqualTo(SisStaff.COL_BARGAINING_UNIT, queryString);
                break;

            case 2: // Local ID
                criteria.addPatternMatch(SisStaff.COL_LOCAL_ID, queryString, false);
                break;

            case 3: // MEPID
                criteria.addPatternMatch(SisStaff.COL_STATE_ID, queryString, false);
                break;

            case 4: // Snapshot
                criteria.addIn(X2BaseBean.COL_OID,
                        ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool()));
                break;

            default:
                // Take all staff in the district
                break;
        }

        return criteria;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields and the "EPIMS Status" field.
     */
    private void initializeFields() {
        m_sklDstrIdField = translateAliasToJavaName(ALIAS_SKL_SIF_DISTRICT_ID, true);
        // Load Input Definition Parameters
        String includeIds = (String) getParameter(PARAM_INCLUDE_SIF_SCHOOL);
        if (!StringUtils.isEmpty(includeIds)) {
            List<String> rcdOids = new ArrayList<String>(Arrays.asList(includeIds.split(SEPARATOR_COMMA)));
            X2Criteria sifDistrIdCriteria = new X2Criteria();
            sifDistrIdCriteria.addIn(X2BaseBean.COL_OID, rcdOids);
            QueryByCriteria byCriteria = new QueryByCriteria(ReferenceCode.class, sifDistrIdCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(byCriteria);
            m_includeSifSchoolIds = new ArrayList();
            for (ReferenceCode code : refCodes) {
                m_includeSifSchoolIds.add(code.getCode());
            }
        }
        m_adjustedDistrictIdField = translateAliasToJavaName(ADJUSTED_DISTRICT_ID, true);
        m_epimsSR09 = translateAliasToJavaName(EPIMS_SR09, true);

        m_wa06SchoolCode = translateAliasToJavaName(DOE_15_SCHOOL, true);

        /*
         * Teacher schedule based properties
         */
        m_wa07MtcJobClassification = translateAliasToJavaName(WA_07_MTC_JOB_CLASSIFICATION, true);
        m_wa08MtcAssignment = translateAliasToJavaName(WA_08_MTC_ASSIGNMENT, true);

        m_wa09MtcGrade = ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                Course.COL_GRADE_LEVEL;

        m_wa10MtcSubjectArea = ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                translateAliasToJavaName(WA_10_MTC_SUBJECT_AREA, true);

        m_wa11MtcSection = ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW;

        m_wa12MtcFullTimeEquivalent = translateAliasToJavaName(WA_12_MTC_FTE, true);
        m_wa13MtcParaprofessional = translateAliasToJavaName(WA_13_MTC_PARAPROFESSIONAL, true);
        m_wa14MtcHighlyQualifiedStatus = translateAliasToJavaName(WA_14_MTC_HIGHLY_QUALIFIED_STATUS, true);
        m_wa15MtcSubjectMatterCompetency = translateAliasToJavaName(WA_15_MTC_SUBJECT_MATTER_COMPETENCY, true);

        /*
         * Position based properties
         */
        m_wa07SfpJobClassification = translateAliasToJavaName(WA_07_SFP_JOB_CLASSIFICATION, true);
        m_wa08SfpAssignment = translateAliasToJavaName(WA_08_SFP_ASSIGNMENT, true);
        m_wa09SfpGrade = translateAliasToJavaName(WA_09_SFP_GRADE, true);
        m_wa10SfpSubjectArea = translateAliasToJavaName(WA_10_SFP_SUBJECT_AREA, true);
        m_wa11SfpSection = translateAliasToJavaName(WA_11_SFP_SECTION, true);
        m_wa12SfpFullTimeEquivalent = StaffPosition.COL_FTE;
        m_wa13SfpParaprofessional = translateAliasToJavaName(WA_13_SFP_PARAPROFESSIONAL, true);
        m_wa14SfpHighlyQualifiedStatus = translateAliasToJavaName(WA_14_SFP_HIGHLY_QUALIFIED_STATUS, true);
        m_wa15SfpSubjectMatterCompetency = translateAliasToJavaName(WA_15_SFP_SUBJECT_MATTER_COMPETENCY, true);

        // Get a map of all terms
        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class);
        m_termMap = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 30);
    }

    /**
     * Populates the map of staff OIDs to StaffPosition beans. Only current positions are included.
     *
     * @param staffSubQuery SubQuery
     * @param staffCount the number of staff matching the subquery
     */
    private void loadPositions(SubQuery staffSubQuery, int staffCount) {
        /*
         * End date is either null or on/after the report date
         */
        Criteria endDateCriteria = new Criteria();
        endDateCriteria.addIsNull(StaffPosition.COL_END_DATE);

        Criteria endDateCriteria2 = new Criteria();
        endDateCriteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, m_reportDate);

        endDateCriteria.addOrCriteria(endDateCriteria2);

        /*
         * Start date is either null or on/before the report date
         */
        Criteria startDateCriteria = new Criteria();
        startDateCriteria.addIsNull(StaffPosition.COL_START_DATE);

        Criteria startDateCriteria2 = new Criteria();
        startDateCriteria2.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_reportDate);

        startDateCriteria.addOrCriteria(startDateCriteria2);

        /*
         * The full criteria includes dates and the subquery
         */
        Criteria positionCriteria = new Criteria();
        positionCriteria.addIn(StaffPosition.COL_STAFF_OID, staffSubQuery);
        positionCriteria.addAndCriteria(endDateCriteria);
        positionCriteria.addAndCriteria(startDateCriteria);

        /*
         * Exclusion criteria
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        String excludeProperty = dictionary.findDataDictionaryFieldByAlias(SFP_EXCLUDE_STATUS).getJavaName();
        X2Criteria exclusionCriteria = new X2Criteria();
        exclusionCriteria.addEmpty(excludeProperty, getBroker().getPersistenceKey());
        exclusionCriteria.addOrEqualTo(excludeProperty, BooleanAsStringConverter.FALSE);

        positionCriteria.addAndCriteria(exclusionCriteria);

        QueryByCriteria positionQuery = new QueryByCriteria(StaffPosition.class, positionCriteria);

        // The order isn't really important, we just want it to be consistent across runs
        positionQuery.addOrderByAscending(StaffPosition.COL_SUBJECT_CODE);
        positionQuery.addOrderByAscending(X2BaseBean.COL_OID);

        m_positionsMap =
                getBroker().getGroupedCollectionByQuery(positionQuery, StaffPosition.COL_STAFF_OID, staffCount);
    }

    /**
     * Populates the map of staff OIDs to ScheduleTeacher beans. All assignments in an active
     * schedule, even those for a non-current term, are included.
     * <p>
     * This method also loads the maps of master OID to MasterTerm beans and master term OID to
     * matrix counts.
     *
     * @param staffSubQuery SubQuery
     * @param staffCount the number of staff matching the subquery
     */
    private void loadTeacherSchedules(SubQuery staffSubQuery, int staffCount) {
        /*
         * Map 1: Staff OIDs to collections of ScheduleTeacher beans
         */
        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

        Criteria scheduleCriteria = new Criteria();
        scheduleCriteria.addIn(ScheduleTeacher.COL_STAFF_OID, staffSubQuery);

        scheduleCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);

        // filter out non-class courses
        scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + "." + MasterSchedule.REL_SCHOOL_COURSE + "."
                + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // filter out terms not currently in session
        Collection list = getAllTermOids(schoolCriteria);

        if (!list.isEmpty()) {
            Criteria termCriteria1 = new Criteria();
            X2Criteria termCriteria2 = new X2Criteria();
            termCriteria1.addIn(ScheduleTeacher.COL_SCHEDULE_TERM_OID, list);
            termCriteria2.addEmpty(ScheduleTeacher.COL_SCHEDULE_TERM_OID, getBroker().getPersistenceKey());
            termCriteria2.addIn(ScheduleTeacher.REL_SECTION + "." + MasterSchedule.COL_SCHEDULE_TERM_OID, list);
            termCriteria1.addOrCriteria(termCriteria2);
            scheduleCriteria.addAndCriteria(termCriteria1);
        }

        /*
         * Exclusion criteria:
         * Teacher exclusion criteria
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        String excludeProperty = dictionary.findDataDictionaryFieldByAlias(SMT_EXCLUDE_STATUS).getJavaName();
        X2Criteria exclusionCriteria = new X2Criteria();
        exclusionCriteria.addEmpty(excludeProperty, getBroker().getPersistenceKey());
        exclusionCriteria.addOrEqualTo(excludeProperty, BooleanAsStringConverter.FALSE);

        scheduleCriteria.addAndCriteria(exclusionCriteria);

        /*
         * Course exclusion criteria
         */
        DataDictionaryField ddField = dictionary.findDataDictionaryFieldByAlias(CRS_EXCLUDE_STATUS);
        if (ddField != null) {
            excludeProperty = ddField.getJavaName();
            exclusionCriteria = new X2Criteria();
            exclusionCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    excludeProperty, BooleanAsStringConverter.TRUE);
            scheduleCriteria.addAndCriteria(exclusionCriteria);
        }

        QueryByCriteria scheduleQuery = new QueryByCriteria(ScheduleTeacher.class, scheduleCriteria);


        // The order isn't really important, we just want it to be consistent across runs
        scheduleQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + "." + MasterSchedule.COL_COURSE_VIEW);
        scheduleQuery.addOrderByAscending(X2BaseBean.COL_OID);

        m_teacherSchedulesMap =
                getBroker().getGroupedCollectionByQuery(scheduleQuery, ScheduleTeacher.COL_STAFF_OID, staffCount);

        /*
         * Map 2: If "Copy Teacher Status" is set, build a map of teacher schedules for
         * last year by teacher.
         */
        if (m_copyTeacherStatus.booleanValue()) {
            // Find SchoolYearContext for LY.
            DistrictSchoolYearContext contextTY = getCurrentContext();
            DistrictSchoolYearContext contextLY = null;
            if (contextTY != null) {
                Criteria syCriteria = new Criteria();
                syCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(contextTY.getSchoolYear() - 1));
                QueryByCriteria syQuery = new QueryByCriteria(DistrictSchoolYearContext.class, syCriteria);
                contextLY = (DistrictSchoolYearContext) getBroker().getBeanByQuery(syQuery);
            }

            // Select ScheduleTeacher for last year based on SchoolScheduleContext and District
            // School Year Context.
            if (contextLY != null) {
                scheduleCriteria = new Criteria();
                scheduleCriteria.addIn(ScheduleTeacher.COL_STAFF_OID, staffSubQuery);

                scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        contextLY.getOid());

                // filter out non-class courses
                scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + "." + MasterSchedule.REL_SCHOOL_COURSE + "."
                        + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

                scheduleQuery = new QueryByCriteria(ScheduleTeacher.class, scheduleCriteria);

                m_teacherLYSchedulesMap = getBroker().getGroupedCollectionByQuery(scheduleQuery,
                        ScheduleTeacher.COL_STAFF_OID, staffCount);
            }
        }
    }

    /**
     * For all the schools returned by the given criteria,
     * get the Term oids which are active today. Returns
     * all reports in the list.
     *
     * @param schoolCriteria Criteria
     * @return Collection
     */
    private Collection getAllTermOids(Criteria schoolCriteria) {

        ArrayList list = new ArrayList();

        QueryByCriteria query = new QueryByCriteria(SisSchool.class, schoolCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisSchool school = (SisSchool) iterator.next();

                list.addAll(getTermOids(school.getActiveScheduleOid(), m_reportDate));
            }
        } finally {
            iterator.close();
        }

        return list;
    }

    /**
     * Return all schedule terms for the school that exist on or before the report date.
     * Similar to the ScheduleManager function but also returns before the specified date.
     *
     * @param scheduleOid String
     * @param date PlainDate
     * @return Collection<trmOid>
     */
    private Collection<String> getTermOids(String scheduleOid, PlainDate date) {
        Criteria termCriteria = new Criteria();
        termCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, date);
        termCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + "."
                + ScheduleTerm.COL_SCHEDULE_OID, scheduleOid);

        SubQuery termQuery = new SubQuery(ScheduleTermDate.class,
                ScheduleTermDate.COL_SCHEDULE_TERM_OID, termCriteria);

        return getBroker().getSubQueryCollectionByQuery(termQuery);
    }
}
