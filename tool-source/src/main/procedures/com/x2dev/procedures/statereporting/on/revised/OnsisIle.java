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
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSection.ClassSession;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSection.ClassSessionPeriod;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentPeriodAttendance;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection.ConedRegisterInfo;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisIle extends OnsisStateReportData {

    /**
     * The Class OnsisIleInfo.
     */
    public static class OnsisIleInfo {
        public static final String OTHER_LANGUAGE_DESC_DELIMETER = "|";
        private static final List<String> PUPIL_OF_BOARD_CODES = Arrays.asList("01", "09");

        /**
         * The Class OnsisIleSection.
         */
        class OnsisIleRecord {
            private String m_campus;
            private String m_internationalLanguage;
            private List<OnSection> m_sections = new LinkedList();
            private Set<OnsisStudent> m_students = new HashSet();
            private String m_timeOfDay;

            /**
             * Instantiates a new onsis ile section.
             *
             * @param campus String
             * @param internationalLanguage String
             * @param timeOfDay String
             */
            public OnsisIleRecord(String campus, String internationalLanguage, String timeOfDay) {
                this.m_campus = campus;
                this.m_internationalLanguage = internationalLanguage;
                this.m_timeOfDay = timeOfDay == null ? "" : timeOfDay;
            }

            /**
             * Adds the section.
             *
             * @param mst MasterSchedule
             */
            public void addSection(OnSection mst) {
                X2Broker broker = getReportData().getBroker();
                Map<String, List<ToolStudentScheduleChange>> changes =
                        mst.getStudentScheduleChanges(broker).stream()
                                .collect(Collectors.groupingBy(scc -> scc.getStudentOid()));
                Map<String, List<ToolStudentSchedule>> schedules =
                        mst.getStudentSchedules(broker).stream()
                                .collect(Collectors.groupingBy(ssc -> ssc.getStudentOid()));
                Stream.concat(mst.getStudentScheduleChanges(broker).stream().map(scc -> scc.getStudentOid()),
                        mst.getStudentSchedules(broker).stream().map(ssc -> ssc.getStudentOid()))
                        .distinct()
                        .map(oid -> ToolBean.getBeanByOid(broker, OnsisStudent.class, oid))
                        .filter(Objects::nonNull)
                        .forEach(std -> {
                            ToolStudentSchedule schedule = null;
                            if (schedules != null && schedules.containsKey(std.getOid())) {
                                schedule = schedules.get(std.getOid()).get(0);
                            }
                            List<ToolStudentScheduleChange> changeList = Collections.EMPTY_LIST;
                            if (changes != null && changes.containsKey(std.getOid())) {
                                changeList = changes.get(std.getOid());
                            }
                            if (!std.getStudentScheduleSpans(broker, schedule, changeList).isEmpty()) {
                                addStudent(std);
                            }
                        });

                m_sections.add(mst);
            }

            /**
             * Adds the student.
             *
             * @param student SisStudent
             */
            public void addStudent(OnsisStudent student) {
                m_students.add(student);
            }

            /**
             * Equals.
             *
             * @param obj Object
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                if (this == obj) {
                    return true;
                }
                if (obj == null) {
                    return false;
                }
                if (getClass() != obj.getClass()) {
                    return false;
                }
                OnsisIleRecord other = (OnsisIleRecord) obj;
                if (m_campus == null) {
                    if (other.m_campus != null) {
                        return false;
                    }
                } else if (!m_campus.equals(other.m_campus)) {
                    return false;
                }
                if (m_internationalLanguage == null) {
                    if (other.m_internationalLanguage != null) {
                        return false;
                    }
                } else if (!m_internationalLanguage.equals(other.m_internationalLanguage)) {
                    return false;
                }
                if (m_timeOfDay == null) {
                    if (other.m_timeOfDay != null) {
                        return false;
                    }
                } else if (!m_timeOfDay.equals(other.m_timeOfDay)) {
                    return false;
                }
                return true;
            }

            /**
             * Gets the campus.
             *
             * @return the campus
             */
            public String getCampus() {
                return m_campus;
            }

            /**
             * Gets the international language.
             *
             * @return the internationalLanguage
             */
            public String getInternationalLanguage() {
                return m_internationalLanguage;
            }

            /**
             * Gets the sections.
             *
             * @return List
             */
            public List<OnSection> getSections() {
                return m_sections;
            }

            /**
             * Gets the students.
             *
             * @return Collection
             */
            public Collection<OnsisStudent> getStudents() {
                return m_students;
            }

            /**
             * Gets the time of day.
             *
             * @return the timeOfDay
             */
            public String getTimeOfDay() {
                return m_timeOfDay;
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                final int prime = 31;
                int result = 1;
                result = prime * result + ((m_campus == null) ? 0 : m_campus.hashCode());
                result = prime * result + ((m_internationalLanguage == null) ? 0 : m_internationalLanguage.hashCode());
                result = prime * result + ((m_timeOfDay == null) ? 0 : m_timeOfDay.hashCode());
                return result;
            }

            /**
             * To string.
             *
             * @return String
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                output.append("Campus: ");
                output.append(m_campus);
                output.append(" Language: ");
                output.append(m_internationalLanguage);
                output.append(" TimeOfDay: ");
                output.append(m_timeOfDay);
                output.append(" Sections: [");
                Iterator<OnSection> sectionIterator = m_sections.iterator();
                while (sectionIterator.hasNext()) {
                    OnSection section = sectionIterator.next();
                    output.append(
                            StringUtils.isEmpty(section.getCourseView()) ? section.getOid() : section.getCourseView());
                    if (sectionIterator.hasNext()) {
                        output.append(",");
                    }
                }
                output.append("] Students: [");
                Iterator<OnsisStudent> studentIterator = m_students.iterator();
                while (studentIterator.hasNext()) {
                    OnsisStudent student = studentIterator.next();
                    output.append(
                            StringUtils.isEmpty(student.getNameView()) ? student.getOid()
                                    : "\"" + student.getNameView() + "\"");
                    if (studentIterator.hasNext()) {
                        output.append(",");
                    }
                }
                output.append("]");
                return output.toString();
            }
        }

        /**
         * The Class OnsisIleProgramCount.
         */
        public class OnsisIleProgramCount {
            private int cntClass;
            private int cntMinutes;
            private int cntInstructor;
            private int cntPupilOfBoard;
            private int cntPupilOther;
            private Set<String> sectionKeys = new HashSet();

            /**
             * Instantiates a new onsis ile program count.
             *
             * @param campusNumber String
             * @param internationalLanguage String
             */
            public OnsisIleProgramCount(String campusNumber, String internationalLanguage) {
                if (m_mapIleRecords.containsKey(campusNumber)) {
                    m_mapIleRecords.get(campusNumber).values().stream()
                            .filter(record -> record.getInternationalLanguage().equals(internationalLanguage))
                            .forEach(this::incrementCounts);
                }
            }

            /**
             * Gets the class count.
             *
             * @return Long
             */
            public Long getClassCount() {
                Long value = Long.valueOf(cntClass);
                return value;
            }

            /**
             * Gets the hour count.
             *
             * @return Long
             */
            public Double getHourCount() {
                double minutes = cntMinutes;
                return minutes / 60;
            }

            /**
             * Gets the instructor count.
             *
             * @return Long
             */
            public Long getInstructorCount() {
                Long value = Long.valueOf(cntInstructor);
                return value;
            }

            /**
             * Gets the student count.
             *
             * @return Long
             */
            public Long getStudentCount() {
                Long value = Long.valueOf(cntPupilOfBoard + cntPupilOther);
                return value;
            }

            /**
             * Gets the student other count.
             *
             * @return Long
             */
            public Long getStudentOtherCount() {
                Long value = Long.valueOf(cntPupilOther);
                return value;
            }

            /**
             * Gets the student pupil of board count.
             *
             * @return Long
             */
            public Long getStudentPupilOfBoardCount() {
                Long value = Long.valueOf(cntPupilOfBoard);
                return value;
            }

            /**
             * Gets the section key.
             *
             * @param mst MasterSchedule
             * @return String
             */
            private String getSectionKey(OnSection mst) {
                return StringUtils.isEmpty(mst.getSectionClassOid()) ? mst.getOid() : mst.getSectionClassOid();
            }

            /**
             * Increment class count.
             *
             * @param mst MasterSchedule
             */
            private void incrementClassCount(OnSection mst) {
                String key = getSectionKey(mst);
                if (!sectionKeys.contains(key)) {
                    ++cntClass;
                    sectionKeys.add(key);
                }
            }

            /**
             * Increment counts.
             *
             * @param record OnsisIleRecord
             */
            private void incrementCounts(OnsisIleRecord record) {
                record.getSections().stream().forEach(this::incrementClassCount);
                record.getSections().stream().forEach(this::incrementMinuteCount);
                record.getSections().stream().forEach(this::incrementInstructorCount);
                record.getStudents().stream().forEach(this::incrementStudentCount);
            }

            /**
             * Increment hour count.
             *
             * @param mst MasterSchedule
             */
            private void incrementMinuteCount(OnSection mst) {
                X2Broker broker = m_reportData.getBroker();
                ToolSchoolCalendar calendar = mst.getSchedule(broker).getCalendar(broker);
                if (calendar != null) {
                    List<ClassSession> sessions = mst.getClassSessions(calendar.getCalendarId(), broker);
                    int overrideMinutes = mst.getOverrideMinutes();
                    Filterable<ToolStudentPeriodAttendance> stdAttendances = mst.getStudentPeriodAttendances(broker);
                    for (ClassSession session : sessions) {
                        List<ToolStudentPeriodAttendance> attendancies =
                                stdAttendances.getGroup(ToolStudentPeriodAttendance.FIELD_DATE, session.getPlainDate());
                        List<ToolStudentPeriodAttendance> nonCEDAttendancies = attendancies.stream()
                                .filter(att -> !ConedRegisterInfo.ATT_CODE_CON_ED_CANCELLED_UNFUNDED
                                        .equals(att.getOtherCode())
                                        && !ConedRegisterInfo.ATT_CODE_CON_ED_CANCELLED_UNFUNDED
                                                .equals(att.getOtherCode02()))
                                .collect(Collectors.toList());
                        if (attendancies.isEmpty() || !nonCEDAttendancies.isEmpty()) {
                            if (overrideMinutes == 0) {
                                for (ClassSessionPeriod period : session.getPeriods()) {
                                    cntMinutes += period.getDuration();
                                }
                            } else {
                                cntMinutes += overrideMinutes;
                            }
                        }
                    }
                }
            }

            /**
             * Increment instructor count.
             *
             * @param mst MasterSchedule
             */
            private void incrementInstructorCount(OnSection mst) {
                X2Broker broker = m_reportData.getBroker();
                mst.getTeacherSections(broker).forEach(mtc -> ++cntInstructor);
            }

            /**
             * Increment student count.
             *
             * @param student SisStudent
             */
            private void incrementStudentCount(OnsisStudent student) {
                GlobalData globalData = m_reportData.getGlobalData();
                X2Broker broker = m_reportData.getBroker();
                List<ValidationError> errors = new ArrayList<>();
                List<AnnualSpan> annualSpans =
                        student.getEnrollmentSpans(broker, globalData.getEndDate(), false, false);
                if (annualSpans != null && !annualSpans.isEmpty()) {
                    OnsisAnnualSpan span = (OnsisAnnualSpan) annualSpans.get(annualSpans.size() - 1);

                    String status = span.getBoardResidentStatus(globalData);
                    if (!StringUtils.isEmpty(status) && PUPIL_OF_BOARD_CODES.contains(status)) {
                        ++cntPupilOfBoard;
                    } else {
                        ++cntPupilOther;
                    }
                }
            }
        }

        /**
         * The Class OnsisIleTimeOfDayCount.
         */
        class OnsisIleTimeOfDayCount {
            private int cntClass;
            private Set<String> sectionKeys = new HashSet();

            /**
             * Instantiates a new onsis ile time of day count.
             *
             * @param campusNumber String
             * @param timeOfDay String
             */
            public OnsisIleTimeOfDayCount(String campusNumber, String timeOfDay) {
                if (m_mapIleRecords.containsKey(campusNumber)) {
                    m_mapIleRecords.get(campusNumber).values().stream()
                            .filter(record -> record.getTimeOfDay().equals(timeOfDay))
                            .forEach(this::incrementCounts);
                }
            }

            /**
             * Gets the class count.
             *
             * @return Long
             */
            public Long getClassCount() {
                Long value = Long.valueOf(cntClass);
                return value;
            }

            /**
             * Gets the section key.
             *
             * @param mst MasterSchedule
             * @return String
             */
            private String getSectionKey(OnSection mst) {
                return StringUtils.isEmpty(mst.getSectionClassOid()) ? mst.getOid() : mst.getSectionClassOid();
            }

            /**
             * Increment class count.
             *
             * @param mst MasterSchedule
             */
            private void incrementClassCount(OnSection mst) {
                String key = getSectionKey(mst);
                if (!sectionKeys.contains(key)) {
                    ++cntClass;
                    sectionKeys.add(key);
                }
            }

            /**
             * Increment counts.
             *
             * @param record OnsisIleRecord
             */
            private void incrementCounts(OnsisIleRecord record) {
                record.getSections().stream().forEach(this::incrementClassCount);
            }


        }

        private Map<String, Map<OnsisIleRecord, OnsisIleRecord>> m_mapIleRecords = new HashMap();
        private Map<String, Map<String, OnsisIleProgramCount>> m_mapLanguageCounts = new HashMap();
        private Map<String, OnsisIleRecord> m_mapSectionToIleRecord = new HashMap();
        private Map<String, Map<String, OnsisIleTimeOfDayCount>> m_mapTimeOfDayCounts = new HashMap();
        private OnsisIle m_reportData;

        /**
         * Instantiates a new onsis ole info.
         *
         * @param reportData OnsisIle
         */
        public OnsisIleInfo(OnsisIle reportData) {
            m_reportData = reportData;
            initialize();
        }

        /**
         * Gets the campus numbers.
         *
         * @return Sets the
         */
        public Set<String> getCampusNumbers() {
            return m_mapIleRecords.keySet();
        }

        /**
         * Gets the ile record.
         *
         * @param section MasterSchedule
         * @return Onsis ile record
         */
        public OnsisIleRecord getIleRecord(MasterSchedule section) {
            OnsisIleRecord ileRecord = null;
            if (section != null) {
                ileRecord = this.m_mapSectionToIleRecord.get(section.getOid());
            }
            return ileRecord;
        }

        /**
         * Gets the international language count.
         *
         * @param campusNumber String
         * @param internationalLanguage String
         * @return Onsis ile program count
         */
        public OnsisIleProgramCount getInternationalLanguageCount(String campusNumber, String internationalLanguage) {
            Map<String, OnsisIleProgramCount> campusCounts = m_mapLanguageCounts.get(campusNumber);
            if (campusCounts == null) {
                campusCounts = new HashMap();
                m_mapLanguageCounts.put(campusNumber, campusCounts);
            }
            OnsisIleProgramCount count = campusCounts.get(internationalLanguage);
            if (count == null) {
                count = new OnsisIleProgramCount(campusNumber, internationalLanguage);
                campusCounts.put(internationalLanguage, count);
            }
            return count;
        }

        /**
         * Gets the international languages.
         *
         * @param campusNumber String
         * @return List
         */
        public List<String> getInternationalLanguages(String campusNumber) {
            List<String> languageCounts = Collections.EMPTY_LIST;
            if (m_mapIleRecords.containsKey(campusNumber)) {
                languageCounts = m_mapIleRecords.get(campusNumber).values().stream()
                        .map(OnsisIleRecord::getInternationalLanguage).distinct().collect(Collectors.toList());
            }
            return languageCounts;
        }

        /**
         * Gets the time of day count.
         *
         * @param campusNumber String
         * @param timeOfDay String
         * @return Onsis ile time of day count
         */
        public OnsisIleTimeOfDayCount getTimeOfDayCount(String campusNumber, String timeOfDay) {
            Map<String, OnsisIleTimeOfDayCount> campusCounts = m_mapTimeOfDayCounts.get(campusNumber);
            if (campusCounts == null) {
                campusCounts = new HashMap();
                m_mapTimeOfDayCounts.put(campusNumber, campusCounts);
            }
            OnsisIleTimeOfDayCount count = campusCounts.get(timeOfDay);
            if (count == null) {
                count = new OnsisIleTimeOfDayCount(campusNumber, timeOfDay);
                campusCounts.put(timeOfDay, count);
            }
            return count;
        }

        /**
         * Gets the time of day counts.
         *
         * @param campusNumber String
         * @return List
         */
        public List<String> getTimeOfDayCounts(String campusNumber) {
            List<String> timeOfDayCounts = Collections.EMPTY_LIST;
            if (m_mapIleRecords.containsKey(campusNumber)) {
                timeOfDayCounts = m_mapIleRecords.get(campusNumber).values().stream()
                        .map(OnsisIleRecord::getTimeOfDay).distinct().collect(Collectors.toList());
            }
            return timeOfDayCounts;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder output = new StringBuilder();

            m_mapIleRecords.values().stream().map(map -> map.values()).flatMap(collection -> collection.stream())
                    .forEach(ileRecord -> output.append(ileRecord.toString() + "\n"));

            return output.toString();
        }

        /**
         * Gets the report data.
         *
         * @return Onsis state report data
         */
        private OnsisStateReportData getReportData() {
            return m_reportData;
        }

        /**
         * Initialize.
         */
        private void initialize() {
            StringBuilder debugOutput = m_reportData.getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            DictionaryExtractor dictionaryExtractor = getReportData().getGlobalData().getDictionaryExtractor();
            List<String> ileCodes = dictionaryExtractor
                    .getRefCodesWithStateValue(OnSection.FIELD_CONED_PROG_TYPE.getField(dictionaryExtractor),
                            Arrays.asList(OnSection.CONED_IILE))
                    .stream()
                    .map(code -> code.getCode()).collect(Collectors.toList());
            if (debugOutput != null) {
                debugOutput.append("ileCodes: " + ileCodes + "\n");
            }


            ToolBean.preload(m_reportData.getGlobalData().getBroker(),
                    m_reportData.getGlobalData().getDictionaryExtractor(), null, ToolStudentSchedule.PARENT_SECTION);
            ToolBean.preload(m_reportData.getGlobalData().getBroker(),
                    m_reportData.getGlobalData().getDictionaryExtractor(), null,
                    ToolStudentScheduleChange.PARENT_SECTION);

            m_reportData.getGlobalData().getSchoolType()
                    .getSchoolClasses(ToolBean.getCachedToolBeans(OnSection.class))
                    .stream()
                    .filter(mst -> {
                        boolean included = ileCodes.contains(mst.getConedProgType());
                        if (debugOutput != null) {
                            debugOutput.append("included=" + included + " - " + mst.toString() + "\n");
                        }
                        return included;
                    })
                    .forEach(mst -> {
                        String timeOfDay = mst.getTimeOfDay();
                        DataDictionaryField internationalField =
                                OnSection.FIELD_INTERNATIONAL_LANGUAGE.getField(dictionaryExtractor);
                        ReferenceCode code = internationalField.hasReferenceTable()
                                ? dictionaryExtractor.getReferenceCodes(internationalField.getReferenceTableOid())
                                        .get(mst.getInternationalLanguage())
                                : null;
                        String internationalLanguage = code == null ? null : code.getStateCode();
                        if ("OTH".equals(internationalLanguage)) {
                            internationalLanguage += OTHER_LANGUAGE_DESC_DELIMETER + code.getDescription();
                        }
                        String campus = mst.getSchool(getReportData().getBroker()).getSchoolId();
                        if (debugOutput != null) {
                            debugOutput.append("timeOfDay=" + timeOfDay + ", internationalLanguage="
                                    + internationalLanguage + ", campus=" + campus + "\n");
                        }

                        Map<OnsisIleRecord, OnsisIleRecord> ileRecords = null;
                        if (m_mapIleRecords.containsKey(campus)) {
                            ileRecords = m_mapIleRecords.get(campus);
                        } else {
                            ileRecords = new TreeMap(new Comparator<OnsisIleRecord>() {
                                @Override
                                public int compare(OnsisIleRecord o1, OnsisIleRecord o2) {
                                    int value = o1.m_internationalLanguage.compareTo(o2.m_internationalLanguage);
                                    if (value == 0) {
                                        value = o1.m_timeOfDay.compareTo(o2.m_timeOfDay);
                                    }
                                    return value;
                                }

                            });
                            m_mapIleRecords.put(campus, ileRecords);
                        }

                        OnsisIleRecord record = new OnsisIleRecord(campus, internationalLanguage, timeOfDay);
                        if (ileRecords.containsKey(record)) {
                            record = ileRecords.get(record);
                        } else {
                            ileRecords.put(record, record);
                        }
                        record.addSection(mst);
                        m_mapSectionToIleRecord.put(mst.getOid(), record);
                    });
            if (debugOutput != null) {
                debugOutput.append("OnsisIleInfo: \n" + this.toString());
                getReportData().log(debugOutput.toString());
            }
        }
    }

    /**
     * The Class OnsisIleEntity.
     */
    public static class OnsisIleEntity extends OnsisStateReportEntity {
        private OnsisIleInfo m_info;
        private OnsisIle m_reportData;
        private List<String> m_campusNumbers;

        /**
         * Gets the campus number.
         *
         * @return String
         */
        public String getCampusNumber() {
            return m_campusNumbers.get(this.getCurrentRow());
        }

        /**
         * Gets the ile info.
         *
         * @return Onsis ile info
         */
        public OnsisIleInfo getIleInfo() {
            return m_info;
        }

        /**
         * Gets the report data.
         *
         * @return Onsis ile
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisIle getReportData() {
            return m_reportData;
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

            m_reportData = (OnsisIle) data;
            m_info = new OnsisIleInfo(m_reportData);
            m_campusNumbers = new ArrayList(m_info.getCampusNumbers());
            Collections.sort(m_campusNumbers);
            this.setRowCount(m_campusNumbers.size());
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
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisIleEntity.class);
    }
}
