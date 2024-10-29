/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.fl;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.*;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DictionaryHelper;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentConductDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentEnrollmentSpanInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.ContextAttributesManager;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import jersey.repackaged.com.google.common.collect.ImmutableSet;


/**
 * The Class FLFasterRecordsData.
 */
public class FLFasterRecordsData extends FLStateReportData {

    public static class FLStudentScheduleHelper extends FLScheduleHelper {
        /**
         * @param data
         * @param startDate
         * @param endDate
         */
        public FLStudentScheduleHelper(FLStateReportData data, PlainDate startDate, PlainDate endDate) {
            super(data, startDate, endDate);
        }

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLScheduleHelper#getSectionCriteria()
         */
        @Override
        X2Criteria getSectionCriteria() {
            X2Criteria sectionCriteria = super.getSectionCriteria();
            X2Criteria studentSectionCriteria = new X2Criteria();
            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID,
                    ((FLFasterRecordsData) getFLData()).getStudentCriteria());
            studentSectionCriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSubQuery);
            SubQuery studentSectionsSubQuery =
                    new SubQuery(StudentSchedule.class, StudentSchedule.COL_SECTION_OID, studentSectionCriteria);
            sectionCriteria.addIn(X2BaseBean.COL_OID, studentSectionsSubQuery);
            return sectionCriteria;
        }
    }

    /**
     * The Class RecordTypeHelper.
     */
    public static class RecordTypeHelper {
        private Collection<String> m_allowedRecordsTypes = null;
        private String m_recordType = null;
        private boolean m_isGeneralRecord;
        private Collection<String> m_transferTypes = null;


        /**
         * Instantiates a new record type helper.
         *
         * @param allowedRecordsTypes Collection
         * @param transferTypes Collection
         * @param recordType String
         */
        public RecordTypeHelper(Collection allowedRecordsTypes, Collection transferTypes,
                String recordType) {
            m_allowedRecordsTypes = allowedRecordsTypes;
            m_transferTypes = transferTypes;
            m_recordType = recordType;
        }


        /**
         * Instantiates a new record type helper.
         *
         * @param allowedRecordsTypes Collection<String>
         * @param transferTypes Collection<String>
         * @param recordId String
         * @param isGeneralRecord boolean
         */
        public RecordTypeHelper(Collection<String> allowedRecordsTypes, Collection<String> transferTypes,
                String recordId, boolean isGeneralRecord) {
            this(allowedRecordsTypes, transferTypes, recordId);
            m_isGeneralRecord = isGeneralRecord;
        }


        /**
         * Checks if is record being reported.
         *
         * @param transferType String
         * @param recordsType String
         * @return true, if is record being reported
         */
        public boolean isRecordBeingReported(String transferType, String recordsType) {
            return m_allowedRecordsTypes.contains(recordsType) && m_transferTypes.contains(transferType);
        }


        /**
         * Gets the record type.
         *
         * @return String
         */
        public String getRecordType() {
            return m_recordType;
        }


        /**
         * Gets the full record type.
         *
         * @param recordsType String
         * @return String
         */
        public String getFullRecordType(String recordsType) {
            if (recordsType == null) {
                throw new X2RuntimeException();
            }
            return m_isGeneralRecord ? "G" + m_recordType : recordsType + m_recordType;
        }


        /**
         * Gets the transfer types.
         *
         * @return Collection
         */
        public Collection<String> getTransferTypes() {
            return m_allowedRecordsTypes;
        }
    }


    /**
     * The Interface RecordInterface.
     */
    public interface RecordInterface {


        /**
         * Gets the entity class.
         *
         * @return Class
         */
        public Class<?> getEntityClass();


        /**
         * Gets the full record type.
         *
         * @param recordsType String
         * @return String
         */
        public String getFullRecordType(String recordsType);


        /**
         * Gets the record type.
         *
         * @return String
         */
        public String getRecordType();


        /**
         * Checks if is record being exported.
         *
         * @param transferType String
         * @param recordsType String
         * @return true, if is record being exported
         */
        public boolean isRecordBeingExported(String transferType, String recordsType);
    }


    /**
     * The Enum Record.
     */
    enum Record implements RecordInterface {
        RECORD_00(Record00.class, Record01.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE, TRANSFER_TYPE_REQUEST), "00")),
        //
        RECORD_01(Record01.class, Record02.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "01")),
        //
        RECORD_02(Record02.class, Record03.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "02")),
        //
        RECORD_03(Record03.class, Record04.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "03")),
        //
        RECORD_04(Record04.class, Record05.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "04")),
        //
        RECORD_05(Record05.class, Record06.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "05")),
        //
        RECORD_06(Record06.class, Record07.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "06")),
        //
        RECORD_07(Record07.class, Record08.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "07")),
        //
        RECORD_08(Record08.class, Record09.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "08")),
        //
        RECORD_09(Record09.class, Record10.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "09")),
        //
        RECORD_10(Record10.class, Record11.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "10")),
        //
        RECORD_11(Record11.class, RecordG99.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "11")),
        //
        RECORD_G99(RecordG99.class, RecordG99ATV.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "99", true)),
        //
        RECORD_G99ATV(RecordG99ATV.class, RecordG99HC.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "99ATV", true)),
        //
        RECORD_G99HC(RecordG99HC.class, RecordG99HS.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "99HC", true)),
        //
        RECORD_G99HS(RecordG99HS.class, RecordG99IMM.class,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "99HS", true)),
        //
        RECORD_G99IMM(RecordG99IMM.class, null,
                new RecordTypeHelper(Arrays.asList(RECORDS_TYPE_INTERDISTRICT, RECORDS_TYPE_SECONDARY),
                        Arrays.asList(TRANSFER_TYPE_RESPONSE), "99IMM", true));

        private Class<?> m_nextRecordEntityClass;
        private Class<?> m_recordEntityClass;
        private RecordTypeHelper m_recordTypeHelper;


        /**
         * Instantiates a new record.
         *
         * @param recordEntityClass Class<?>
         * @param nextRecordEntityClass Class<?>
         * @param recordTypeHelper RecordTypeHelper
         */
        private Record(Class<?> recordEntityClass, Class<?> nextRecordEntityClass, RecordTypeHelper recordTypeHelper) {
            m_recordEntityClass = recordEntityClass;
            m_recordTypeHelper = recordTypeHelper;
            m_nextRecordEntityClass = nextRecordEntityClass;
        }


        /**
         * Find by record entity class.
         *
         * @param recordEntityClass Class<?>
         * @return Record
         */
        public static Record findByRecordEntityClass(Class<?> recordEntityClass) {
            Record record = null;
            for (Record value : values()) {
                if (value.getEntityClass() == recordEntityClass) {
                    record = value;
                    break;
                }
            }
            return record;
        }


        /**
         * Find by record type.
         *
         * @param recordType String
         * @return Record
         */
        public static Record findByRecordType(String recordType) {
            Record record = null;
            for (Record value : values()) {
                if (value.getRecordType().equals(recordType)) {
                    record = value;
                    break;
                }
            }
            return record;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.RecordInterface#getEntityClass()
         */
        @Override
        public Class<?> getEntityClass() {
            return m_recordEntityClass;
        }


        /**
         * Gets the format definition id.
         *
         * @return String
         */
        public String getFormatDefinitionId() {
            return m_recordTypeHelper.getRecordType();
        }


        /**
         * Gets the next being reported record.
         *
         * @param fromRecord Record
         * @param transferType String
         * @param recordsType String
         * @return Record
         */
        public static Record getNextBeingReportedRecord(Record fromRecord, String transferType, String recordsType) {
            Record nextRecord = fromRecord.getNextRecord();
            while (nextRecord != null && !nextRecord.isRecordBeingExported(transferType, recordsType)) {
                nextRecord = nextRecord.getNextRecord();
            }
            return nextRecord;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.RecordInterface#getFullRecordType(java.lang.String)
         */
        @Override
        public String getFullRecordType(String recordsType) {
            return m_recordTypeHelper.getFullRecordType(recordsType);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.RecordInterface#isRecordBeingExported(java.lang.String,
         *      java.lang.String)
         */
        @Override
        public boolean isRecordBeingExported(String transferType, String recordsType) {
            return m_recordTypeHelper.isRecordBeingReported(transferType, recordsType);
        }


        /**
         * Gets the next record.
         *
         * @return Record
         */
        private Record getNextRecord() {
            Record nextRecord = null;
            if (m_nextRecordEntityClass != null) {
                nextRecord = findByRecordEntityClass(m_nextRecordEntityClass);
            }
            return nextRecord;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.RecordInterface#getRecordType()
         */
        @Override
        public String getRecordType() {
            return m_recordTypeHelper.getRecordType();
        }
    }



    /**
     * The Class FLRecordEntity.
     */
    public static abstract class FLRecordEntity extends StateReportEntity {
        private FLFasterRecordsData m_data = null;
        private SisStudent m_student = null;

        /**
         * Instantiates a new FL record entity.
         */
        public FLRecordEntity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            return Record.findByRecordEntityClass(getClass()).getFormatDefinitionId();
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getData()
         */
        @Override
        public FLFasterRecordsData getData() {
            return m_data;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_student.getNameView() + " [LASID: " + m_student.getLocalId() + "] ";
            return name;
        }


        /**
         * Gets the full record type code.
         *
         * @param recordsType String
         * @return String
         */
        public String getFullRecordTypeCode(String recordsType) {
            return Record.findByRecordEntityClass(this.getClass()).getFullRecordType(recordsType);
        }


        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return m_student;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (FLFasterRecordsData) data;
            m_student = (SisStudent) bean;
            // System.out.println("Record: " + this.getClass() + ", time has passed: "
            // + (System.currentTimeMillis() - m_lastTimeIntitialized));
        }
    }



    /**
     * The Class Record00.
     */
    public static class Record00 extends FLRecordEntity {
        //
    }



    /**
     * The Class Record01.
     */
    public static class Record01 extends FLRecordEntity {
        //
    }



    /**
     * The Class Record02.
     */
    public static class Record02 extends FLRecordEntity {
        //
    }



    /**
     * The Class Record03.
     */
    public static class Record03 extends FLRecordEntity {

        private static final String ALIAS_MIGRANT_STATUS = "all-std-FLMigrantStatus";

        private Map<Integer, DistrictSchoolYearContext> m_contextsByYears;
        private FLFasterRecordsData m_data;
        private Map<Integer, List<StudentEnrollmentSpan>> m_schoolYearSpans = new HashMap<>();
        private List<Map<Integer, StudentEnrollmentSpan>> m_schoolYearSpanList;
        private List<Integer> m_schoolYears;
        private SisStudent m_student;


        /**
         * Gets the current span.
         *
         * @return Student enrollment span
         * @throws X2BaseException exception
         */
        public StudentEnrollmentSpan getCurrentSpan() throws X2BaseException {
            if (isStudentMigrant(m_student)) {
                return m_schoolYearSpanList.get(getCurrentRow()).values().iterator().next();
            }
            return null;
        }


        /**
         * Gets the current year.
         *
         * @return Integer
         * @throws X2BaseException exception
         */
        public Integer getCurrentYear() throws X2BaseException {
            if (isStudentMigrant(m_student)) {
                return m_schoolYearSpanList.get(getCurrentRow()).keySet().iterator().next();
            }
            return m_schoolYears.get(getCurrentRow());
        }


        /**
         * Gets the context.
         *
         * @return District school year context
         * @throws X2BaseException exception
         */
        public DistrictSchoolYearContext getContext() throws X2BaseException {
            return m_contextsByYears.get(getCurrentYear());
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;
            m_data = (FLFasterRecordsData) data;

            m_contextsByYears = m_data.m_contextsByYears;

            FLFasterRecordsData flData = (FLFasterRecordsData) data;
            List<StudentEnrollmentSpan> spans = flData.getStudentHelper().getStudentEnrollmentSpans(m_student, false);
            for (StudentEnrollmentSpan span : spans) {
                PlainDate enrollmentDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                Integer schoolYearForStartDate = getSchoolYearByDate(enrollmentDate);
                Integer schoolYearForEndDate = null;
                if (span.getLastActiveDate() != null) {
                    schoolYearForEndDate = getSchoolYearByDate(span.getLastActiveDate());
                }
                if (schoolYearForEndDate == null) {
                    schoolYearForEndDate = getSchoolYearByDate(m_data.getSurveyPeriod().getDateCertain());
                }

                List<Integer> schoolYears = new ArrayList<Integer>();
                for (int i = schoolYearForStartDate.intValue(); i <= schoolYearForEndDate.intValue(); i++) {
                    schoolYears.add(Integer.valueOf(i));
                }

                for (Integer schoolYear : schoolYears) {
                    List<StudentEnrollmentSpan> yearSpans = m_schoolYearSpans.get(schoolYear);
                    if (yearSpans == null) {
                        yearSpans = new ArrayList<>();
                        m_schoolYearSpans.put(schoolYear, yearSpans);
                    }
                    yearSpans.add(span);
                }
            }

            if (isStudentMigrant(m_student)) {
                m_schoolYearSpanList = new ArrayList<>();
                for (Entry<Integer, List<StudentEnrollmentSpan>> entry : m_schoolYearSpans.entrySet()) {
                    Integer schoolYear = entry.getKey();
                    List<StudentEnrollmentSpan> spansOfYear = entry.getValue();
                    for (StudentEnrollmentSpan spanOfYear : spansOfYear) {
                        Map<Integer, StudentEnrollmentSpan> yearSpan = new HashMap<>();
                        yearSpan.put(schoolYear, spanOfYear);
                        m_schoolYearSpanList.add(yearSpan);
                    }
                }
                Collections.sort(m_schoolYearSpanList, new Comparator<Map<Integer, StudentEnrollmentSpan>>() {
                    @Override
                    public int compare(Map<Integer, StudentEnrollmentSpan> o1, Map<Integer, StudentEnrollmentSpan> o2) {
                        return o1.keySet().iterator().next().compareTo(o2.keySet().iterator().next());
                    }
                });
                setRowCount(m_schoolYearSpanList.size());
            } else {
                m_schoolYears = new ArrayList<>();
                for (Entry<Integer, List<StudentEnrollmentSpan>> entry : m_schoolYearSpans.entrySet()) {
                    Integer schoolYear = entry.getKey();
                    m_schoolYears.add(schoolYear);
                }
                Collections.sort(m_schoolYears);
                setRowCount(m_schoolYears.size());
            }
        }


        /**
         * Gets the school year by date.
         *
         * @param date PlainDate
         * @return Integer
         */
        private Integer getSchoolYearByDate(PlainDate date) {
            for (Entry<Integer, DistrictSchoolYearContext> entry : m_contextsByYears.entrySet()) {
                Integer year = entry.getKey();
                DistrictSchoolYearContext context = entry.getValue();
                PlainDate startDate = context.getStartDate();
                PlainDate endDate = context.getEndDate();
                if (!date.after(endDate) && !date.before(startDate)) {
                    return year;
                }
            }
            throw new X2RuntimeException();
        }


        /**
         * Checks if is student migrant.
         *
         * @param student SisStudent
         * @return true, if is student migrant
         * @throws X2BaseException exception
         */
        private boolean isStudentMigrant(SisStudent student) throws X2BaseException {
            String migrantStatus = (String) m_data.getFieldValueByAlias(student, ALIAS_MIGRANT_STATUS);
            if (StringUtils.isEmpty(migrantStatus) || "Z".equals(migrantStatus)) {
                return false;
            }
            return true;
        }
    }


    /**
     * The Class Record04.
     */
    public static class Record04 extends FLRecordEntity {
        private List<StudentScheduleInfo> m_studentScheduleInfos = new ArrayList<StudentScheduleInfo>();
        private Map<Integer, DistrictSchoolYearContext> m_contextsByYears;


        /**
         * Gets the current info.
         *
         * @return Student schedule info
         */
        public StudentScheduleInfo getCurrentInfo() {
            return m_studentScheduleInfos.get(getCurrentRow());
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            final FLFasterRecordsData flData = (FLFasterRecordsData) data;
            SisStudent student = (SisStudent) bean;
            m_contextsByYears = flData.m_contextsByYears;

            for (Integer schoolYear : m_contextsByYears.keySet()) {
                StudentScheduleHelper helper = flData.getScheduleHelperForYear(schoolYear);

                List<StudentScheduleInfo> infos = helper.getStudentScheduleInfo(student);

                for (StudentScheduleInfo info : infos) {
                    if (info.getTranscript() != null && info.getTranscript().getSchoolYear() != 0) {
                        m_studentScheduleInfos.add(info);
                    }
                }
            }

            Collections.sort(m_studentScheduleInfos, new Comparator<StudentScheduleInfo>() {
                @Override
                public int compare(StudentScheduleInfo o1, StudentScheduleInfo o2) {
                    try {
                        String crsStateId1 =
                                (String) flData.getFieldValueByAlias(o1.getCourse(),
                                        RetrieveTranscript.ALIAS_CRS_STATE_ID);
                        String crsStateId2 =
                                (String) flData.getFieldValueByAlias(o2.getCourse(),
                                        RetrieveTranscript.ALIAS_CRS_STATE_ID);
                        if (crsStateId1 == null) {
                            if (crsStateId2 == null) {
                                return 0;
                            }
                            return -1;
                        }
                        if (crsStateId2 == null) {
                            return 1;
                        }
                        return crsStateId1.compareTo(crsStateId2);
                    } catch (X2BaseException e) {
                        e.printStackTrace();
                    }
                    return 0;
                }
            });

            setRowCount(m_studentScheduleInfos.size());
        }
    }



    /**
     * The Class Record05.
     */
    public static class Record05 extends FLRecordEntity {
        public static final String DDX_ID_CTE = "FL-PGM-CTE";
        public static final String DDX_ID_DROP = "FL-PGM-DROP";
        public static final String DDX_ID_ELL = "FL-PGM-ELL";

        private static final int MAX_CTE_PER_RECORD = 4;

        private List<StudentProgramParticipation> m_ctePrograms = new ArrayList<>();
        private List<StudentProgramParticipation> m_dropoutPrograms = new ArrayList<>();
        private StudentProgramParticipation m_ellProgram = null;


        /**
         * Gets the cte program.
         *
         * @param num int
         * @return Student program participation
         */
        public StudentProgramParticipation getCteProgram(int num) {
            int currentCteNum = (MAX_CTE_PER_RECORD * getCurrentRow()) + (num - 1);
            return m_ctePrograms.size() > currentCteNum ? m_ctePrograms.get(currentCteNum) : null;
        }


        /**
         * Gets the dropout program.
         *
         * @param num int
         * @return Student program participation
         */
        public StudentProgramParticipation getDropoutProgram(int num) {
            return (getCurrentRow() == 0 && m_dropoutPrograms.size() > num - 1) ? m_dropoutPrograms.get(num - 1) : null;
        }


        /**
         * Gets the ell program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getEllProgram() {
            return (getCurrentRow() == 0) ? m_ellProgram : null;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            SisStudent student = (SisStudent) bean;

            m_ctePrograms.addAll(fasterData.getStudentPrograms(student.getOid(), DDX_ID_CTE));
            m_dropoutPrograms.addAll(fasterData.getStudentPrograms(student.getOid(), DDX_ID_DROP));
            m_ellProgram = fasterData.getStudentPrograms(student.getOid(), DDX_ID_ELL).size() > 0
                    ? fasterData.getStudentPrograms(student.getOid(), DDX_ID_ELL).iterator().next()
                    : null;

            setRowCount(Math.max(1,
                    (m_ctePrograms.size() % MAX_CTE_PER_RECORD != 0) ? ((m_ctePrograms.size() / MAX_CTE_PER_RECORD) + 1)
                            : (m_ctePrograms.size() / MAX_CTE_PER_RECORD)));
        }
    }


    /**
     * The Class Record06.
     */
    public static class Record06 extends FLRecordEntity {
        public List<StudentProgramParticipation> m_exceptPrograms = new ArrayList<>();

        private static final String DDX_ID = "FL-PGM-EXCEPT";


        /**
         * Gets the program.
         *
         * @param pgmNum int
         * @return Student program participation
         */
        public StudentProgramParticipation getProgram(int pgmNum) {
            return m_exceptPrograms.size() < pgmNum ? null : m_exceptPrograms.get(pgmNum - 1);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            List<StudentProgramParticipation> programs =
                    fasterData.getStudentHelperForYear(Integer.valueOf(fasterData.getCurrentContext().getSchoolYear()))
                            .getStudentPrograms(bean.getOid(), DDX_ID, fasterData.getSurveyPeriod());
            if (programs != null) {
                m_exceptPrograms.addAll(programs);
            }
        }
    }



    /**
     * The Class Record07.
     */
    public static class Record07 extends FLRecordEntity {


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            setRowCount(0);
        }
    }



    /**
     * The Class Record08.
     */
    public static class Record08 extends FLRecordEntity {

        private static final int MAX_TESTS_PER_ROW = 6;

        private List<StudentAssessment> m_studentAssessments;


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData flData = (FLFasterRecordsData) data;
            m_studentAssessments = flData.getStudentAssessments(ASSESSMENT_DEFINITION_ID_TEST, bean.getOid());
            setRowCount((int) Math.ceil((double) m_studentAssessments.size() / MAX_TESTS_PER_ROW));
        }


        /**
         * Gets the student assessments.
         *
         * @return List
         */
        public List<StudentAssessment> getStudentAssessments() {
            if (m_studentAssessments == null) {
                return null;
            }
            int from = getCurrentRow() * MAX_TESTS_PER_ROW;
            int to = Math.min(from + MAX_TESTS_PER_ROW, m_studentAssessments.size());
            return m_studentAssessments.subList(from, to);
        }
    }


    /**
     * The Class Record09.
     */
    public static class Record09 extends FLRecordEntity {
        private List<ConductAction> m_actions = null;


        /**
         * Gets the current action.
         *
         * @return Conduct action
         */
        public ConductAction getCurrentAction() {
            return m_actions.get(getCurrentRow());
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

            m_actions = fasterData.getStudentActions(bean.getOid());

            if (m_actions == null || m_actions.isEmpty()) {
                setRowCount(0);
            } else {
                setRowCount(m_actions.size());
            }
        }
    }


    /**
     * The Class Record10.
     */
    public static class Record10 extends FLRecordEntity {

        /**
         * The Class PASNumber.
         */
        public class PASNumber {
            String gradeLevel;
            String pasNumber;
            String subjectContent;
            PlainDate testDate;
            String testName;
        }

        private static final String ALIAS_TEST_NAME = "asm-test-name";
        private static final String ALIAS_TEST_PAS_NUMBER = "asm-test-pasNumber";
        private static final String ALIAS_TEST_SUBJECT_CONTENT = "asm-test-subjectContent";

        private static final int MAX_NUM_SUBJECT_CONTENTS = 9;

        private List<PASNumber> m_numbers = new ArrayList<PASNumber>();


        /**
         * Gets the pas number object.
         *
         * @param groupNumber int
         * @return PAS number
         */
        public PASNumber getPasNumberObject(int groupNumber) {
            return m_numbers.size() < groupNumber ? new PASNumber() : m_numbers.get(groupNumber - 1);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData flData = (FLFasterRecordsData) data;
            List<StudentAssessment> studentAssessments =
                    flData.getStudentAssessments(ASSESSMENT_DEFINITION_ID_TEST, bean.getOid());
            for (StudentAssessment assessment : studentAssessments) {
                for (int i = 1; i <= MAX_NUM_SUBJECT_CONTENTS; i++) {
                    String pasNumber = (String) assessment.getFieldValueByAliasExtended(ALIAS_TEST_PAS_NUMBER + i,
                            flData.getAsmDataDictionary(assessment));
                    if (!StringUtils.isEmpty(pasNumber)) {
                        PASNumber pasNumberObject = new PASNumber();
                        pasNumberObject.testDate = assessment.getDate();
                        pasNumberObject.testName = (String) assessment.getFieldValueByAliasExtended(ALIAS_TEST_NAME,
                                flData.getAsmDataDictionary(assessment));
                        pasNumberObject.gradeLevel = assessment.getGradeLevelCode();
                        pasNumberObject.subjectContent =
                                (String) assessment.getFieldValueByAliasExtended(ALIAS_TEST_SUBJECT_CONTENT + i,
                                        flData.getAsmDataDictionary(assessment));
                        pasNumberObject.pasNumber = pasNumber;
                        m_numbers.add(pasNumberObject);
                    }
                }
            }
            setRowCount(m_numbers.size() > 0 ? 1 : 0);
        }
    }


    /**
     * The Class Record11.
     */
    public static class Record11 extends FLRecordEntity {
        //
    }



    /**
     * The Class RecordG99.
     */
    public static class RecordG99 extends FLRecordEntity {
        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            setRowCount(0);
        }
    }



    /**
     * The Class RecordG99ATV.
     */
    public static class RecordG99ATV extends FLRecordEntity {
        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            setRowCount(0);
        }
    }



    /**
     * The Class RecordG99IMM.
     */
    public static class RecordG99IMM extends FLRecordEntity {
        private List<HealthImmunizationSeries> m_series = new ArrayList<HealthImmunizationSeries>();


        /**
         * Gets the current immunization.
         *
         * @return Health immunization series
         */
        public HealthImmunizationSeries getCurrentImmunization() {
            return m_series.get(getCurrentRow());
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            List<HealthImmunizationSeries> seriesList = fasterData.getStudentImmunizationSeries(bean.getOid());
            if (seriesList != null) {
                for (HealthImmunizationSeries series : seriesList) {
                    HealthImmunizationDefinition immDefinition = series.getImmunizationDefinition();
                    if (immDefinition != null) {
                        String immId = series.getImmunizationDefinition().getId();
                        String immunizationType =
                                fasterData.findCodeByDependency(REF_TABLE_IMMUNIZATION_TYPES_TO_FASTER, immId);
                        if (!StringUtils.isEmpty(immunizationType)) {
                            m_series.add(series);
                        }
                    }
                }
            }

            setRowCount(m_series.size());
        }
    }



    /**
     * The Class RecordG99HC.
     */
    public static class RecordG99HC extends FLRecordEntity {
        private List<HealthCondition> m_healthCondition = new ArrayList<HealthCondition>();


        /**
         * Gets the current health condition.
         *
         * @return Health condition
         */
        public HealthCondition getCurrentHealthCondition() {
            return m_healthCondition.get(getCurrentRow());
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

            m_healthCondition = fasterData.getStudentHealthCondition(bean.getOid());

            setRowCount(m_healthCondition.size());
        }
    }



    /**
     * The Class RecordG99HS.
     */
    public static class RecordG99HS extends FLRecordEntity {
        private List<HealthScreening> m_healthScreenings = new ArrayList<HealthScreening>();


        /**
         * Gets the current health screening.
         *
         * @return Health screening
         */
        public HealthScreening getCurrentHealthScreening() {
            return m_healthScreenings.get(getCurrentRow());
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.FLRecordEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

            Collection<HealthScreening> healthScreenings = fasterData.getStudentHealthScreenings(bean.getOid());
            if (healthScreenings != null) {
                m_healthScreenings.addAll(healthScreenings);
            }

            setRowCount(m_healthScreenings.size());
        }
    }


    /**
     * The Class RetrieveAddressedInst.
     */
    public static class RetrieveAddressedInst implements FieldRetriever {
        public static final String CALC_ID = "ADDRESSED";

        private static final String CALC_PARAM_DST = "DST";
        private static final String CALC_PARAM_SKL = "SKL";
        private static final String CALC_PARAM_SPEEDE_ID = "SPEEDE_ID";

        private static final String INPUT_PARAM_INSTITUTION_ID = "institutionId";
        private static final String INPUT_PARAM_SCHOOL = "addressedSchool";

        private String m_addressedDistrict = null;
        private String m_addressedSchool = null;
        private String m_institutionId = null;


        /**
         * Instantiates a new retrieve addressed inst.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveAddressedInst(FLFasterRecordsData data) {
            String addressedSklCodeOid = (String) data.getParameter(INPUT_PARAM_SCHOOL);
            if (!StringUtils.isEmpty(addressedSklCodeOid)) {
                ReferenceCode code =
                        (ReferenceCode) data.getBroker().getBeanByOid(ReferenceCode.class, addressedSklCodeOid);
                String[] parsedSchoolCode = code.getCode().split("-");
                m_addressedDistrict = parsedSchoolCode[0];
                m_addressedSchool = parsedSchoolCode[1];
            }
            String rcdCodeOid = (String) data.getParameter(INPUT_PARAM_INSTITUTION_ID);
            if (!StringUtils.isEmpty(rcdCodeOid)) {
                ReferenceCode code = (ReferenceCode) data.getBroker().getBeanByOid(ReferenceCode.class, rcdCodeOid);
                m_institutionId = code.getCode();
            }
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String calcParameter = (String) field.getParameter();
            switch (calcParameter) {
                case CALC_PARAM_DST:
                    value = m_addressedDistrict;
                    break;
                case CALC_PARAM_SKL:
                    value = m_addressedSchool;
                    break;
                case CALC_PARAM_SPEEDE_ID:
                    value = m_institutionId;
                    break;

                default:
                    break;
            }
            return value;
        }
    }


    /**
     * The Class RetrieveAttendance.
     */
    public static class RetrieveAttendance implements FieldRetriever {
        public static final String CALC_ID = "ATTENDANCE";

        private static final String PARAM_ABSENT = "ABSENT";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            if (entity instanceof Record03) {
                Record03 entity03 = (Record03) entity;
                SisStudent student = (SisStudent) entity.getBean();
                FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
                StudentInfo info = fasterData.getStudentHelper().getStudentInfo(student);
                int value = 0;

                value = info.getAbsentDates(entity03.getContext()).size();
                if (!PARAM_ABSENT.equals(field.getParameter().toString())) {
                    value = info.getMemberDates(entity03.getContext()).size() - value;
                }
                return Integer.valueOf(value);
            }
            return null;
        }
    }


    /**
     * The Class RetrieveAttendanceSummer.
     */
    public static class RetrieveAttendanceSummer implements FieldRetriever {
        public static final String CALC_ID = "ATTENDANCE_SUMMER";

        private static final String PARAM_ABSENT = "ABSENT";

        private DataDictionaryField m_fieldSheduleTermCode;


        /**
         * Instantiates a new retrieve attendance summer.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveAttendanceSummer(FLFasterRecordsData data) {
            m_fieldSheduleTermCode =
                    data.getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(),
                            ScheduleTerm.COL_CODE);
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

            SisStudent student = (SisStudent) entity.getBean();
            StudentInfo stdInfo = fasterData.getStudentHelper().getStudentInfo(student);
            Record03 entity03 = (Record03) entity;

            Set<ScheduleTermDate> summerDates = new HashSet<ScheduleTermDate>();

            DistrictSchoolYearContext defaultContext = data.getCurrentContext();
            data.setCurrentContext(entity03.getContext());

            List<StudentScheduleInfo> schInfoList =
                    fasterData.getScheduleHelperForYear(entity03.getCurrentYear()).getStudentScheduleInfo(student);

            for (int i = 0; i < schInfoList.size(); i++) {
                StudentScheduleInfo schInfo = schInfoList.get(i);
                ScheduleTerm term = schInfo.getSection().getScheduleTerm();
                String termCode = (String) fasterData.getFieldValue(term, m_fieldSheduleTermCode);
                if (s_summerTermCodes.contains(termCode)) {
                    summerDates.addAll(term.getScheduleTermDates());
                }
            }

            int value = getSummerDaysNumber(stdInfo.getAbsentDates(entity03.getContext()), summerDates);
            if (!PARAM_ABSENT.equals(field.getParameter().toString())) {
                value = getSummerDaysNumber(stdInfo.getMemberDates(entity03.getContext()), summerDates) - value;
            }

            data.setCurrentContext(defaultContext);

            return Integer.valueOf(value);
        }


        /**
         * Gets the summer days number.
         *
         * @param dates ImmutableSet<PlainDate>
         * @param summerDates Set<ScheduleTermDate>
         * @return int
         */
        private int getSummerDaysNumber(ImmutableSet<PlainDate> dates, Set<ScheduleTermDate> summerDates) {
            int res = 0;
            for (PlainDate date : dates) {
                for (ScheduleTermDate termDate : summerDates) {
                    if (date.compareTo(termDate.getStartDate()) >= 0 && date.compareTo(termDate.getEndDate()) <= 0) {
                        res++;
                        break;
                    }
                }
            }
            return res;
        }
    }


    /**
     * The Class RetrieveConduct.
     */
    public static class RetrieveConduct implements FieldRetriever {
        public static final String CALC_ID = "CONDUCT";

        private static final String CALC_PARAM_ACTION_CODE = "ACTION_CODE";
        private static final String CALC_PARAM_ACTION_DATE = "ACTION_DATE";
        private static final String CALC_PARAM_ACTION_DUR = "ACTION_DUR";
        private static final String CALC_PARAM_ACTION_DST_NUM = "ACTION_DST_NUM";
        private static final String CALC_PARAM_ACTION_SKL_NUM = "ACTION_SKL_NUM";
        private static final String CALC_PARAM_INCIDENT_DATE = "INCIDENT_DATE";
        private static final String CALC_PARAM_INCIDENT_TYPE = "INCIDENT_TYPE";

        private static final String ALIAS_ACTION_CODE = "all-act-ActionCode";
        private static final String ALIAS_INCIDENT_TYPE = "all-cnd-IncidentType";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            if (entity instanceof Record09) {
                FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

                Record09 record09 = (Record09) entity;

                ConductAction action = record09.getCurrentAction();
                SisSchool actionSchool = action.getSchool();
                ConductIncident incident = action.getIncident();

                String parameter = (String) field.getParameter();

                switch (parameter) {
                    case CALC_PARAM_INCIDENT_TYPE:
                        return fasterData.getFieldValueByAlias(incident, ALIAS_INCIDENT_TYPE);
                    case CALC_PARAM_INCIDENT_DATE:
                        return incident.getIncidentDate();
                    case CALC_PARAM_ACTION_CODE:
                        return fasterData.getFieldValueByAlias(action, ALIAS_ACTION_CODE);
                    case CALC_PARAM_ACTION_DST_NUM:
                        return fasterData.getFieldValueByAlias(actionSchool.getOrganization1(), ALIAS_ORG_DST_NUM);
                    case CALC_PARAM_ACTION_SKL_NUM:
                        return fasterData.getFieldValueByAlias(actionSchool, ALIAS_SKL_STATE_ID);
                    case CALC_PARAM_ACTION_DATE:
                        return action.getActionStartDate();
                    case CALC_PARAM_ACTION_DUR:
                        return action.getActionPenaltyTime();

                    default:
                        break;
                }
            }
            return null;
        }

    }


    /**
     * The Class RetrieveContact.
     */
    public static class RetrieveContact implements FieldRetriever {
        public static final String CALC_ID = "CONTACT";

        public static final String ALIAS_PARENT_GUARDIAN_CODE = "all-ctj-ParentGuardCode";

        public static final List<String> s_parentGuardiansCodes = Arrays.asList("P", "G", "O", "A", "S");

        private static final String CALC_PARAM_CODE = "CODE";
        private static final String CALC_PARAM_NAME = "NAME";

        private static final String GENDER_CODE_MALE = "M";
        private static final String GENDER_CODE_FEMALE = "F";

        private static final String FIELD_PREFIX_CODE = "Parent/Guard Code ";
        private static final String FIELD_PREFIX_NAME = "Parent/Guard Name ";

        private FLFasterRecordsData m_data = null;
        private DataDictionaryField m_fieldParentGuardCode = null;


        /**
         * Instantiates a new retrieve contact.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveContact(FLFasterRecordsData data) {
            m_data = data;
            m_fieldParentGuardCode = m_data.getFieldByAlias(ALIAS_PARENT_GUARDIAN_CODE, m_data.getDataDictionary());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            String parameter = (String) field.getParameter();
            String toReplace = null;
            if (parameter.equals(CALC_PARAM_CODE)) {
                toReplace = FIELD_PREFIX_CODE;
            } else if (parameter.equals(CALC_PARAM_NAME)) {
                toReplace = FIELD_PREFIX_NAME;
            }
            if (toReplace == null) {
                throw new X2RuntimeException();
            }
            StudentContact sourceContact = null;
            String gender = field.getFieldId().replace(toReplace, "");
            if (GENDER_CODE_FEMALE.equals(gender) || GENDER_CODE_MALE.equals(gender)) {
                for (StudentContact contact : m_data.getStudentContacts(entity.getBean().getOid())) {
                    Person person = contact.getPerson();
                    if (person != null) {
                        if (person.getGenderCode().equals(gender)) {
                            sourceContact = contact;
                            break;
                        }
                    }
                }
                if (sourceContact != null) {
                    if (parameter.equals(CALC_PARAM_CODE)) {
                        value = m_data.getFieldValue(sourceContact, m_fieldParentGuardCode);
                    }
                }
            } else {
                String femaleName = entity.getFieldValue(FIELD_PREFIX_NAME + GENDER_CODE_FEMALE);
                String maleName = entity.getFieldValue(FIELD_PREFIX_NAME + GENDER_CODE_MALE);

                if (StringUtils.isEmpty(femaleName) || StringUtils.isEmpty(maleName)) {
                    for (StudentContact contact : m_data.getStudentContacts(entity.getBean().getOid())) {
                        String contactName = getContactName(contact);
                        if (!StringUtils.isEmpty(contactName) && !contactName.equals(femaleName)
                                && !contactName.equals(maleName)) {
                            sourceContact = contact;
                        }
                    }
                }
                if (sourceContact != null) {
                    if (parameter.equals(CALC_PARAM_CODE)) {
                        Person person = sourceContact.getPerson();
                        if (person != null) {
                            value = person.getGenderCode();
                        }
                    }
                }
            }
            if (sourceContact != null && parameter.equals(CALC_PARAM_NAME)) {
                value = getContactName(sourceContact);
            }

            return value;
        }


        /**
         * Gets the contact name.
         *
         * @param contact StudentContact
         * @return String
         */
        private String getContactName(StudentContact contact) {
            Person person = contact.getPerson();
            if (person != null) {
                String firstName = contact.getPerson().getFirstName();
                String lastName = contact.getPerson().getLastName();
                if (!StringUtils.isEmpty(firstName) && !StringUtils.isEmpty(lastName)) {
                    return lastName + ", " + firstName;
                }
            }

            return null;
        }
    }


    /**
     * The Class RetrieveCredsNeeded.
     */
    public static class RetrieveCredsNeeded implements FieldRetriever {
        public static final String CALC_ID = "CREDS_NEEDED";

        private static final String CALC_PARAM_ALGEBRA_1 = "ALGEBRA I";
        private static final String CALC_PARAM_ALGEBRA_2 = "ALGEBRA II";

        private static final String PATTERN_ALG_1_YEAR_NINE_GRADE = "^(?!200).*$";
        private static final String PATTERN_ALG_2_YEAR_NINE_GRADE = "^20102011|20112012$";

        private static final String REF_TABLE_NAME_CREDITS = "FL Local Subject Area Requirements";

        private FLFasterRecordsData m_data;
        private Map<String, String> m_subjectCreditsMap = null;


        /**
         * Instantiates a new retrieve creds needed.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveCredsNeeded(FLFasterRecordsData data) {
            m_data = data;

            X2Criteria rtbCriteria = new X2Criteria();
            rtbCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                    REF_TABLE_NAME_CREDITS);
            QueryByCriteria rtbQuery = new QueryByCriteria(ReferenceCode.class, rtbCriteria);

            m_subjectCreditsMap = new HashMap<>();
            Collection<ReferenceCode> codes = m_data.getBroker().getCollectionByQuery(rtbQuery);
            for (ReferenceCode code : codes) {
                m_subjectCreditsMap.put(code.getCode(), code.getDependencyCode());
            }
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            if (CALC_PARAM_ALGEBRA_2.equals(parameter)) {
                String pattern = null;
                switch (parameter) {
                    case CALC_PARAM_ALGEBRA_2:
                        pattern = PATTERN_ALG_2_YEAR_NINE_GRADE;
                        break;

                    default:
                        break;
                }
                String yearEntere9Grade = entity.getFieldValue(FIELD_YEAR_ENTERED_9_GRADE);
                if (StringUtils.isEmpty(yearEntere9Grade) || !yearEntere9Grade.matches(pattern)) {
                    return null;
                }
            }
            return m_subjectCreditsMap.get(parameter);
        }

    }


    /**
     * The Class RetrieveCteEllDropoutInfo.
     */
    public static class RetrieveCteEllDropoutInfo implements FieldRetriever {
        public static final String CALC_ID = "CTE_ELL_DROP";

        private static final String ALIAS_CTE_CAPE_ID = "pgm-cte-cape-id";
        private static final String ALIAS_CTE_CERT_EARNED_DATE = "pgm-cte-cert-earned-date";
        private static final String ALIAS_CTE_FULL_PGM_COMPLETER = "pgm-cte-full-program-compl";
        private static final String ALIAS_CTE_INDUST_CERT_OUT = "pgm-cte-industry-cert";
        private static final String ALIAS_CTE_INDUST_CERT_ID = "pgm-cte-industry-cert-id";
        private static final String ALIAS_CTE_PROGRAM_ID = "pgm-cte-program-id";

        private static final String ALIAS_DRP_CODE = "pgm-dropout-code";
        private static final String ALIAS_DRP_OUTCOME = "pgm-drp-outcome";
        private static final String ALIAS_DRP_PLACEMENT_REASON = "pgm-drp-placement-reasons";

        private static final String ALIAS_ELL_BASIS_OF_ENTRY = "pgm-basis-of-entry";
        private static final String ALIAS_ELL_BASIS_OF_EXIT_FIRST = "pgm-basis-of-exit-first";
        private static final String ALIAS_ELL_BASIS_OF_EXIT_SECOND = "pgm-basis-of-exit-second";
        private static final String ALIAS_ELL_CLASSIF_DATE = "pgm-classification-date";
        private static final String ALIAS_ELL_EXTEN_OF_INSTRUCT = "pgm-extension-of-instruction";
        private static final String ALIAS_ELL_FIRST_SEMI_DATE = "pgm-first-semi-rev-date";
        private static final String ALIAS_ELL_PROGRAM_CODE = "pgm-ell-code";
        private static final String ALIAS_ELL_RECLASSIF_DATE = "pgm-reclassification-date";
        private static final String ALIAS_ELL_RECLASSIF_EXIT_DATE = "pgm-reclassification-exit-date";
        private static final String ALIAS_ELL_REEVAL_DATE = "pgm-reevaluation-date";
        private static final String ALIAS_ELL_REPORT_CARD = "pgm-report-card-date";
        private static final String ALIAS_ELL_SECOND_SEMI_DATE = "pgm-second-semi-rev-date";
        private static final String ALIAS_ELL_SECOND_YEAR_END = "pgm-second-year-date";
        private static final String ALIAS_ELL_STUDENT_PLAN_DATE = "pgm-student-plan-date";

        private static final String CALC_PARAM_ELL_PK_12 = "ELL PK-12";
        private static final String CALC_PARAM_ELL_ENTRY_BASIS = "ELL Entry Basis";
        private static final String CALC_PARAM_ELL_ENTRY_DATE = "ELL Entry Date";
        private static final String CALC_PARAM_ELL_CLASSIF_DATE = "ELL Classif Date";
        private static final String CALC_PARAM_ELL_STD_PLAN_DATE = "ELL Std Plan Date";
        private static final String CALC_PARAM_ELL_REEVAL_DATE = "ELL Reeval Date";
        private static final String CALC_PARAM_ELL_INSTRUCT_EXT = "ELL Instruct Ext";
        private static final String CALC_PARAM_ELL_EXIT_DATE = "ELL Exit Date ";
        private static final String CALC_PARAM_ELL_RECLAS_DATE = "ELL Reclas Date";
        private static final String CALC_PARAM_ELL_RECLAS_EXIT_DATE = "ELL Reclas Exit Date";
        private static final String CALC_PARAM_ELL_REPORT_CARD = "ELL Report Card ";
        private static final String CALC_PARAM_ELL_SEMIANN_REVIEW = "ELL Semiann Review ";
        private static final String CALC_PARAM_ELL_YEAR_2_END = "ELL Year 2 end";
        private static final String CALC_PARAM_ELL_EXIT_BASIS = "ELL Exit Basis ";

        private static final List<String> s_cteFields = Arrays.asList(FIELD_CTE_PGM_CODE, FIELD_CTE_PGM_NAME,
                FIELD_CTE_FULL_PGM_COMPL, FIELD_CTE_PGM_CERT_EARNED_DATE, FIELD_CTE_INDUS_CERT_ID,
                FIELD_CTE_INDUS_CERT_OUT, FIELD_CTE_ACADEMY_ID);

        private static final List<String> s_dropFields =
                Arrays.asList(FIELD_DRP_PROGRAM_CODE, FIELD_DRP_PLACEMNT_REASONS, FIELD_DRP_OUTCOMES);


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            Record05 record05 = (Record05) entity;
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

            if (s_cteFields.contains(parameter)) {
                String pgmCodeNum = field.getFieldId().replace(parameter, "");
                StudentProgramParticipation pgm = record05.getCteProgram(Integer.parseInt(pgmCodeNum));
                if (pgm != null) {
                    String aliasToUse = null;
                    switch (parameter) {
                        case FIELD_CTE_PGM_CODE:
                            aliasToUse = ALIAS_CTE_PROGRAM_ID;
                            break;
                        case FIELD_CTE_PGM_NAME: {
                            String pgmCode = (String) fasterData.getFieldValueByAlias(pgm, ALIAS_CTE_PROGRAM_ID,
                                    fasterData.getProgramDataset(Record05.DDX_ID_CTE).getDataDictionary());
                            return fasterData.findDependencyByCode(REF_TABLE_COURSE_NUM_TO_ABBREV_TITLE, pgmCode);
                        }
                        case FIELD_CTE_FULL_PGM_COMPL:
                            aliasToUse = ALIAS_CTE_FULL_PGM_COMPLETER;
                            break;
                        case FIELD_CTE_PGM_CERT_EARNED_DATE:
                            aliasToUse = ALIAS_CTE_CERT_EARNED_DATE;
                            break;
                        case FIELD_CTE_INDUS_CERT_ID:
                            aliasToUse = ALIAS_CTE_INDUST_CERT_ID;
                            break;
                        case FIELD_CTE_INDUS_CERT_OUT:
                            aliasToUse = ALIAS_CTE_INDUST_CERT_OUT;
                            break;
                        case FIELD_CTE_ACADEMY_ID:
                            aliasToUse = ALIAS_CTE_CAPE_ID;
                            break;

                        default:
                            break;
                    }
                    if (aliasToUse != null) {
                        return fasterData.getFieldValueByAlias(pgm, aliasToUse,
                                fasterData.getProgramDataset(Record05.DDX_ID_CTE).getDataDictionary());
                    }
                }
            }

            StudentProgramParticipation ellPgm = record05.getEllProgram();
            if (ellPgm != null) {
                String aliasToUse = null;
                switch (parameter) {
                    case CALC_PARAM_ELL_PK_12:
                        aliasToUse = ALIAS_ELL_PROGRAM_CODE;
                        break;
                    case CALC_PARAM_ELL_ENTRY_BASIS:
                        aliasToUse = ALIAS_ELL_BASIS_OF_ENTRY;
                        break;
                    case CALC_PARAM_ELL_ENTRY_DATE:
                        return ellPgm.getStartDate();
                    case CALC_PARAM_ELL_CLASSIF_DATE:
                        aliasToUse = ALIAS_ELL_CLASSIF_DATE;
                        break;
                    case CALC_PARAM_ELL_STD_PLAN_DATE:
                        aliasToUse = ALIAS_ELL_STUDENT_PLAN_DATE;
                        break;
                    case CALC_PARAM_ELL_REEVAL_DATE:
                        aliasToUse = ALIAS_ELL_REEVAL_DATE;
                        break;
                    case CALC_PARAM_ELL_INSTRUCT_EXT:
                        aliasToUse = ALIAS_ELL_EXTEN_OF_INSTRUCT;
                        break;
                    case CALC_PARAM_ELL_EXIT_DATE:
                        return ellPgm.getEndDate();
                    case CALC_PARAM_ELL_RECLAS_DATE:
                        aliasToUse = ALIAS_ELL_RECLASSIF_DATE;
                        break;
                    case CALC_PARAM_ELL_RECLAS_EXIT_DATE:
                        aliasToUse = ALIAS_ELL_RECLASSIF_EXIT_DATE;
                        break;
                    case CALC_PARAM_ELL_REPORT_CARD:
                        aliasToUse = ALIAS_ELL_REPORT_CARD;
                        break;
                    case CALC_PARAM_ELL_SEMIANN_REVIEW: {
                        String semiNum = field.getFieldId().replace(parameter, "");
                        aliasToUse = "1".equals(semiNum) ? ALIAS_ELL_FIRST_SEMI_DATE : ALIAS_ELL_SECOND_SEMI_DATE;
                        break;
                    }
                    case CALC_PARAM_ELL_YEAR_2_END:
                        aliasToUse = ALIAS_ELL_SECOND_YEAR_END;
                        break;
                    case CALC_PARAM_ELL_EXIT_BASIS: {
                        String basisNum = field.getFieldId().replace(CALC_PARAM_ELL_EXIT_BASIS, "");
                        aliasToUse =
                                "1".equals(basisNum) ? ALIAS_ELL_BASIS_OF_EXIT_FIRST : ALIAS_ELL_BASIS_OF_EXIT_SECOND;
                        break;
                    }

                    default:
                        break;
                }
                if (aliasToUse != null) {
                    return fasterData.getFieldValueByAlias(ellPgm, aliasToUse,
                            fasterData.getProgramDataset(Record05.DDX_ID_ELL).getDataDictionary());
                }
            }

            if (s_dropFields.contains(parameter)) {
                int dropoutPgmNum = Integer.parseInt(field.getFieldId().replace(parameter, ""));
                StudentProgramParticipation dropPgm = record05.getDropoutProgram(dropoutPgmNum);
                if (dropPgm != null) {
                    String aliasToUse = null;
                    switch (parameter) {
                        case FIELD_DRP_PROGRAM_CODE:
                            aliasToUse = ALIAS_DRP_CODE;
                            break;
                        case FIELD_DRP_PLACEMNT_REASONS:
                            aliasToUse = ALIAS_DRP_PLACEMENT_REASON;
                            break;
                        case FIELD_DRP_OUTCOMES:
                            aliasToUse = ALIAS_DRP_OUTCOME;
                            break;

                        default:
                            break;
                    }
                    if (aliasToUse != null) {
                        String value = (String) fasterData.getFieldValueByAlias(dropPgm, aliasToUse,
                                fasterData.getProgramDataset(Record05.DDX_ID_DROP).getDataDictionary());
                        if (value != null) {
                            return value.replaceAll("[,]", "");
                        }
                    }
                }
            }

            return null;
        }

    }


    /**
     * The Class RetrieveDecimalWoPoint.
     */
    public static class RetrieveDecimalWoPoint implements FieldRetriever {
        public static final String CALC_ID = "DECIMAL_WO_POINT";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String pattern = (String) field.getParameter();
            DecimalFormat formatter = new DecimalFormat(pattern);
            String decimal = (String) entity.getBean().getFieldValueByBeanPath(field.getBeanPath());
            if (decimal != null) {
                return formatter.format(Double.parseDouble(decimal)).replaceAll("[.]", "");
            }
            return null;
        }

    }


    /**
     * The Class RetrieveEll.
     */
    public static class RetrieveEll implements FieldRetriever {
        public static final String CALC_ID = "ELL";

        private static final String ALIAS_HOME_LANG_SURVEY_DATE = "pgm-survey-date";

        private static final String CALC_PARAM_HOME_LANG = "HOME_LANG_DATE";

        private static final String DDX_ID = "FL-PGM-ELL";

        private FLFasterRecordsData m_data;
        private StudentProgramDataset m_studentProgramDataset;


        /**
         * Instantiates a new retrieve ell.
         *
         * @param data FLFasterRecordsData
         * @throws X2BaseException exception
         */
        public RetrieveEll(FLFasterRecordsData data) throws X2BaseException {
            m_data = data;
            m_studentProgramDataset =
                    m_data.getStudentHelper().getStudentProgramDataset(DDX_ID, m_data.getSurveyPeriod());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String parameter = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();

            StudentProgramParticipation ellProgram =
                    m_data.getStudentHelper().getStudentProgram(student.getOid(), DDX_ID, m_data.getSurveyPeriod());

            switch (parameter) {
                case CALC_PARAM_HOME_LANG:
                    if (ellProgram != null) {
                        value = m_data.getFieldValueByAlias(ellProgram, ALIAS_HOME_LANG_SURVEY_DATE,
                                m_studentProgramDataset.getDataDictionary());
                    }
                    break;
                default:
                    break;
            }

            return value;
        }

    }


    /**
     * The Class RetrieveExcept.
     */
    public static class RetrieveExcept implements FieldRetriever {
        public static final String CALC_ID = "EXCEPT";

        private static final String ALIAS_CONSENT_EV_DATE = "pgm-evaluation-consent-date";
        private static final String ALIAS_DISMISSAL_DATE = "pgm-dismissal-date";
        private static final String ALIAS_ELIG_DATE = "pgm-eligibility-determination";
        private static final String ALIAS_EVAL_COMPL_DATE = "pgm-eval-completion-date";
        private static final String ALIAS_MSPS_DATE = "pgm-min-edu-perf-st-date";
        private static final String ALIAS_PLACEMENT_DATE = "pgm-placement-date";
        private static final String ALIAS_PLACEMENT_STATUS = "pgm-placement-status";
        private static final String ALIAS_PRIMARY = "pgm-primary";
        private static final String ALIAS_PLAN_DATE = "pgm-plan-date";
        private static final String ALIAS_REFERRAL_DATE = "pgm-referral-date";

        private static final String CALC_PARAM_EVAL_DATE = "EVAL_DATE";
        private static final String CALC_PARAM_MSPS_DATE = "MSPS_DATE";
        private static final String CALC_PARAM_PLAN_DATE = "PLAN_DATE";
        private static final String CALC_PARAM_PRIMARY = "PRIMARY";

        private static final String DDX_ID = "FL-PGM-EXCEPT";

        private static final List<String> s_numerableFields =
                Arrays.asList(FIELD_PLACEMENT_DATE, FIELD_ELIG_DETERM_DATE, FIELD_PLACEMENT_STATUS, FIELD_REFERRAL_DATE,
                        FIELD_DISMISSAL_DATE, FIELD_EXCEPTIONALITY, FIELD_EVAL_COMPL_DATE, FIELD_CONSENT_DATE_EV);

        private FLFasterRecordsData m_fasterData;
        private StudentProgramDataset m_studentProgramDataset;


        /**
         * Instantiates a new retrieve except.
         *
         * @param data FLFasterRecordsData
         * @throws X2BaseException exception
         */
        public RetrieveExcept(FLFasterRecordsData data) throws X2BaseException {
            m_fasterData = data;
            m_studentProgramDataset =
                    m_fasterData.getStudentHelper().getStudentProgramDataset(DDX_ID, m_fasterData.getSurveyPeriod());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            StudentProgramParticipation exceptPgm =
                    m_fasterData.getStudentHelper().getStudentProgram(entity.getBean().getOid(),
                            DDX_ID, m_fasterData.getSurveyPeriod());

            if (exceptPgm != null) {
                DataDictionary dictionary = m_studentProgramDataset.getDataDictionary();

                switch (parameter) {
                    case CALC_PARAM_MSPS_DATE:
                        return m_fasterData.getFieldValueByAlias(exceptPgm, ALIAS_MSPS_DATE, dictionary);

                    default:
                        break;
                }

                if (entity instanceof Record06) {
                    Record06 record06 = (Record06) entity;
                    switch (parameter) {
                        case CALC_PARAM_PLAN_DATE:
                            return m_fasterData.getFieldValueByAlias(exceptPgm, ALIAS_PLAN_DATE, dictionary);
                        case CALC_PARAM_PRIMARY:
                            return m_fasterData.getFieldValueByAlias(exceptPgm, ALIAS_PRIMARY, dictionary);
                        case CALC_PARAM_EVAL_DATE:
                            return m_fasterData.getFieldValueByAlias(exceptPgm, ALIAS_EVAL_COMPL_DATE, dictionary);
                        default:
                            break;
                    }

                    if (s_numerableFields.contains(parameter)) {
                        int pgmNum = Integer.parseInt(field.getFieldId().replace(parameter, ""));
                        StudentProgramParticipation currentExceptPgm = record06.getProgram(pgmNum);
                        String aliasToUse = null;
                        switch (parameter) {
                            case FIELD_PLACEMENT_DATE:
                                aliasToUse = ALIAS_PLACEMENT_DATE;
                                break;
                            case FIELD_ELIG_DETERM_DATE:
                                aliasToUse = ALIAS_ELIG_DATE;
                                break;
                            case FIELD_PLACEMENT_STATUS:
                                aliasToUse = ALIAS_PLACEMENT_STATUS;
                                break;
                            case FIELD_REFERRAL_DATE:
                                aliasToUse = ALIAS_REFERRAL_DATE;
                                break;
                            case FIELD_DISMISSAL_DATE:
                                aliasToUse = ALIAS_DISMISSAL_DATE;
                                break;
                            case FIELD_EXCEPTIONALITY:
                                aliasToUse = ALIAS_PRIMARY;
                                break;
                            case FIELD_EVAL_COMPL_DATE:
                                aliasToUse = ALIAS_EVAL_COMPL_DATE;
                                break;
                            case FIELD_CONSENT_DATE_EV:
                                aliasToUse = ALIAS_CONSENT_EV_DATE;
                                break;
                            default:
                                break;
                        }
                        if (aliasToUse != null) {
                            return m_fasterData.getFieldValueByAlias(currentExceptPgm, aliasToUse, dictionary);
                        }
                    }
                }
            }

            return null;
        }
    }


    /**
     * The Class RetrieveHealthInfo.
     */
    public static class RetrieveHealthInfo implements FieldRetriever {
        public static final String CALC_ID = "HLTH_INFO";

        private static final String ALIAS_HCN_DISEASE_COND_RESOLUTION = "all-hcn-DiseaseCondResolution";
        private static final String ALIAS_HCN_DISEASE_TYPE_CODE = "all-hcn-DiseaseTypeCode";
        private static final String ALIAS_HCN_FIRST_ENCOUNTER_DATE = "all-hcn-FirstEncounterDate";
        private static final String ALIAS_HCN_MEDICAL_TREAT_CODE = "all-hcn-MedTreatmentCode";

        private static final String ALIAS_TREATMENT_RECEIVED = "all-hsc-TreatmentReceived";
        private static final String ALIAS_TYPE_CODE = "all-hsc-IcdCptScreenType";

        private static final String CALC_PARAM_CRIT_CONTACT = "CRIT_HLTH_CONT";
        private static final String CALC_PARAM_DATE_HC = "DATE_HC";
        private static final String CALC_PARAM_DATE_HS = "DATE_HS";
        private static final String CALC_PARAM_DATE_FORMAT_HC = "DATE_FORMAT_HC";
        private static final String CALC_PARAM_DATE_FORMAT_HS = "DATE_FORMAT_HS";
        private static final String CALC_PARAM_HC_MED_TREAT = "HC_MED_TREAT";
        private static final String CALC_PARAM_HC_RESOLUTION = "HC_RESOLUTION";
        private static final String CALC_PARAM_HC_TYPE_CODE = "HC_TYPE_CODE";
        private static final String CALC_PARAM_RESULTS = "RESULTS";
        private static final String CALC_PARAM_SCREEN_HEARING = "HEARING";
        private static final String CALC_PARAM_SCREEN_VISION = "VISION";
        private static final String CALC_PARAM_TYPE_CODE = "TYPE_CODE";

        private static final String CODE_ASPEN_SCR_RESULT_FAIL = "Fail";
        private static final String CODE_ASPEN_SCR_RESULT_PASS = "Pass";

        private static final String CODE_FASTER_SCR_RESULT_FAIL = "ABN";
        private static final String CODE_FASTER_SCR_RESULT_NOT_AVAILABLE = "B33";
        private static final String CODE_FASTER_SCR_RESULT_PASS = "NOR";

        private static final String SCREENING_ID_HEARING = "HSC-HEARING";
        private static final String SCREENING_ID_VISION = "HSC-VISION";

        private FLFasterRecordsData m_fasterData;
        private SisStaff m_schoolHealthStaff;


        /**
         * Instantiates a new retrieve health info.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveHealthInfo(FLFasterRecordsData data) {
            m_fasterData = data;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            switch (parameter) {
                case CALC_PARAM_CRIT_CONTACT:
                    SisStaff healthStaff = getSchoolHealthStaff();
                    if (healthStaff != null) {
                        String name = healthStaff.getNameView();
                        String phoneNum = getPersonPhone(healthStaff.getPerson());
                        return name + ", " + phoneNum;
                    }
                    break;

                case CALC_PARAM_SCREEN_HEARING: {
                    HealthScreening hlthScreening =
                            getHealthScreeningByType(SCREENING_ID_HEARING, entity.getBean().getOid());
                    String screeningResult = getScreeningResult(hlthScreening);
                    if (!StringUtils.isEmpty(screeningResult)) {
                        return "H" + screeningResult;
                    }
                }
                    break;

                case CALC_PARAM_SCREEN_VISION: {
                    HealthScreening hlthScreening =
                            getHealthScreeningByType(SCREENING_ID_VISION, entity.getBean().getOid());
                    String screeningResult = getScreeningResult(hlthScreening);
                    if (!StringUtils.isEmpty(screeningResult)) {
                        return "V" + screeningResult;
                    }
                }
                    break;

                default:
                    break;
            }

            if (entity instanceof RecordG99HS) {
                RecordG99HS recordG99HS = (RecordG99HS) entity;
                HealthScreening healthScreening = recordG99HS.getCurrentHealthScreening();

                switch (parameter) {
                    case CALC_PARAM_TYPE_CODE:
                        return m_fasterData.getFieldValueByAlias(healthScreening, ALIAS_TYPE_CODE);

                    case CALC_PARAM_DATE_FORMAT_HS:
                        return healthScreening.getDate() == null ? null : DATE_FORMAT_DB;

                    case CALC_PARAM_DATE_HS:
                        return healthScreening.getDate();

                    case CALC_PARAM_RESULTS:
                        String resultCode = healthScreening.getResultCode();
                        if (CODE_ASPEN_SCR_RESULT_FAIL.equals(resultCode)) {
                            return CODE_FASTER_SCR_RESULT_FAIL;
                        }
                        if (CODE_ASPEN_SCR_RESULT_PASS.equals(resultCode)) {
                            return CODE_FASTER_SCR_RESULT_PASS;
                        }
                        return CODE_FASTER_SCR_RESULT_NOT_AVAILABLE;

                    default:
                        break;
                }
            }

            if (entity instanceof RecordG99HC) {
                RecordG99HC recordG99HC = (RecordG99HC) entity;
                HealthCondition healthCondition = recordG99HC.getCurrentHealthCondition();

                String aliasToUse = null;
                switch (parameter) {
                    case CALC_PARAM_HC_TYPE_CODE:
                        aliasToUse = ALIAS_HCN_DISEASE_TYPE_CODE;
                        break;
                    case CALC_PARAM_HC_MED_TREAT:
                        aliasToUse = ALIAS_HCN_MEDICAL_TREAT_CODE;
                        break;
                    case CALC_PARAM_DATE_FORMAT_HC:
                        return m_fasterData.getFieldValueByAlias(healthCondition,
                                ALIAS_HCN_FIRST_ENCOUNTER_DATE) == null ? null : DATE_FORMAT_DB;
                    case CALC_PARAM_DATE_HC:
                        aliasToUse = ALIAS_HCN_FIRST_ENCOUNTER_DATE;
                        break;
                    case CALC_PARAM_HC_RESOLUTION:
                        aliasToUse = ALIAS_HCN_DISEASE_COND_RESOLUTION;
                        break;

                    default:
                        break;
                }
                if (aliasToUse != null) {
                    return m_fasterData.getFieldValueByAlias(healthCondition, aliasToUse);
                }
            }

            return null;
        }


        /**
         * Gets the health screening by type.
         *
         * @param scrId String
         * @param studentOid String
         * @return Health screening
         * @throws X2BaseException exception
         */
        public HealthScreening getHealthScreeningByType(String scrId, String studentOid) throws X2BaseException {
            Collection<HealthScreening> hlthScreenings = m_fasterData.getStudentHealthScreenings(studentOid);
            String ddxOid = m_fasterData.getExtendedDataDictionaryById(scrId).getOid();
            for (HealthScreening hs : hlthScreenings) {
                if (ddxOid.equals(hs.getExtendedDataDictionaryOid())) {
                    return hs;
                }
            }
            return null;
        }


        /**
         * Gets the school health staff.
         *
         * @return Sis staff
         */
        public SisStaff getSchoolHealthStaff() {
            if (m_schoolHealthStaff == null) {
                SisStaff personToContact = getSchoolStaffByType("Nurse");

                if (personToContact == null) {
                    personToContact = getSchoolStaffByType("Central Office");
                }

                m_schoolHealthStaff = personToContact;
            }
            return m_schoolHealthStaff;
        }


        /**
         * Gets the school staff by type.
         *
         * @param type String
         * @return Sis staff
         */
        public SisStaff getSchoolStaffByType(String type) {
            if (m_fasterData.getSchool() != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, m_fasterData.getSchool().getOid());
                criteria.addEqualTo(SisStaff.COL_STATUS, "Active");
                criteria.addEqualTo(SisStaff.COL_STAFF_TYPE, type);
                QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
                return (SisStaff) m_fasterData.getBroker().getBeanByQuery(query);
            }
            return null;
        }


        /**
         * Gets the screening result.
         *
         * @param hs HealthScreening
         * @return String
         * @throws X2BaseException exception
         */
        public String getScreeningResult(HealthScreening hs) throws X2BaseException {
            if (hs != null) {
                if ("Pass".equals(hs.getResultCode())) {
                    return "Y";
                }
                if ("Fail".equals(hs.getResultCode())) {
                    Boolean treatmentReceived =
                            (Boolean) m_fasterData.getFieldValueByAlias(hs, ALIAS_TREATMENT_RECEIVED);
                    if (treatmentReceived != null && treatmentReceived.booleanValue()) {
                        return "T";
                    }
                    return "N";
                }
            }
            return null;
        }


        /**
         * Gets the person phone.
         *
         * @param person SisPerson
         * @return String
         */
        public String getPersonPhone(SisPerson person) {
            return person.getPhone01() == null
                    ? (person.getPhone02() == null ? person.getPhone03() : person.getPhone02())
                    : person.getPhone01();
        }
    }


    /**
     * The Class RetrieveImmunization.
     */
    public static class RetrieveImmunization implements FieldRetriever {
        public static final String CALC_ID = "IMMUNIZATION";

        private static final String ALIAS_HIM_VACCINE_TYPE = "all-him-FLVaccineType";
        private static final String ALIAS_HIS_IMMUNIZATION_DATE = "all-his-FLImmunizationDate";
        private static final String ALIAS_HIS_IMMUNIZED_ELIGIBLE_FORM = "all-his-FLImmunizedEligibleForm";
        private static final String ALIAS_HIS_EXEMPTION = "all-his-FLExemption";
        private static final String ALIAS_HIS_HAD_DISEASE_IND = "all-his-FLHadDisease";
        private static final String ALIAS_HIS_SOURCE_CODE = "all-his-FLSourceCode";

        private static final String ALIAS_STD_IMM_ELIGIBLE = "all-std-FLImmunizedEligibleForm";
        private static final String ALIAS_STD_IMM_EXEMPTION = "all-std-FLExemption";

        private static final String CALC_PARAM_DATE_FORMAT = "DATE_FORMAT_IMM";
        private static final String CALC_PARAM_IMM_DATE = "DATE_IMM";
        private static final String CALC_PARAM_SOURCE_CODE = "SOURCE_CODE";
        private static final String CALC_PARAM_STATUS_CODE = "STATUS_CODE";
        private static final String CALC_PARAM_STATUS_DATE = "STATUS_DATE";
        private static final String CALC_PARAM_TYPE_CODE = "TYPE_CODE";

        private static final String CODE_DATE_EXEMPT = "EXEMPT  ";
        private static final String CODE_DATE_WO_DATE = "99999999";
        private static final String CODE_DOSE_WO_DOSE = "0";

        private static final String CODE_EXEMPTION_MEDICAL = "Medical";
        private static final String CODE_EXEMPTION_PERSONAL = "Personal";
        private static final String CODE_EXEMPTION_RELIGIOUS = "Religious";

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyyMMdd");
        private FLFasterRecordsData m_fasterData = null;
        private Map<String, List<HealthImmunizationDose>> m_studentDoses = new HashMap<>();
        private Map<String, Map<String, List<HealthImmunizationDose>>> m_studentGroupedDosesLists = new HashMap<>();


        /**
         * Instantiates a new retrieve immunization.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveImmunization(FLFasterRecordsData data) {
            m_fasterData = data;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();

            if (parameter.equals(CALC_PARAM_STATUS_DATE)) {
                List<HealthImmunizationDose> doses = getStudentDosesList(entity.getBean().getOid());
                HealthImmunizationDose currentDose = null;
                int currentStatusNum = Integer.parseInt(field.getFieldId().replaceAll(FIELD_STATUS_DATE, ""));
                if (doses.size() >= currentStatusNum) {
                    currentDose = doses.get(currentStatusNum - 1);
                }

                if (currentDose != null) {
                    String vaccineType =
                            getVaccineType(currentDose.getImmunizationSeries().getImmunizationDefinition());

                    int doseNum =
                            getDoseNum(entity.getBean().getOid(), currentDose.getImmunizationSeriesOid(), currentDose);
                    return vaccineType + doseNum + m_dateFormat.format(currentDose.getDate());
                }

                int currentStatusNumBasedOnSeries = currentStatusNum - doses.size();
                List<HealthImmunizationSeries> seriesWoDoses = getStudentSeriesWoDoses(entity.getBean().getOid());
                if (seriesWoDoses.size() >= currentStatusNumBasedOnSeries) {
                    HealthImmunizationSeries currentSeries = seriesWoDoses.get(currentStatusNumBasedOnSeries - 1);

                    Boolean isEligibleForm =
                            (Boolean) m_fasterData.getFieldValueByAlias(currentSeries,
                                    ALIAS_HIS_IMMUNIZED_ELIGIBLE_FORM);
                    if (isEligibleForm != null && isEligibleForm.booleanValue()) {
                        String vaccineType = getVaccineType(currentSeries.getImmunizationDefinition());
                        return vaccineType + CODE_DOSE_WO_DOSE + CODE_DATE_WO_DATE;
                    }

                    String exemption = (String) m_fasterData.getFieldValueByAlias(currentSeries,
                            ALIAS_HIS_EXEMPTION);
                    if (!StringUtils.isEmpty(exemption)) {
                        String vaccineType = getVaccineType(currentSeries.getImmunizationDefinition());
                        return vaccineType + CODE_DOSE_WO_DOSE + CODE_DATE_EXEMPT;
                    }
                }

                int currentStatusNumBasedOnStudent = currentStatusNum - doses.size() - seriesWoDoses.size();

                if (currentStatusNumBasedOnStudent == 1) {
                    SisStudent student = (SisStudent) entity.getBean();
                    Boolean eligible = (Boolean) m_fasterData.getFieldValueByAlias(student, ALIAS_STD_IMM_ELIGIBLE);

                    if (eligible != null && eligible.booleanValue()) {
                        return 8 + CODE_DOSE_WO_DOSE + CODE_DATE_WO_DATE;
                    }
                }

                if (currentStatusNumBasedOnStudent == 2) {
                    SisStudent student = (SisStudent) entity.getBean();
                    String exemption = (String) m_fasterData.getFieldValueByAlias(student, ALIAS_STD_IMM_EXEMPTION);
                    if (!StringUtils.isEmpty(exemption)) {
                        return 9 + CODE_DOSE_WO_DOSE + CODE_DATE_EXEMPT;
                    }
                }
            }

            if (entity instanceof RecordG99IMM) {
                RecordG99IMM recordG99Imm = (RecordG99IMM) entity;
                HealthImmunizationSeries series = recordG99Imm.getCurrentImmunization();
                List<HealthImmunizationDose> doses =
                        getStudentDosesBySeriesOid(entity.getBean().getOid(), series.getOid());
                switch (parameter) {
                    case CALC_PARAM_TYPE_CODE:
                        String immId = series.getImmunizationDefinition().getId();
                        return m_fasterData.findCodeByDependency(REF_TABLE_IMMUNIZATION_TYPES_TO_FASTER, immId);
                    case CALC_PARAM_DATE_FORMAT:
                        String immDate = entity.getFieldValue(FIELD_IMM_DATE).trim();
                        return StringUtils.isEmpty(immDate) ? null : DATE_FORMAT_DB;
                    case CALC_PARAM_IMM_DATE: {
                        if (doses != null && !doses.isEmpty()) {
                            LinkedList<HealthImmunizationDose> dosesList = new LinkedList<>(doses);
                            HealthImmunizationDose lastDose = dosesList.getLast();
                            return lastDose.getDate();
                        }
                        return m_fasterData.getFieldValueByAlias(series, ALIAS_HIS_IMMUNIZATION_DATE);
                    }
                    case CALC_PARAM_STATUS_CODE:
                        String exemption = (String) m_fasterData.getFieldValueByAlias(series, ALIAS_HIS_EXEMPTION);
                        if (!StringUtils.isEmpty(exemption)) {
                            switch (exemption) {
                                case CODE_EXEMPTION_MEDICAL:
                                    return "10";
                                case CODE_EXEMPTION_PERSONAL:
                                    return "11";
                                case CODE_EXEMPTION_RELIGIOUS:
                                    return "12";

                                default:
                                    break;
                            }
                        }
                        if (doses != null && !doses.isEmpty()) {
                            return "0" + doses.size();
                        }
                        Boolean hadDiseaseIndicator =
                                (Boolean) m_fasterData.getFieldValueByAlias(series, ALIAS_HIS_HAD_DISEASE_IND);
                        if (hadDiseaseIndicator != null && hadDiseaseIndicator.booleanValue()) {
                            return "13";
                        }
                        return "14";
                    case CALC_PARAM_SOURCE_CODE:
                        return m_fasterData.getFieldValueByAlias(series, ALIAS_HIS_SOURCE_CODE);

                    default:
                        break;
                }
            }

            return null;
        }


        /**
         * Gets the dose num.
         *
         * @param studentOid String
         * @param seriesOid String
         * @param dose HealthImmunizationDose
         * @return int
         */
        private int getDoseNum(String studentOid, String seriesOid, HealthImmunizationDose dose) {
            return getStudentDosesBySeriesOid(studentOid, seriesOid).indexOf(dose) + 1;
        }


        /**
         * Gets the student doses by series oid.
         *
         * @param studentOid String
         * @param seriesOid String
         * @return List
         */
        private List<HealthImmunizationDose> getStudentDosesBySeriesOid(String studentOid, String seriesOid) {
            if (m_studentGroupedDosesLists.get(studentOid) == null) {
                Map<String, List<HealthImmunizationDose>> seriesDosesMap =
                        new HashMap<String, List<HealthImmunizationDose>>();
                m_studentGroupedDosesLists.put(studentOid, seriesDosesMap);
                List<HealthImmunizationDose> studentDoses = getStudentDosesList(studentOid);
                for (HealthImmunizationDose dose : studentDoses) {
                    String currentSeriesOid = dose.getImmunizationSeriesOid();
                    List<HealthImmunizationDose> seriesDoses = seriesDosesMap.get(currentSeriesOid);
                    if (seriesDoses == null) {
                        seriesDoses = new ArrayList<>();
                        seriesDosesMap.put(currentSeriesOid, seriesDoses);
                    }
                    seriesDoses.add(dose);
                }
            }
            return m_studentGroupedDosesLists.get(studentOid).get(seriesOid);
        }


        /**
         * Gets the student doses list.
         *
         * @param studentOid String
         * @return List
         */
        private List<HealthImmunizationDose> getStudentDosesList(String studentOid) {
            ArrayList<HealthImmunizationDose> doses = new ArrayList<>();
            for (HealthImmunizationSeries his : m_fasterData.getStudentImmunizationSeries(studentOid)) {
                List<HealthImmunizationDose> currentDoses = new ArrayList(his.getImmunizationDoses());
                Collections.sort(currentDoses, new Comparator<HealthImmunizationDose>() {
                    @Override
                    public int compare(HealthImmunizationDose o1, HealthImmunizationDose o2) {
                        return o1.getDate().compareTo(o2.getDate());
                    }
                });
                doses.addAll(currentDoses);
            }

            m_studentDoses.put(studentOid, doses);

            return m_studentDoses.get(studentOid);
        }


        /**
         * Gets the student series wo doses.
         *
         * @param studentOid String
         * @return List
         */
        private List<HealthImmunizationSeries> getStudentSeriesWoDoses(String studentOid) {
            List<HealthImmunizationSeries> series = m_fasterData.getStudentImmunizationSeries(studentOid);
            List<HealthImmunizationSeries> seriesWoDoses = new ArrayList<HealthImmunizationSeries>();
            if (series != null) {
                for (HealthImmunizationSeries currentSeries : series) {
                    if (getStudentDosesBySeriesOid(studentOid, currentSeries.getOid()) == null) {
                        seriesWoDoses.add(currentSeries);
                    }
                }
            }
            return seriesWoDoses;
        }


        /**
         * Gets the vaccine type.
         *
         * @param him HealthImmunizationDefinition
         * @return String
         * @throws X2BaseException exception
         */
        private String getVaccineType(HealthImmunizationDefinition him)
                throws X2BaseException {
            String vaccineType = (String) m_fasterData.getFieldValueByAlias(him, ALIAS_HIM_VACCINE_TYPE);
            if (StringUtils.isEmpty(vaccineType)) {
                // as we don't have vaccine type for this immunization definition, return anything
                // that would cause validation error
                return "X";
            }
            return vaccineType;
        }
    }


    /**
     * The Class RetrieveMessageType.
     */
    public static class RetrieveMessageType implements FieldRetriever {
        public static final String CALC_ID = "MESSAGE_TYPE";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            return fasterData.getMessageType();
        }

    }


    /**
     * The Class RetrieveMigrant.
     */
    public static class RetrieveMigrant implements FieldRetriever {
        public static final String CALC_ID = "MIGRANT";

        private static final String ALIAS_QAD = "pgm-qualarrival-date";
        private static final String ALIAS_QAD_FROM_CITY = "pgm-qad-from-city";
        private static final String ALIAS_QAD_FROM_COUNTRY = "pgm-qad-from-country";
        private static final String ALIAS_QAD_FROM_STATE = "pgm-qad-from-state";
        private static final String ALIAS_QAD_TO_CITY = "pgm-qad-to-city";
        private static final String ALIAS_QAD_TO_STATE = "pgm-qad-to-state";
        private static final String ALIAS_RESIDENCY_DATE = "pgm-residency-date";
        private static final String ALIAS_SERVICES_CONTINUATION = "pgm-continuation";
        private static final String ALIAS_SERVICES_PRIORITY = "pgm-priority";

        private static final String ALIAS_STD_MIGRANT_STATUS_TERM = "all-std-FLMigrantStatus";

        private static final String CALC_PARAM_ANNUAL_TERM = "ANNUAL_TERM";
        private static final String CALC_PARAM_DST = "DST";
        private static final String CALC_PARAM_ENR_DATE = "ENR_DATE";
        private static final String CALC_PARAM_FROM_CITY = "FROM_CITY";
        private static final String CALC_PARAM_FROM_COUNTRY = "FROM_COUNTRY";
        private static final String CALC_PARAM_FROM_STATE = "FROM_STATE";
        private static final String CALC_PARAM_QAD = "QAD";
        private static final String CALC_PARAM_RESIDENCY_DATE = "RESIDENCY_DATE";
        private static final String CALC_PARAM_SKL = "SKL";
        private static final String CALC_PARAM_SKL_ENR_DATE = "SKL_ENR_DATE";
        private static final String CALC_PARAM_SKL_WDRAW_DATE = "SKL_WDRAW_DATE";
        private static final String CALC_PARAM_SERVICES_CONT = "SRVCS_CONT";
        private static final String CALC_PARAM_SERVICES_PRIOR = "SRVCS_PRIOR";
        private static final String CALC_PARAM_SUMMER_TERM = "SUMMER_TERM";
        private static final String CALC_PARAM_TO_CITY = "TO_CITY";
        private static final String CALC_PARAM_TO_STATE = "TO_STATE";

        private static final String DDX_ID = "FL-PGM-MIGRANT";

        private static final String PREFIX_QA = "QA ";

        private FLFasterRecordsData m_data;
        private DataDictionaryField m_fieldSheduleTermCode;
        private StudentProgramDataset m_studentProgramDataset;


        /**
         * Instantiates a new retrieve migrant.
         *
         * @param data FLFasterRecordsData
         * @throws X2BaseException exception
         */
        public RetrieveMigrant(FLFasterRecordsData data) throws X2BaseException {
            m_data = data;

            m_studentProgramDataset =
                    m_data.getStudentHelper().getStudentProgramDataset(DDX_ID, m_data.getSurveyPeriod());
            m_fieldSheduleTermCode = m_data.getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(),
                    ScheduleTerm.COL_CODE);
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();

            SisStudent student = (SisStudent) entity.getBean();

            StudentProgramParticipation migrantPgm =
                    m_data.getStudentHelper().getStudentProgram(entity.getBean().getOid(),
                            DDX_ID, m_data.getSurveyPeriod());

            switch (parameter) {
                case CALC_PARAM_RESIDENCY_DATE:
                    return getMmddccyy(m_data.getFieldValueByAlias(migrantPgm, ALIAS_RESIDENCY_DATE,
                            m_studentProgramDataset.getDataDictionary()));

                case CALC_PARAM_SUMMER_TERM: {
                    if (isMigrant(student)) {
                        List<StudentScheduleInfo> schInfoList =
                                m_data.getScheduleHelperForYear(Integer.valueOf(m_data.getCurrentContext().getSchoolYear()))
                                        .getStudentScheduleInfo(student);
                        for (int i = 0; i < schInfoList.size(); i++) {
                            StudentScheduleInfo schInfo = schInfoList.get(i);
                            ScheduleTerm term = schInfo.getSection().getScheduleTerm();
                            String termCode = (String) m_data.getFieldValue(term, m_fieldSheduleTermCode);
                            if (s_summerTermCodes.contains(termCode)) {
                                return "S";
                            }
                        }
                    }
                }
                    break;

                case CALC_PARAM_ANNUAL_TERM: {
                    if (isMigrant(student)) {
                        List<StudentScheduleInfo> schInfoList =
                                m_data.getScheduleHelperForYear(Integer.valueOf(m_data.getCurrentContext().getSchoolYear()))
                                        .getStudentScheduleInfo(student);
                        return schInfoList.size() > 0 ? "3" : null;
                    }
                }
                    break;

                case CALC_PARAM_ENR_DATE: {
                    if (isMigrant(student) && migrantPgm != null) {
                        return migrantPgm.getStartDate();
                    }
                }
                    break;

                case CALC_PARAM_SERVICES_CONT: {
                    if (migrantPgm != null) {
                        return m_data.getFieldValueByAlias(migrantPgm, ALIAS_SERVICES_CONTINUATION,
                                m_studentProgramDataset.getDataDictionary());
                    }
                }
                    break;

                case CALC_PARAM_SERVICES_PRIOR: {
                    if (isMigrant(student) && migrantPgm != null) {
                        Boolean designatedAsPriority =
                                (Boolean) m_data.getFieldValueByAlias(migrantPgm, ALIAS_SERVICES_PRIORITY,
                                        m_studentProgramDataset.getDataDictionary());
                        if (designatedAsPriority != null) {
                            return designatedAsPriority.booleanValue() ? "Y" : "N";
                        }
                    }
                }
                    break;

                case CALC_PARAM_SKL_ENR_DATE: {
                    if (entity instanceof Record03) {
                        Record03 entity03 = (Record03) entity;
                        StudentEnrollmentSpan span = entity03.getCurrentSpan();
                        if (span != null) {
                            return span.getFirstActiveDate();
                        }
                    }
                }
                    break;

                case CALC_PARAM_SKL_WDRAW_DATE: {
                    if (entity instanceof Record03) {
                        Record03 entity03 = (Record03) entity;
                        StudentEnrollmentSpan span = entity03.getCurrentSpan();
                        if (span != null) {
                            return span.getLastActiveDate();
                        }
                    }
                }
                    break;

                case CALC_PARAM_DST: {
                    if (entity instanceof Record03) {
                        Record03 entity03 = (Record03) entity;
                        StudentEnrollmentSpan span = entity03.getCurrentSpan();
                        if (span != null) {
                            return m_data.getFieldValueByAlias(span.getSchool().getOrganization1(), ALIAS_ORG_DST_NUM);
                        }
                    }
                }
                    break;

                case CALC_PARAM_SKL: {
                    if (entity instanceof Record03) {
                        Record03 entity03 = (Record03) entity;
                        StudentEnrollmentSpan span = entity03.getCurrentSpan();
                        if (span != null) {
                            return m_data.getFieldValueByAlias(span.getSchool(), ALIAS_SKL_STATE_ID);
                        }
                    }
                }
                    break;

                default:
                    break;
            }

            if (field.getFieldId().startsWith(PREFIX_QA)) {
                int idx = getQaIndex(field.getFieldId());
                if (idx == 1) { // On an outgoing transcript, only provide one
                    switch (parameter) {
                        case CALC_PARAM_QAD:
                            return getMmddccyy(m_data.getFieldValueByAlias(migrantPgm, ALIAS_QAD,
                                    m_studentProgramDataset.getDataDictionary()));
                        case CALC_PARAM_FROM_CITY:
                            return m_data.getFieldValueByAlias(migrantPgm, ALIAS_QAD_FROM_CITY,
                                    m_studentProgramDataset.getDataDictionary());
                        case CALC_PARAM_FROM_COUNTRY:
                            return m_data.getFieldValueByAlias(migrantPgm, ALIAS_QAD_FROM_COUNTRY,
                                    m_studentProgramDataset.getDataDictionary());
                        case CALC_PARAM_FROM_STATE:
                            return m_data.getFieldValueByAlias(migrantPgm, ALIAS_QAD_FROM_STATE,
                                    m_studentProgramDataset.getDataDictionary());
                        case CALC_PARAM_TO_CITY:
                            return m_data.getFieldValueByAlias(migrantPgm, ALIAS_QAD_TO_CITY,
                                    m_studentProgramDataset.getDataDictionary());
                        case CALC_PARAM_TO_STATE:
                            return m_data.getFieldValueByAlias(migrantPgm, ALIAS_QAD_TO_STATE,
                                    m_studentProgramDataset.getDataDictionary());
                        default:
                            break;
                    }
                }
            }
            return null;
        }


        /**
         * Checks if is migrant.
         *
         * @param student SisStudent
         * @return true, if is migrant
         * @throws X2BaseException exception
         */
        private boolean isMigrant(SisStudent student) throws X2BaseException {
            String migrantStatus = (String) m_data.getFieldValueByAlias(student, ALIAS_STD_MIGRANT_STATUS_TERM);
            return !StringUtils.isEmpty(migrantStatus) && !"Z".equals(migrantStatus);
        }


        /**
         * Gets the mmddccyy.
         *
         * @param value Object
         * @return Object
         */
        private Object getMmddccyy(Object value) {
            return value != null && value instanceof PlainDate ? DATE_FORMAT_MMDDCCYY.format(value) : value;
        }


        /**
         * Gets the qa index.
         *
         * @param name String
         * @return int
         */
        private int getQaIndex(String name) {
            int idx = 0;
            if (!StringUtils.isEmpty(name) && name.contains(" ")) {
                String num = name.substring(name.lastIndexOf(" ") + 1);
                idx = Integer.parseInt(num);
            }
            return idx;
        }
    }


    /**
     * The Class RetrieveMilitary.
     */
    public static class RetrieveMilitary implements FieldRetriever {
        public static final String CALC_ID = "MILITARY";

        private static final String CALC_PARAM = "MILITARY_FAMILY";

        private static final String DDX_ID = "FL-PGM-MFS";

        private static final String DEFAULT_VALUE = "N";
        private static final String PK_VALUE = "Z";
        private static final String TRUE_VALUE = "Y";

        private FLFasterRecordsData m_data;


        /**
         * Instantiates a new retrieve military.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveMilitary(FLFasterRecordsData data) {
            m_data = data;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String parameter = (String) field.getParameter();

            switch (parameter) {
                case CALC_PARAM:
                    value = DEFAULT_VALUE;
                    String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
                    if (FLStudentHelper.GRADE_LEVEL_PK.equals(gradeLevel)) {
                        value = PK_VALUE;
                    } else {
                        List<StudentProgramParticipation> pgms =
                                m_data.getStudentHelper().getStudentPrograms(entity.getBean().getOid(),
                                        DDX_ID, m_data.getSurveyPeriod());
                        if (pgms != null && !pgms.isEmpty()) {
                            value = TRUE_VALUE;
                        }

                    }
                    break;

                default:
                    break;
            }

            return value;
        }
    }


    /**
     * The Class RetrieveProgram.
     */
    public static class RetrieveProgram implements FieldRetriever {
        public static final String CALC_ID = "PROGRAM";

        private static final String CALC_PARAM_ESFL = "ESFL";
        private static final String CALC_PARAM_FFSP = "FFSP";

        private static final int AGE_THROUGH_ESFL = 7;
        private static final int AGE_THROUGH_FFSP = 3;

        private static final String ALIAS_ESFL = "all-std-EvenStartFamilyLitPgm";
        private static final String ALIAS_FFSP = "all-std-FirstStartPgm";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            SisStudent student = (SisStudent) entity.getBean();
            String parameter = (String) field.getParameter();
            int studentAge = student.getPerson().getAgeAsOfDate(fasterData.getSurveyPeriod().getDateCertain());
            switch (parameter) {
                case CALC_PARAM_ESFL: {
                    if (studentAge < AGE_THROUGH_ESFL) {
                        return fasterData.getFieldValueByAlias(student, ALIAS_ESFL);
                    }
                }
                    break;

                case CALC_PARAM_FFSP: {
                    if (studentAge < AGE_THROUGH_FFSP) {
                        return fasterData.getFieldValueByAlias(student, ALIAS_FFSP);
                    }
                }
                    break;

                default:
                    break;
            }
            return null;
        }

    }


    /**
     * The Class RetrieveRace.
     */
    public static class RetrieveRace implements FieldRetriever {
        public static final String CALC_ID = "RACE";

        private static final String CALC_PARAM_CATEGORY = "CATEGORY";

        private static final String RACE_AMERICAN_INDIAN = "I";
        private static final String RACE_ASIAN = "A";
        private static final String RACE_BLACK = "B";
        private static final String RACE_HISPANIC = "H";
        private static final String RACE_MULTIRACIAL = "M";
        private static final String RACE_PACIFIC_ISLANDER = "P";
        private static final String RACE_WHITE = "W";

        private static final String VALUE_NO = "N";
        private static final String VALUE_YES = "Y";

        private static final List<String> s_raceCodes =
                Arrays.asList(RACE_WHITE, RACE_BLACK, RACE_ASIAN, RACE_PACIFIC_ISLANDER, RACE_AMERICAN_INDIAN);

        private Map<String, String> m_raceStateCodes = new HashMap();


        /**
         * Instantiates a new retrieve race.
         *
         * @param data StateReportData
         */
        public RetrieveRace(StateReportData data) {
            super();

            DataDictionaryField field =
                    data.getDataDictionary().findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (field != null && field.getReferenceTableOid() != null) {
                Map<String, ReferenceCode> codes = data.getReferenceCodes(field.getReferenceTableOid());
                for (ReferenceCode item : codes.values()) {
                    m_raceStateCodes.put(item.getCode(), item.getStateCode());
                }
            }
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String value = VALUE_NO;
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            Collection<Race> races = fasterData.getStudentRaceMap().get(student.getPersonOid());
            if (!CALC_PARAM_CATEGORY.equals(field.getParameter())) {
                String raceCode = (String) field.getParameter();
                if (races != null) {
                    for (Race race : races) {
                        if (raceCode.equalsIgnoreCase(m_raceStateCodes.get(race.getRaceCode()))) {
                            value = VALUE_YES;
                            break;
                        }
                    }
                }
            } else if (CALC_PARAM_CATEGORY.equals(field.getParameter())) {
                Collection<String> raceCodes = new ArrayList<>();
                for (Race race : races) {
                    raceCodes.add(race.getRaceCode());
                }

                if (isHispanic(student)) {
                    value = RACE_HISPANIC;
                } else if (isMultiracial(races)) {
                    value = RACE_MULTIRACIAL;
                } else if (raceCodes.contains(RACE_WHITE)) {
                    value = RACE_WHITE;
                } else if (raceCodes.contains(RACE_BLACK)) {
                    value = RACE_BLACK;
                } else if (raceCodes.contains(RACE_ASIAN) || raceCodes.contains(RACE_PACIFIC_ISLANDER)) {
                    value = RACE_ASIAN;
                } else if (raceCodes.contains(RACE_AMERICAN_INDIAN)) {
                    value = RACE_AMERICAN_INDIAN;
                }
            }

            return value;
        }


        /**
         * Checks if is hispanic.
         *
         * @param student SisStudent
         * @return true, if is hispanic
         */
        private boolean isHispanic(SisStudent student) {
            return student.getPerson().getHispanicLatinoIndicator();
        }


        /**
         * Checks if is multiracial.
         *
         * @param races Collection<Race>
         * @return true, if is multiracial
         */
        private boolean isMultiracial(Collection<Race> races) {
            int raceCounter = 0;
            for (Race race : races) {
                if (s_raceCodes.contains(race.getRaceCode())) {
                    raceCounter++;
                }
            }
            return raceCounter > 1;
        }
    }


    /**
     * The Class RetrieveRecordType.
     */
    public static class RetrieveRecordType implements FieldRetriever {
        public static final String CALC_ID = "RECORD_TYPE";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLRecordEntity recordEntity = (FLRecordEntity) entity;
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            return recordEntity.getFullRecordTypeCode(fasterData.getRecordsType());
        }

    }


    /**
     * The Class RetrieveSendingInst.
     */
    public static class RetrieveSendingInst implements FieldRetriever {
        public static final String CALC_ID = "SENDING";

        private static final String ALIAS_DISTRICT_NUMBER = "all-org-DistrictNumber";
        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private static final String CALC_PARAM_DST = "DST";
        private static final String CALC_PARAM_SKL = "SKL";

        private String m_sendingDistrict = null;
        private String m_sendingSchool = null;


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;

            if (m_sendingDistrict == null) {
                m_sendingDistrict =
                        (String) fasterData.getFieldValueByAlias(data.getSchool().getOrganization1(),
                                ALIAS_DISTRICT_NUMBER);
            }

            if (m_sendingSchool == null) {
                m_sendingSchool = (String) fasterData.getFieldValueByAlias(data.getSchool(), ALIAS_SCHOOL_NUMBER);
            }

            String value = null;
            String calcParameter = (String) field.getParameter();
            switch (calcParameter) {
                case CALC_PARAM_DST:
                    value = m_sendingDistrict;
                    break;
                case CALC_PARAM_SKL:
                    value = m_sendingSchool;
                    break;

                default:
                    break;
            }
            return value;
        }
    }


    /**
     * The Class RetrieveStudentInfo.
     */
    public static class RetrieveStudentInfo implements FieldRetriever {
        public static final String CALC_ID = "STD_INFO";

        private static final String ALIAS_COMPLETION_CERTIFICATE_TYPE = "all-std-CertificateOfCompletion";
        private static final String ALIAS_COMPLETION_CERTIFICATE_DATE = "all-std-CertificateOfCompletionDate";
        private static final String ALIAS_DIPLOMA_DATE = "all-std-DiplomaDate";
        private static final String ALIAS_DIPLOMA_TYPE = "all-std-DiplomaType";
        private static final String ALIAS_FEE_STATUS = "all-std-WdisFeeStatusFirst";
        private static final String ALIAS_NINTH_GRADE = "all-std-NinthGradeYear";
        private static final String ALIAS_PHYS_EDU_WAIVER = "all-std-PhysEduWaiver";
        private static final String ALIAS_PROMO_STATUS = "all-std-GradePromotionStatus";
        private static final String ALIAS_RESIDENT_STATUS = "all-enr-ResidentStatus";

        private static final String CALC_PARAM_COC_TYPE = "COC_TYPE";
        private static final String CALC_PARAM_COC_DATE = "COC_DATE";
        private static final String CALC_PARAM_DIPLOMA_DATE = "DIPLOMA_DATE";
        private static final String CALC_PARAM_DIPLOMA_TYPE = "DIPLOMA_TYPE";
        private static final String CALC_PARAM_FEE_STATUS = "Adult Fee Status 1st";
        private static final String CALC_PARAM_FEFP_NUM = "FEFP_NUM";
        private static final String CALC_PARAM_GRADE_LVL = "GRADE_LVL";
        private static final String CALC_PARAM_NINE_GRADE = "NINTH_GRADE";
        private static final String CALC_PARAM_PHYS_EDU = "PHYS_EDU";
        private static final String CALC_PARAM_PROMO_STATUS = "PROMO_STATUS";
        private static final String CALC_PARAM_RESIDENT_STATUS = "RESIDENT_STATUS";
        private static final String CALC_PARAM_RESID_TUITION_PURPOSES = "RES_TUITION_PURP";
        private static final String CALC_PARAM_SCHOOL_NAME = "SCHOOL_NAME";
        private static final String CALC_PARAM_SCHOOL_ADDRESS_1 = "SCHOOL_ADDRESS_1";
        private static final String CALC_PARAM_SCHOOL_ADDRESS_2 = "SCHOOL_ADDRESS_2";
        private static final String CALC_PARAM_SCHOOL_PHONE = "SCHOOL_PHONE";
        private static final String CALC_PARAM_WDRAW_CODE = "WDRAW_CODE";
        private static final String CALC_PARAM_WDRAW_DATE = "WDRAW_DATE";

        private static final String PATTERN_KG_TO_8 = "^KG|0[1-8]$";

        private FLFasterRecordsData m_data;

        private final List<String> REPORTING_GRADES = Arrays.asList("09", "10", "11", "12");


        /**
         * Instantiates a new retrieve student info.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveStudentInfo(FLFasterRecordsData data) {
            m_data = data;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();

            SisStudent student = (SisStudent) entity.getBean();
            StudentInfo studentInfo = m_data.getStudentHelper().getStudentInfo(student);

            switch (parameter) {
                case CALC_PARAM_GRADE_LVL:
                    return studentInfo.getGradeLevel(m_data.getSurveyPeriod().getStartDate());

                case CALC_PARAM_NINE_GRADE:
                    if (REPORTING_GRADES
                            .contains(studentInfo.getGradeLevel(m_data.getSurveyPeriod().getSnapshotDate()))) {
                        Object ninthGrade = m_data.getFieldValueByAlias(student, ALIAS_NINTH_GRADE);
                        if (ninthGrade != null) {
                            Integer ninthGradeStartYear = Integer.valueOf(ninthGrade.toString());
                            int ninthGradeEndYear = ninthGradeStartYear.intValue() + 1;
                            return ninthGradeStartYear.toString() + ninthGradeEndYear;
                        }
                    }
                    break;

                case CALC_PARAM_SCHOOL_NAME: {
                    SisSchool school = studentInfo.getSchool(m_data.getSurveyPeriod().getDateCertain());
                    return school.getName();
                }

                case CALC_PARAM_SCHOOL_ADDRESS_1: {
                    SisSchool school = studentInfo.getSchool(m_data.getSurveyPeriod().getDateCertain());
                    if (school.getAddress() != null) {
                        return school.getAddress().getAddressLine01();
                    }
                }
                    break;

                case CALC_PARAM_SCHOOL_ADDRESS_2: {
                    SisSchool school = studentInfo.getSchool(m_data.getSurveyPeriod().getDateCertain());
                    if (school.getAddress() != null) {
                        return school.getAddress().getAddressLine02();
                    }
                }
                    break;

                case CALC_PARAM_SCHOOL_PHONE: {
                    SisSchool school = studentInfo.getSchool(m_data.getSurveyPeriod().getDateCertain());
                    if (school.getAddress() != null) {
                        String phoneNumber = school.getAddress().getPhone01() == null ? school.getAddress().getPhone02()
                                : school.getAddress().getPhone01();
                        if (phoneNumber != null) {
                            return phoneNumber.replaceAll("[ \\-()]", "");
                        }
                    }
                }
                    break;

                case CALC_PARAM_RESID_TUITION_PURPOSES: {
                    List<StudentScheduleInfo> infos =
                            m_data.getScheduleHelperForYear(Integer.valueOf(m_data.getCurrentContext().getSchoolYear()))
                                    .getStudentScheduleInfo(student);
                    StudentScheduleInfo recentInfo = null;
                    for (StudentScheduleInfo info : infos) {
                        if (recentInfo == null || !recentInfo.getEntryDate().after(info.getEntryDate())) {
                            recentInfo = info;
                        }
                    }
                    if (recentInfo != null) {
                        return recentInfo.getResidencyTuitionPurposes();
                    }
                }
                    break;

                case CALC_PARAM_FEE_STATUS: {
                    List<StudentScheduleInfo> infos =
                            m_data.getScheduleHelperForYear(Integer.valueOf(m_data.getCurrentContext().getSchoolYear()))
                                    .getStudentScheduleInfo(student);
                    StudentScheduleInfo recentInfo = null;
                    for (StudentScheduleInfo info : infos) {
                        if (recentInfo == null || !recentInfo.getEntryDate().after(info.getEntryDate())) {
                            recentInfo = info;
                        }
                    }
                    if (recentInfo != null) {
                        return recentInfo.getAdultFeeStatusFirst();
                    }
                    return m_data.getFieldValueByAlias(student, ALIAS_FEE_STATUS);
                }

                case CALC_PARAM_WDRAW_CODE: {
                    StudentEnrollmentSpanInfo spanInfo = studentInfo.getStdEnrollmentSpanInfo();
                    if (spanInfo != null) {
                        StudentEnrollment wdrawEnrollment = spanInfo.getWithdrawalEnrollment();
                        if (wdrawEnrollment != null) {
                            return wdrawEnrollment.getEnrollmentCode();
                        }
                    }
                }
                    break;

                case CALC_PARAM_WDRAW_DATE: {
                    StudentEnrollmentSpanInfo spanInfo = studentInfo.getStdEnrollmentSpanInfo();
                    if (spanInfo != null) {
                        StudentEnrollment wdrawEnrollment = spanInfo.getWithdrawalEnrollment();
                        if (wdrawEnrollment != null) {
                            return wdrawEnrollment.getEnrollmentDate();
                        }
                    }
                }
                    break;

                case CALC_PARAM_DIPLOMA_DATE: {
                    return m_data.getFieldValueByAlias(student, ALIAS_DIPLOMA_DATE);
                }

                case CALC_PARAM_DIPLOMA_TYPE: {
                    return m_data.getFieldValueByAlias(student, ALIAS_DIPLOMA_TYPE);
                }

                case CALC_PARAM_COC_DATE: {
                    return m_data.getFieldValueByAlias(student, ALIAS_COMPLETION_CERTIFICATE_DATE);
                }

                case CALC_PARAM_COC_TYPE: {
                    return m_data.getFieldValueByAlias(student, ALIAS_COMPLETION_CERTIFICATE_TYPE);
                }

                case CALC_PARAM_RESIDENT_STATUS: {
                    StudentEnrollment enrollment = m_data.getStudentHelper().getEnrollmentForDate(student.getOid(),
                            m_data.getSurveyPeriod().getSnapshotDate(), StudentEnrollment.ENTRY);
                    String residentStatus = null;
                    if (enrollment != null) {
                        residentStatus = (String) m_data.getFieldValueByAlias(enrollment, ALIAS_RESIDENT_STATUS);
                    }
                    return StringUtils.isEmpty(residentStatus) ? "3" : residentStatus;
                }

                case CALC_PARAM_PHYS_EDU: {
                    String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
                    if (gradeLevel.matches(PATTERN_KG_TO_8)) {
                        return m_data.getFieldValueByAlias(student, ALIAS_PHYS_EDU_WAIVER);
                    }
                    break;
                }

                case CALC_PARAM_PROMO_STATUS: {
                    if (entity instanceof Record03) {
                        Record03 entity03 = (Record03) entity;
                        DistrictSchoolYearContext context = entity03.getContext();
                        return m_data.getStudentHistoryValue(student, ALIAS_PROMO_STATUS, context);
                    }
                }
                    break;

                case CALC_PARAM_FEFP_NUM:
                    return studentInfo.getFefpProgram();

                default:
                    break;
            }

            return null;
        }
    }


    /**
     * The Class RetrieveTestInfo.
     */
    public static class RetrieveTestInfo implements FieldRetriever {
        public static final String CALC_ID = "TEST";

        public static final String CALC_PARAM_TEST_DATE = "DATE";
        public static final String CALC_PARAM_TEST_FORM = "FORM";
        public static final String CALC_PARAM_TEST_GRADE_LEVEL = "GRADE_LEVEL";
        public static final String CALC_PARAM_TEST_LEVEL = "LEVEL";
        public static final String CALC_PARAM_TEST_NAME = "NAME";
        public static final String CALC_PARAM_TEST_PAS_NUMBER = "PAS_NUM";
        public static final String CALC_PARAM_TEST_SUBJECT_CONTENT = "SUBJ_CONTENT";
        public static final String CALC_PARAM_TEST_SCORE = "SCORE";
        public static final String CALC_PARAM_TEST_SCORE_TYPE = "SCORE_TYPE";

        private static final String ALIAS_TEST_FORM = "asm-test-form";
        private static final String ALIAS_TEST_LEVEL = "asm-test-level";
        private static final String ALIAS_TEST_NAME = "asm-test-name";
        private static final String ALIAS_TEST_SCORE = "asm-test-score";
        private static final String ALIAS_TEST_SCORE_TYPE = "asm-test-scoreType";
        private static final String ALIAS_TEST_SUBJECT_CONTENT = "asm-test-subjectContent";

        private static int POSITION_TEST_NUM = 0;
        private static int POSITION_TEST_SUBJECT_NUM = 1;
        private static int POSITION_TEST_SUBJECT_SCORE_NUM = 2;

        private static Map<String, String> s_calcParamToFieldNamePrefix = new HashMap<>();

        static {
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_DATE, FIELD_TEST_DATE);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_FORM, FIELD_TEST_FORM);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_GRADE_LEVEL, FIELD_TEST_GRADE_LEVEL);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_LEVEL, FIELD_TEST_LEVEL);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_NAME, FIELD_TEST_NAME);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_SUBJECT_CONTENT, FIELD_TEST_SC);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_SCORE, FIELD_TEST_S);
            s_calcParamToFieldNamePrefix.put(CALC_PARAM_TEST_SCORE_TYPE, FIELD_TEST_ST);
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            String calcParam = (String) field.getParameter();

            if (entity instanceof Record08) {
                String fieldNum = getFieldNum(calcParam, field.getFieldId());
                int testNum = Integer.valueOf(String.valueOf(fieldNum.charAt(POSITION_TEST_NUM))).intValue();
                List<StudentAssessment> studentAssessments = ((Record08) entity).getStudentAssessments();
                if (studentAssessments != null && testNum <= studentAssessments.size()) {
                    int subjectNum = -1;
                    int scoreNum = -1;
                    if (fieldNum.length() >= 2) {
                        subjectNum =
                                Integer.valueOf(String.valueOf(fieldNum.charAt(POSITION_TEST_SUBJECT_NUM))).intValue();
                    }
                    if (fieldNum.length() == 3) {
                        scoreNum =
                                Integer.valueOf(String.valueOf(fieldNum.charAt(POSITION_TEST_SUBJECT_SCORE_NUM)))
                                        .intValue();
                    }
                    StudentAssessment studentAssessment = studentAssessments.get(testNum - 1);
                    switch (calcParam) {
                        case CALC_PARAM_TEST_GRADE_LEVEL:
                            return studentAssessment.getGradeLevelCode();
                        case CALC_PARAM_TEST_DATE:
                            return studentAssessment.getDate();
                        case CALC_PARAM_TEST_NAME:
                            return getTestValue(fasterData, studentAssessment, ALIAS_TEST_NAME);
                        case CALC_PARAM_TEST_FORM:
                            return getTestValue(fasterData, studentAssessment, ALIAS_TEST_FORM);
                        case CALC_PARAM_TEST_LEVEL:
                            return getTestValue(fasterData, studentAssessment, ALIAS_TEST_LEVEL);
                        case CALC_PARAM_TEST_SUBJECT_CONTENT:
                            return getSubjectContent(fasterData, studentAssessment, ALIAS_TEST_SUBJECT_CONTENT,
                                    subjectNum);
                        case CALC_PARAM_TEST_SCORE_TYPE:
                            return getScore(fasterData, studentAssessment, ALIAS_TEST_SCORE_TYPE, subjectNum,
                                    scoreNum == 1 ? 'A' : 'B');
                        case CALC_PARAM_TEST_SCORE:
                            return getScore(fasterData, studentAssessment, ALIAS_TEST_SCORE, subjectNum,
                                    scoreNum == 1 ? 'A' : 'B');
                        default:
                            return null;
                    }
                }
            }

            if (entity instanceof Record10) {
                Record10 record = (Record10) entity;

                switch (calcParam) {
                    case CALC_PARAM_TEST_GRADE_LEVEL: {
                        int groupNumber = Integer.parseInt(field.getFieldId().replace(FIELD_TEST_GRADE_LEVEL, ""));
                        return record.getPasNumberObject(groupNumber).gradeLevel;
                    }
                    case CALC_PARAM_TEST_DATE: {
                        int groupNumber = Integer.parseInt(field.getFieldId().replace(FIELD_TEST_DATE, ""));
                        return record.getPasNumberObject(groupNumber).testDate;
                    }
                    case CALC_PARAM_TEST_NAME: {
                        int groupNumber = Integer.parseInt(field.getFieldId().replace(FIELD_TEST_NAME, ""));
                        return record.getPasNumberObject(groupNumber).testName;
                    }
                    case CALC_PARAM_TEST_SUBJECT_CONTENT: {
                        int groupNumber = Integer.parseInt(field.getFieldId().replace(FIELD_TEST_SC, ""));
                        return record.getPasNumberObject(groupNumber).subjectContent;
                    }
                    case CALC_PARAM_TEST_PAS_NUMBER: {
                        int groupNumber = Integer.parseInt(field.getFieldId().replace(FIELD_TEST_PAS_NUM, ""));
                        return record.getPasNumberObject(groupNumber).pasNumber;
                    }
                    default:
                        return null;
                }
            }

            return null;
        }


        /**
         * Gets the test value.
         *
         * @param fasterData FLFasterRecordsData
         * @param studentAssessment StudentAssessment
         * @param alias String
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getTestValue(FLFasterRecordsData fasterData, StudentAssessment studentAssessment, String alias)
                throws X2BaseException {
            return fasterData.getFieldValueByAlias(studentAssessment, alias,
                    fasterData.getAsmDataDictionary(studentAssessment));
        }


        /**
         * Gets the subject content.
         *
         * @param fasterData FLFasterRecordsData
         * @param studentAssessment StudentAssessment
         * @param alias String
         * @param subjectNum int
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getSubjectContent(FLFasterRecordsData fasterData,
                                         StudentAssessment studentAssessment,
                                         String alias,
                                         int subjectNum)
                throws X2BaseException {
            return getTestValue(fasterData, studentAssessment, alias + subjectNum);
        }


        /**
         * Gets the score.
         *
         * @param fasterData FLFasterRecordsData
         * @param studentAssessment StudentAssessment
         * @param alias String
         * @param subjectNum int
         * @param index char
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getScore(FLFasterRecordsData fasterData,
                                StudentAssessment studentAssessment,
                                String alias,
                                int subjectNum,
                                char index)
                throws X2BaseException {
            return getTestValue(fasterData, studentAssessment, alias + index + subjectNum);
        }


        /**
         * Gets the field num.
         *
         * @param calcParam String
         * @param fieldName String
         * @return String
         */
        private String getFieldNum(String calcParam, String fieldName) {
            String fieldNamePrefix = s_calcParamToFieldNamePrefix.get(calcParam);
            return fieldName.replace(fieldNamePrefix, "");
        }

    }


    /**
     * The Class RetrieveTestProduction.
     */
    public static class RetrieveTestProduction implements FieldRetriever {
        public static final String CALC_ID = "TEST_PROD_SWITCH";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
            return fasterData.isProduction();
        }

    }

    /**
     * The Class RetrieveTranscript.
     */
    public static class RetrieveTranscript implements FieldRetriever {
        public static final String CALC_ID = "TRANSCRIPT";

        public static final String ALIAS_CRS_STATE_ID = "all-crs-StateId";

        public static final String ALIAS_TRN_FROM_FASTER_INDICATOR = "all-trn-ImportedFromFasterInd";

        private static final String ALIAS_CRS_CTE_PGM_CODE = "all-crs-CTEProgramCode";
        private static final String ALIAS_CRS_FLAG = "all-crs-Flag";
        private static final String ALIAS_CRS_ONLINE = "all-crs-OnlineCourse";
        private static final String ALIAS_TRN_COURSE_ABBREVIATED_TITLE = "all-trn-CourseAbbrevTitle";
        private static final String ALIAS_TRN_COURSE_FLAG = "all-trn-CourseFlag";
        private static final String ALIAS_TRN_COURSE_NUMBER = "all-trn-CourseNumber";
        private static final String ALIAS_TRN_COURSE_ONLINE = "all-trn-CourseOnline";
        private static final String ALIAS_TRN_CRED_EARNED_CODE_TYPE = "all-trn-CredEarnedCodeType";
        private static final String ALIAS_TRN_CRED_EARNED_DST_NUM = "all-trn-CredEarnedDstNum";
        private static final String ALIAS_TRN_CRED_EARNED_SKL_NUM = "all-trn-CredEarnedSklNumCode";
        private static final String ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NAME = "all-trn-CredEarnedTakenSklName";
        private static final String ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NUM = "all-trn-CredEarnedTakenSklNum";
        private static final String ALIAS_TRN_TERM = "all-trn-Term";
        private static final String ALIAS_TRN_SUBJ_AREA_REQS = "all-trn-SubjAreaReqs";

        private static final BigDecimal BIG_DECIMAL_100 = new BigDecimal(100);

        private static final String CALC_PARAM_ABSENCES = "ABSENCES";
        private static final String CALC_PARAM_AGE_PGM_CODE = "AGE_PGM_CODE";
        private static final String CALC_PARAM_CREDIT_ATTEMPT = "CREDIT_ATTEMPT";
        private static final String CALC_PARAM_CREDIT_EARNED = "CREDIT_EARNED";
        private static final String CALC_PARAM_CRS_FLAG = "COURSE_FLAG";
        private static final String CALC_PARAM_CRS_NUM = "CRS_NUMBER";
        private static final String CALC_PARAM_CRS_ONLINE = "ONLINE_IND";
        private static final String CALC_PARAM_DST_NUM = "CRED_EARNED_DST_NUM";
        private static final String CALC_PARAM_EARNED_CODE_TYPE = "EARNED_CODE_TYPE";
        private static final String CALC_PARAM_EARNED_SKL_NUM = "EARNED_SKL_NUM_CODE";
        private static final String CALC_PARAM_FINAL_GRADE = "FINAL_GRADE";
        private static final String CALC_PARAM_GRADE = "GRADE";
        private static final String CALC_PARAM_GRADE_LEVEL = "GRADE_LEVEL";
        private static final String CALC_PARAM_IN_PROGRESS_HRS = "IN_PROGRESS_HRS";
        private static final String CALC_PARAM_SKL_NAME = "SKL_NAME_EARNED_TAKEN";
        private static final String CALC_PARAM_SKL_NUM = "SKL_NUM_EARNED_TAKEN";
        private static final String CALC_PARAM_SUBJ_REQS = "STATE_SUBJ_REQS";
        private static final String CALC_PARAM_SUBST_CRS_NUM = "SUBST_CRS_NUM";
        private static final String CALC_PARAM_SUBST_STATE_SUBJ_REQ = "SUBST_ST_SUBJ_REQS";
        private static final String CALC_PARAM_TERM = "TERM";
        private static final String CALC_PARAM_TERM_END_DATE = "TERM_END_DATE";
        private static final String CALC_PARAM_TERM_NAME = "TERM_NAME";
        private static final String CALC_PARAM_TERM_START_DATE = "TERM_START_DATE";
        private static final String CALC_PARAM_TITLE_ABBREV = "TITLE_ABBREV";
        private static final String CALC_PARAM_WEEKS_GRADE_CYCLE = "WEEKS_GRADE_CYCLE";

        private static final String CODE_TYPE_SKL_NUM_STATE_ASSIGNED = "75";

        private static final int NUM_OF_WEEKS_IN_SCHOOL_YEAR = 36;

        private GradesManager m_gradesManager;


        /**
         * Instantiates a new retrieve transcript.
         *
         * @param data FLFasterRecordsData
         */
        public RetrieveTranscript(FLFasterRecordsData data) {
            m_gradesManager = new GradesManager(data.getBroker());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            if (entity instanceof Record04) {
                Record04 recordEntity = (Record04) entity;
                FLFasterRecordsData fasterData = (FLFasterRecordsData) data;
                String parameter = (String) field.getParameter();
                StudentScheduleInfo info = recordEntity.getCurrentInfo();
                SisSchool school = info.getSchool();
                Course course = info.getCourse();

                Boolean importedFromFasterIndicator =
                        (Boolean) fasterData.getFieldValueByAlias(info.getTranscript(),
                                ALIAS_TRN_FROM_FASTER_INDICATOR);
                if (importedFromFasterIndicator != null && importedFromFasterIndicator.booleanValue()) {
                    switch (parameter) {
                        case CALC_PARAM_DST_NUM:
                            return fasterData.getFieldValueByAlias(info.getTranscript(), ALIAS_TRN_CRED_EARNED_DST_NUM);
                        case CALC_PARAM_EARNED_CODE_TYPE:
                            return fasterData.getFieldValueByAlias(info.getTranscript(),
                                    ALIAS_TRN_CRED_EARNED_CODE_TYPE);
                        case CALC_PARAM_EARNED_SKL_NUM:
                            return fasterData.getFieldValueByAlias(info.getTranscript(), ALIAS_TRN_CRED_EARNED_SKL_NUM);
                        case CALC_PARAM_SKL_NAME:
                            return fasterData.getFieldValueByAlias(info.getTranscript(),
                                    ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NAME);
                        case CALC_PARAM_SKL_NUM:
                            return fasterData.getFieldValueByAlias(info.getTranscript(),
                                    ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NUM);
                        case CALC_PARAM_TERM:
                            return fasterData.getFieldValueByAlias(info.getTranscript(), ALIAS_TRN_TERM);
                        case CALC_PARAM_CRS_NUM:
                            return fasterData.getFieldValueByAlias(info.getTranscript(), ALIAS_TRN_COURSE_NUMBER);
                        case CALC_PARAM_TITLE_ABBREV:
                            return fasterData.getFieldValueByAlias(info.getTranscript(),
                                    ALIAS_TRN_COURSE_ABBREVIATED_TITLE);
                        case CALC_PARAM_SUBJ_REQS:
                            return fasterData.getFieldValueByAlias(info.getTranscript(),
                                    ALIAS_TRN_SUBJ_AREA_REQS);
                        case CALC_PARAM_CRS_FLAG:
                            return fasterData.getFieldValueByAlias(info.getTranscript(), ALIAS_TRN_COURSE_FLAG);
                        case CALC_PARAM_CRS_ONLINE:
                            return fasterData.getFieldValueByAlias(info.getTranscript(), ALIAS_TRN_COURSE_ONLINE);
                        default:
                            break;
                    }
                } else {
                    switch (parameter) {
                        case CALC_PARAM_DST_NUM:
                            return fasterData.getFieldValueByAlias(info.getSchool().getOrganization1(),
                                    ALIAS_ORG_DST_NUM);
                        case CALC_PARAM_EARNED_CODE_TYPE:
                            return CODE_TYPE_SKL_NUM_STATE_ASSIGNED;
                        case CALC_PARAM_EARNED_SKL_NUM:
                            return fasterData.getFieldValueByAlias(info.getSchool(),
                                    ALIAS_SKL_STATE_ID);
                        case CALC_PARAM_SKL_NAME:
                            return info.getSchool().getName();
                        case CALC_PARAM_SKL_NUM:
                            return fasterData.getFieldValueByAlias(info.getSchool(),
                                    ALIAS_SKL_STATE_ID);
                        case CALC_PARAM_CRS_NUM:
                            return fasterData.getFieldValueByAlias(info.getCourse(),
                                    ALIAS_CRS_STATE_ID);
                        case CALC_PARAM_TITLE_ABBREV: {
                            String courseStateId =
                                    (String) fasterData.getFieldValueByAlias(info.getCourse(),
                                            ALIAS_CRS_STATE_ID);
                            return fasterData.findDependencyByCode(REF_TABLE_COURSE_NUM_TO_ABBREV_TITLE, courseStateId);
                        }
                        case CALC_PARAM_SUBJ_REQS: {
                            String courseStateId =
                                    (String) fasterData.getFieldValueByAlias(info.getCourse(),
                                            ALIAS_CRS_STATE_ID);
                            return fasterData.findDependencyByCode(REF_TABLE_COURSE_NUM_TO_SUB_AREA_REQ, courseStateId);
                        }
                        case CALC_PARAM_CRS_FLAG:
                            String courseFlag = (String) fasterData.getFieldValueByAlias(info.getCourse(),
                                    ALIAS_CRS_FLAG);
                            if (!StringUtils.isEmpty(courseFlag)) {
                                return courseFlag.replaceAll(",", "");
                            }
                            break;
                        case CALC_PARAM_CRS_ONLINE:
                            return fasterData.getFieldValueByAlias(info.getCourse(),
                                    ALIAS_CRS_ONLINE);
                        case CALC_PARAM_GRADE:
                            return null;
                        case CALC_PARAM_CREDIT_ATTEMPT: {
                            BigDecimal creditAsNumber = null;
                            if (!StringUtils.isEmpty(info.getTranscript().getPotentialCredit())) {
                                try {
                                    creditAsNumber =
                                            new BigDecimal(info.getTranscript().getPotentialCredit())
                                                    .multiply(BIG_DECIMAL_100);
                                } catch (NumberFormatException nfe) {
                                    // nothing. The credit is not numeric.
                                }
                            }
                            if (creditAsNumber == null) {
                                SchoolCourse schoolCourse = info.getTranscript().getSchoolCourse();
                                if (schoolCourse != null && schoolCourse.getCredit() != null) {
                                    creditAsNumber = schoolCourse.getCredit().multiply(BIG_DECIMAL_100);
                                } else if (course != null && course.getCredit() != null) {
                                    creditAsNumber = course.getCredit().multiply(BIG_DECIMAL_100);
                                }
                            }
                            return creditAsNumber;
                        }
                        case CALC_PARAM_CREDIT_EARNED:
                            return info.getTranscript().getTotalCredit() == null ? BigDecimal.ZERO
                                    : info.getTranscript().getTotalCredit().multiply(BIG_DECIMAL_100);

                        case CALC_PARAM_FINAL_GRADE: {
                            String finalGrade = info.getTranscript().getFinalGrade();

                            if (!StringUtils.isEmpty(finalGrade)) {
                                GradeScale scale =
                                        fasterData.m_gradeScales.get(info.getTranscript().getTranscriptDefinitionOid());
                                if (StringUtils.isNumeric(finalGrade) && scale != null) {
                                    // Try the final grade as a number.
                                    BigDecimal gradeAsNumber = null;
                                    try {
                                        gradeAsNumber = new BigDecimal(finalGrade);
                                    } catch (NumberFormatException nfe) {
                                        // nothing. The grade is not numeric.
                                    }

                                    if (gradeAsNumber != null) {
                                        finalGrade =
                                                m_gradesManager.getLetterValue(gradeAsNumber, scale, school,
                                                        info.getTranscript().getSchoolCourseOid());
                                    }
                                }
                            }
                            return finalGrade;
                        }

                        case CALC_PARAM_WEEKS_GRADE_CYCLE: {
                            int gradeTermPerYear =
                                    info.getTranscript().getTranscriptDefinition().getGradeTermDefinition()
                                            .getGradeTermsPerYear();
                            return Integer.valueOf(NUM_OF_WEEKS_IN_SCHOOL_YEAR / gradeTermPerYear);
                        }

                        case CALC_PARAM_IN_PROGRESS_HRS: {
                            Integer enrolledHrs = info.getMasterScheduleInfo()
                                    .getSectionHoursInDateRange(info.getEntryDate(), info.getExitDate());
                            Set<PlainDate> absences =
                                    info.getStudentInfo().getAbsentDates(info.getTranscript().getDistrictContext());
                            Integer absenceHrs = Integer.valueOf(0);
                            for (PlainDate absence : absences) {
                                absenceHrs = Integer.valueOf(absenceHrs.intValue() +
                                        info.getMasterScheduleInfo().getSectionHoursInDateRange(absence, absence)
                                                .intValue());
                            }
                            return Integer.valueOf(enrolledHrs.intValue() - absenceHrs.intValue());
                        }

                        case CALC_PARAM_ABSENCES: {
                            int numOfNotAttendedPeriods = 0;
                            Set<PlainDate> absenceDates =
                                    info.getStudentInfo().getAbsentDates(info.getTranscript().getDistrictContext());

                            for (PlainDate absenceDate : absenceDates) {
                                numOfNotAttendedPeriods +=
                                        info.getMasterScheduleInfo().getSectionNumOfPeriodsOnDate(absenceDate);
                            }
                            return Integer.valueOf(numOfNotAttendedPeriods);
                        }

                        case CALC_PARAM_TERM_NAME:
                            return info.getMasterScheduleInfo().getSection().getScheduleTerm().getName();

                        case CALC_PARAM_TERM_START_DATE: {
                            Collection<ScheduleTermDate> datesCollection =
                                    info.getMasterScheduleInfo().getSection().getScheduleTerm().getScheduleTermDates();
                            if (datesCollection != null) {
                                ScheduleTermDate dates = datesCollection.iterator().next();
                                if (dates != null) {
                                    return dates.getStartDate();
                                }
                            }
                        }
                            break;

                        case CALC_PARAM_TERM_END_DATE: {
                            Collection<ScheduleTermDate> datesCollection =
                                    info.getMasterScheduleInfo().getSection().getScheduleTerm().getScheduleTermDates();
                            if (datesCollection != null) {
                                ScheduleTermDate dates = datesCollection.iterator().next();
                                if (dates != null) {
                                    return dates.getEndDate();
                                }
                            }
                        }
                            break;

                        case CALC_PARAM_SUBST_CRS_NUM: {
                            String flag = (String) fasterData.getFieldValueByAlias(info.getCourse(), ALIAS_CRS_FLAG);
                            if (!StringUtils.isEmpty(flag) && flag.contains("*")) {
                                String courseId = (String) fasterData.getFieldValueByAlias(info.getCourse(),
                                        ALIAS_CRS_STATE_ID);
                                if ("1802300".equals(courseId) || "1802310".equals(courseId)) {
                                    return "0800300";
                                }
                            }
                        }
                            break;

                        case CALC_PARAM_SUBST_STATE_SUBJ_REQ: {
                            String flag = (String) fasterData.getFieldValueByAlias(info.getCourse(), ALIAS_CRS_FLAG);
                            if (!StringUtils.isEmpty(flag) && flag.contains("*")) {
                                String courseId = (String) fasterData.getFieldValueByAlias(info.getCourse(),
                                        ALIAS_CRS_STATE_ID);
                                if ("1802300".equals(courseId) || "1802310".equals(courseId)) {
                                    return fasterData.findDependencyByCode(REF_TABLE_COURSE_NUM_TO_SUB_AREA_REQ,
                                            "0800300");
                                }
                            }
                        }
                            break;

                        case CALC_PARAM_AGE_PGM_CODE: {
                            String ageIndicator =
                                    (String) fasterData.getFieldValueByAlias(info.getMasterScheduleInfo().getSection(),
                                            ALIAS_WDIS_AGE_INDICTOR);
                            if (!StringUtils.isEmpty(ageIndicator)) {
                                return fasterData.getFieldValueByAlias(info.getCourse(), ALIAS_CRS_CTE_PGM_CODE);
                            }
                        }
                            break;

                        default:
                            break;
                    }
                }
                switch (parameter) {
                    case CALC_PARAM_GRADE_LEVEL:
                        return info.getTranscript().getGradeLevel();
                    case CALC_PARAM_TERM:
                        return info.getMasterScheduleInfo().getSection().getScheduleTerm().getCode();

                    default:
                        break;
                }
            }
            return null;
        }
    }


    /**
     * The Class RetrieveYear.
     */
    public static class RetrieveYear implements FieldRetriever {
        public static final String CALC_ID = "YEAR";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            if (entity instanceof Record03) {
                Record03 fasterEntity = (Record03) entity;
                Integer currentYear = fasterEntity.getCurrentYear();
                return (currentYear.intValue() - 1) + currentYear.toString();
            } else if (entity instanceof Record04) {
                Record04 fasterEntity = (Record04) entity;
                StudentScheduleInfo info = fasterEntity.getCurrentInfo();
                if (info.getTranscript() != null) {
                    return String.valueOf(info.getTranscript().getSchoolYear() - 1)
                            + String.valueOf(info.getTranscript().getSchoolYear());
                } else if (info.getSection() != null) {
                    int schoolYear = info.getSection().getSchedule().getDistrictContext().getSchoolYear();
                    return String.valueOf(schoolYear - 1) + String.valueOf(schoolYear);
                }
            }
            return null;
        }
    }


    /**
     * The Class FasterSurveyPeriodMockup.
     */
    public class FasterSurveyPeriodMockup implements SurveyPeriod {

        private PlainDate m_reportDate = null;


        /**
         * Instantiates a new faster survey period mockup.
         */
        public FasterSurveyPeriodMockup() {
            m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
            if (m_reportDate == null) {
                m_reportDate = new PlainDate(new Date());
            }
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod#getCode()
         */
        @Override
        public String getCode() {
            return "";
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod#getDateCertain()
         */
        @Override
        public PlainDate getDateCertain() {
            return m_reportDate;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod#getEndDate()
         */
        @Override
        public PlainDate getEndDate() {
            return getCurrentContext().getEndDate();
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod#getFLTermCode()
         */
        @Override
        public String getFLTermCode() {
            return "";
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod#getSnapshotDate()
         */
        @Override
        public PlainDate getSnapshotDate() {
            return m_reportDate;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod#getStartDate()
         */
        @Override
        public PlainDate getStartDate() {
            return getCurrentContext().getStartDate();
        }

    }

    public static final String ALIAS_ORG_DST_NUM = "all-org-DistrictNumber";
    public static final String ALIAS_SKL_STATE_ID = "all-skl-StateId";

    public static final String DATE_FORMAT_DB = "DB";

    public static final String RECORDS_TYPE_INTERDISTRICT = "I";
    public static final String RECORDS_TYPE_SECONDARY = "S";

    public static final String REF_TABLE_COURSE_NUM_TO_ABBREV_TITLE = "FL Course To Abbrev Title";
    public static final String REF_TABLE_COURSE_NUM_TO_SUB_AREA_REQ = "FL Course Num To Sub Area Req";
    public static final String REF_TABLE_IMMUNIZATION_TYPES_TO_FASTER = "FL ICD CPT Immunization Types";

    public static final String TRANSFER_TYPE_RESPONSE = "Response";
    public static final String TRANSFER_TYPE_REQUEST = "Request";

    protected static final String INPUT_PARAM_CURRENT_SELECTION = "##current";

    private static final String ASSESSMENT_DEFINITION_ID_TEST = "TEST";

    private static final SimpleDateFormat DATE_FORMAT_MMDDCCYY = new SimpleDateFormat("MMddyyyy");

    private static final int INITIAL_MAP_SIZE = 1000;
    private static final String INPUT_PARAM_OMIT_HEADER_RECORD = "omitHeaderRecord";
    private static final String INPUT_PARAM_PRODUCTION_INDICATOR = "isProduction";
    private static final String INPUT_PARAM_MESSAGE_TYPE = "messageType";
    private static final String INPUT_PARAM_RECORDS_TYPE = "recordsType";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_RESTRICT_BY_RECORD_TYPE = "restrictByRecordType";
    private static final String INPUT_PARAM_TRANSFER_TYPE = "transferType";

    private static final Collection<String> s_summerTermCodes = Arrays.asList("4", "5", "S", "T", "U", "V", "W", "X");

    private Map<String, DataDictionary> m_asdDictionaries = new HashMap<>();
    private Map<Integer, DistrictSchoolYearContext> m_contextsByYears;
    private Map<String, ExtendedDataDictionary> m_ddxById = new HashMap<>();
    private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
    private DataDictionary m_dictionary = null;
    private Map<String, DataDictionaryField> m_fieldsByAlias = null;
    private Map<String, GradeScale> m_gradeScales;
    private Boolean m_isProduction;
    private String m_messageType = null;
    private Boolean m_omitHeaderRecord = null;
    private Map<String, ExportFormatDefinition> m_procIdEfds = new HashMap<>();
    private Map<String, StudentProgramDataset> m_programDatasets = new HashMap<>();
    private Map<String, Collection<Race>> m_raceMap;
    private String m_recordsType = null;
    private Map<String, Map<String, String>> m_refCodesDependencyCode = null;
    private String m_restrictByRecordType = null;
    private Map<String, Map<String, List<StudentAssessment>>> m_studentAssessments = new HashMap<>();
    private StudentConductDataset m_conductDataset = null;
    private Map<String, List<StudentContact>> m_studentContacts = null;
    private Map<String, List<StudentContextAttributes>> m_studentContextAttributes = null;
    private Map<String, List<HealthCondition>> m_studentHealthConditions = null;
    private Map<String, List<HealthImmunizationSeries>> m_studentImmunizationSeries = null;
    private Map<String, List<HealthScreening>> m_studentHealthScreenings = null;
    private Map<String, List<Transcript>> m_studentTranscript = null;
    private SurveyPeriod m_surveyPeriod = null;
    private String m_transferType = null;
    private Map<Integer, StudentScheduleHelper> m_yearScheduleHelper = new HashMap<>();
    private Map<Integer, FLStudentHelper> m_yearStudentHelper = new HashMap<>();


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#applyInputCriteria(org.apache.ojb.broker.query.Criteria,
     *      boolean, java.lang.String)
     */
    @Override
    public void applyInputCriteria(Criteria criteria, boolean applySchool, String prefixPath) {
        super.applyInputCriteria(criteria, applySchool, prefixPath);

        String fullPrefixPath = EMPTY_STRING;
        if (!StringUtils.isEmpty(prefixPath)) {
            fullPrefixPath = prefixPath + ModelProperty.PATH_DELIMITER;
        }

        SubQuery studentSubQuery =
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());

        criteria.addIn(fullPrefixPath + X2BaseBean.COL_OID, studentSubQuery);
    }


    /**
     * Find code by dependency.
     *
     * @param refTableName String
     * @param dependency String
     * @return String
     */
    public String findCodeByDependency(String refTableName, String dependency) {
        Map<String, String> dependencies = getRefCodeDepCodesMap(refTableName);
        List<String> sortedKeys = new ArrayList<String>(dependencies.keySet());
        Collections.sort(sortedKeys);
        for (String key : sortedKeys) {
            String value = dependencies.get(key);
            if (dependency.equals(value)) {
                return key;
            }
        }
        return null;
    }


    /**
     * Find dependency by code.
     *
     * @param refTableName String
     * @param code String
     * @return String
     */
    public String findDependencyByCode(String refTableName, String code) {
        return getRefCodeDepCodesMap(refTableName).get(code);
    }


    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    public DataDictionary getDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }


    /**
     * Gets the dictionary by id.
     *
     * @param id String
     * @return Data dictionary
     */
    public DataDictionary getDictionaryById(String id) {
        if (id == null) {
            return getDictionary();
        }
        DataDictionary dictionary = m_dictionariesById.get(id);
        if (dictionary == null) {
            ExtendedDataDictionary ddx = getExtendedDataDictionaryById(id);
            dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
            m_dictionariesById.put(id, dictionary);
        }
        return dictionary;
    }


    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return Data dictionary field
     */
    public DataDictionaryField getFieldByAlias(String alias, DataDictionary dataDictionary) {
        if (m_fieldsByAlias == null) {
            m_fieldsByAlias = new HashMap<>();
        }
        if (m_fieldsByAlias.get(alias) == null) {
            DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                m_fieldsByAlias.put(alias, field);
            }
        }
        return m_fieldsByAlias.get(alias);
    }


    /**
     * Gets the field value by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getFieldValueByAlias(X2BaseBean bean, String alias) throws X2BaseException {
        DataDictionaryField field = getFieldByAlias(alias, getDataDictionary());
        return getFieldValue(bean, field);
    }


    /**
     * Gets the field value by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getFieldValueByAlias(X2BaseBean bean, String alias, DataDictionary dataDictionary)
            throws X2BaseException {
        DataDictionaryField field = getFieldByAlias(alias, dataDictionary);
        return getFieldValue(bean, field);
    }


    /**
     * Gets the message type.
     *
     * @return String
     */
    public String getMessageType() {
        if (m_messageType == null) {
            String rcdCodeOid = (String) getParameter(INPUT_PARAM_MESSAGE_TYPE);
            ReferenceCode code = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdCodeOid);
            m_messageType = code.getCode();
        }
        return m_messageType;
    }


    /**
     * Gets the program dataset.
     *
     * @param pgmId String
     * @return Student program dataset
     * @throws X2BaseException exception
     */
    public StudentProgramDataset getProgramDataset(String pgmId) throws X2BaseException {
        if (m_programDatasets.get(pgmId) == null) {
            DistrictSchoolYearContext veryFirstContext = null;
            for (DistrictSchoolYearContext context : m_contextsByYears.values()) {
                if (veryFirstContext == null || veryFirstContext.getStartDate().after(context.getStartDate())) {
                    veryFirstContext = context;
                }
            }

            StudentProgramDataset dataset = getStudentHelper().getStudentProgramDataset(pgmId,
                    veryFirstContext.getStartDate(), getCurrentContext().getEndDate());
            m_programDatasets.put(pgmId, dataset);
        }
        return m_programDatasets.get(pgmId);
    }


    /**
     * Gets the schedule helper for year.
     *
     * @param year Integer
     * @return Student schedule helper
     * @throws X2BaseException exception
     */
    public StudentScheduleHelper getScheduleHelperForYear(Integer year) throws X2BaseException {
        StudentScheduleHelper studentScheduleHelper = m_yearScheduleHelper.get(year);
        if (studentScheduleHelper == null) {
            DistrictSchoolYearContext defaultContext = getCurrentContext();
            setCurrentContext(m_contextsByYears.get(year));
            FLScheduleHelper scheduleHelper =
                    new FLStudentScheduleHelper(this, getCurrentContext().getStartDate(),
                            getSurveyPeriod().getEndDate());
            studentScheduleHelper = getStudentHelperForYear(year).new StudentScheduleHelper(scheduleHelper,
                    getCurrentContext().getStartDate(), getSurveyPeriod().getEndDate());
            m_yearScheduleHelper.put(year, studentScheduleHelper);
            setCurrentContext(defaultContext);
        }
        return studentScheduleHelper;
    }


    /**
     * Gets the student actions.
     *
     * @param studentOid String
     * @return List
     * @throws X2BaseException exception
     */
    public List<ConductAction> getStudentActions(String studentOid) throws X2BaseException {
        if (m_conductDataset == null) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(m_surveyPeriod.getDateCertain());
            calendar.add(Calendar.YEAR, -3);
            m_conductDataset = getStudentHelper().getStudentConductDataset(new PlainDate(calendar.getTime()),
                    m_surveyPeriod.getDateCertain());
        }
        return m_conductDataset.getConductActions(studentOid);
    }


    /**
     * Gets the student assessments.
     *
     * @param assessmentDefinitionId String
     * @param studentOid String
     * @return List
     */
    public List<StudentAssessment> getStudentAssessments(String assessmentDefinitionId, String studentOid) {
        if (m_studentAssessments.get(assessmentDefinitionId) == null) {
            initializeStudentAssessments(assessmentDefinitionId);
        }
        if (m_studentAssessments.get(assessmentDefinitionId).get(studentOid) == null) {
            m_studentAssessments.get(assessmentDefinitionId).put(studentOid, new ArrayList<StudentAssessment>());
        }
        return m_studentAssessments.get(assessmentDefinitionId).get(studentOid);
    }


    /**
     * Gets the student contacts.
     *
     * @param studentOid String
     * @return Collection
     */
    public Collection<StudentContact> getStudentContacts(String studentOid) {
        if (m_studentContacts == null) {
            initializeStudentContacts();
        }
        if (m_studentContacts.get(studentOid) == null) {
            m_studentContacts.put(studentOid, new ArrayList<StudentContact>());
        }
        return m_studentContacts.get(studentOid);
    }


    /**
     * Gets the student context attributes.
     *
     * @param student SisStudent
     * @param context DistrictSchoolYearContext
     * @return Student context attributes
     */
    public StudentContextAttributes getStudentContextAttributes(SisStudent student, DistrictSchoolYearContext context) {
        if (m_studentContextAttributes == null) {
            m_studentContextAttributes = initializeStudentRelatedBeans(StudentContextAttributes.class,
                    StudentContextAttributes.COL_STUDENT_OID);
        }
        List<StudentContextAttributes> studentContextAttributes = m_studentContextAttributes.get(student.getOid());
        if (studentContextAttributes != null) {
            for (StudentContextAttributes attributes : studentContextAttributes) {
                if (attributes.getContextOid().equals(context.getOid())) {
                    return attributes;
                }
            }
        }
        return null;
    }


    /**
     * Gets the student health condition.
     *
     * @param studentOid String
     * @return List
     */
    public List<HealthCondition> getStudentHealthCondition(String studentOid) {
        if (m_studentHealthConditions == null) {
            m_studentHealthConditions =
                    initializeStudentRelatedBeans(HealthCondition.class, HealthCondition.COL_STUDENT_OID);
        }
        if (m_studentHealthConditions.get(studentOid) == null) {
            m_studentHealthConditions.put(studentOid, new ArrayList<HealthCondition>());
        }
        return m_studentHealthConditions.get(studentOid);
    }


    /**
     * Gets the student health screenings.
     *
     * @param studentOid String
     * @return Collection
     */
    public Collection<HealthScreening> getStudentHealthScreenings(String studentOid) {
        if (m_studentHealthScreenings == null) {
            initializeStudentHealthScreenings();
        }
        if (m_studentHealthScreenings.get(studentOid) == null) {
            m_studentHealthScreenings.put(studentOid, new ArrayList<HealthScreening>());
        }
        return m_studentHealthScreenings.get(studentOid);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getStudentHelper()
     */
    @Override
    public FLStudentHelper getStudentHelper() throws X2BaseException {
        return getStudentHelperForYear(Integer.valueOf(getCurrentContext().getSchoolYear()));
    }


    /**
     * Gets the student helper for year.
     *
     * @param year Integer
     * @return FL student helper
     * @throws X2BaseException exception
     */
    public FLStudentHelper getStudentHelperForYear(Integer year) throws X2BaseException {
        FLStudentHelper studentHelper = m_yearStudentHelper.get(year);
        if (studentHelper == null) {
            DistrictSchoolYearContext defaultContext = getCurrentContext();
            setCurrentContext(m_contextsByYears.get(year));
            studentHelper = new FLStudentHelper(this);
            studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION,
                    Boolean.TRUE);
            studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getQuery().getCriteria());
            studentHelper.getStudentCriteria().addIn(X2BaseBean.COL_OID, studentSubQuery);
            m_yearStudentHelper.put(year, studentHelper);
            setCurrentContext(defaultContext);
        }
        return studentHelper;
    }


    /**
     * Gets the student history value.
     *
     * @param student SisStudent
     * @param alias String
     * @param context DistrictSchoolYearContext
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getStudentHistoryValue(SisStudent student, String alias, DistrictSchoolYearContext context)
            throws X2BaseException {

        if (context.getOid().equals(getCurrentContext().getOid())) {
            return getFieldValueByAlias(student, alias);
        }

        String id = DictionaryHelper.translateAlias(alias, getDictionary(), true);
        ModelProperty property = new ModelProperty(SisStudent.class, id, getDictionary());

        StudentContextAttributes attribute = getStudentContextAttributes(student, context);
        if (attribute != null) {
            Map<String, String> blobContent =
                    ContextAttributesManager.parseBlobContents(attribute.getBlobInformation());
            if (blobContent.containsKey(property.getDictionaryPath())) {
                String valueAsString = blobContent.get(property.getDictionaryPath());
                DictionaryHelper helper = new DictionaryHelper(getBroker(),
                        LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
                return helper.getValueAsObject(property.getField(), valueAsString);
            }
        }
        return null;
    }


    /**
     * Gets the student immunization series.
     *
     * @param studentOid String
     * @return List
     */
    public List<HealthImmunizationSeries> getStudentImmunizationSeries(String studentOid) {
        if (m_studentImmunizationSeries == null) {
            m_studentImmunizationSeries = initializeStudentRelatedBeans(HealthImmunizationSeries.class,
                    HealthImmunizationSeries.COL_STUDENT_OID);
        }
        if (m_studentImmunizationSeries.get(studentOid) == null) {
            m_studentImmunizationSeries.put(studentOid, new ArrayList<HealthImmunizationSeries>());
        }
        return m_studentImmunizationSeries.get(studentOid);
    }


    /**
     * Gets the student programs.
     *
     * @param studentOid String
     * @param pgmId String
     * @return List
     * @throws X2BaseException exception
     */
    public List<StudentProgramParticipation> getStudentPrograms(String studentOid, String pgmId)
            throws X2BaseException {
        StudentProgramDataset dataset = getProgramDataset(pgmId);
        List<StudentProgramParticipation> programs = new ArrayList<>();
        if (dataset != null) {
            if (dataset.getPrograms(studentOid) != null) {
                programs.addAll(dataset.getPrograms(studentOid));
            }
        }

        return programs;
    }


    /**
     * Gets the student transcripts.
     *
     * @param studentOid String
     * @return Collection
     */
    public Collection<Transcript> getStudentTranscripts(String studentOid) {
        if (m_studentTranscript == null) {
            initializeStudentTranscripts();
        }
        if (m_studentTranscript.get(studentOid) == null) {
            m_studentTranscript.put(studentOid, new ArrayList<Transcript>());
        }
        return m_studentTranscript.get(studentOid);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getSurveyPeriod()
     */
    @Override
    public SurveyPeriod getSurveyPeriod() {
        return m_surveyPeriod;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        m_surveyPeriod = new FasterSurveyPeriodMockup();

        m_restrictByRecordType = (String) getParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE);
        m_omitHeaderRecord = (Boolean) getParameter(INPUT_PARAM_OMIT_HEADER_RECORD);

        m_recordsType = (String) getParameter(INPUT_PARAM_RECORDS_TYPE);
        m_transferType = (String) getParameter(INPUT_PARAM_TRANSFER_TYPE);

        loadGradeScales();
        initializeSchoolYearDates();

        X2Criteria stdCriteria = getStudentCriteria();
        X2Criteria efdCriteria = new X2Criteria();
        efdCriteria.addLike(ExportFormatDefinition.COL_PROCEDURE_ID, "EXPDATA-FL-FST-%%%%%");
        QueryByCriteria efdQuery = new QueryByCriteria(ExportFormatDefinition.class, efdCriteria);
        m_procIdEfds = getBroker().getMapByQuery(efdQuery, ExportFormatDefinition.COL_PROCEDURE_ID, 17);

        setQuery(new QueryByCriteria(SisStudent.class, stdCriteria));
        if (StringUtils.isEmpty(m_restrictByRecordType)) {
            if (m_omitHeaderRecord != null && m_omitHeaderRecord.booleanValue()) {
                setEntityClass(Record01.class);
            } else {
                setEntityClass(Record00.class);
            }
        } else {
            setEntityClass(Record.findByRecordType(m_restrictByRecordType).getEntityClass());
        }

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
    }


    /**
     * Initialize school year dates.
     */
    public void initializeSchoolYearDates() {
        m_contextsByYears = getBroker().getMapByQuery(
                new QueryByCriteria(DistrictSchoolYearContext.class, new X2Criteria()),
                DistrictSchoolYearContext.COL_SCHOOL_YEAR, 10);
    }


    /**
     * Initialize student assessments.
     *
     * @param assessmentDefinitionId String
     */
    public void initializeStudentAssessments(String assessmentDefinitionId) {
        SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());

        X2Criteria asmCriteria = new X2Criteria();
        asmCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER +
                AssessmentDefinition.COL_ID, assessmentDefinitionId);
        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID, stdSubQuery);
        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);

        Map<String, List<StudentAssessment>> studentAssessments =
                getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID, 10);
        m_studentAssessments.put(assessmentDefinitionId, studentAssessments);
    }


    /**
     * Initialize student contacts.
     */
    public void initializeStudentContacts() {
        X2Criteria contactsCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getQuery().getCriteria());
        contactsCriteria.addIn(StudentContact.COL_STUDENT_OID, studentSubQuery);
        DataDictionaryField guardianCode =
                getFieldByAlias(RetrieveContact.ALIAS_PARENT_GUARDIAN_CODE, getDataDictionary());
        contactsCriteria.addIn(guardianCode.getJavaName(), RetrieveContact.s_parentGuardiansCodes);
        QueryByCriteria contactsQuery = new QueryByCriteria(StudentContact.class, contactsCriteria);
        m_studentContacts = getBroker().getGroupedCollectionByQuery(contactsQuery, StudentContact.COL_STUDENT_OID, 10);
    }


    /**
     * Initialize student health screenings.
     */
    public void initializeStudentHealthScreenings() {
        X2Criteria hsCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getQuery().getCriteria());
        hsCriteria.addIn(HealthScreening.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria hsQuery = new QueryByCriteria(HealthScreening.class, hsCriteria);
        m_studentHealthScreenings =
                getBroker().getGroupedCollectionByQuery(hsQuery, HealthScreening.COL_STUDENT_OID, 10);
    }


    /**
     * Initialize student related beans.
     *
     * @param <T> the generic type
     * @param clazz Class<T>
     * @param colStudentOid String
     * @return Map
     */
    public <T> Map<String, List<T>> initializeStudentRelatedBeans(Class<T> clazz, String colStudentOid) {
        X2Criteria criteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getQuery().getCriteria());
        criteria.addIn(colStudentOid, studentSubQuery);
        QueryByCriteria query = new QueryByCriteria(clazz, criteria);
        return getBroker().getGroupedCollectionByQuery(query, colStudentOid, 10);
    }


    /**
     * Initialize student transcripts.
     */
    public void initializeStudentTranscripts() {
        X2Criteria transcriptCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getQuery().getCriteria());
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
        transcriptCriteria.addNotEqualTo(Transcript.COL_SCHOOL_YEAR, "0");
        DataDictionaryField fromFasterInd =
                getFieldByAlias(RetrieveTranscript.ALIAS_TRN_FROM_FASTER_INDICATOR, getDataDictionary());
        transcriptCriteria.addNotEqualTo(fromFasterInd.getJavaName(), BooleanAsStringConverter.TRUE);
        QueryByCriteria transcriptQuery = new QueryByCriteria(Transcript.class, transcriptCriteria);
        transcriptQuery.addOrderBy(Transcript.COL_SCHOOL_YEAR, true);
        m_studentTranscript = getBroker().getGroupedCollectionByQuery(transcriptQuery, Transcript.COL_STUDENT_OID, 10);
    }


    /**
     * Checks if is production.
     *
     * @return Boolean
     */
    public Boolean isProduction() {
        if (m_isProduction == null) {
            m_isProduction = (Boolean) getParameter(INPUT_PARAM_PRODUCTION_INDICATOR);
        }
        return m_isProduction;
    }


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#next()
     */
    @Override
    public StateReportEntity next() throws X2BaseException {
        StateReportEntity nextEntity = super.next();
        if (StringUtils.isEmpty(m_restrictByRecordType)) {
            while (nextEntity == null) {
                Record currentRecord = Record.findByRecordEntityClass(getEntityClass());
                Record nextRecord =
                        Record.getNextBeingReportedRecord(currentRecord, getTransferType(), getRecordsType());
                if (nextRecord != null) {
                    setEntityClass(nextRecord.getEntityClass());
                    // reopen
                    close();
                    open();
                    nextEntity = super.next();
                } else {
                    break;
                }
            }
            if (nextEntity != null) {
                Record currentRecord = Record.findByRecordEntityClass(getEntityClass());
                ExportFormatDefinition efd = getEfdByRecordType(currentRecord.getFormatDefinitionId());
                setEfdOid(efd.getOid());
            }
        }

        return nextEntity;
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param ddxId String
     * @return Extended data dictionary
     */
    protected ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
        ExtendedDataDictionary extendedDataDictionary = m_ddxById.get(ddxId);
        if (extendedDataDictionary == null) {
            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
            m_ddxById.put(ddxId, extendedDataDictionary);
        }

        return extendedDataDictionary;
    }


    /**
     * Gets the student criteria.
     *
     * @return X 2 criteria
     */
    protected X2Criteria getStudentCriteria() {
        X2Criteria stdCriteria = new X2Criteria();
        String currentSelection = (String) getParameter(INPUT_PARAM_CURRENT_SELECTION);
        if (!StringUtils.isEmpty(currentSelection)) {
            stdCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList(currentSelection.split(";")));
        }
        return stdCriteria;
    }


    /**
     * Gets the asm data dictionary.
     *
     * @param stdAsm StudentAssessment
     * @return Data dictionary
     */
    private DataDictionary getAsmDataDictionary(StudentAssessment stdAsm) {
        String asdId = stdAsm.getAssessmentDefinition().getId();
        if (m_asdDictionaries.get(asdId) == null) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                    stdAsm.getExtendedDataDictionary(), getBroker().getPersistenceKey());
            m_asdDictionaries.put(asdId, dictionary);
        }
        return m_asdDictionaries.get(asdId);
    }


    /**
     * Gets the efd by record type.
     *
     * @param recordType String
     * @return Export format definition
     */
    private ExportFormatDefinition getEfdByRecordType(String recordType) {
        for (Entry<String, ExportFormatDefinition> entry : m_procIdEfds.entrySet()) {
            if (entry.getKey().endsWith(recordType)) {
                return entry.getValue();
            }
        }
        return null;
    }


    /**
     * Gets the records type.
     *
     * @return String
     */
    private String getRecordsType() {
        return m_recordsType;
    }


    /**
     * Gets the ref code dep codes map.
     *
     * @param refTableName String
     * @return Map
     */
    private Map<String, String> getRefCodeDepCodesMap(String refTableName) {
        if (m_refCodesDependencyCode == null) {
            m_refCodesDependencyCode = new HashMap<>();
        }
        if (m_refCodesDependencyCode.get(refTableName) == null) {
            Map<String, String> codeDependency = new HashMap<>();
            m_refCodesDependencyCode.put(refTableName, codeDependency);
            X2Criteria refCodesCriteria = new X2Criteria();
            refCodesCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                    refTableName);
            QueryByCriteria refCodesQuery = new QueryByCriteria(ReferenceCode.class, refCodesCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(refCodesQuery);
            for (ReferenceCode refCode : refCodes) {
                codeDependency.put(refCode.getCode(), refCode.getDependencyCode());
            }
        }
        return m_refCodesDependencyCode.get(refTableName);
    }


    /**
     * Gets the student race map.
     *
     * @return Map
     */
    private Map<String, Collection<Race>> getStudentRaceMap() {
        if (m_raceMap == null) {
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, getStudentCriteria());
            X2Criteria raceCriteria = new X2Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, studentSubQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceMap =
                    getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID,
                            INITIAL_MAP_SIZE);
        }
        return m_raceMap;
    }


    /**
     * Gets the transfer type.
     *
     * @return String
     */
    private String getTransferType() {
        return m_transferType;
    }


    /**
     * Load grade scales.
     */
    private void loadGradeScales() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) iterator.next();
                m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            iterator.close();
        }
    }


    /**
     * Register field retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveRecordType.CALC_ID, new RetrieveRecordType());
        calcs.put(RetrieveYear.CALC_ID, new RetrieveYear());
        calcs.put(RetrieveAddressedInst.CALC_ID, new RetrieveAddressedInst(this));
        calcs.put(RetrieveSendingInst.CALC_ID, new RetrieveSendingInst());
        calcs.put(RetrieveTranscript.CALC_ID, new RetrieveTranscript(this));
        calcs.put(RetrieveMessageType.CALC_ID, new RetrieveMessageType());
        calcs.put(RetrieveTestProduction.CALC_ID, new RetrieveTestProduction());
        calcs.put(RetrieveImmunization.CALC_ID, new RetrieveImmunization(this));
        calcs.put(RetrieveEll.CALC_ID, new RetrieveEll(this));
        calcs.put(RetrieveTestInfo.CALC_ID, new RetrieveTestInfo());
        calcs.put(RetrieveRace.CALC_ID, new RetrieveRace(this));
        calcs.put(RetrieveStudentInfo.CALC_ID, new RetrieveStudentInfo(this));
        calcs.put(RetrieveMilitary.CALC_ID, new RetrieveMilitary(this));
        calcs.put(RetrieveMigrant.CALC_ID, new RetrieveMigrant(this));
        calcs.put(RetrieveExcept.CALC_ID, new RetrieveExcept(this));
        calcs.put(RetrieveContact.CALC_ID, new RetrieveContact(this));
        calcs.put(RetrieveHealthInfo.CALC_ID, new RetrieveHealthInfo(this));
        calcs.put(RetrieveCredsNeeded.CALC_ID, new RetrieveCredsNeeded(this));
        calcs.put(RetrieveProgram.CALC_ID, new RetrieveProgram());
        calcs.put(RetrieveAttendance.CALC_ID, new RetrieveAttendance());
        calcs.put(RetrieveAttendanceSummer.CALC_ID, new RetrieveAttendanceSummer(this));
        calcs.put(RetrieveDecimalWoPoint.CALC_ID, new RetrieveDecimalWoPoint());
        calcs.put(RetrieveCteEllDropoutInfo.CALC_ID, new RetrieveCteEllDropoutInfo());
        calcs.put(RetrieveConduct.CALC_ID, new RetrieveConduct());
        addCalcs(calcs);
    }
}
