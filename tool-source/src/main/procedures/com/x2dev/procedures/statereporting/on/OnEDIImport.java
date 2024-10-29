/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2023 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaff;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * The Class OnEDIImport.
 *
 * @author Follett Software Company
 * @copyright 2023
 */
public class OnEDIImport extends TextImportJavaSource {

    /**
     * The Class ImportedRecord.
     */
    private class ImportedRecord {
        private String m_ediId;
        private int m_lineNumber;
        private String m_localId;
        private String m_teacherEmail;

        /**
         * Instantiates a new imported record.
         *
         * @param localId the local id
         * @param teacherEmail the teacher email
         * @param ediId the edi id
         * @param lineNumber int
         */
        public ImportedRecord(String localId, String teacherEmail, String ediId, int lineNumber) {
            m_ediId = ediId;
            m_lineNumber = lineNumber;
            m_localId = localId;
            m_teacherEmail = teacherEmail;
        }

        /**
         * Gets the edi id.
         *
         * @return String
         */
        public String getEdiId() {
            return m_ediId;
        }

        /**
         * Gets the line number.
         *
         * @return int
         */
        public int getLineNumber() {
            return m_lineNumber;
        }

        /**
         * Gets the local id.
         *
         * @return String
         */
        public String getLocalId() {
            return m_localId;
        }

        /**
         * Gets the teacher email.
         *
         * @return String
         */
        public String getTeacherEmail() {
            return m_teacherEmail;
        }

    }

    private static final String HEADING_EDI_ID = "EDI ID";
    private static final String HEADING_FR_EDI_ID = "IDENTIFICATEUR IMDPE";
    private static final String HEADING_FR_LOCAL_ID = "ID LOCAL DE L'ENFANT";
    private static final String HEADING_FR_TEACHER_EMAIL = "COURRIEL DE L'ENSEIGNANT(E)";
    private static final String HEADING_LOCAL_ID = "CHILDS LOCAL ID";
    private static final String HEADING_TEACHER_EMAIL = "TEACHEREMAIL";
    private static final List<String> ONSIS_CODES_ELEMENTARY_SCHOOL = Arrays.asList("01", "03");
    private static final String LINE_NUMBER_SEPARATOR = ":";
    private static final String PARAM_COMMIT = "commit";
    private static final String PARAM_MATCH_EMAIL = "matchEmail";
    private static final String PARAM_SKIP_ROWS = "skipRows";

    // Messages
    private static final int MSG_ERROR_FILE_FORMAT_INDEX = 0;
    private static final int MSG_ERROR_HEADING_NOT_FOUND_INDEX = 1;
    private static final int MSG_ERROR_MATCHING_RECORD_NOT_FOUND_INDEX = 2;
    private static final int MSG_ERROR_RECORD_NOT_FOUND_INDEX = 3;
    private static final int MSG_ERROR_SETUP_INDEX = 4;
    private static final int MSG_ERROR_TEACHER_EMAIL_INDEX = 5;
    private static final int MSG_ERRORS_INDEX = 6;
    private static final String MSG_KEY_STRINGS = "ied.IMP-ON-EDI.strings";
    private static final int MSG_MODE_COMMIT_INDEX = 7;
    private static final int MSG_MODE_REVIEW_INDEX = 8;

    private boolean m_commit;
    private PlainDate m_currentDate;
    private transient LocalizationMessageResources m_defaultMessageResource;
    private DictionaryExtractor m_dictExtractor;
    private List<String> m_errors = new LinkedList();
    private int m_fieldCount;
    private Map<String, Integer> m_headingIndexes = new HashMap();
    private Map<String, ImportedRecord> m_importedRecords = new HashMap();
    private boolean m_matchEmail = false;
    private String[] m_msgStrings;
    private Filterable<OnSchool> m_schools;
    private List<String> m_setupErrors = new LinkedList();
    private int m_skipRows;
    private Locale m_userLocale;

    /**
     * After import data.
     *
     * @param file File
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#afterImportData(java.io.File)
     */
    @Override
    protected void afterImportData(File file) {
        preload();
        ToolBean.getCachedToolBeans(OnStudent.class).stream().forEach(student -> {
            OnSection homeroom = getStudentHomeroom(student);
            OnStaff staff = homeroom == null ? null : getSectionTeacher(homeroom);
            String ediId = StringUtils.unNullify(student.getEdiID());
            String email = staff == null ? "" : (staff.getEmail01() == null ? "" : staff.getEmail01());
            ImportedRecord record = m_importedRecords.remove(student.getLocalId());
            if (record == null) {
                addError(0, String.format(getMessageString(MSG_ERROR_RECORD_NOT_FOUND_INDEX), student.getLocalId(),
                        student.getNameView()));
                this.incrementSkipCount();
            } else if (!m_matchEmail || email.equals(record.getTeacherEmail())) {
                this.incrementMatchCount();
                if (m_commit) {
                    String newEdiId = record.getEdiId();
                    if (StringUtils.isEmpty(student.getEdiID()) || !student.getEdiID().equals(newEdiId)) {
                        SisStudent bean = getBroker().getBeanByOid(SisStudent.class, student.getOid());
                        bean.setFieldValueByAlias(OnStudent.FIELD_EDI_ID.getAlias(), newEdiId);
                        getBroker().saveBeanForced(bean);
                    }
                    this.incrementUpdateCount();
                }
            } else {
                String errorEmail = String.format(getMessageString(MSG_ERROR_TEACHER_EMAIL_INDEX), student.getLocalId(),
                        student.getNameView(),
                        email, record.getTeacherEmail());
                addError(record.getLineNumber(), errorEmail);
                this.incrementSkipCount();
            }
        });
        m_importedRecords.values().stream()
                .sorted(Comparator.comparingInt(ImportedRecord::getLineNumber))
                .forEach(record -> {
                    addError(record.getLineNumber(), String
                            .format(getMessageString(MSG_ERROR_MATCHING_RECORD_NOT_FOUND_INDEX), record.getLocalId()));
                    this.incrementSkipCount();
                });
        if (!m_errors.isEmpty()) {
            Collections.sort(m_errors, new Comparator<String>() {
                @Override
                public int compare(String error1, String error2) {
                    String[] err1Strings = error1.split(LINE_NUMBER_SEPARATOR);
                    String[] err2Strings = error2.split(LINE_NUMBER_SEPARATOR);
                    if (err1Strings.length > 0 && err2Strings.length > 0) {
                        String lineNum1 = err1Strings[0];
                        String lineNum2 = err2Strings[0];
                        if (lineNum1.chars().allMatch(Character::isDigit)
                                && lineNum2.chars().allMatch(Character::isDigit)) {
                            return Integer.parseInt(lineNum1) - Integer.parseInt(lineNum2);
                        }
                    }
                    return 0;
                }
            });
        }
    }

    /**
     * Display only init errors if exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        String mode = m_commit ? getMessageString(MSG_MODE_COMMIT_INDEX) : getMessageString(MSG_MODE_REVIEW_INDEX);
        buffer.append(mode);
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(getImportStatistics().toString());
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
        if (m_setupErrors.isEmpty()) {
            if (!m_errors.isEmpty()) {
                m_errors.add("\n");
                exportList(m_errors);
            }
        } else {
            exportList(m_setupErrors);
        }
    }

    /**
     * Gets the field count.
     *
     * @return the field count
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return m_fieldCount;
    }

    /**
     * Return StringBuilder which contain the text that would show to user when the import procedure
     * finished.
     *
     * @return StringBuilder
     */
    @Override
    protected StringBuilder getImportStatistics() {
        String sourceFileName = ((File) getParameter(FILE_KEY)).getName();

        LocalizationMessageResources resources =
                LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        StringBuilder buffer = new StringBuilder(256);
        Integer insertCount = Integer.valueOf(getInsertCount());
        Integer matchCount = Integer.valueOf(getMatchCount());
        Integer skipCount = Integer.valueOf(getSkipCount());
        Integer updateCount = Integer.valueOf(getUpdateCount());
        Integer total = Integer.valueOf(getMatchCount() + getInsertCount() + getSkipCount());

        buffer.append(resources.getMessage(getLocale(), "message.import.results"));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.doubleRule"));
        buffer.append('\n');
        buffer.append('\n');
        String fileName = resources.getMessage(getLocale(), "message.import.results.fileName", sourceFileName);
        if (Locale.FRANCE.equals(getLocale())) {
            fileName = fileName.replace("&nbsp;", " ");
        }
        buffer.append(fileName);
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.matchCount", matchCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.updateCount", updateCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.insertCount", insertCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.skipCount", skipCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.rule", skipCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.total", total));
        buffer.append('\n');

        if (!getInvalidRecords().isEmpty()) {
            buffer.append('\n');
            buffer.append(resources.getMessage(getLocale(), "message.import.results.invalidHeader", skipCount));
            buffer.append('\n');
            buffer.append(resources.getMessage(getLocale(), "message.import.results.invalidRule", skipCount));
            buffer.append('\n');
            buffer.append('\n');

            for (KeyValuePair<Integer, String> invalidRecord : getInvalidRecords()) {
                String message = resources.getMessage(getLocale(),
                        "message.import.results.invalidRecord",
                        invalidRecord.getKey(),
                        invalidRecord.getValue());
                buffer.append(message);
                buffer.append('\n');
            }
        }

        return buffer;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        if (m_setupErrors.isEmpty()) {
            super.importData(sourceFile);
        }
    }


    /**
     * Import record.
     *
     * @param record the record
     * @param lineNumber the line number
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        if (lineNumber > m_skipRows) {
            String localId = getRecordValue(record, HEADING_LOCAL_ID, HEADING_FR_LOCAL_ID);
            String teacherEmail = getRecordValue(record, HEADING_TEACHER_EMAIL, HEADING_FR_TEACHER_EMAIL);
            String ediId = getRecordValue(record, HEADING_EDI_ID, HEADING_FR_EDI_ID);
            m_importedRecords.put(localId, new ImportedRecord(localId, teacherEmail, ediId, lineNumber));
        }

    }

    /**
     * Initialize.
     *
     * @throws X2BaseException the x 2 base exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());
        m_currentDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnScheduleTeacher.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnSection.class);
        ToolBean.registerClass(OnStaff.class);
        ToolBean.registerClass(OnStudent.class);

        m_skipRows = ((Integer) getParameter(PARAM_SKIP_ROWS)).intValue();
        if (getParameter(PARAM_MATCH_EMAIL) instanceof Boolean) {
            m_matchEmail = ((Boolean) getParameter(PARAM_MATCH_EMAIL)).booleanValue();
        }
        Boolean commit = (Boolean) getParameter(PARAM_COMMIT);
        m_commit = commit != null ? commit.booleanValue() : true; // default: commit & review

        readInputHeading();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_userLocale = userData.getLocale();
    }

    /**
     * Adds one validation error.
     *
     * @param lineNumber int
     * @param errorMessage String
     * @return true, if successful
     */
    private boolean addError(int lineNumber, String errorMessage) {
        m_errors.add("" + lineNumber + LINE_NUMBER_SEPARATOR + " " + errorMessage);
        return true;
    }

    /**
     * Adds one validation error.
     *
     * @param errorType String
     * @param errorMessage String
     */
    private void addSetupError(String errorType, String errorMessage) {
        m_setupErrors.add(errorType + "-" + errorMessage);
    }

    /**
     * Write a list of strings to the results handler.
     *
     * @param list List<String>
     * @throws X2BaseException exception
     */
    private void exportList(List<String> list) throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        buffer.append('\n');
        if (!list.isEmpty()) {
            buffer.append(getMessageString(MSG_ERRORS_INDEX) + '\n');
        }
        for (String err : list) {
            buffer.append(err);
            buffer.append('\n');
        }
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the message resource.
     *
     * @return the message resource
     */
    private LocalizationMessageResources getMessageResource() {
        if (m_defaultMessageResource == null) {
            try {
                m_defaultMessageResource =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_userLocale);
            } catch (Exception e) {
                m_defaultMessageResource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        LocalizationCache.getCurrentLocale());
            }
        }
        return m_defaultMessageResource;
    }

    /**
     * Gets the message string.
     *
     * @param index the index
     * @return the message string
     */
    private String getMessageString(int index) {
        if (m_msgStrings == null) {
            m_msgStrings = getMessageResource().getMessage(MSG_KEY_STRINGS).split("\\|");
        }
        return m_msgStrings[index];
    }

    /**
     * Gets the schools.
     *
     * @return Filterable
     */
    private Filterable<OnSchool> getSchools() {
        if (m_schools == null) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addNotEqualTo(ToolSchool.FIELD_INACTIVE_INDICATOR.resolve(null), Boolean.TRUE);
            schoolCriteria.addNotEqualTo(ToolSchool.FIELD_ARCHIVE_INDICATOR.resolve(null), Boolean.TRUE);
            m_schools = FilterableFactory
                    .create(getBroker(), OnSchool.class, schoolCriteria,
                            Arrays.asList(OnSchool.FIELD_NAME, OnSchool.FIELD_OID))
                    .filter(new Filter() {
                        @Override
                        public boolean isFiltered(Object toFilter) {
                            OnSchool school = (OnSchool) toFilter;
                            return isKindergartenSchool(school);
                        }
                    });
        }
        return m_schools;
    }

    /**
     * Gets the student schedule criteria.
     *
     * @param schoolOids Collection<String>
     * @return X 2 criteria
     */
    private X2Criteria getStudentScheduleCriteria(Collection<String> schoolOids) {
        X2Criteria scheduleCriteria = new X2Criteria();
        // From active Schedule for the selected year.
        scheduleCriteria.addEqualTo(SisBeanPaths.STUDENT_SCHEDULE.schedule().activeSchoolScheduleContexts()
                .districtContextOid().getPath(), getCurrentContext().getOid());

        // From current schools.
        scheduleCriteria.addIn(SisBeanPaths.STUDENT_SCHEDULE.schedule().schoolOid().getPath(),
                schoolOids);

        String pathPrefix = SisBeanPaths.STUDENT_SCHEDULE.section().getPath()
                + ModelProperty.PATH_DELIMITER;

        scheduleCriteria.addNotEqualTo(
                pathPrefix + ModelProperty.PATH_DELIMITER
                        + OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictExtractor()),
                BooleanAsStringConverter.TRUE);

        scheduleCriteria.addNotEqualTo(pathPrefix + ModelProperty.PATH_DELIMITER
                + OnSection.FIELD_CRS_EXCLUDE.resolve(getDictExtractor()),
                BooleanAsStringConverter.TRUE);

        scheduleCriteria.addEqualTo(
                pathPrefix + OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictExtractor()),
                OnSection.COURSE_CODE_TYPE_HOMEROOM);

        return scheduleCriteria;
    }

    /**
     * Checks if is day school.
     *
     * @param school SisSchool
     * @return true, if is day school
     */
    private boolean isKindergartenSchool(OnSchool school) {
        return (StringUtils.isEmpty(school.getSpecialCondition()) || "0".equals(school.getSpecialCondition())) &&
                ONSIS_CODES_ELEMENTARY_SCHOOL.contains(school.getSchoolLevelCodeState()) &&
                school.getStartGrade() <= -1;
    }

    /**
     * Preload.
     */
    private void preload() {
        Filterable<OnSchool> schools = getSchools();
        CollectionCriteriaHelper helper = null;
        try {

            X2Criteria criteria = new X2Criteria();

            X2Criteria idCriteria = new X2Criteria();
            if (m_importedRecords.keySet().size() > ToolBean.MAX_SAFE_PARAMETERS) {
                helper = new CollectionCriteriaHelper(m_importedRecords.keySet(), getBroker());
                helper.applyToCriteria(ToolStudent.FIELD_LOCAL_ID.resolve(getDictExtractor()), idCriteria);
            } else {
                idCriteria.addIn(ToolStudent.FIELD_LOCAL_ID.resolve(getDictExtractor()), m_importedRecords.keySet());
            }
            criteria.addAndCriteria(idCriteria);

            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(ToolBean.DistrictManager.getOrganization(getBroker()),
                            ToolStudent.FIELD_ENROLLMENT_STATUS.resolve(getDictExtractor())));
            criteria.addIn(ToolStudent.FIELD_SCHOOL_OID.resolve(getDictExtractor()), schools.getKeySet());

            List<String> gradeLevelCodes = getDictExtractor().getRefCodesWithStateValue(
                    ToolStudent.FIELD_GRADE_LEVEL.getField(getDictExtractor()),
                    Arrays.asList("K"))
                    .stream()
                    .map(code -> code.getCode())
                    .collect(Collectors.toList());
            criteria.addIn(ToolStudent.FIELD_GRADE_LEVEL.resolve(getDictExtractor()), gradeLevelCodes);

            // preload students to report
            FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class, criteria, null);
        } finally {
            if (helper != null) {
                helper.cleanup();
            }
        }

        // preload enrollments
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);

        // Filter to include only students with arrived status
        ToolBean.filterCachedToolBeans(OnStudent.class, student -> {
            List<ToolEnrollment> enrollments = student.getEnrollments(getBroker());
            if (enrollments.isEmpty()) {
                return false;
            }
            OnEnrollment enrollment = (OnEnrollment) enrollments.get(0);
            return "Arrived".equals(enrollment.getArrivalStatus());
        });
        // preload student schedules
        ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                getStudentScheduleCriteria(schools.getKeySet()));
        ToolBean.preload(getBroker(), getDictExtractor(), null, ToolStudentSchedule.PARENT_STUDENT);

        // preload sections
        List<String> sectionOids = ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream()
                .map(ToolStudentSchedule::getSectionOid)
                .collect(Collectors.toList());
        ToolBean.loadByOid(getBroker(), getDictExtractor(), OnSection.class, sectionOids);

        // preload schedule teacher
        ToolBean.preload(getBroker(), getDictExtractor(), null, OnScheduleTeacher.PARENT_SECTION);
    }

    /**
     * Gets the record value.
     *
     * @param record List<String>
     * @param heading String
     * @param headingFr the heading fr
     * @return String
     */
    private String getRecordValue(List<String> record, String heading, String headingFr) {
        String value = null;
        Integer index = m_headingIndexes.get(heading);
        if (index == null) {
            index = m_headingIndexes.get(headingFr);
        }
        if (index != null) {
            value = record.get(index.intValue());
        }
        return value;
    }

    /**
     * Gets the section teacher.
     *
     * @param homeroom OnSection
     * @return On staff
     */
    private OnStaff getSectionTeacher(OnSection homeroom) {
        return homeroom.getTeacherSections(getBroker()).stream()
                .filter(mtc -> mtc.getPrimaryTeacherIndicator())
                .map(mtc -> mtc.getStaff(getBroker()))
                .map(staff -> (OnStaff) staff)
                .findFirst().orElse(null);
    }


    /**
     * Gets the student homeroom.
     *
     * @param student OnStudent
     * @return On section
     */
    private OnSection getStudentHomeroom(OnStudent student) {
        return student.getStudentSchedules(getBroker()).stream()
                .map(ssc -> (OnSection) ssc.getSection(getBroker()))
                .filter(mst -> mst.getDateRange(getBroker()).contains(m_currentDate))
                .findFirst().orElse(null);
    }

    /**
     * Read a list of column headings from the input file.
     *
     * @return List
     */
    private List<String> readInputHeading() {
        // Set temporary field count - needed to use splitLine()
        m_fieldCount = 100;
        List<String> record = new ArrayList();
        File sourceFile = (File) getParameter(FILE_KEY);
        if (sourceFile != null && sourceFile.getAbsolutePath().endsWith(".csv")) {
            setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
            setUseValueDelimiters(true);
            setValueWrapper('"');

            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new FileReader(sourceFile), 4096);
                String line = reader.readLine();
                record = splitLine(line, 1);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException e) {
                        // Do nothing
                    }
                }
            }
            int index = 0;
            for (String heading : record) {
                m_headingIndexes.put(heading.toUpperCase(), Integer.valueOf(index));
                ++index;
            }
            m_fieldCount = record.size();
        } else {
            addSetupError(getMessageString(MSG_ERROR_SETUP_INDEX),
                    getMessageString(MSG_ERROR_FILE_FORMAT_INDEX));
        }
        if (m_headingIndexes.get(HEADING_EDI_ID) == null && m_headingIndexes.get(HEADING_FR_EDI_ID) == null) {
            addSetupError(getMessageString(MSG_ERROR_SETUP_INDEX),
                    Locale.FRANCE.equals(getLocale())
                            ? String.format(getMessageString(MSG_ERROR_HEADING_NOT_FOUND_INDEX), HEADING_FR_EDI_ID)
                            : String.format(getMessageString(MSG_ERROR_HEADING_NOT_FOUND_INDEX), HEADING_EDI_ID));
        }
        if (m_headingIndexes.get(HEADING_LOCAL_ID) == null && m_headingIndexes.get(HEADING_FR_LOCAL_ID) == null) {
            addSetupError(getMessageString(MSG_ERROR_SETUP_INDEX),
                    Locale.FRANCE.equals(getLocale())
                            ? String.format(getMessageString(MSG_ERROR_HEADING_NOT_FOUND_INDEX), HEADING_FR_LOCAL_ID)
                            : String.format(getMessageString(MSG_ERROR_HEADING_NOT_FOUND_INDEX), HEADING_LOCAL_ID));
        }
        if (m_headingIndexes.get(HEADING_TEACHER_EMAIL) == null
                && m_headingIndexes.get(HEADING_FR_TEACHER_EMAIL) == null) {
            addSetupError(getMessageString(MSG_ERROR_SETUP_INDEX),
                    Locale.FRANCE.equals(getLocale())
                            ? String.format(getMessageString(MSG_ERROR_HEADING_NOT_FOUND_INDEX),
                                    HEADING_FR_TEACHER_EMAIL)
                            : String.format(getMessageString(MSG_ERROR_HEADING_NOT_FOUND_INDEX),
                                    HEADING_TEACHER_EMAIL));
        }
        return record;
    }
}
