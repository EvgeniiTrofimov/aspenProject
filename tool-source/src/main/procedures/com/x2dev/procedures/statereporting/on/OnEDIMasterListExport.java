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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.microsoft.sqlserver.jdbc.StringUtils;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolPersonAddress;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAddress;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaff;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisConstants;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2023
 */
public class OnEDIMasterListExport extends ExportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String FIELD_SCHOOL_BOARD_NAME = "SchoolBoardName";
    private static final String FIELD_SCHOOL_BOARD_BSID = "SchoolBoardBsid";
    private static final String FIELD_EY_LEAD_NAME = "EYLeadName";
    private static final String FIELD_EY_LEAD_EMAIL = "EYLeadEmail";
    private static final String FIELD_SCHOOL_NAME = "SchoolName";
    private static final String FIELD_SCHOOL_BSID = "SchoolBsid";
    private static final String FIELD_TEACHER_FIRST_NAME = "TeacherFirstName";
    private static final String FIELD_TEACHER_LAST_NAME = "TeacherLastName";
    private static final String FIELD_TEACHER_EMAIL = "TeacherEmail";
    private static final String FIELD_STUDENT_EDI_ID = "StudentEdiId";
    private static final String FIELD_STUDENT_OEN = "StudentOen";
    private static final String FIELD_STUDENT_LEGAL_LAST_NAME = "StudentLegalLastName";
    private static final String FIELD_STUDENT_LEGAL_FIRST_NAME = "StudentLegalFirstName";
    private static final String FIELD_STUDENT_LOCAL_ID = "StudentLocalId";
    private static final String FIELD_STUDENT_GENDER = "StudentGender";
    private static final String FIELD_STUDENT_BIRTHDATE_DD = "StudentBirthdateDD";
    private static final String FIELD_STUDENT_BIRTHDATE_MM = "StudentBirthdateMM";
    private static final String FIELD_STUDENT_BIRTHDATE_YYYY = "StudentBirthdateYYYY";
    private static final String FIELD_STUDENT_POSTAL_CODE = "StudentPostalCode";

    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_CUSTOM_NAME = "customName";
    private static final String INPUT_PARAM_INCLUDE_EDI_ID = "includeEdiID";
    private static final String INPUT_PARAM_INCLUDE_STUDENT_INFO = "includeStudentInfo";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String MSG_KEY_COLUMNS = "ied.EXP-ON-EDI-MASTER.columns";
    private static final List<String> ONSIS_CODES_ELEMENTARY_SCHOOL = Arrays.asList("01", "03");
    private static final int STUDENT_INDEX_EDI_ID = 18;
    private static final int STUDENT_INFO_INDEX_BEGIN = 9;
    private static final int STUDENT_INFO_INDEX_END = 11;

    /**
     * Class members.
     */
    private DictionaryExtractor m_dictExtractor;
    private PlainDate m_currentDate;
    private Boolean m_includeEdiID;
    private Boolean m_includeStudentInfo;
    private boolean m_isAllSchools = true;
    private transient LocalizationMessageResources m_defaultMessageResource;
    private List<String> m_schoolOids;
    private Filterable<OnSchool> m_schools;
    private Locale m_userLocale;

    /**
     * Gets the custom file name.
     *
     * @return the custom file name
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        // initialize here - this executes befor initialize method
        m_includeEdiID = Boolean.FALSE;
        Object includeEdiID = getParameter(INPUT_PARAM_INCLUDE_EDI_ID);
        if (includeEdiID != null && includeEdiID instanceof Boolean) {
            m_includeEdiID = ((Boolean) includeEdiID).booleanValue();
        }

        ToolBean.setDictionaryExtractor(getDictExtractor());

        String customName = (String) getParameter(INPUT_PARAM_CUSTOM_NAME);

        String prefix = "EDIMasterExport";

        String fileNamePrefix = StringUtils.isEmpty(customName) ? prefix : customName;
        String extension = m_includeEdiID.booleanValue() ? ".txt" : ".csv";

        return fileNamePrefix.replaceAll("\\s", "_") + extension;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        preload();

        ToolBean.getCachedToolBeans(OnStudent.class).stream().forEach(student -> {
            OnSchool school = (OnSchool) student.getSchool(getBroker());
            OnAddress address = getStudentAddress(student, school);
            OnSection homeroom = getStudentHomeroom(student);
            OnStaff staff = homeroom == null ? null : getSectionTeacher(homeroom);

            String dateStr = "";
            PlainDate date = student.getDob();
            if (date != null) {
                dateStr = OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.format(student.getDob());
            }
            String[] dateComponents = dateStr.split("/");

            grid.append();
            grid.set(FIELD_SCHOOL_BOARD_NAME, school.getBoardName());
            grid.set(FIELD_SCHOOL_BOARD_BSID, school.getBoardBsid());
            grid.set(FIELD_EY_LEAD_NAME, school.getEYLeadName());
            grid.set(FIELD_EY_LEAD_EMAIL, school.getEYLeadEmail());
            grid.set(FIELD_SCHOOL_NAME, school.getName());
            grid.set(FIELD_SCHOOL_BSID, school.getBsid());
            grid.set(FIELD_TEACHER_FIRST_NAME, staff == null ? "" : staff.getFirstName());
            grid.set(FIELD_TEACHER_LAST_NAME, staff == null ? "" : staff.getLastName());
            grid.set(FIELD_TEACHER_EMAIL, staff == null ? "" : staff.getEmail01());
            if (includeStudentInfo()) {
                grid.set(FIELD_STUDENT_OEN, student.getOen());
                grid.set(FIELD_STUDENT_LEGAL_LAST_NAME, student.getLegalLastName());
                grid.set(FIELD_STUDENT_LEGAL_FIRST_NAME, student.getLegalFirstName());
            }
            grid.set(FIELD_STUDENT_LOCAL_ID, student.getLocalId());
            grid.set(FIELD_STUDENT_GENDER, student.getGenderType());
            grid.set(FIELD_STUDENT_BIRTHDATE_DD, dateComponents.length > 2 ? dateComponents[2] : "");
            grid.set(FIELD_STUDENT_BIRTHDATE_MM, dateComponents.length > 1 ? dateComponents[1] : "");
            grid.set(FIELD_STUDENT_BIRTHDATE_YYYY, dateComponents.length > 0 ? dateComponents[0] : "");
            String postalCode = "";
            if (address != null & !com.x2dev.utils.StringUtils.isEmpty(address.getPostalCode())) {
                postalCode = address.getPostalCode().replaceAll("[^a-zA-Z0-9]", "");
            }
            grid.set(FIELD_STUDENT_POSTAL_CODE, postalCode);
            if (m_includeEdiID.booleanValue()) {
                grid.set(FIELD_STUDENT_EDI_ID, student.getEdiID());
            }
        });
        grid.sort(Arrays.asList(FIELD_SCHOOL_BOARD_NAME, FIELD_SCHOOL_NAME, FIELD_TEACHER_LAST_NAME,
                FIELD_TEACHER_FIRST_NAME, FIELD_TEACHER_EMAIL, FIELD_STUDENT_LOCAL_ID), true);
        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columns = new LinkedList();
        columns.add(FIELD_SCHOOL_BOARD_NAME);
        columns.add(FIELD_SCHOOL_BOARD_BSID);
        columns.add(FIELD_EY_LEAD_NAME);
        columns.add(FIELD_EY_LEAD_EMAIL);
        columns.add(FIELD_SCHOOL_NAME);
        columns.add(FIELD_SCHOOL_BSID);
        columns.add(FIELD_TEACHER_FIRST_NAME);
        columns.add(FIELD_TEACHER_LAST_NAME);
        columns.add(FIELD_TEACHER_EMAIL);
        if (includeStudentInfo()) {
            columns.add(FIELD_STUDENT_OEN);
            columns.add(FIELD_STUDENT_LEGAL_LAST_NAME);
            columns.add(FIELD_STUDENT_LEGAL_FIRST_NAME);
        }
        columns.add(FIELD_STUDENT_LOCAL_ID);
        columns.add(FIELD_STUDENT_GENDER);
        columns.add(FIELD_STUDENT_BIRTHDATE_DD);
        columns.add(FIELD_STUDENT_BIRTHDATE_MM);
        columns.add(FIELD_STUDENT_BIRTHDATE_YYYY);
        columns.add(FIELD_STUDENT_POSTAL_CODE);
        if (m_includeEdiID.booleanValue()) {
            columns.add(FIELD_STUDENT_EDI_ID);
        }
        return columns;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columns = new LinkedList();
        LocalizationMessageResources messageResource = getMessageResource();
        String columnsText = messageResource.getMessage(MSG_KEY_COLUMNS);

        int index = 0;
        for (String column : columnsText.split("\\|")) {
            if (index < STUDENT_INDEX_EDI_ID) {
                if (includeStudentInfo() || index < STUDENT_INFO_INDEX_BEGIN
                        || index > STUDENT_INFO_INDEX_END) {
                    columns.add(column);
                }

            } else if (m_includeEdiID.booleanValue()) {
                columns.add(column);
            }
            ++index;
        }
        return columns;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());
        m_currentDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        ToolBean.registerClass(OnAddress.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnScheduleTeacher.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnSection.class);
        ToolBean.registerClass(OnStaff.class);
        ToolBean.registerClass(OnStudent.class);

        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (isAllSchools != null) {
            m_isAllSchools = isAllSchools;
        }
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
     * Gets the school oids.
     *
     * @return List
     */
    private List<String> getSchoolOids() {
        if (m_schoolOids == null) {
            String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
            m_schoolOids = Arrays.asList(schoolOids.split(","));
        }
        return m_schoolOids;
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
                            return (m_isAllSchools || getSchoolOids().contains(school.getOid()))
                                    && isKindergartenSchool(school);
                        }
                    });
        }
        return m_schools;
    }

    /**
     * @param homeroom
     * @return
     */
    private OnStaff getSectionTeacher(OnSection homeroom) {
        return homeroom.getTeacherSections(getBroker()).stream()
                .filter(mtc -> mtc.getPrimaryTeacherIndicator())
                .map(mtc -> mtc.getStaff(getBroker()))
                .map(staff -> (OnStaff) staff)
                .findFirst().orElse(null);
    }

    /**
     * @param student
     * @return
     */
    private OnAddress getStudentAddress(OnStudent student, OnSchool school) {
        OnAddress address = null;
        if (student.getShelteredStudentIndicator()) {
            address = (OnAddress) school.getAddress(getBroker());
        } else {
            ToolPersonAddress personAddress =
                    student.getPersonAddresses(getBroker()).stream().findFirst().orElse(null);
            address = personAddress == null ? (OnAddress) student.getPhysicalAddress(getBroker())
                    : (OnAddress) personAddress.getAddress(getBroker());
        }
        return address;
    }

    private OnSection getStudentHomeroom(OnStudent student) {
        return student.getStudentSchedules(getBroker()).stream()
                .map(ssc -> (OnSection) ssc.getSection(getBroker()))
                .filter(mst -> mst.getDateRange(getBroker()).contains(m_currentDate))
                .findFirst().orElse(null);
    }

    /**
     * Gets the student schedule criteria.
     *
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
     * @return
     */
    private boolean includeStudentInfo() {
        if (m_includeStudentInfo == null) {
            m_includeStudentInfo = Boolean.TRUE;
            Object includeInfo = getParameter(INPUT_PARAM_INCLUDE_STUDENT_INFO);
            if (includeInfo != null && includeInfo instanceof Boolean) {
                m_includeStudentInfo = ((Boolean) includeInfo).booleanValue();
            }
        }
        return m_includeStudentInfo.booleanValue();
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

    private void preload() {
        Filterable<OnSchool> schools = getSchools();

        X2Criteria criteria = new X2Criteria();
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

        // preload student address
        X2Criteria personAddressCriteria = new X2Criteria();
        personAddressCriteria.addEqualTo(ToolPersonAddress.FIELD_ADDRESS_TYPE.resolve(null),
                ToolPersonAddress.ADDRESS_TYPE_PHYSICAL);
        personAddressCriteria.addLessOrEqualThan(ToolPersonAddress.FIELD_START_DATE.resolve(null), m_currentDate);
        X2Criteria dateCriteria = new X2Criteria();
        dateCriteria.addEmpty(ToolPersonAddress.FIELD_END_DATE.resolve(null), getBroker().getPersistenceKey());
        X2Criteria orDateCriteria = new X2Criteria();
        orDateCriteria.addGreaterOrEqualThan(ToolPersonAddress.FIELD_END_DATE.resolve(null), m_currentDate);
        dateCriteria.addOrCriteria(orDateCriteria);
        personAddressCriteria.addAndCriteria(dateCriteria);

        ToolBean.addAndCriteria(getBroker(), ToolPersonAddress.class, personAddressCriteria);

        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolPersonAddress.FIELD_START_DATE, ToolPersonAddress.FIELD_END_DATE),
                ToolStudent.CHILD_PERSON_ADDRESSES);

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
}
