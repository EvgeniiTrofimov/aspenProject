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
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

/**
 * The Class OnEDIMinistryListExport.
 *
 * @author Follett Software Company
 * @copyright 2023
 */
public class OnEDIMinistryListExport extends ExportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String FIELD_SCHOOL_BOARD_NAME = "SchoolBoardName";
    private static final String FIELD_SCHOOL_BOARD_BSID = "SchoolBoardBsid";
    private static final String FIELD_STUDENT_BIRTHDATE = "StudentBirthdate";
    private static final String FIELD_STUDENT_EDI_ID = "StudentEDIId";
    private static final String FIELD_STUDENT_LEGAL_FIRST_NAME = "StudentLegalFirstName";
    private static final String FIELD_STUDENT_LEGAL_LAST_NAME = "StudentLegalLastName";
    private static final String FIELD_STUDENT_OEN = "StudentOen";

    private static final SimpleDateFormat DATE_FORMATTER_DD_MM_YYYY_SLASHES = new SimpleDateFormat("dd/MM/yyyy");
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_CUSTOM_NAME = "customName";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String MSG_KEY_COLUMNS = "ied.EXP-ON-EDI-MINISTRY.columns";
    private static final List<String> ONSIS_CODES_ELEMENTARY_SCHOOL = Arrays.asList("01", "03");

    /**
     * Class members.
     */
    private DictionaryExtractor m_dictExtractor;
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
        ToolBean.setDictionaryExtractor(getDictExtractor());

        String customName = (String) getParameter(INPUT_PARAM_CUSTOM_NAME);

        String prefix = "EDIMinistryExport";

        String fileNamePrefix = StringUtils.isEmpty(customName) ? prefix : customName;
        String extension = ".txt";

        return fileNamePrefix.replaceAll("\\s", "_") + extension;
    }

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        preload();

        ToolBean.getCachedToolBeans(OnStudent.class).stream().forEach(student -> {
            OnSchool school = (OnSchool) student.getSchool(getBroker());
            String dateStr = "";
            PlainDate date = student.getDob();
            if (date != null) {
                dateStr = DATE_FORMATTER_DD_MM_YYYY_SLASHES.format(student.getDob());
            }

            grid.append();
            grid.set(FIELD_SCHOOL_BOARD_NAME,
                    !com.x2dev.utils.StringUtils.isEmpty(school.getBoardName()) ? school.getBoardName().toUpperCase()
                            : "");
            grid.set(FIELD_SCHOOL_BOARD_BSID, school.getBoardBsid());
            grid.set(FIELD_STUDENT_EDI_ID, student.getEdiID());
            grid.set(FIELD_STUDENT_OEN, student.getOen());
            grid.set(FIELD_STUDENT_LEGAL_LAST_NAME,
                    !com.x2dev.utils.StringUtils.isEmpty(student.getLegalLastName())
                            ? student.getLegalLastName().toUpperCase()
                            : "");
            grid.set(FIELD_STUDENT_LEGAL_FIRST_NAME,
                    !com.x2dev.utils.StringUtils.isEmpty(student.getLegalFirstName())
                            ? student.getLegalFirstName().toUpperCase()
                            : "");
            grid.set(FIELD_STUDENT_BIRTHDATE, dateStr);
        });
        grid.sort(Arrays.asList(FIELD_SCHOOL_BOARD_NAME, FIELD_STUDENT_LEGAL_LAST_NAME, FIELD_STUDENT_LEGAL_FIRST_NAME),
                true);
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columns = new LinkedList();
        columns.add(FIELD_SCHOOL_BOARD_NAME);
        columns.add(FIELD_SCHOOL_BOARD_BSID);
        columns.add(FIELD_STUDENT_EDI_ID);
        columns.add(FIELD_STUDENT_OEN);
        columns.add(FIELD_STUDENT_LEGAL_LAST_NAME);
        columns.add(FIELD_STUDENT_LEGAL_FIRST_NAME);
        columns.add(FIELD_STUDENT_BIRTHDATE);
        return columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columns = new LinkedList();
        LocalizationMessageResources messageResource = getMessageResource();
        String columnsText = messageResource.getMessage(MSG_KEY_COLUMNS);

        for (String column : columnsText.split("\\|")) {
            columns.add(column);
        }
        return columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
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

        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnEnrollment.class);

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

        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(ToolBean.DistrictManager.getOrganization(getBroker()),
                        ToolStudent.FIELD_ENROLLMENT_STATUS.resolve(getDictExtractor())));
        criteria.addIn(ToolStudent.FIELD_SCHOOL_OID.resolve(getDictExtractor()), schools.getKeySet());
        criteria.addNotEmpty(OnStudent.FIELD_EDI_ID.resolve(getDictExtractor()), getBroker().getPersistenceKey());

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
            List<ToolEnrollment> enrollments = student
                    .getEnrollments(getBroker()).stream().filter(enr -> DateUtils.isBetween(enr.getEnrollmentDate(),
                            getCurrentContext().getStartDate(), getCurrentContext().getEndDate())
                            && "Active".equalsIgnoreCase(enr.getStatusCode()))
                    .collect(Collectors.toList());
            return !enrollments.isEmpty();
        });
    }
}
