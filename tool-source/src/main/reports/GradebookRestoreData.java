/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.beans.path.BeanColumnPath;
import com.follett.fsc.core.k12.beans.path.BeanTablePath;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolBroker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.model.business.gradebook.restore.SisGradebookRestoreManager;
import com.x2dev.sis.model.business.gradebook.restore.SisGradebookRestoreManager.GradebookRestoreCache;
import com.x2dev.sis.web.gradebook.ConfigStudentField;
import com.x2dev.sis.web.gradebook.ConfigStudentFieldHelper;
import com.x2dev.sis.web.gradebook.GradeInputUtils;
import com.x2dev.sis.web.gradebook.LimitedColumnScoreGrid;
import com.x2dev.sis.web.gradebook.ScoreGridDisplayMode;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.MapUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.*;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the "Gradebook Restore Preview" report. This report must be run from a grade
 * input page
 * (i.e., a page where the current node is a LimitedColumnScoreGrid instance). It can be run for
 * only one section at a time. It displays the columns that would appear on the grade input
 * page if a restore was done for the input date.
 * <p>
 * Because the current grade input page may display more columns than can fit on a single page,
 * additional pages are added if necessary to display all columns. To accomplish this, the gradebook
 * sheet format is included as a subreport. The main data source is a grid, each row of which
 * represents a "page" of the report.
 *
 * @author X2 Development Corporation
 */
public class GradebookRestoreData extends ReportJavaSourceNet {

    private static final long serialVersionUID = 1L;

    private static final String COL_ASSIGNMENT = "assignment";
    private static final String COL_BEAN = "bean";
    private static final String COL_COLOR_MAP = "colorMap";
    private static final String COL_COLUMN_HEADER_MAP = "columnHeaderMap";
    private static final String COL_COLUMN_OID_MAP = "columnOidMap";
    private static final String COL_COLUMN_MAP = "columnMap";
    private static final String COL_DATASOURCE = "datasource";
    private static final String COL_FORMAT = "format";
    private static final String COL_ICON_MODE = "iconMode";
    private static final String COL_SPECIAL_CODE_MAP = "specialCodeMap";
    private static final String COL_STANDARDS_MODE = "standardsMode";
    private static final String COL_STAFF_VIEW = "staffView";
    private static final String COL_STUDENT_NAME = "studentName";
    private static final String COL_SUBMISSIONS_GOOGLE = "submissionsGoogle";
    private static final String COL_SUBMISSIONS_LATE = "submissionsLate";
    private static final String COL_SUBMISSIONS_SET = "submissionsSet";
    private static final String COL_VIEW = "view";

    private static final int MAX_PORTRAIT_COLUMNS = 8;
    private static final int MAX_LANDSCAPE_COLUMNS = 13;
    // private static final String LANDSCAPE_ID = "L";
    private static final String PARAM_ALTERNATE_DISPLAY_PARAM = "alternateDisplay";
    private static final String PARAM_HIDE_NAME_PARAM = "hideStudentNames";
    private static final String PARAM_MODE = "iconMode";
    private static final String PARAM_MODE_DROPPED = "1";
    private static final String PARAM_MODE_REMARK = "2";
    private static final String PARAM_MODE_FOOTNOTES = "3";
    private static final String PARAM_MODE_SUBMISSIONS = "4";
    private static final String PARAM_ORIENTATION_PARAM = "orientation";
    private static final String PARAM_PREVIEW_DATE = "previewDate";
    private static final String PARAM_PREVIEW_TIME = "previewTime";
    private static final String PARAMETER_DICTIONARY = "dictionary";
    private static final String PORTRAIT_ID = "P";
    private static final String REPORT_ID_PREFIX = "SYS-GBK-010-";
    private static final String SORT_FIELD = "sortField";
    private static final String SUBREPORT_ID_PREFIX = "SYS-GBK-010-SUB-";

    private int m_alternateDisplay;
    private List<GradebookColumnDefinition> m_columns;
    private LimitedColumnScoreGrid m_grid = null;
    private boolean m_isStaffView = false;
    private boolean m_showPointsInHeader;
    private String m_staffOid;
    private ArrayList<String> m_submissionsLate;
    private Map<String, String> m_submissionsMap;
    private Map<String, String> m_submissionsGoogleDrive;
    private Set<String> m_submissionsSet;
    private Map<String, String> m_submissionsTimes;

    /**
     * Gather data.
     *
     * @return JRDataSource
     *
     * @throws CloneNotSupportedException
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws CloneNotSupportedException {
        boolean hideNames = ((Boolean) getParameter(PARAM_HIDE_NAME_PARAM)).booleanValue();
        if (hideNames) {
            resortGrid();
        }
        // Roll back to the input date
        PlainDate previewDate = (PlainDate) getParameter(PARAM_PREVIEW_DATE);
        PlainTime previewTime = (PlainTime) getParameter(PARAM_PREVIEW_TIME);
        SisGradebookRestoreManager manager =
                new SisGradebookRestoreManager(previewDate, previewTime, (ToolBroker) getBroker());
        GradebookRestoreCache cache = manager.applyToScoreGrid(m_grid, m_staffOid);

        boolean hasStandardsScore = hasStandardsScore();
        m_columns = new LinkedList(m_grid.getColumnDefinitions());

        ReportDataGrid pages = new ReportDataGrid();
        byte[] subreportFormat = getSubreportFormat();
        int columnCount = m_columns.size();
        int maxColumnsPerPage =
                PORTRAIT_ID.equals(getParameter(PARAM_ORIENTATION_PARAM)) ? MAX_PORTRAIT_COLUMNS
                        : MAX_LANDSCAPE_COLUMNS;
        int pageCount = (int) Math.ceil((double) columnCount / maxColumnsPerPage);

        setFormatId(REPORT_ID_PREFIX + getParameter(PARAM_ORIENTATION_PARAM));



        // For each page (aka subreport), load the necessary data maps
        for (int i = 0; i < pageCount; i++) {
            HashMap<String, String> columnMap = new HashMap<String, String>(columnCount);
            HashMap<String, String> columnHeaderMap = new HashMap<String, String>(columnCount);
            HashMap<String, String> columnOidMap = new HashMap<String, String>(columnCount);

            loadColumnMaps(columnMap, columnHeaderMap, columnOidMap, i, maxColumnsPerPage, columnCount,
                    hasStandardsScore);

            HashMap colorMap = new HashMap<String, String>();
            HashMap specialCodeMap = new HashMap<String, String>();

            loadIcons(colorMap, specialCodeMap, i, maxColumnsPerPage, columnCount);

            m_grid.beforeTop();

            pages.append();
            pages.set(COL_FORMAT, subreportFormat);
            pages.set(COL_DATASOURCE, m_grid);
            pages.set(COL_COLUMN_HEADER_MAP, columnHeaderMap);
            pages.set(COL_COLUMN_MAP, columnMap);
            pages.set(COL_COLUMN_OID_MAP, columnOidMap);
            pages.set(COL_COLOR_MAP, colorMap);
            pages.set(COL_ICON_MODE, getParameter(PARAM_MODE));
            pages.set(COL_SPECIAL_CODE_MAP, specialCodeMap);
            pages.set(COL_STANDARDS_MODE, hasStandardsScore);
            pages.set(COL_STAFF_VIEW, m_isStaffView);
            pages.set(COL_SUBMISSIONS_GOOGLE, m_submissionsGoogleDrive);
            pages.set(COL_SUBMISSIONS_LATE, m_submissionsLate);
            pages.set(COL_SUBMISSIONS_SET, m_submissionsSet);
        }

        /*
         * Load audit list grids
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        addParameter(PARAMETER_DICTIONARY, dictionary);

        addGridGradebookPreference(cache, pages);
        addGridGradebookStudentInformation(cache, pages);
        addGridGradebookRemark(cache, pages);
        addGridGradeScale(cache, pages);

        manager.destroy();

        pages.beforeTop();

        return pages;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_alternateDisplay = getParameter(PARAM_ALTERNATE_DISPLAY_PARAM) == null ? 0
                : ((Integer) getParameter(PARAM_ALTERNATE_DISPLAY_PARAM)).intValue();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        LimitedColumnScoreGrid currentGrid = (LimitedColumnScoreGrid) userData.getCurrentGrid();

        try {
            m_grid = currentGrid.clone();
            m_grid.beforeTop();
        } catch (CloneNotSupportedException e) {
            throw new X2BaseException(e);
        }

        m_showPointsInHeader = Boolean.parseBoolean(userData.getPreferenceValue(
                SisPreferenceConstants.GRADEBOOK_SHOW_POINTS_IN_HEADER));

        m_staffOid = userData.getStaffOid();
        if (StringUtils.isEmpty(m_staffOid)) {
            m_staffOid = m_grid.getSection().getPrimaryStaffOid();
        }

        m_isStaffView = userData.getApplicationContext().isStaffView();

        // Submission info
        m_submissionsGoogleDrive = new HashMap<String, String>(32);
        m_submissionsLate = new ArrayList<String>();
        m_submissionsMap = new HashMap<String, String>(32);
        m_submissionsSet = new HashSet<String>(32);
        m_submissionsTimes = new HashMap<>(32);

        Collection<GradebookColumnDefinition> gradebookColumnDefinitions =
                userData.getCurrentNode().getId().endsWith(".input") ? m_grid.getColumnDefinitions()
                        : m_grid.getColumnDefinitionsSingleView();

        // Load submission data while we have access to userData
        GradebookManager.loadAssignmentSubmissions(gradebookColumnDefinitions, null, m_submissionsMap,
                m_submissionsGoogleDrive, m_submissionsSet, m_submissionsTimes, m_submissionsLate, userData,
                getBroker());
    }

    /**
     * Go through cache looking for GradebookPreference beans restored that don't exist in the
     * database.
     * Adds them to the passed list of beans.
     *
     * @param beans
     * @param restoreCache
     */
    private void addCacheGradebookPreference(List<GradebookPreference> beans, GradebookRestoreCache restoreCache) {
        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradebookPreference.class);
        for (X2BaseBean bean : cache.values()) {
            if (bean != null) {
                GradebookPreference pref = (GradebookPreference) bean;

                /*
                 * If not in the list and for our section, add it in the correct sorted position,
                 * which is by COL_KEY
                 */
                if (!beans.contains(pref) && pref.getMasterScheduleOid().equals(m_grid.getSection().getOid())) {
                    boolean found = false;
                    int index = -1;
                    for (GradebookPreference gpf : beans) {
                        index++;
                        if (gpf.getKey().compareTo(pref.getKey()) < 0) {
                            continue;
                        }

                        beans.add(index, pref);
                        found = true;
                        break;
                    }

                    // If it should be at the end, just add it
                    if (!found) {
                        beans.add(pref);
                    }
                }
            }
        }
    }

    /**
     * Go through cache looking for GradebookStudentInformation beans restored that don't exist in
     * the database.
     *
     * @param beans
     * @param restoreCache
     */
    private void addCacheGradebookStudentInformation(LinkedHashMap<GradebookStudentInformation, String> beans,
                                                     GradebookRestoreCache restoreCache) {
        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradebookStudentInformation.class);
        for (X2BaseBean bean : cache.values()) {
            if (bean != null) {
                GradebookStudentInformation info = (GradebookStudentInformation) bean;

                // If not in the list and for our section, add it in the correct sorted position,
                // which is by COL_KEY
                if (!beans.containsKey(info) && info.getMasterScheduleOid().equals(m_grid.getSection().getOid())) {
                    String customfields = populateCustomFields(info);
                    beans.put(info, customfields);

                    MapUtils.sortBeans(beans, GradebookStudentInformation.COL_STUDENT_OID, false, true, true);
                }
            }
        }
    }

    /**
     * Adds a page for GradebookPreferences reflected as they would exist after a restore
     * to the tool input date and time.
     *
     * @param restoreCache
     * @param pages
     */
    private void addGridGradebookPreference(GradebookRestoreCache restoreCache, ReportDataGrid pages) {
        List<GradebookPreference> beans = new LinkedList<GradebookPreference>();

        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradebookPreference.class);

        try (QueryIterator<GradebookPreference> iterator = getGradebookPreferenceQuery()) {
            while (iterator.hasNext()) {
                GradebookPreference bean = iterator.next();

                if (restoreCache.containsBean(GradebookPreference.class, bean.getOid())) {
                    bean = (GradebookPreference) cache.get(bean.getOid());
                }

                if (bean != null) {
                    beans.add(bean);
                }
            }
        }

        addCacheGradebookPreference(beans, restoreCache);

        addTableToGrid(1, beans, null, null, pages);
    }

    /**
     * Adds a page for GradebookRemark reflected as they would exist after a restore
     * to the tool input date and time.
     *
     * @param restoreCache
     * @param pages
     */
    private void addGridGradebookRemark(GradebookRestoreCache restoreCache, ReportDataGrid pages) {
        LinkedHashMap<GradebookRemark, List<String>> beans = new LinkedHashMap<GradebookRemark, List<String>>();

        try (ReportQueryIterator iterator = getGradebookRemarkQuery()) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String remarkOid = (String) row[0];
                String student = (String) row[1];
                String assignment = (String) row[2];

                addRemark(remarkOid, student, assignment, beans, restoreCache);

                if (m_isStaffView) {
                    // Teacher private remark
                    remarkOid = (String) row[3];
                    addRemark(remarkOid, student, assignment, beans, restoreCache);
                }
            }
        }

        // Add remarks that may only be in the cache
        addGradebookGridRemarkCache(beans, restoreCache);

        // Order by the remark text
        beans = MapUtils.sortBeans(beans, GradebookRemark.COL_TEXT, false, true, true);

        addTableToGrid(3, null, null, beans, pages);
    }

    /**
     * Adds remarks to the passed bean map which may only exist in the cache.
     *
     * @param beans
     * @param restoreCache
     */
    private void addGradebookGridRemarkCache(LinkedHashMap<GradebookRemark, List<String>> beans,
                                             GradebookRestoreCache restoreCache) {
        // Go through cache looking for scores restored that don't exist in the database
        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradebookScore.class);
        for (X2BaseBean bean : cache.values()) {
            if (bean != null) {
                GradebookScore score = (GradebookScore) bean;

                // Grab the related info for the score
                Student cacheStudent = restoreCache.getBean(Student.class, score.getStudentOid());
                GradebookColumnDefinition cacheAssignment =
                        restoreCache.getBean(GradebookColumnDefinition.class, score.getColumnDefinitionOid());

                String student = cacheStudent != null ? cacheStudent.getNameView() : "";
                String assignment = cacheAssignment != null ? cacheAssignment.getColumnName() : "";

                addRemark(score.getGradebookRemarkOid(), student, assignment, beans, restoreCache);

                if (m_isStaffView) {
                    addRemark(score.getGradebookPrivateRemarkOid(), student, assignment, beans, restoreCache);
                }
            }
        }
    }

    /**
     * Returns information values of the student name and assignment for the passed remark.
     * <p>
     * Looks up all data through the cache to assure that rolled back data is reflected.
     *
     * @param remarkOid
     * @param student
     * @param assignment
     * @param restoreCache
     *
     * @return List<String>
     */
    private List<String> addGridGradebookRemarkInfo(String remarkOid,
                                                    String student,
                                                    String assignment,
                                                    GradebookRestoreCache restoreCache) {
        List<String> studentAndAssignmentName = new ArrayList<String>();

        if (StringUtils.isEmpty(remarkOid)) {
            return studentAndAssignmentName;
        }

        // Go through cache looking for a score for the passed remark
        GradebookScore score = null;
        for (X2BaseBean cacheBean : restoreCache.getBeanCache(GradebookScore.class).values()) {
            if (cacheBean != null) {
                GradebookScore cacheScore = (GradebookScore) cacheBean;
                if (remarkOid.equals(cacheScore.getGradebookRemarkOid()) ||
                        (m_isStaffView && remarkOid.equals(cacheScore.getGradebookRemarkOid()))) {
                    score = cacheScore;
                    break;
                }
            }
        }

        /*
         * If a cached score was found, use it to get the view fields, else use the passed values.
         */
        if (score != null) {
            Student cacheStudent = restoreCache.getBean(Student.class, score.getStudentOid());
            GradebookColumnDefinition cacheAssignment =
                    restoreCache.getBean(GradebookColumnDefinition.class, score.getColumnDefinitionOid());

            studentAndAssignmentName.add(cacheStudent != null ? cacheStudent.getNameView() : "");
            studentAndAssignmentName.add(cacheAssignment != null ? cacheAssignment.getColumnName() : "");
        } else {
            studentAndAssignmentName.add(student);
            studentAndAssignmentName.add(assignment);
        }

        return studentAndAssignmentName;
    }


    /**
     * Adds a page for GradebookStudentInformation reflected as they would exist after a restore
     * to the tool input date and time.
     *
     * @param restoreCache
     * @param pages
     */
    private void addGridGradebookStudentInformation(GradebookRestoreCache restoreCache, ReportDataGrid pages) {
        LinkedHashMap<GradebookStudentInformation, String> beans =
                new LinkedHashMap<GradebookStudentInformation, String>();

        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradebookStudentInformation.class);

        try (QueryIterator<GradebookStudentInformation> iterator = getGradebookStudentInformationQuery()) {
            while (iterator.hasNext()) {
                GradebookStudentInformation bean = iterator.next();

                if (restoreCache.containsBean(GradebookStudentInformation.class, bean.getOid())) {
                    bean = (GradebookStudentInformation) cache.get(bean.getOid());
                }

                if (bean != null) {
                    String customfields = populateCustomFields(bean);
                    beans.put(bean, customfields);
                }
            }
        }

        addCacheGradebookStudentInformation(beans, restoreCache);

        addTableToGrid(2, null, beans, null, pages);
    }

    /**
     * Adds a page for GradeScale reflected as they would exist after a restore
     * to the tool input date and time.
     *
     * @param restoreCache
     * @param pages
     */
    private void addGridGradeScale(GradebookRestoreCache restoreCache, ReportDataGrid pages) {
        LinkedHashMap<GradeScale, String> beans = new LinkedHashMap<GradeScale, String>();

        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradeScale.class);

        try (QueryIterator<GradeScale> iterator = getGradeScaleQuery()) {
            while (iterator.hasNext()) {
                GradeScale bean = iterator.next();

                if (restoreCache.containsBean(GradeScale.class, bean.getOid())) {
                    bean = (GradeScale) cache.get(bean.getOid());
                }

                if (bean != null) {
                    beans.put(bean, addGridGradeScaleGrades(bean, restoreCache));
                }
            }
        }

        // Go through cache looking for beans restored that don't exist in the database
        for (X2BaseBean bean : cache.values()) {
            if (bean != null) {
                GradeScale scale = (GradeScale) bean;

                // If not in the list, add it
                if (!beans.containsKey(scale)) {
                    beans.put(scale, addGridGradeScaleGrades(scale, restoreCache));
                }
            }
        }

        addTableToGrid(4, null, beans, null, pages);
    }

    /**
     * Return a view of the GradeScaleGradeDefinitions for the passed scale.
     *
     * @param bean
     * @param restoreCache
     *
     * @return String
     */
    private String addGridGradeScaleGrades(GradeScale bean, GradebookRestoreCache restoreCache) {
        List<GradeScaleGradeDefinition> gsgs = new ArrayList<GradeScaleGradeDefinition>();

        for (GradeScaleGradeDefinition gsg : bean.getGradeScaleDefinitions(getBroker())) {
            if (restoreCache.containsBean(GradeScaleGradeDefinition.class, gsg.getOid())) {
                gsg = (GradeScaleGradeDefinition) restoreCache.getBean(GradeScaleGradeDefinition.class,
                        gsg.getOid());
            }

            if (gsg != null) {
                gsgs.add(gsg);
            }
        }

        // Go through cache looking for beans restored that don't exist in the database
        for (X2BaseBean cacheBean : restoreCache.getBeanCache(GradeScaleGradeDefinition.class).values()) {
            if (cacheBean != null) {
                GradeScaleGradeDefinition gsg = (GradeScaleGradeDefinition) cacheBean;

                // If not in the list, add it
                if (!gsgs.contains(gsg)) {
                    gsgs.add(gsg);
                }
            }
        }

        gsgs = CollectionUtils.sortBeans(gsgs, GradeScaleGradeDefinition.COL_GRADE_CODE, false, true);

        Collection<String> codes =
                CollectionUtils.getPropertyCollection(gsgs, GradeScaleGradeDefinition.COL_GRADE_CODE);

        return StringUtils.convertCollectionToDelimitedString(codes, ", ");
    }

    /**
     * Adds the remark for the passed oid to the passed bean map.
     * <p>
     * Looks up all data through the cache to assure that rolled back data is reflected.
     *
     * @param remarkOid
     * @param student
     * @param assignment
     * @param beans
     * @param restoreCache
     */
    private void addRemark(String remarkOid,
                           String student,
                           String assignment,
                           LinkedHashMap<GradebookRemark, List<String>> beans,
                           GradebookRestoreCache restoreCache) {

        // Nothing to do if the remark OID is empty
        if (StringUtils.isEmpty(remarkOid)) {
            return;
        }

        // Nothing to do if remark is already in the map
        if (CollectionUtils.contains(beans.keySet(), X2BaseBean.COL_OID, remarkOid)) {
            return;
        }

        Map<String, X2BaseBean> cache = restoreCache.getBeanCache(GradebookRemark.class);

        GradebookRemark bean = null;
        if (restoreCache.containsBean(GradebookRemark.class, remarkOid)) {
            bean = (GradebookRemark) cache.get(remarkOid);
        } else {
            bean = getBroker().getBeanByOid(GradebookRemark.class, remarkOid);
        }

        // Bean may be null if deleted as part of the restore, won't show those
        if (bean != null) {
            List studentAndAssignmentName = new LinkedList<String>();
            studentAndAssignmentName.add(student);
            studentAndAssignmentName.add(assignment);

            beans.put(bean, studentAndAssignmentName);
        }
    }

    /**
     * Adds the passed list of beans as a datasource on the root pages grid.
     *
     * @param format
     * @param beansList
     * @param beansMap
     * @param beansMapList
     * @param pages
     */
    private void addTableToGrid(int format,
                                List<? extends X2BaseBean> beansList,
                                Map<? extends X2BaseBean, String> beansMap,
                                Map<? extends X2BaseBean, List<String>> beansMapList,
                                ReportDataGrid pages) {
        ReportDataGrid grid = new ReportDataGrid();

        if (beansList != null) {
            for (X2BaseBean bean : beansList) {
                grid.append();
                grid.set(COL_BEAN, bean);
            }
        } else if (beansMap != null) {
            for (Map.Entry<? extends X2BaseBean, String> entry : beansMap.entrySet()) {
                grid.append();
                grid.set(COL_BEAN, entry.getKey());
                grid.set(COL_VIEW, entry.getValue());
            }
        } else if (beansMapList != null) {
            for (X2BaseBean entry : beansMapList.keySet()) {
                grid.append();
                grid.set(COL_BEAN, entry);
                Object[] valueArray = beansMapList.get(entry).toArray();
                grid.set(COL_STUDENT_NAME, valueArray[0]);
                grid.set(COL_ASSIGNMENT, valueArray[1]);
            }
        }

        grid.beforeTop();

        pages.append();
        pages.set(COL_FORMAT, getSubreportTableFormat(format));
        pages.set(COL_DATASOURCE, grid);
        pages.set(COL_COLUMN_HEADER_MAP, new HashMap(0));
        pages.set(COL_COLUMN_MAP, new HashMap(0));
        pages.set(COL_COLUMN_OID_MAP, new HashMap(0));
        pages.set(COL_COLOR_MAP, new HashMap(0));
        pages.set(COL_ICON_MODE, getParameter(PARAM_MODE));
        pages.set(COL_SPECIAL_CODE_MAP, new HashMap(0));
        pages.set(COL_STANDARDS_MODE, false);
        pages.set(COL_STAFF_VIEW, m_isStaffView);
        pages.set(COL_SUBMISSIONS_GOOGLE, m_submissionsGoogleDrive);
        pages.set(COL_SUBMISSIONS_LATE, m_submissionsLate);
        pages.set(COL_SUBMISSIONS_SET, m_submissionsSet);
    }

    /**
     * Returns the query to use to retrieve GradebookPreferences belonging to the grid's section.
     *
     * @return QueryIterator<GradebookPreference>
     */
    private QueryIterator<GradebookPreference> getGradebookPreferenceQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(GradebookPreference.COL_MASTER_SCHEDULE_OID, m_grid.getSection().getOid());

        QueryByCriteria query = new QueryByCriteria(GradebookPreference.class, criteria);
        query.addOrderByAscending(GradebookPreference.COL_KEY);

        return getBroker().getIteratorByQuery(query);
    }

    /**
     * Retrieve the GradebookRemark OIDs (public and private) which are being used by
     * GradebookScores in the current grid's section.
     *
     * @return ReportQueryIterator
     */
    private ReportQueryIterator getGradebookRemarkQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(
                GradebookScore.REL_COLUMN_DEFINITION + "." + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID,
                m_grid.getSection().getOid());

        X2Criteria andCriteria = new X2Criteria();
        andCriteria.addNotEmpty(GradebookScore.COL_GRADEBOOK_REMARK_OID, getBroker().getPersistenceKey());

        if (m_isStaffView) {
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addNotEmpty(GradebookScore.COL_GRADEBOOK_PRIVATE_REMARK_OID, getBroker().getPersistenceKey());

            andCriteria.addOrCriteria(orCriteria);
        }

        criteria.addAndCriteria(andCriteria);

        String[] cols;
        if (m_isStaffView) {
            cols = new String[] {GradebookScore.COL_GRADEBOOK_REMARK_OID,
                    GradebookScore.REL_STUDENT + "." + Student.COL_NAME_VIEW,
                    GradebookScore.REL_COLUMN_DEFINITION + "." + GradebookColumnDefinition.COL_COLUMN_NAME,
                    GradebookScore.COL_GRADEBOOK_PRIVATE_REMARK_OID};
        } else {
            cols = new String[] {GradebookScore.COL_GRADEBOOK_REMARK_OID,
                    GradebookScore.REL_STUDENT + "." + Student.COL_NAME_VIEW,
                    GradebookScore.REL_COLUMN_DEFINITION + "." + GradebookColumnDefinition.COL_COLUMN_NAME,};
        }

        ReportQueryByCriteria query = new ReportQueryByCriteria(GradebookScore.class, cols, criteria);

        return getBroker().getReportQueryIteratorByQuery(query);
    }

    /**
     * Retrieves the GradebookStudentInformation records for the current grid's section.
     *
     * @return QueryIterator<GradebookStudentInformation>
     */
    private QueryIterator<GradebookStudentInformation> getGradebookStudentInformationQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(GradebookStudentInformation.COL_MASTER_SCHEDULE_OID, m_grid.getSection().getOid());

        QueryByCriteria query = new QueryByCriteria(GradebookStudentInformation.class, criteria);
        query.addOrderByAscending(GradebookStudentInformation.REL_STUDENT + "." + Student.COL_NAME_VIEW);
        query.setPathOuterJoin(GradebookStudentInformation.REL_STUDENT);

        return getBroker().getIteratorByQuery(query);
    }

    /**
     * Returns GradeScale's owned by the staff.
     *
     * @return QueryIterator<GradeScale>
     */
    private QueryIterator<GradeScale> getGradeScaleQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(GradeScale.COL_OWNER_TYPE, Ownable.OWNER_TYPE_STAFF);
        criteria.addEqualTo(GradeScale.COL_OWNER_OID, m_staffOid);

        QueryByCriteria query = new QueryByCriteria(GradeScale.class, criteria);
        query.addOrderByAscending(GradeScale.COL_GRADE_SCALE_NAME);

        return getBroker().getIteratorByQuery(query);
    }

    /**
     * Returns the compiled subreport format.
     *
     * @return byte[]
     */
    private byte[] getSubreportFormat() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, SUBREPORT_ID_PREFIX + getParameter(PARAM_ORIENTATION_PARAM));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        Report subreport = (Report) getBroker().getBeanByQuery(query);
        return subreport.getCompiledFormat();
    }

    /**
     * Returns the compiled subreport format.
     *
     * @return byte[]
     */
    private byte[] getSubreportTableFormat(int tableIndex) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, SUBREPORT_ID_PREFIX + getParameter(PARAM_ORIENTATION_PARAM) + tableIndex);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        Report subreport = (Report) getBroker().getBeanByQuery(query);
        return subreport.getCompiledFormat();
    }

    /**
     * Returns true if the report should pull ReportingStandardScore from the cell instead of the
     * GradebookScore for display.
     *
     * @return boolean
     */
    private boolean hasStandardsScore() {
        boolean hasStandardsScore = false;

        // Determine if in standards mode
        boolean hasReportingStandards = m_grid.hasReportingStandards() && m_grid.getCurrentAssignmentOid() != null
                && m_grid.getSectionReportingStandards(m_grid.getCurrentAssignmentOid()) != null;
        boolean standardsMode = m_grid.getView() == LimitedColumnScoreGrid.VIEW_STANDARDS;
        String selectedStandardOid = m_grid.getSelectedStandardOid();

        if (standardsMode && !hasReportingStandards && !m_grid.isPostColumnTerm()
                && !StringUtils.isEmpty(selectedStandardOid)) {
            hasStandardsScore = true;
        }

        return hasStandardsScore;
    }

    /**
     * Loads assignment column lookup information into the passed maps for
     * the current page of the report.
     *
     * @param columnMap
     * @param columnHeaderMap
     * @param columnOidMap
     * @param i
     * @param maxColumnsPerPage
     * @param columnCount
     * @param hasStandardsScore
     */
    private void loadColumnMaps(HashMap<String, String> columnMap,
                                HashMap<String, String> columnHeaderMap,
                                HashMap<String, String> columnOidMap,
                                int i,
                                int maxColumnsPerPage,
                                int columnCount,
                                boolean hasStandardsScore) {
        int columnNumber = 0;
        int startColumn = i * maxColumnsPerPage;
        int endColumn = Math.min(startColumn + maxColumnsPerPage, columnCount);

        for (int j = startColumn; j < endColumn; j++) {
            GradebookColumnDefinition column = m_columns.get(j);

            String columnId = null;
            String columnHeader = null;
            String columnOid = column.getOid();

            if (column.getColumnTypeCode() == GradebookColumnDefinition.COLUMN_TYPE_IMPLICIT_AVERAGE) {
                columnId = column.getImplicitAverageKey();
                columnHeader = GradeInputUtils.getAverageLabel(columnId,
                        m_grid.getSchoolContext(), true, false, getBroker(), getLocale());

                if (columnId.endsWith(AverageCalculator.PORTAL_AVERAGE_CODE_SUFFIX)) {
                    String suffix = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(getLocale(), "label.gradebook.portalView");
                    columnHeader += " " + suffix;
                }
            } else if (column.getRubricCriterion() != null) {
                columnId = column.getRubricParentColumnOid() + "." + column.getRubricCriterion().getOid();
                columnHeader = column.getColumnCode();
            } else if (!(column.isPostColumn()
                    && column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_COMMENT)
                    && column.getColumnTypeCode() != GradebookColumnDefinition.COLUMN_TYPE_MISSING) {

                if (!hasStandardsScore) {
                    columnId = column.getColumnCode();
                    columnHeader = column.getColumnCode();

                    if (m_showPointsInHeader && column.getTotalPoints() != null) {
                        columnHeader += "\n" + column.getTotalPoints().toString() + " pts";
                    }
                } else {
                    columnId = column.getOid() + "_" + m_grid.getSelectedStandardOid();
                    columnHeader = column.getColumnCode();

                    GradebookColumnType type = column.getColumnType();
                    if (type != null) {
                        columnHeader += "\n" + type.getColumnType();
                    }
                }
            }

            if (columnId != null) {
                columnMap.put(Integer.toString(columnNumber), columnId);
                columnHeaderMap.put(Integer.toString(columnNumber), columnHeader);
                columnOidMap.put(Integer.toString(columnNumber), columnOid);

                columnNumber++;
            }
        }
    }

    /**
     * Loads the various icon maps.
     *
     * @param colorMap
     * @param specialCodeMap
     * @param i
     * @param maxColumnsPerPage
     * @param columnCount
     */
    private void loadIcons(HashMap colorMap, HashMap specialCodeMap, int i, int maxColumnsPerPage, int columnCount) {
        // For each student and column
        Map<String, Collection<GradebookGarnish>> garnishes = m_grid.getGradebookGarnishes();
        m_grid.beforeTop();
        while (m_grid.next()) {
            int startColumn = i * maxColumnsPerPage;
            int endColumn = Math.min(startColumn + maxColumnsPerPage, columnCount);

            for (int j = startColumn; j < endColumn; j++) {
                GradebookColumnDefinition column = m_columns.get(j);

                GradebookScore score = m_grid.getScore(column.getOid());
                if (score != null) {
                    if (score.getSpecialCodeIndicator()) {
                        GradebookSpecialCode specialCode = m_grid.getGradesManager().getSpecialCodeForScore(score);
                        if (specialCode != null) {
                            specialCodeMap.put(score.getOid(), specialCode.getColor());
                        }
                    }

                    if (m_grid.getScoreGridDisplayMode() == ScoreGridDisplayMode.COLOR_MODE) {
                        Collection<GradebookGarnish> garnishesByScale = garnishes.get(column.getGradeScaleOid());
                        GradebookGarnish garnish = m_grid.getGradesManager().getGarnish(garnishesByScale, score);
                        if (garnish != null) {
                            colorMap.put(score.getOid(), garnish.getColor());
                        }
                    }
                }
            }
        }
    }

    /**
     * Create a view field from the student data fields that were customized by the staff member.
     *
     * @param GradebookStudentInformation gst
     *
     * @return String
     */
    private String populateCustomFields(GradebookStudentInformation gst) {
        ConfigStudentFieldHelper helper = new ConfigStudentFieldHelper();

        List<ConfigStudentField> fields =
                helper.loadPreferenceData(getBroker().getPersistenceKey(),
                        gst.getMasterSchedule().getSchoolCourse().getSchool(),
                        gst.getMasterScheduleOid());
        StringBuilder stringBuilder = new StringBuilder();

        Iterator<ConfigStudentField> iterator = fields.iterator();
        while (iterator.hasNext()) {
            ConfigStudentField field = iterator.next();
            if (!field.isEnabled()) {
                iterator.remove();
            }
        }

        for (ConfigStudentField field : fields) {
            if (field.isEnabled()) {
                BeanColumnPath bcp =
                        BeanTablePath.getTable(GradebookStudentInformation.class).getColumn(field.getFieldOID());
                Object value = gst.getFieldValueByBeanPath(bcp.toString());
                if (value != null) {
                    if (stringBuilder.length() != 0) {
                        stringBuilder.append(", ");
                    }
                    stringBuilder.append(field.getLabel() + ": " + value.toString());
                }

            }
        }
        return stringBuilder.toString();
    }

    /**
     * Sorts grid by the student LASID.
     */
    private void resortGrid() {
        m_grid.beforeTop();
        while (m_grid.next()) {
            SisStudent student = m_grid.getStudent();
            m_grid.set(SORT_FIELD, m_alternateDisplay == 0 ? student.getLocalId() : student.getStateId());
        }

        m_grid.beforeTop();
        m_grid.sort(SORT_FIELD, true);
    }
}
