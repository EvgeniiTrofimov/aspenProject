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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.presentation.FieldFormatter;
import com.follett.fsc.core.k12.web.presentation.FieldFormatterFactory;
import com.x2dev.sis.model.beans.*;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;
import java.util.logging.Level;

import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;

/**
 * Data source for the Grade Post Verification report, which lists the sections for which grades
 * have not yet been posted for a given grade term.
 *
 * @author X2 Development Corporation
 */
public class GradePostVerificationData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;
    // Input parameters
    private static final String COLUMN_DEFINITION_OID_PARAM = "transcriptColumnOid";
    private static final String EXCLUDE_EMPTY_PARAM = "excludeEmpty";
    private static final String GRADE_POST_CONTROL_OID_PARAM = "gradePostControlOid";
    private static final String GRADE_TERM_OID_PARAM = "gradeTermOid";
    private static final String NON_POSTS_ONLY_PARAM = "nonPostsOnly";
    private static final String SORT_PARAM = "sort";

    // Report parameters
    private static final String GRADE_POST_CONTROL_PARAM = "gradePostControl";
    private static final String GRADE_TERM_PARAM = "gradeTerm";
    private static final String TRANSCRIPT_COLUMN_PARAM = "transcriptColumn";
    private static final String LOCALE_PARAM = "locale";
    private static final String MESSAGE_FORMATTER_PARAM = "messageFormatter";

    // Data grid field constants
    private static final String GRADE_POST_FIELD = "gradePost";
    private static final String MASTER_SCHEDULE_FIELD = "master";
    private static final String STUDENT_GRADE_COUNT_FIELD = "gradeCount";
    private static final String STUDENT_COUNT_FIELD = "studentCount";
    private static final String CLASS = "class";
    private static final String ROW_NUMBER = "rowNumber";
    public static final String GROUP_BY_CLASS = "groupByClass";
    public static final String COURSE_VIEW = "courseView";
    public static final String DESCRIPTION = "description";
    public static final String STAFF_VIEW = "staffView";
    public static final String HIDE_FROM_GRADE_INPUT = "hideFromGradeInput";
    public static final String COMBINED_GRADEBOOK = "combinedGradebook";

    /*
     * Get input parameter values
     */
    boolean nonPostsOnly;
    boolean excludeEmpty;
    String gradeTermOid;
    String gradePostControlOid;
    boolean groupByClass;
    boolean hideFromGradeInput;
    String transcriptColumnOid;
    String sortParam;
    String scheduleOid;
    GradeTerm gradeTerm;
    TranscriptColumnDefinition transcriptColumn ;
    GradePostControl postControl;
    Map<String, GradePost> postMap;
    Map gradeCounts;
    Map studentCounts;
    Map<String, Set<String>> definitionOidToTermOids;

    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        /*
         * Get input parameter values
         */
        this.nonPostsOnly = ((Boolean) getParameter(NON_POSTS_ONLY_PARAM)).booleanValue();
        this.excludeEmpty = ((Boolean) getParameter(EXCLUDE_EMPTY_PARAM)).booleanValue();
        this.gradeTermOid = (String) getParameter(GRADE_TERM_OID_PARAM);
        this.gradePostControlOid = (String) getParameter(GRADE_POST_CONTROL_OID_PARAM);
        this.groupByClass = ((Boolean) getParameter(GROUP_BY_CLASS)).booleanValue();
        this.transcriptColumnOid = (String) getParameter(COLUMN_DEFINITION_OID_PARAM);
        this.hideFromGradeInput = (Boolean) getParameter(HIDE_FROM_GRADE_INPUT);

        // Preload data
        this.scheduleOid = ((SisSchool) getSchool()).getActiveScheduleOid();
        this.gradeTerm = getBroker().getBeanByOid(GradeTerm.class, gradeTermOid);
        this.transcriptColumn = getBroker().getBeanByOid(TranscriptColumnDefinition.class,
                transcriptColumnOid);
        this.postControl = getBroker().getBeanByOid(GradePostControl.class, gradePostControlOid);
        this.postMap = getPostMap(gradeTermOid, gradePostControlOid, scheduleOid);
        this.gradeCounts = getGradeCounts(transcriptColumn, postControl);
        this.sortParam = (String) getParameter(SORT_PARAM);
        this.studentCounts = getStudentCounts(postControl);
        this.definitionOidToTermOids = getDefinitionOidToTermOids();

        // Add report parameters
        FieldFormatter messageFormatter = FieldFormatterFactory.createFieldFormatter(
                new ModelProperty(GradePostControl.class.getName(),
                        GradePostControl.COL_SUMMARY, getBroker().getPersistenceKey()),
                null);

        addParameter(GRADE_POST_CONTROL_PARAM, postControl);
        addParameter(GRADE_TERM_PARAM, gradeTerm);
        addParameter(TRANSCRIPT_COLUMN_PARAM, transcriptColumn);
        addParameter(MESSAGE_FORMATTER_PARAM, messageFormatter);
        addParameter(LOCALE_PARAM, getLocale());
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid mainGrid = new ReportDataGrid(2000, 10);
        List<String> processedMasterScheduleOids = new ArrayList<>();

        X2Criteria baseCriteria = buildBaseCriteria(this.nonPostsOnly, this.excludeEmpty, this.gradeTermOid,
                this.gradePostControlOid, this.scheduleOid, this.postControl);

        X2Criteria classCombinedGradebookCriteria = new X2Criteria();
        classCombinedGradebookCriteria.addEqualTo(MasterSchedule.REL_SECTION_CLASS + PATH_DELIMITER +
                ScheduleClass.COL_GRADEBOOK_COMBINED_INDICATOR, BooleanAsStringConverter.TRUE);
        classCombinedGradebookCriteria.addAndCriteria(baseCriteria);
        BeanQuery classCombinedGradebookQuery = new BeanQuery(MasterSchedule.class, classCombinedGradebookCriteria);
        addClassSectionsToGrid(mainGrid, processedMasterScheduleOids, classCombinedGradebookQuery, true,
                true);

        X2Criteria classNotCombinedGradebookCriteria = new X2Criteria();
        classNotCombinedGradebookCriteria.addEqualTo(MasterSchedule.REL_SECTION_CLASS + PATH_DELIMITER +
                ScheduleClass.COL_GRADEBOOK_COMBINED_INDICATOR, BooleanAsStringConverter.FALSE);
        classNotCombinedGradebookCriteria.addAndCriteria(baseCriteria);
        BeanQuery classNotCombinedGradebookQuery = new BeanQuery(MasterSchedule.class, classNotCombinedGradebookCriteria);
        addClassSectionsToGrid(mainGrid, processedMasterScheduleOids, classNotCombinedGradebookQuery, true,
                false);

        X2Criteria standardSectionsCriteria = new X2Criteria();
        standardSectionsCriteria.addNotIn(X2BaseBean.COL_OID, processedMasterScheduleOids);
        standardSectionsCriteria.addAndCriteria(baseCriteria);
        BeanQuery standardSectionsQuery = new BeanQuery(MasterSchedule.class, standardSectionsCriteria);
        addIndividualSectionsToGrid(mainGrid, standardSectionsQuery, processedMasterScheduleOids,
                false,false);

        sortGrid(mainGrid);

        mainGrid.beforeTop();
        return mainGrid;
    }

    private void addClassSectionsToGrid(ReportDataGrid mainGrid,
                                        List<String> processedMasterScheduleOids,
                                        BeanQuery classQuery,
                                        boolean isScheduleClass,
                                        boolean isGradebookCombined) {
        String[] groupByColumns = {MasterSchedule.COL_SECTION_CLASS_OID,
                MasterSchedule.REL_SECTION_CLASS + PATH_DELIMITER + ScheduleClass.COL_PRIMARY_SECTION_OID};
        int[] initialSizes = {1, 2};
        Map<String, Map<String, List<MasterSchedule>>> combinedGradebooksByPrimarySectionOid =
                getBroker().getGroupedCollectionByQuery(classQuery, groupByColumns, initialSizes);

        for (Map.Entry<String, Map<String, List<MasterSchedule>>> sectionClassEntry :
                combinedGradebooksByPrimarySectionOid.entrySet()) {
            String sectionClassOid = sectionClassEntry.getKey();
            for (Map.Entry<String, List<MasterSchedule>> primarySectionEntry : sectionClassEntry.getValue().entrySet()) {
                String primarySectionOid = primarySectionEntry.getKey();

                GradePost post = this.postMap.get(primarySectionOid);
                if (this.nonPostsOnly && isGradebookCombined && post != null) {
                    // These oids should not be included in the other queries
                    for (MasterSchedule section : primarySectionEntry.getValue()) {
                        processedMasterScheduleOids.add(section.getOid());
                    }
                    continue;
                }

                for (MasterSchedule section : primarySectionEntry.getValue()) {
                    addToGrid(mainGrid, processedMasterScheduleOids, section, isScheduleClass, isGradebookCombined,
                            section.getOid().equals(primarySectionOid));
                }
            }
        }
    }

    /**
     * Since the gatherData() method relies on 3 separate queries. it is impossible to sort and group the data until the
     * end. This method takes care of sorting uses the values from the input definition.
     * @param mainGrid
     */
    private void sortGrid(ReportDataGrid mainGrid) {
        List<String> sortList = new LinkedList<>();
        List<Boolean> ascList = new LinkedList<>();

        if (this.groupByClass) {
            sortList.add(CLASS);
            ascList.add(Boolean.TRUE);
        }

        for (String sortCol : Arrays.asList(this.sortParam.split(","))) {
            sortList.add(sortCol.trim());
            ascList.add(Boolean.TRUE);
        }

        mainGrid.sort(sortList, ascList, false);
    }

    private void sortGridByClass(ReportDataGrid mainGrid) {
        String[] sortColumns = {CLASS, ROW_NUMBER};
        mainGrid.sort(Arrays.asList(sortColumns), false);
    }

    private void addIndividualSectionsToGrid(ReportDataGrid mainGrid, BeanQuery query, List<String> masterScheduleOids,
                                             boolean isScheduleClass, boolean isGradebookCombined) {
        /*
         * Iterate over the sections, populating the data grid
         */
        QueryIterator sections = getBroker().getIteratorByQuery(query);
        try {
            while (sections.hasNext()) {
                MasterSchedule section = (MasterSchedule) sections.next();
                addToGrid(mainGrid, masterScheduleOids,  section, isScheduleClass, isGradebookCombined,
                        false);
            }
        } finally {
            sections.close();
        }
    }

    private void addToGrid(ReportDataGrid mainGrid,
                           List<String> masterScheduleOids,
                           MasterSchedule section,
                           boolean isScheduleClass,
                           boolean isGradebookCombined,
                           boolean isPrimarySection) {
        String sectionOid = section.getOid();
        masterScheduleOids.add(sectionOid);

        ScheduleTerm scheduleTerm = section.getScheduleTerm();
        String transcriptDefinitionOid = section.getSchoolCourse().getTranscriptDefinitionOid();

        Set<String> availableGradeTermOids = this.definitionOidToTermOids.get(transcriptDefinitionOid);
        if (scheduleTerm != null && scheduleTerm.coversGradeTerm(this.gradeTerm)
                && availableGradeTermOids.contains(this.gradeTermOid)) {
            GradePost post = this.postMap.get(sectionOid);
            Integer gradeCount = (Integer) this.gradeCounts.get(sectionOid);
            Integer studentCount = (Integer) this.studentCounts.get(sectionOid);

            mainGrid.append();

            String courseView = section.getCourseView();
            if (isGradebookCombined && isPrimarySection) {
                if (!StringUtils.isBlank(courseView)) {
                    courseView += "*";
                } else {
                    courseView = "*";
                }
            }

            mainGrid.set(COURSE_VIEW, courseView);
            mainGrid.set(DESCRIPTION, section.getDescription());
            mainGrid.set(STAFF_VIEW, section.getStaffView());
            mainGrid.set(GRADE_POST_FIELD, post);
            mainGrid.set(STUDENT_GRADE_COUNT_FIELD, gradeCount);
            mainGrid.set(STUDENT_COUNT_FIELD, studentCount);

            if (this.groupByClass) {
                ScheduleClass sectionClass = section.getSectionClass();
                mainGrid.set(CLASS, buildClassName(isScheduleClass, sectionClass));
                mainGrid.set(COMBINED_GRADEBOOK, isGradebookCombined);
            } else {
                mainGrid.set(CLASS, "");
            }
        }
    }

    private static String buildClassName(boolean isScheduleClass, ScheduleClass sectionClass) {
        String displayName = "";
        if (!isScheduleClass) {
            return null;
        }

        if (sectionClass != null) {
            String sectionClassName = sectionClass.getName();
            String sectionClassId = sectionClass.getId();

            if (!StringUtils.isBlank(sectionClassId)) {
                displayName = sectionClassId;
            }

            if (!StringUtils.isBlank(sectionClassName)) {
                if (!StringUtils.isBlank(displayName)) {
                    displayName += " ";
                }
                displayName += sectionClassName;
            }
        }

        return displayName;
    }

    private X2Criteria buildBaseCriteria(boolean nonPostsOnly, boolean excludeEmpty, String gradeTermOid,
                                         String gradePostControlOid, String scheduleOid, GradePostControl postControl) {
        /*
         * Query for the master records to display; only include students applicable to the selected
         * post control
         */
        X2Criteria baseCriteria = new X2Criteria();
        baseCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, scheduleOid);
        baseCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);
        if (this.hideFromGradeInput) {
            baseCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_HIDE_GRADE_INPUT_INDICATOR, Boolean.FALSE);
        }

        if (excludeEmpty) {
            baseCriteria.addGreaterThan(MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(0));
        }

        if (nonPostsOnly) {
            Criteria postCriteria = new Criteria();
            postCriteria.addEqualTo(GradePost.COL_GRADE_POST_CONTROL_OID, gradePostControlOid);
            postCriteria.addEqualTo(GradePost.COL_GRADE_TERM_OID, gradeTermOid);

            SubQuery postQuery = new SubQuery(GradePost.class, GradePost.COL_MASTER_SCHEDULE_OID, postCriteria);

            baseCriteria.addNotIn(X2BaseBean.COL_OID, postQuery);
        }

        if (postControl.getCriteriaType() == GradePostControl.POST_TYPE_YOG) {
            Criteria studentScheduleCriteria = new Criteria();
            studentScheduleCriteria.addEqualToField(StudentSchedule.COL_SECTION_OID,
                    Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);
            studentScheduleCriteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                    postControl.getYogCollection());

            SubQuery studentScheduleSubquery = new SubQuery(StudentSchedule.class,
                    X2BaseBean.COL_OID, studentScheduleCriteria);

            baseCriteria.addExists(studentScheduleSubquery);
        }
        return baseCriteria;
    }

    /**
     * Returns a map, which contains all available transcript definition keys as ids and their
     * available grade term oids.
     *
     * @return Map<String, Set<String>>
     */
    private Map<String, Set<String>> getDefinitionOidToTermOids() {
        Map<String, Set<String>> definitionOidToTermOids = new HashMap<>();
        Query transcriptDefinitionQuery = new BeanQuery(TranscriptDefinition.class);
        try (QueryIterator iterator = getBroker().getIteratorByQuery(transcriptDefinitionQuery)) {
            while (iterator.hasNext()) {
                TranscriptDefinition definition = (TranscriptDefinition) iterator.next();
                if (definition.getGradeTermDefinition() == null
                        || definition.getGradeTermDefinition().getGradeTerms() == null) {
                    continue;
                }

                Set<String> gradeTermOids = new HashSet<>();
                for (GradeTerm definitionGradeTerm : definition.getGradeTermDefinition().getGradeTerms()) {
                    gradeTermOids.add(definitionGradeTerm.getOid());
                }
                definitionOidToTermOids.put(definition.getOid(), gradeTermOids);
            }
        }
        return definitionOidToTermOids;
    }

    /**
     * Returns a map containing the number of students with grades keyed on master OID.
     *
     * @param transcriptColumn TranscriptColumnDefinition
     * @param postControl GradePostControl
     * @return Map of Integer objects
     */
    private Map getGradeCounts(TranscriptColumnDefinition transcriptColumn, GradePostControl postControl) {
        HashMap gradeCounts = new HashMap(1500);

        if (transcriptColumn != null) {
            StringBuilder countSql = new StringBuilder(200);
            countSql.append("SELECT GCD_MST_OID, COUNT(*) ");
            countSql.append("FROM GRADEBOOK_SCORE ");
            countSql.append("INNER JOIN GRADEBOOK_COLUMN_DEFINITION ");
            countSql.append("ON GSC_GCD_OID = GCD_OID ");
            countSql.append("INNER JOIN SCHEDULE_MASTER ");
            countSql.append("ON GCD_MST_OID = MST_OID ");

            /*
             * If the post control applies only to students of particular YOGs, count grades for
             * those YOGs only. To do this we must join to the student table.
             */
            if (postControl.getCriteriaType() == GradePostControl.POST_TYPE_YOG) {
                countSql.append("INNER JOIN STUDENT ");
                countSql.append("ON GSC_STD_OID = STD_OID ");
            }

            countSql.append("WHERE GCD_GTC_OID = '" + transcriptColumn.getOid() + "' ");
            countSql.append("AND MST_SCH_OID = '" + ((SisSchool) getSchool()).getActiveScheduleOid() + "' ");

            if (postControl.getCriteriaType() == GradePostControl.POST_TYPE_YOG) {
                countSql.append("AND STD_YOG IN (");
                countSql.append(postControl.getCriteria());
                countSql.append(") ");
            }

            countSql.append("GROUP BY GCD_MST_OID");

            Connection connection = getBroker().borrowConnection();
            Statement statement = null;
            ResultSet countResults = null;

            try {
                statement = connection.createStatement();
                countResults = statement.executeQuery(countSql.toString());

                while (countResults.next()) {
                    String masterOid = countResults.getString(1);
                    int count = countResults.getInt(2);

                    gradeCounts.put(masterOid, Integer.valueOf(count));
                }
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                try {
                    countResults.close();
                    statement.close();
                } catch (Exception e) {
                    AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
                }

                getBroker().returnConnection();
            }
        }

        return gradeCounts;
    }

    /**
     * Returns a map containing the number of students keyed on master OID.
     *
     * @param postControl GradePostControl
     * @return Map of Integer objects
     */
    private Map getStudentCounts(GradePostControl postControl) {
        HashMap studentCounts = new HashMap(1500);

        StringBuilder countSql = new StringBuilder(200);
        countSql.append("SELECT SSC_MST_OID, COUNT(*) ");
        countSql.append("FROM STUDENT_SCHEDULE ");

        /*
         * If the post control applies only to students of particular YOGs, count grades for
         * those YOGs only. To do this we must join to the student table.
         */
        if (postControl.getCriteriaType() == GradePostControl.POST_TYPE_YOG) {
            countSql.append("INNER JOIN STUDENT ");
            countSql.append("ON SSC_STD_OID = STD_OID ");
        }

        countSql.append("WHERE SSC_SCH_OID = '" + ((SisSchool) getSchool()).getActiveScheduleOid() + "' ");

        if (postControl.getCriteriaType() == GradePostControl.POST_TYPE_YOG) {
            countSql.append("AND STD_YOG IN (");
            countSql.append(postControl.getCriteria());
            countSql.append(") ");
        }

        countSql.append("GROUP BY SSC_MST_OID");

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet countResults = null;

        try {
            statement = connection.createStatement();
            countResults = statement.executeQuery(countSql.toString());

            while (countResults.next()) {
                String masterOid = countResults.getString(1);
                int count = countResults.getInt(2);

                studentCounts.put(masterOid, Integer.valueOf(count));
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            try {
                countResults.close();
                statement.close();
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
            }

            getBroker().returnConnection();
        }

        return studentCounts;
    }

    /**
     * Returns a map containing GradePost records keyed on master OID.
     *
     * @param gradeTermOid String
     * @param gradePostControlOid String
     * @param scheduleOid String
     * @return Map of GradePost objects
     */
    private Map<String, GradePost> getPostMap(String gradeTermOid, String gradePostControlOid, String scheduleOid) {
        Criteria postCriteria = new Criteria();
        postCriteria.addEqualTo(GradePost.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.COL_SCHEDULE_OID, scheduleOid);
        postCriteria.addEqualTo(GradePost.COL_GRADE_TERM_OID, gradeTermOid);
        postCriteria.addEqualTo(GradePost.COL_GRADE_POST_CONTROL_OID, gradePostControlOid);

        QueryByCriteria postQuery = new QueryByCriteria(GradePost.class, postCriteria);
        postQuery.addOrderByAscending(GradePost.COL_TIMESTAMP);

        Map<String, GradePost> postMap = getBroker().getMapByQuery(postQuery,
                GradePost.COL_MASTER_SCHEDULE_OID, 2000);

        return postMap;
    }
}
