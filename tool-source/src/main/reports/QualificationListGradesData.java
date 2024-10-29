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
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.QualificationList;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.qlist.QualificationListManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Qualification List" report. This report lists students qualifying for all
 * lists in a user specified category.
 * <p>
 * The QualificationListManager class is used to determine eligibility for a given list. Lists are
 * processed in priority order within the category. A given student appears only once on the report
 * under the highest priority eligible list.
 *
 * @author X2 Development Corporation
 */
public class QualificationListGradesData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "category" input parameter. The value is a String.
     */
    public static final String CATEGORY_PARAM = "category";

    /**
     * Name for the "column definition OID" input parameter. The value is a String.
     */
    public static final String COLUMN_DEFINITION_OID_PARAM = "transcriptColumnDefinitionOid";

    /**
     * Name for the "context OID" input parameter. The value is a String.
     */
    public static final String CONTEXT_OID_PARAM = "contextOid";

    /**
     * Name for the "grades to include" input parameter. The value is an Integer.
     */
    public static final String GRADES_TO_INCLUDE_PARAM = "gradesToInclude";

    /**
     * Name for the "query by" input parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" input parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "student sort" input paramter. The value is a String.
     */
    public static final String SORT_PARAM = "studentSort";

    // Grid fields
    private static final String COL_COLUMN = "column";
    private static final String COL_GRADE = "grade";
    private static final String COL_QUALIFICATION_LIST = "list";
    private static final String COL_STUDENT = "student";
    private static final String COL_TRANSCRIPT = "transcript";

    // Report parameters
    private static final String COLUMN_PARAM = "column";
    private static final String CONTEXT_PARAM = "context";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 5);

        String category = (String) getParameter(CATEGORY_PARAM);
        String columnDefinitionOid = (String) getParameter(COLUMN_DEFINITION_OID_PARAM);
        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        boolean includeQualifyingOnly = Integer.valueOf(0).equals(getParameter(GRADES_TO_INCLUDE_PARAM));

        if (!StringUtils.isEmpty(category) &&
                !StringUtils.isEmpty(contextOid) &&
                !StringUtils.isEmpty(columnDefinitionOid)) {
            TranscriptColumnDefinition transcriptColumn = (TranscriptColumnDefinition) getBroker()
                    .getBeanByOid(TranscriptColumnDefinition.class, columnDefinitionOid);
            DistrictSchoolYearContext context =
                    (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);

            /*
             * Create a Criteria that will find the transcripts to consider; this will be passed to
             * the qualification list manager
             *
             * Note that this does not contain criteria for the grade column; that is added by the
             * qualification list manager
             */
            Criteria transcriptCriteria = new Criteria();
            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, contextOid);
            transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());

            addUserCriteria(transcriptCriteria,
                    (String) getParameter(QUERY_BY_PARAM),
                    (String) getParameter(QUERY_STRING_PARAM),
                    SisStudent.class,
                    Transcript.COL_STUDENT_OID);

            /*
             * Retrieve and process the qualification lists in the specified category in priority
             * order.
             */
            Criteria qlistCriteria = new Criteria();
            qlistCriteria.addEqualTo(QualificationList.COL_CATEGORY, category);

            int level = getOrganization().getOrganizationDefinition().getLevel();
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            Collection<ModelProperty> properties =
                    OrganizationManager.getOrganizationPaths(QualificationList.class, dictionary, level);

            if (!CollectionUtils.isEmpty(properties)) {
                for (ModelProperty property : properties) {
                    qlistCriteria.addAndCriteria(
                            OrganizationManager.getOrganizationAccessCriteria(getOrganization(), property));
                }
            }

            QueryByCriteria qlistQuery = new QueryByCriteria(QualificationList.class, qlistCriteria);
            qlistQuery.addOrderByAscending(QualificationList.COL_PRIORITY);

            HashMap qualifyingStudents = new HashMap();

            QueryIterator qlists = getBroker().getIteratorByQuery(qlistQuery);
            try {
                while (qlists.hasNext()) {
                    QualificationList qlist = (QualificationList) qlists.next();

                    /*
                     * Retrieve the students eligible for the current list
                     */
                    QualificationListManager qlistManager =
                            new QualificationListManager(qlist, transcriptColumn, getBroker());

                    String[] userSort = getUserSortOrderAsStringArray((String) getParameter(SORT_PARAM));
                    List currentGrades = qlistManager.evaluateGrades(transcriptCriteria,
                            Arrays.asList(userSort),
                            transcriptColumn.getGradeScale(),
                            includeQualifyingOnly);

                    /*
                     * Populate the grid
                     */
                    Iterator gradesIterator = currentGrades.iterator();
                    while (gradesIterator.hasNext()) {
                        KeyValuePair transcriptColumnPair = (KeyValuePair) gradesIterator.next();
                        Transcript transcript = (Transcript) transcriptColumnPair.getKey();
                        TranscriptColumnDefinition column =
                                (TranscriptColumnDefinition) transcriptColumnPair.getValue();
                        SisStudent student = transcript.getStudent();

                        /*
                         * Check to see if the student qualified for a higher priority list. If so,
                         * exclude them here.
                         *
                         * Add the student if:
                         *
                         * - They have not yet qualified for a list (qualifyingList == null)
                         *
                         * - They have already qualified for the current list
                         * (qualifyingList.equals(qlist.getOid())).
                         * This allows all of a student's qualifying grades for a given list to
                         * be added.
                         */
                        String qualifyingList = (String) qualifyingStudents.get(student.getOid());
                        if (qualifyingList == null || qualifyingList.equals(qlist.getOid())) {
                            grid.append();
                            grid.set(COL_GRADE,
                                    transcript.getFieldValueByBeanPath(column.getTranscriptBeanAttribute()));
                            grid.set(COL_STUDENT, student);
                            grid.set(COL_TRANSCRIPT, transcript);
                            grid.set(COL_QUALIFICATION_LIST, qlist.getName());
                            grid.set(COL_COLUMN, column);

                            qualifyingStudents.put(student.getOid(), qlist.getOid());
                        }
                    }
                }
            } finally {
                qlists.close();
            }

            addParameter(COLUMN_PARAM, transcriptColumn);
            addParameter(CONTEXT_PARAM, context);
        }

        grid.beforeTop();

        return grid;
    }
}
