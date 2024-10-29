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
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.QualificationList;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.qlist.QualificationListManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
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
 * <p>
 * This procedure supports creation of a record set per qualifiaction list within the given
 * category.
 *
 * @author X2 Development Corporation
 */
public class QualificationListData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "category" input parameter. The value is a String.
     */
    private static final String CATEGORY_PARAM = "category";

    /**
     * Name for the "column definition OID" input parameter. The value is a String.
     */
    private static final String COLUMN_DEFINITION_OID_PARAM = "transcriptColumnDefinitionOid";

    /**
     * Name for the "context OID" input parameter. The value is a String.
     */
    private static final String CONTEXT_OID_PARAM = "contextOid";

    /**
     * Name for the "create snapshots" input parameter. The value is a Boolean.
     */
    private static final String CREATE_RECORD_SETS_PARAM = "createRecordSets";

    /**
     * Name for the "query by" input parameter. The value is a String.
     */
    private static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" input parameter. The value is a String.
     */
    private static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "student sort" input paramter. The value is a String.
     */
    private static final String SORT_PARAM = "studentSort";

    // Grid fields
    private static final String COL_QUALIFICATION_LIST = "list";
    private static final String COL_STUDENT = "student";

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
        Boolean createRecordSets = (Boolean) getParameter(CREATE_RECORD_SETS_PARAM);

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

            HashSet qualifyingStudentOids = new HashSet(200);

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
                    List currentStudents = qlistManager.evaluateStudents(transcriptCriteria, Arrays.asList(userSort));

                    /*
                     * Remove a pre-existing record set if one exists and we are creating a new
                     * one
                     */
                    RecordSet recordSet = null;
                    ModelBroker broker = null;
                    if (createRecordSets.booleanValue()) {
                        broker = new ModelBroker(getPrivilegeSet());
                        recordSet = prepareRecordSet(qlist.getName(), transcriptColumn, context,
                                broker);
                    }

                    /*
                     * Populate the grid & record set
                     */
                    Iterator studentOids = currentStudents.iterator();
                    while (studentOids.hasNext()) {
                        String studentOid = (String) studentOids.next();

                        if (!qualifyingStudentOids.contains(studentOid)) {
                            SisStudent student =
                                    (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);

                            grid.append();
                            grid.set(COL_STUDENT, student);
                            grid.set(COL_QUALIFICATION_LIST, qlist.getName());

                            if (createRecordSets.booleanValue()) {
                                RecordSetKey recordSetKey =
                                        X2BaseBean.newInstance(RecordSetKey.class, getBroker().getPersistenceKey());
                                recordSetKey.setRecordSetOid(recordSet.getOid());
                                recordSetKey.setObjectOid(studentOid);
                                broker.saveBean(recordSetKey);
                            }

                            qualifyingStudentOids.add(studentOid);
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

    /**
     * Prepares a record set to be populated with the members of the passed qualification list.
     * <ol>
     * <li>Deletes the record set for the passed list if one exists
     * <li>Creates and returns a new record set
     * </ol>
     *
     * @param listName String
     * @param transcriptColumn TranscriptColumnDefinition
     * @param context DistrictSchoolYearContext
     * @param broker Required to delete & save
     * @return RecordSet
     */
    private RecordSet prepareRecordSet(String listName,
                                       TranscriptColumnDefinition transcriptColumn,
                                       DistrictSchoolYearContext context,
                                       ModelBroker broker) {
        // Get the length of the record set name field
        DataDictionaryField rsNameField =
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()).findDataDictionaryField(
                        RecordSet.class.getName(), RecordSet.COL_NAME);

        int fieldLength = rsNameField.getDatabaseLength();

        String recordSetName = listName + " - " +
                transcriptColumn.getGradeName().trim() + " - " +
                context.getContextId().trim();

        if (recordSetName.length() > fieldLength) {
            recordSetName = recordSetName.substring(0, fieldLength - 1);
        }

        Criteria criteria = new Criteria();
        criteria.addEqualTo(RecordSet.COL_NAME, recordSetName);
        criteria.addEqualTo(RecordSet.COL_OWNER_OID, getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(RecordSet.class, criteria);
        RecordSet recordSet = (RecordSet) broker.getBeanByQuery(query);

        if (recordSet != null) {
            broker.deleteBean(recordSet); // Assumes RSN-->RSK cascading delete!
        }

        DataDictionaryTable table =
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()).findDataDictionaryTableByClass(
                        SisStudent.class.getName());

        recordSet = X2BaseBean.newInstance(RecordSet.class, getBroker().getPersistenceKey());
        recordSet.setName(recordSetName);
        recordSet.setDataTableOid(table.getSystemOid());
        recordSet.setOwnerType(Ownable.OWNER_TYPE_SCHOOL);
        recordSet.setOwnerOid(getSchool().getOid());

        broker.saveBean(recordSet);

        return recordSet;
    }
}
