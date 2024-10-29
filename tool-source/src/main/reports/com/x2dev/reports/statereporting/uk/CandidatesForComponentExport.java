/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.ExamComponent;
import com.x2dev.sis.model.beans.ExamEntry;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeason;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.OptionComponent;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "Candidates for Component" export.
 *
 * @author X2 Development Corporation
 *
 */
public class CandidatesForComponentExport extends ExportJavaSource {
    /*
     * Input parameters
     */
    private static final String SEASON_OID_PARAM = "seasonOid";
    private static final String SERIES_OID_PARAM = "seriesOid";
    private static final String SORT_PARAM = "sort";

    /*
     * Grid fields
     */
    private static final String FIELD_AWARDING_BODY = "Awarding Body";
    private static final String FIELD_CANDIDATE_NUMBER = "Candidate Number";
    private static final String FIELD_CLASS_GROUP = "Class Group";
    private static final String FIELD_COMPONENT_CODE = "Component Code";
    private static final String FIELD_COMPONENT_TITLE = "Component Title";
    private static final String FIELD_DOB = "DOB";
    private static final String FIELD_EXAM_DATE = "Exam Date";
    private static final String FIELD_GENDER = "Sex";
    private static final String FIELD_SCHOOL_YEAR = "Year";
    private static final String FIELD_SERIES = "Series";
    private static final String FIELD_SESSION = "Session";
    private static final String FIELD_STUDENT_NAME = "Candidate";
    private static final String FIELD_UCI = "UCI";

    /*
     * UK aliases
     */
    private static final String CANDIDATE_NUMBER_ALIAS = "DFE CANDIDATE NUMBER";
    private static final String CLASS_GROUP_ALIAS = "class-group";
    private static final String UCI_ALIAS = "DFE UCI";

    /*
     * UK date format
     */
    private static final String UK_DATE_FORMAT = "dd-MM-yyyy";

    private Map<String, String> m_awardingBody;
    private String m_candidateNumberBeanPath;
    private List<String> m_columns;
    private DateFormat m_dateFormat;
    private DataDictionary m_dictionary;
    private Map<String, Collection<SisStudent>> m_entryMap;
    private ExamSeason m_season;
    private Integer m_schoolYear;
    private ExamSeries m_series;
    private String m_uciBeanPath;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        loadAwardingBodies();
        loadEntryData();

        Criteria optionComponentsCriteria = new Criteria();

        Collection<String> optionOids = m_entryMap.keySet();
        if (!CollectionUtils.isEmpty(optionOids)) {
            optionComponentsCriteria.addIn(OptionComponent.COL_OPTION_OID, optionOids);
        } else {
            addNoMatchCriteria(optionComponentsCriteria);
        }

        optionComponentsCriteria
                .addNotNull(OptionComponent.REL_COMPONENT + PATH_DELIMITER + ExamComponent.COL_TIMETABLE_DATE);
        optionComponentsCriteria
                .addNotNull(OptionComponent.REL_COMPONENT + PATH_DELIMITER + ExamComponent.COL_TIMETABLE_SESSION_CODE);

        QueryByCriteria query = new QueryByCriteria(OptionComponent.class, optionComponentsCriteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                OptionComponent optionComponent = (OptionComponent) iterator.next();

                ExamComponent component = optionComponent.getComponent();
                ExamOption option = optionComponent.getOption();

                String optionOid = option.getOid();
                Collection<SisStudent> students = m_entryMap.get(optionOid);
                if (!CollectionUtils.isEmpty(students)) {
                    String awardingBody = m_awardingBody.get(option.getAwardingBody());
                    String seriesCode = option.getSeries().getCode();

                    String componentCode = component.getComponentCode();
                    String componentTitle = component.getTitle();
                    PlainDate examDate = component.getTimetableDate();
                    String session = component.getTimetableSessionCode();

                    for (SisStudent student : students) {
                        grid.append();
                        grid.set(FIELD_AWARDING_BODY, awardingBody);
                        grid.set(FIELD_SERIES, seriesCode);
                        grid.set(FIELD_SCHOOL_YEAR, String.valueOf(m_schoolYear));
                        grid.set(FIELD_COMPONENT_CODE, componentCode);
                        grid.set(FIELD_COMPONENT_TITLE, componentTitle);
                        grid.set(FIELD_EXAM_DATE, m_dateFormat.format(examDate));
                        grid.set(FIELD_SESSION, session);

                        if (m_candidateNumberBeanPath != null) {
                            grid.set(FIELD_CANDIDATE_NUMBER,
                                    student.getFieldValueByBeanPath(m_candidateNumberBeanPath));
                        }

                        grid.set(FIELD_CLASS_GROUP, student.getFieldValueByAlias(CLASS_GROUP_ALIAS));
                        grid.set(FIELD_STUDENT_NAME, student.getNameView());
                        grid.set(FIELD_GENDER, student.getPerson().getGenderCode());
                        grid.set(FIELD_DOB, m_dateFormat.format(student.getPerson().getDob()));

                        if (m_uciBeanPath != null) {
                            grid.set(FIELD_UCI, student.getFieldValueByBeanPath(m_uciBeanPath));
                        }
                    }
                }
            }
        } finally {
            iterator.close();
        }

        /*
         * Sort order
         */
        List<String> sortColumns = new ArrayList<String>();

        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 2: // Timetable date
                sortColumns.add(FIELD_EXAM_DATE);
                sortColumns.add(FIELD_SESSION);
                sortColumns.add(FIELD_COMPONENT_CODE);
                sortColumns.add(FIELD_AWARDING_BODY);
                break;

            case 3: // Awarding body
                sortColumns.add(FIELD_AWARDING_BODY);
                sortColumns.add(FIELD_COMPONENT_CODE);
                sortColumns.add(FIELD_EXAM_DATE);
                sortColumns.add(FIELD_SESSION);
                break;

            default: // default by component code
                sortColumns.add(FIELD_COMPONENT_CODE);
                sortColumns.add(FIELD_EXAM_DATE);
                sortColumns.add(FIELD_SESSION);
                sortColumns.add(FIELD_AWARDING_BODY);
                break;
        }

        if (m_candidateNumberBeanPath != null) {
            sortColumns.add(FIELD_CANDIDATE_NUMBER);
        } else {
            sortColumns.add(FIELD_STUDENT_NAME);
        }

        grid.sort(sortColumns, false);

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
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
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
     * @throws ToolRunException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws ToolRunException {
        String seasonOid = (String) getParameter(SEASON_OID_PARAM);
        m_season = (ExamSeason) getBroker().getBeanByOid(ExamSeason.class, seasonOid);
        m_schoolYear = Integer.valueOf(m_season.getDistrictContext().getSchoolYear());

        String seriesOid = (String) getParameter(SERIES_OID_PARAM);
        if (!StringUtils.isEmpty(seriesOid)) {
            m_series = (ExamSeries) getBroker().getBeanByOid(ExamSeries.class, seriesOid);
        }

        m_dateFormat = new SimpleDateFormat(UK_DATE_FORMAT);

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        m_candidateNumberBeanPath = translateAliasToJavaName(CANDIDATE_NUMBER_ALIAS);
        m_uciBeanPath = translateAliasToJavaName(UCI_ALIAS);

        m_columns = new ArrayList<String>(14);
        m_columns.add(FIELD_AWARDING_BODY);
        m_columns.add(FIELD_SERIES);
        m_columns.add(FIELD_SCHOOL_YEAR);
        m_columns.add(FIELD_COMPONENT_CODE);
        m_columns.add(FIELD_COMPONENT_TITLE);
        m_columns.add(FIELD_EXAM_DATE);
        m_columns.add(FIELD_SESSION);
        m_columns.add(FIELD_CANDIDATE_NUMBER);
        m_columns.add(FIELD_STUDENT_NAME);
        m_columns.add(FIELD_CLASS_GROUP);
        m_columns.add(FIELD_GENDER);
        m_columns.add(FIELD_DOB);
        m_columns.add(FIELD_UCI);

        setUseValueDelimiters(true);
        setUseValueWrappers(true);
    }

    /**
     * Loads a map of awarding body descriptions keyed on code.
     *
     * @throws ToolRunException exception
     */
    private void loadAwardingBodies() throws ToolRunException {
        DataDictionaryField awardingBodyField =
                m_dictionary.findDataDictionaryField(ExamOption.class.getName(), ExamOption.COL_AWARDING_BODY);
        if (awardingBodyField != null && awardingBodyField.hasReferenceTable()) {
            m_awardingBody = new HashMap<String, String>();

            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, awardingBodyField.getReferenceTableOid());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ReferenceCode refCode = (ReferenceCode) iterator.next();

                    m_awardingBody.put(refCode.getCode(), refCode.getDescription());
                }
            } finally {
                iterator.close();
            }
        } else {
            throw new ToolRunException("'Awarding Body' field on EXAM_OPTION is not linked to a reference table.");
        }
    }

    /**
     * Queries on ExamEntry and loads a map of collection of students keyed on option OID.
     */
    private void loadEntryData() {
        m_entryMap = new HashMap<String, Collection<SisStudent>>();

        Criteria criteria = new Criteria();
        if (m_series != null) {
            criteria.addEqualTo(ExamEntry.REL_OPTION + PATH_DELIMITER + ExamOption.COL_SERIES_OID, m_series.getOid());
        } else {
            criteria.addEqualTo(ExamEntry.REL_OPTION + PATH_DELIMITER + ExamOption.REL_SERIES + PATH_DELIMITER +
                    ExamSeries.COL_SEASON_OID, m_season.getOid());
        }

        QueryByCriteria query = new QueryByCriteria(ExamEntry.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExamEntry entry = (ExamEntry) iterator.next();

                String optionOid = entry.getOptionOid();

                Collection<SisStudent> students = m_entryMap.get(optionOid);
                if (students == null) {
                    students = new HashSet<SisStudent>();
                }

                students.add(entry.getStudent());
                m_entryMap.put(optionOid, students);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Translates an alias into a Java bean path name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }

        return javaName;
    }
}
