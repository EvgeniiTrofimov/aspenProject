/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.ExamComponent;
import com.x2dev.sis.model.beans.ExamEntry;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeason;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.OptionComponent;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.springframework.util.CollectionUtils;

/**
 * Data source for "Individual Candidate Statement of Entry" report for UK.
 *
 * @author X2 Development Corporation
 */
public class IndvCandidateStatementEntryData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /*
     * Input parameters
     */
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String END_DATE_PARAM = "endDate";
    private static final String INCLUDE_ALL_PARAM = "includeAll";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String SEASON_OID_PARAM = "seasonOid";
    private static final String SECTION_OIDS_PARAM = "sectionOids";
    private static final String STUDENT_SORT_PARAM = "studentSort";
    private static final String START_DATE_PARAM = "startDate";
    private static final String STUDENT_OIDS_PARAM = "studentOids";

    /*
     * Report parameters
     */
    private static final String SEASON_PARAM = "season";

    /*
     * Grid fields
     */
    private static final String FIELD_AWARDING_BODY = "awardingBody";
    private static final String FIELD_COMPONENT = "component";
    private static final String FIELD_STUDENT = "student";

    private Map<String, String> m_awardingBody;
    private Map<String, Collection<ExamComponent>> m_componentsByOptionOid;
    private PlainDate m_endDate;
    private ExamSeason m_season;
    private Map<String, Collection<ExamOption>> m_optionsByStudentOid;
    private PlainDate m_startDate;
    private Map<String, SisStudent> m_studentsByOid;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        boolean includeAll = ((Boolean) getParameter(INCLUDE_ALL_PARAM)).booleanValue();

        loadAwardingBodies();
        loadStudents();
        loadOptionComponent();

        for (String studentOid : m_studentsByOid.keySet()) {
            SisStudent student = m_studentsByOid.get(studentOid);

            Collection<ExamOption> options = m_optionsByStudentOid.get(studentOid);
            if (!CollectionUtils.isEmpty(options)) {
                for (ExamOption option : options) {
                    Collection<ExamComponent> components = m_componentsByOptionOid.get(option.getOid());
                    if (!CollectionUtils.isEmpty(components)) {
                        String awardingBody = m_awardingBody.get(option.getAwardingBody());
                        for (ExamComponent component : components) {
                            if (isValidComponent(component)) {
                                grid.append();
                                grid.set(FIELD_STUDENT, student);
                                grid.set(FIELD_COMPONENT, component);
                                grid.set(FIELD_AWARDING_BODY, awardingBody);
                            }
                        }
                    }
                }
            } else if (includeAll) {
                grid.append();
                grid.set(FIELD_STUDENT, student);
            }
        }

        addParameter(SEASON_PARAM, m_season);

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        String seasonOid = (String) getParameter(SEASON_OID_PARAM);
        m_season = (ExamSeason) getBroker().getBeanByOid(ExamSeason.class, seasonOid);

        m_componentsByOptionOid = new HashMap<String, Collection<ExamComponent>>();
        m_optionsByStudentOid = new HashMap<String, Collection<ExamOption>>();
    }

    /**
     * Returns the student criteria based on selections from input definition.
     * 
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria criteria = new Criteria();

        String sectionOidsParam = (String) getParameter(SECTION_OIDS_PARAM);
        String studentOidsParam = (String) getParameter(STUDENT_OIDS_PARAM);

        /*
         * Input definition was originally designed with redundant student select options. It is
         * assumed that a user will only select one of the following:
         * a) multi-select sections
         * b) multi-select students
         * c) students to include along with active only
         */
        if (!StringUtils.isEmpty(sectionOidsParam)) {
            List<String> sectionOids = StringUtils.convertDelimitedStringToList(sectionOidsParam, ',', true);
            criteria.addIn(X2BaseBean.COL_OID, getStudentOids(sectionOids));
        } else if (!StringUtils.isEmpty(studentOidsParam)) {
            List<String> studentOids = StringUtils.convertDelimitedStringToList(studentOidsParam, ',', true);
            criteria.addIn(X2BaseBean.COL_OID, studentOids);
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(criteria, queryBy, queryString, null, X2BaseBean.COL_OID);

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        SisStudent.COL_ENROLLMENT_STATUS));
            }

            if (isSchoolContext()) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            }
        }

        return criteria;
    }

    /**
     * Returns a unique collection of student OIDs from the passed section OIDs.
     *
     * @param sectionOids List<String>
     * @return Collection
     */
    private Collection<String> getStudentOids(List<String> sectionOids) {
        Criteria criteria = new Criteria();
        criteria.addIn(StudentSection.COL_SECTION_OID, sectionOids);

        SubQuery studentSub = new SubQuery(StudentSchedule.class,
                StudentSchedule.COL_STUDENT_OID,
                criteria,
                true);

        return getBroker().getSubQueryCollectionByQuery(studentSub);
    }

    /**
     * Determines whether the passed component is valid.
     *
     * @param component ExamComponent
     * @return boolean - true if timetable date from passed component is between the start
     *         and end date entered on input definition
     */
    private boolean isValidComponent(ExamComponent component) {
        boolean valid = false;

        PlainDate examDate = component.getTimetableDate();
        if (examDate != null &&
                (examDate.after(m_startDate) || examDate.equals(m_startDate)) &&
                (examDate.before(m_endDate) || examDate.equals(m_endDate))) {
            valid = true;
        }

        return valid;
    }

    /**
     * Loads a map of awarding body descriptions keyed on code.
     *
     * @throws ToolRunException exception
     */
    private void loadAwardingBodies() throws ToolRunException {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField awardingBodyField =
                dictionary.findDataDictionaryField(ExamOption.class.getName(), ExamOption.COL_AWARDING_BODY);
        if (awardingBodyField == null || !awardingBodyField.hasReferenceTable()) {
            throw new ToolRunException("'Awarding Body' field on EXAM_OPTION is not linked to a reference table.");
        }

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
    }

    /**
     * Queries on OptionComponent and loads the following:
     * <ul>
     * <li>Map of collection of ExamOption beans keyed on student OID.
     * <li>Map of collection of ExamComponent beans keyed on option OID.
     * </ul>
     */
    private void loadOptionComponent() {
        Criteria optionCriteria = new Criteria();
        optionCriteria.addEqualTo(ExamEntry.REL_OPTION + PATH_DELIMITER + ExamOption.REL_SERIES + PATH_DELIMITER +
                ExamSeries.COL_SEASON_OID, m_season.getOid());

        Collection<String> studentOids = m_studentsByOid.keySet();
        if (!CollectionUtils.isEmpty(studentOids)) {
            optionCriteria.addIn(ExamEntry.COL_STUDENT_OID, studentOids);
        } else {
            addNoMatchCriteria(optionCriteria);
        }

        SubQuery optionSub = new SubQuery(ExamEntry.class,
                ExamEntry.COL_OPTION_OID,
                optionCriteria,
                true);

        Criteria optionComponentCriteria = new Criteria();
        optionComponentCriteria.addIn(OptionComponent.COL_OPTION_OID, optionSub);

        QueryByCriteria query = new QueryByCriteria(OptionComponent.class, optionComponentCriteria);
        query.addOrderByAscending(OptionComponent.REL_COMPONENT + PATH_DELIMITER + ExamComponent.COL_TIMETABLE_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                OptionComponent optionComponent = (OptionComponent) iterator.next();

                populateEntriesByStudent(optionComponent.getOption());
                populateComponentsByOption(optionComponent);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of student beans keyed on student OID.
     */
    private void loadStudents() {
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, getStudentCriteria());

        String sortParam = (String) getParameter(STUDENT_SORT_PARAM);
        applyUserSort(query, sortParam);

        m_studentsByOid = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 100);
    }

    /**
     * Populates a map of ExamComponent beans keyed on option OID.
     *
     * @param optionComponent OptionComponent
     */
    private void populateComponentsByOption(OptionComponent optionComponent) {
        String optionOid = optionComponent.getOptionOid();

        Collection<ExamComponent> components = m_componentsByOptionOid.get(optionOid);
        if (components == null) {
            components = new ArrayList<ExamComponent>();
        }

        components.add(optionComponent.getComponent());
        m_componentsByOptionOid.put(optionOid, components);
    }

    /**
     * Retrieves a collection of ExamEntry beans from the passed ExamOption bean, iterates
     * over the collection and populates a map of ExamOption beans keyed on student OID.
     *
     * @param option ExamOption
     */
    private void populateEntriesByStudent(ExamOption option) {
        Collection<ExamEntry> entries = option.getEntries(getBroker());
        if (!CollectionUtils.isEmpty(entries)) {
            for (ExamEntry entry : entries) {
                String studentOid = entry.getStudentOid();

                Collection<ExamOption> options = m_optionsByStudentOid.get(studentOid);
                if (options == null) {
                    options = new ArrayList<ExamOption>();
                }

                if (!options.contains(option)) {
                    options.add(option);
                }

                m_optionsByStudentOid.put(studentOid, options);
            }
        }
    }
}
