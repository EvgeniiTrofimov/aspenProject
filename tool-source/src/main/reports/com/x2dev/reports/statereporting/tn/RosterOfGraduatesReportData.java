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

package com.x2dev.reports.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import dori.jasper.engine.JRDataSource;

/**
 * Data source of Data sources for the "Roster of Graduates" report and sub reports.
 *
 * @author X2 Development Corporation
 */
public class RosterOfGraduatesReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    // Aliases for document type and date
    private static final String ALIAS_DOCUMENT_DATE = "DOE COMPLETION DOCUMENT DATE";
    private static final String ALIAS_DOCUMENT_TYPE = "DOE COMPLETION DOCUMENT TYPE";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    // Fields
    private static final String FIELD_DIPLOMA_DATE = "graduationDate";
    private static final String FIELD_DIPLOMA_TYPE = "diplomaType";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_NAME = "studentName";

    private static final String INPUT_PARAM_DISTRICT_SUMMARY = "includeDistrictSummary";
    private static final String INPUT_PARAM_EXCLUDE_EMPTY = "excludeEmpty";
    private static final String INPUT_PARAM_SUMMARY_ONLY = "summaryOnly";
    public static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_SCHOOLS = "schoolOids";

    // Parameters
    private static final String PARAM_DISTRICT = "district";

    private DataDictionary m_dictionary;
    private String m_documentTypeJavaName;
    private String m_documentDateJavaName;
    private boolean m_excludeEmptyDiplomaType;
    private boolean m_includeDistrictSummary;
    private boolean m_summaryOnly;

    /**
     * Prepares the data source that will be used by the Jasper design. This method is called after
     * <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        Map<String, ReferenceCode> diplomaTypes = getDiplomaTypes();
        ReportDataGrid grid = new ReportDataGrid();

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        Collection<SisSchool> schools = getSchools();
        for (SisSchool school : schools) {
            for (String diplomaType : diplomaTypes.keySet()) {
                QueryIterator students = getBroker().getIteratorByQuery(getStudentQuery(school, diplomaType));
                Map<String, Collection<SisStudent>> studentMap = getDateStudentsMap(students);
                String typeDiscription = diplomaTypes.get(diplomaType).getDescription();
                if (studentMap.keySet().isEmpty()) {
                    if (!m_summaryOnly && !m_excludeEmptyDiplomaType) {
                        populateGrid(grid, null, school, typeDiscription, null);
                    }
                } else {
                    for (String date : studentMap.keySet()) {
                        Collection<SisStudent> studentList = studentMap.get(date);
                        Iterator studentIter = studentList.iterator();

                        while (studentIter.hasNext()) {
                            SisStudent student = (SisStudent) studentIter.next();

                            if (!m_summaryOnly) {
                                populateGrid(grid, date, school, typeDiscription, student);
                            }

                            // Generate district summary for selected schools
                            if (m_includeDistrictSummary) {
                                populateGrid(grid, date, null, typeDiscription, student);
                            }
                        }
                    }
                }
            }
        }
        addParameter(PARAM_DISTRICT, getOrganization());

        // Sorts grid in ascending order by FIELD_SCHOOL_NAME, FIELD_DIPLOMA_TYPE and
        // FIELD_STUDENT_NAME
        // This is done to data for DistrictSummary will be at the end
        List<String> list = new ArrayList<String>();
        list.add(FIELD_SCHOOL_NAME);
        list.add(FIELD_DIPLOMA_TYPE);
        list.add(FIELD_DIPLOMA_DATE);
        list.add(FIELD_STUDENT_NAME);
        grid.sort(list, false);

        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        DataDictionaryField fieldDocumetType = dictionary.findDataDictionaryFieldByAlias(ALIAS_DOCUMENT_TYPE);
        m_documentTypeJavaName = fieldDocumetType.getJavaName();
        DataDictionaryField fieldDocumentDate = dictionary.findDataDictionaryFieldByAlias(ALIAS_DOCUMENT_DATE);
        m_documentDateJavaName = fieldDocumentDate.getJavaName();

        if (getParameter(INPUT_PARAM_DISTRICT_SUMMARY) != null) {
            m_includeDistrictSummary = ((Boolean) getParameters().get(INPUT_PARAM_DISTRICT_SUMMARY)).booleanValue();
        }
        if (getParameter(INPUT_PARAM_SUMMARY_ONLY) != null) {
            m_summaryOnly = ((Boolean) getParameters().get(INPUT_PARAM_SUMMARY_ONLY)).booleanValue();
        }
        if (getParameter(INPUT_PARAM_EXCLUDE_EMPTY) != null) {
            m_excludeEmptyDiplomaType = ((Boolean) getParameters().get(INPUT_PARAM_EXCLUDE_EMPTY)).booleanValue();
        }
    }

    /**
     * Returns set of document dates for students
     * from Query Iterator.
     *
     * @param studentIterator QueryIterator
     * @return Map<String, Collection<SisStudent>>
     */
    private Map<String, Collection<SisStudent>> getDateStudentsMap(QueryIterator studentIterator) {
        Map<String, Collection<SisStudent>> studentMap = new HashMap<String, Collection<SisStudent>>();
        try {
            while (studentIterator.hasNext()) {
                SisStudent student = (SisStudent) studentIterator.next();
                String documentDate = (String) student.getFieldValueByAlias(ALIAS_DOCUMENT_DATE);
                /*
                 * if (!StringUtils.isEmpty(documentDate))
                 * {
                 */
                if (studentMap.containsKey(documentDate)) {
                    studentMap.get(documentDate).add(student);
                } else {
                    Collection<SisStudent> students = new ArrayList<SisStudent>();
                    students.add(student);
                    studentMap.put(documentDate, students);
                }
                /* } */
            }

        } finally {
            studentIterator.close();
        }

        return studentMap;
    }

    /**
     * get a map of the diploma types. The key is the reference code and the value is the
     * description.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getDiplomaTypes() {
        DataDictionaryField field =
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_DOCUMENT_TYPE);
        return field.getReferenceTable().getCodeMap(getBroker());
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                X2Criteria schoolCriteria = new X2Criteria();
                schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                schools = getBroker().getCollectionByQuery(schoolQuery);
            }
        }

        return schools;
    }

    /**
     * Create query for student.
     *
     * @param school SisSchool
     * @param diplomaType String
     * @return Report query by criteria
     */
    private ReportQueryByCriteria getStudentQuery(SisSchool school, String diplomaType) {
        TNStudentMultiYearHelper multiYearHelper =
                new TNStudentMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addAndCriteria(multiYearHelper.getWithAttributesCriteria());

        multiYearHelper.adjustCriteria(
                studentCriteria, Strategy.NOT_EQUAL_TO,
                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

        multiYearHelper.adjustCriteria(
                studentCriteria, Strategy.NOT_EQUAL_TO,
                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        multiYearHelper.adjustCriteria(studentCriteria, Strategy.EQUAL_TO, SisStudent.COL_SCHOOL_OID, school.getOid());
        multiYearHelper.adjustCriteria(studentCriteria, Strategy.BETWEEN, m_documentDateJavaName,
                getCurrentContext().getStartDate(), getCurrentContext().getEndDate());
        multiYearHelper.adjustCriteria(studentCriteria, Strategy.EQUAL_TO, m_documentTypeJavaName, diplomaType);

        ReportQueryByCriteria studentQuery = new ReportQueryByCriteria(SisStudent.class, studentCriteria);
        studentQuery
                .addOrderByAscending(SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_GENDER_CODE);
        studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        return studentQuery;
    }

    /**
     * Populate grid method.
     *
     * @param grid ReportDataGrid
     * @param date String
     * @param school SisSchool
     * @param description String
     * @param student SisStudent
     */
    private void populateGrid(ReportDataGrid grid,
                              String date,
                              SisSchool school,
                              String description,
                              SisStudent student) {
        grid.append();
        grid.set(FIELD_DIPLOMA_DATE, date);
        grid.set(FIELD_SCHOOL, school);
        grid.set(FIELD_SCHOOL_NAME, school == null ? null : school.getName());
        grid.set(FIELD_DIPLOMA_TYPE, description);
        grid.set(FIELD_STUDENT, student);
        grid.set(FIELD_STUDENT_NAME, student == null ? null : student.getNameView());
    }

}
