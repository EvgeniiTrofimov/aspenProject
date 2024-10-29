/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TransitionPlanExport.
 */
public class TransitionPlanExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Export elements
     */
    private static final String FIELD_DISTRICT = "DistrictNumber";
    private static final String FIELD_SCHOOL_NAME = "SchoolName";
    private static final String FIELD_STUDENT_ID = "PupilNumber";
    private static final String FIELD_FIRST_NAME = "UsualFirstName";
    private static final String FIELD_LAST_NAME = "UsualLastName";
    private static final String FIELD_CITIZENSHIP = "Citizenship";
    private static final String FIELD_GRADE_LEVEL = "Grade";
    private static final String FIELD_GENDER = "Gender";
    private static final String FIELD_DOB = "DateOfBirth";
    private static final String FIELD_ABORIGINAL = "AboriginalAncestry";
    private static final String FIELD_SPED_ELIGIBILITY = "SPEDEligibility";
    private static final String FIELD_EXEPTIONALITY = "ExceptionalityCode";
    private static final String FIELD_WITHDRAW_DATE = "WithdrawDate";
    private static final String FIELD_PLAN_DATE = "PlanDate";
    private static final String FIELD_PLAN_COUNSELOR = "PlanCounselor";
    private static final String FIELD_ADHOC_1 = "ProgramDesinationOther";
    private static final String FIELD_ADHOC_2 = "ProgramDesination1";
    private static final String FIELD_ADHOC_3 = "ProgramDesination2";
    private static final String FIELD_ADHOC_4 = "ProgramDesination3";
    private static final String FIELD_ADHOC_5 = "ProgramDesination4";
    private static final String FIELD_ADHOC_6 = "ProgramDesination5";
    private static final String FIELD_NARRATIVE_1 = "Narrative1";
    private static final String FIELD_NARRATIVE_2 = "Narrative2";
    private static final String FIELD_NARRATIVE_3 = "Narrative3";

    /*
     * Alias values
     */
    private static final String ABORIGINAL_ALIAS = "psn-indian-ancestry";
    private static final String CITIZENSHIP_ALIAS = "std-citizenship-status";
    private static final String EXCEPTIONALITY_ALIAS = "std-exceptionality";
    private static final String SPED_ELIGIBILITY_ALIAS = "std-eligibility";
    private static final String USUAL_FIRST_ALIAS = "psn-preferred-first-name";
    private static final String USUAL_LAST_ALIAS = "psn-surname";

    /*
     * Other fields
     */
    private static final int FIELD_COUNT = 24;

    /*
     * General members
     */
    private List<String> m_columns;
    private DateFormat m_dateFormatter;
    private DataDictionary m_dictionary;
    private int m_field_count;

    /*
     * Reference lookup
     */
    private Map<String, Map<String, ReferenceCode>> m_citizenshipCodeLookup;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_field_count) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, buildCriteria());
        query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                addBeanToGrid(student, grid);
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.ReportDataGridJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.ReportDataGridJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_field_count = FIELD_COUNT;

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_dateFormatter = new SimpleDateFormat("dd-MM-yyyy");

        // Set columns

        m_columns = new ArrayList<String>(m_field_count);

        m_columns.add(FIELD_DISTRICT);
        m_columns.add(FIELD_SCHOOL_NAME);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_FIRST_NAME);
        m_columns.add(FIELD_LAST_NAME);
        m_columns.add(FIELD_CITIZENSHIP);
        m_columns.add(FIELD_GRADE_LEVEL);
        m_columns.add(FIELD_GENDER);
        m_columns.add(FIELD_DOB);
        m_columns.add(FIELD_ABORIGINAL);
        m_columns.add(FIELD_SPED_ELIGIBILITY);
        m_columns.add(FIELD_EXEPTIONALITY);
        m_columns.add(FIELD_WITHDRAW_DATE);
        m_columns.add(FIELD_PLAN_DATE);
        m_columns.add(FIELD_PLAN_COUNSELOR);
        m_columns.add(FIELD_ADHOC_1);
        m_columns.add(FIELD_ADHOC_2);
        m_columns.add(FIELD_ADHOC_3);
        m_columns.add(FIELD_ADHOC_4);
        m_columns.add(FIELD_ADHOC_5);
        m_columns.add(FIELD_ADHOC_6);
        m_columns.add(FIELD_NARRATIVE_1);
        m_columns.add(FIELD_NARRATIVE_2);
        m_columns.add(FIELD_NARRATIVE_3);

        m_citizenshipCodeLookup = initializeReferenceTable(CITIZENSHIP_ALIAS);
    }

    /**
     * Appends the bean information to the grid.
     *
     * @param student SisStudent
     * @param grid DataGrid
     */
    private void addBeanToGrid(SisStudent student, DataGrid grid) {
        SisPerson person = student.getPerson();
        boolean deleteRow = false;
        try {

            if (person != null) {
                grid.append();
                deleteRow = true;

                grid.set(FIELD_DISTRICT, student.getSchool().getParentOrganization().getId());
                grid.set(FIELD_SCHOOL_NAME, student.getSchool().getName());
                grid.set(FIELD_STUDENT_ID, student.getLocalId());
                grid.set(FIELD_GRADE_LEVEL, student.getGradeLevel());
                grid.set(FIELD_GENDER, person.getGenderCode());
                grid.set(FIELD_DOB, formatDate(person.getDob()));
                grid.set(FIELD_ABORIGINAL, student.getFieldValueByAlias(ABORIGINAL_ALIAS));
                grid.set(FIELD_SPED_ELIGIBILITY, student.getFieldValueByAlias(SPED_ELIGIBILITY_ALIAS));
                grid.set(FIELD_EXEPTIONALITY, student.getFieldValueByAlias(EXCEPTIONALITY_ALIAS));

                /*
                 * Pull usual first name, if exists
                 */
                String firstName = (String) person.getFieldValueByAlias(USUAL_FIRST_ALIAS);
                if (StringUtils.isEmpty(firstName)) {
                    firstName = person.getFirstName();
                }
                grid.set(FIELD_FIRST_NAME, firstName);

                /*
                 * Pull usual last name, if exists
                 */
                String lastName = (String) person.getFieldValueByAlias(USUAL_LAST_ALIAS);
                if (StringUtils.isEmpty(lastName)) {
                    lastName = person.getLastName();
                }
                grid.set(FIELD_LAST_NAME, lastName);

                /*
                 * Lookup citizenship description
                 */
                String citizenship = (String) person.getFieldValueByAlias(CITIZENSHIP_ALIAS);
                grid.set(FIELD_CITIZENSHIP, lookupReferenceValue(m_citizenshipCodeLookup,
                        citizenship,
                        ReferenceCode.COL_DESCRIPTION,
                        student.getSchool()));

                /*
                 * Set adhoc values
                 */
                grid.set(FIELD_PLAN_DATE, student.getFieldC002());
                grid.set(FIELD_PLAN_COUNSELOR, student.getFieldD006());
                grid.set(FIELD_ADHOC_1, student.getFieldA024());
                grid.set(FIELD_ADHOC_2, student.getFieldA025());
                grid.set(FIELD_ADHOC_3, student.getFieldA053());
                grid.set(FIELD_ADHOC_4, student.getFieldA055());
                grid.set(FIELD_ADHOC_5, student.getFieldA056());
                grid.set(FIELD_ADHOC_6, student.getFieldD007());
                grid.set(FIELD_NARRATIVE_1, student.getFieldD008());
                grid.set(FIELD_NARRATIVE_2, student.getFieldD009());
                grid.set(FIELD_NARRATIVE_3, student.getFieldD010());
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(SisStudent.class.getName());
            strBldr.append(" with OID: [");
            strBldr.append(student.getOid());
            strBldr.append("] for the Student with Local ID: [");
            strBldr.append(student.getLocalId());
            strBldr.append("].");

            // deleteRow is true if an incomplete row has been added to the grid from grid.append()
            if (!deleteRow) {
                strBldr.append("Null encountered before adding to export.");
            } else {
                strBldr.append("Null encountered when setting Columns.");
                grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
            }

            strBldr.append("\n\n\nNullPointerException: \n");
            strBldr.append(ExceptionUtils.getStackTrace(npe));
            logToolMessage(Level.WARNING, strBldr.toString(), false);
        }
    }

    /**
     * Builds the criteria used to return the staff to include in the export.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(SisStudent.REL_SCHOOL, SisStudent.COL_SCHOOL_OID));

        X2Criteria transitionCriteria = new X2Criteria();
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_A024));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_A025));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_A053));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_A055));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_A056));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_D007));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_D008));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_D009));
        transitionCriteria.addOrCriteria(getEmptyCriteria(SisStudent.COL_FIELD_D010));

        criteria.addAndCriteria(transitionCriteria);

        return criteria;
    }

    /**
     * Returns criteria checking if the passed field is not empty.
     *
     * @param path String
     * @return Criteria
     */
    private Criteria getEmptyCriteria(String path) {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(path, getBroker().getPersistenceKey());

        return criteria;
    }

    /**
     * Formats the date to the defined style. If date is null empty string is returned.
     *
     * @param date PlainDate
     * @return String
     */
    private String formatDate(PlainDate date) {
        String value = "";

        if (date != null) {
            value = m_dateFormatter.format(date);
        }

        return value;
    }


    /**
     * Initializes the reference lookup map that allows searching by owner and code.
     *
     * @param alias String
     * @return Map<String, Map<String, ReferenceCode>>
     */
    private Map<String, Map<String, ReferenceCode>> initializeReferenceTable(String alias) {
        Map<String, Map<String, ReferenceCode>> ownerMap = new HashMap<String, Map<String, ReferenceCode>>(16);

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                for (ReferenceCode code : table.getReferenceCodes(getBroker())) {
                    Map<String, ReferenceCode> codeMap = ownerMap.get(code.getOwnerOid());
                    if (codeMap == null) {
                        codeMap = new HashMap<String, ReferenceCode>(64);
                        ownerMap.put(code.getOwnerOid(), codeMap);
                    }

                    codeMap.put(code.getCode(), code);
                }
            }
        }

        return ownerMap;
    }

    /**
     * Looks up the reference code in the owner-oriented map. If there is a code owned by the
     * organization the property
     * is returned. Otherwise it steps up the org's organization hierarchy.
     *
     * @param ownerMap Map<String,Map<String,ReferenceCode>>
     * @param code String
     * @param codeProperty String
     * @param organization Organization
     * @return String
     */
    private String lookupReferenceValue(Map<String, Map<String, ReferenceCode>> ownerMap,
                                        String code,
                                        String codeProperty,
                                        Organization organization) {
        String value = code;

        ReferenceCode referenceCode = null;

        Map<String, ReferenceCode> codeMap = ownerMap.get(organization.getOid());
        if (codeMap != null) {
            referenceCode = codeMap.get(code);
        }

        if (referenceCode != null) {
            value = (String) referenceCode.getFieldValueByBeanPath(codeProperty);
        } else if (organization.getParentOrganization() != null) {
            value = lookupReferenceValue(ownerMap, code, codeProperty, organization.getParentOrganization());
        }

        return value;
    }

    /**
     * Looks up the reference code in the owner-oriented map. If there is a code owned by the school
     * the property
     * is returned. Otherwise it steps up the school's organization hierarchy.
     *
     * @param ownerMap Map<String,Map<String,ReferenceCode>>
     * @param code String
     * @param codeProperty String
     * @param school SisSchool
     * @return String
     */
    private String lookupReferenceValue(Map<String, Map<String, ReferenceCode>> ownerMap,
                                        String code,
                                        String codeProperty,
                                        SisSchool school) {
        String value = code;

        ReferenceCode referenceCode = null;

        Map<String, ReferenceCode> codeMap = ownerMap.get(school.getOid());
        if (codeMap != null) {
            referenceCode = codeMap.get(code);
        }

        if (referenceCode != null) {
            value = (String) referenceCode.getFieldValueByBeanPath(codeProperty);
        } else if (school.getParentOrganization() != null) {
            value = lookupReferenceValue(ownerMap, code, codeProperty, school.getParentOrganization());
        }

        return value;
    }
}
