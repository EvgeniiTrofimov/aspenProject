/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataAudit;
import com.follett.fsc.core.k12.beans.DataAudit.ChangeType;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.AuditXmlManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class StudentAddressExport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class StudentAddressExport extends ExportJavaSource {

    private static final String COLUMN_DATE_BEGIN = "DATE_BEGIN";
    private static final String COLUMN_DATE_END = "DATE_END";
    private static final String COLUMN_STD_ADRS_VIEW = "ADRS_VIEW";
    private static final String COLUMN_STD_ID_LOCAL = "ID_LOCAL";
    private static final String COLUMN_STD_OID = "OID";

    private static final int DEFAULT_INITIAL_SIZE = 32;

    private static final String FIELD_STD_VIEW_ADRS = "stdViewAdrs";

    private static final String TABLE_OID_STD = "tblStudent";

    private final SimpleDateFormat m_dateFormatter = new SimpleDateFormat("yyyyMMdd_hhmmss");

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(getColumnNames().size());

        X2Criteria studentCriteria = buildStudentCriteria();
        Map<String, List<DataAudit>> studentAuditMap = listStudentAuditRecords(studentCriteria);
        for (SisStudent student : listStudents(studentCriteria)) {
            if (studentAuditMap.containsKey(student.getOid())) {
                addStudentToGrid(grid, student, studentAuditMap.get(student.getOid()));
            }
        }
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
        return Arrays.asList(COLUMN_STD_OID, COLUMN_STD_ID_LOCAL, COLUMN_DATE_BEGIN, COLUMN_DATE_END,
                COLUMN_STD_ADRS_VIEW);
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return this.getColumnNames();
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
     * Adds the student to grid.
     *
     * @param grid DataGrid
     * @param student SisStudent
     * @param auditRows List<DataAudit>
     */
    private void addStudentToGrid(DataGrid grid, SisStudent student, List<DataAudit> auditRows) {
        for (int i = 0; i < auditRows.size(); i++) {
            DataAudit audit = auditRows.get(i);
            HashMap<String, String> previousValues = new HashMap<>();
            HashMap<String, String> changedValues = new HashMap<>();
            AuditXmlManager.parseAuditChangeDefinition(audit, previousValues, changedValues);
            Date date = new Date(audit.getTimestamp());
            Date endDate = i < auditRows.size() - 1 ? new Date(auditRows.get(i + 1).getTimestamp()) : null;
            String stdAdrs = changedValues.get(FIELD_STD_VIEW_ADRS);
            if (!StringUtils.isEmpty(stdAdrs)) {
                grid.append();
                grid.set(COLUMN_STD_OID, student.getOid());
                grid.set(COLUMN_STD_ID_LOCAL, student.getLocalId());
                grid.set(COLUMN_STD_ADRS_VIEW, stdAdrs);
                grid.set(COLUMN_DATE_BEGIN, m_dateFormatter.format(date));
                grid.set(COLUMN_DATE_END, endDate != null ? m_dateFormatter.format(endDate) : "");
            }
        }
    }

    /**
     * Builds the student criteria.
     *
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria() {
        X2Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }
        String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
        return criteria;
    }

    /**
     * List student audit records.
     *
     * @param studentsCriteria X2Criteria
     * @return Map
     */
    private Map<String, List<DataAudit>> listStudentAuditRecords(X2Criteria studentsCriteria) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DataAudit.COL_TABLE_OID, TABLE_OID_STD);
        criteria.addIn(DataAudit.COL_CHANGE_TYPE,
                Arrays.asList(Integer.valueOf(ChangeType.CREATE.ordinal()),
                        Integer.valueOf(ChangeType.MODIFY.ordinal())));
        criteria.addLike(DataAudit.COL_CHANGE_DEFINITION, "%" + FIELD_STD_VIEW_ADRS + "%");

        SubQuery studentsQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentsCriteria, true);
        criteria.addIn(DataAudit.COL_OBJECT_OID, studentsQuery);
        QueryByCriteria query = new QueryByCriteria(DataAudit.class, criteria);
        query.addOrderByAscending(DataAudit.COL_TIMESTAMP);
        return getBroker().getGroupedCollectionByQuery(query, DataAudit.COL_OBJECT_OID, DEFAULT_INITIAL_SIZE);
    }

    /**
     * List students.
     *
     * @param criteria X2Criteria
     * @return Collection
     */
    private Collection<SisStudent> listStudents(X2Criteria criteria) {
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);
        return getBroker().getCollectionByQuery(query);
    }
}
