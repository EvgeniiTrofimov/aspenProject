/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2015 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.tools.procedures;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.X2ObjectCache;
import com.follett.fsc.core.k12.beans.AtRiskDefinition;
import com.follett.fsc.core.k12.beans.AtRiskResult;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.DataTableConfig.OrganizationAccess;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to cleanup at risk results for selected definition and to also
 * delete related student alerts.
 */
public class AtRiskResultsCleanupProcedure extends ProcedureJavaSource {
    /*
     * Input parameters
     */
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String AT_RISK_DEFINITION_OID_PARAM = "ridOid";
    private static final String DELETE_ALERTS_ONLY = "deleteAlertsOnly";
    private static final String END_DATE_PARAM = "endDate";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String START_DATE_PARAM = "startDate";

    /*
     * Other constants
     */
    private static final String AT_RISK_RESULT_STUDENT_ORG_PATH = "relRirStdOid.stdOrgOID#";

    private Map<String, StudentAlert> m_atRiskAlertByStudentOid;
    private PlainDate m_endDate;
    private String m_ridOid;
    private PlainDate m_startDate;
    private Criteria m_studentCriteria;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource.execute() throws Exception
     */
    @Override
    protected void execute() throws Exception {
        boolean deleteAlertsOnly = ((Boolean) getParameter(DELETE_ALERTS_ONLY)).booleanValue();

        if (!m_startDate.after(m_endDate)) {
            m_ridOid = BeanManager.getFullOid(m_ridOid, getBroker().getPersistenceKey());
            loadAtRiskStudentAlerts();

            Calendar calendar = Calendar.getInstance(getTimeZone(), getLocale());
            calendar.setTime(m_startDate);

            Timestamp startTime = new Timestamp(calendar.getTimeInMillis());

            calendar.setTime(m_endDate);

            calendar.add(Calendar.DATE, 1);
            calendar.add(Calendar.SECOND, -1);

            Timestamp endTime = new Timestamp(calendar.getTimeInMillis());

            Criteria criteria = new Criteria();
            criteria.addEqualTo(AtRiskResult.COL_DEFINITION_OID, m_ridOid);

            criteria.addAndCriteria(m_studentCriteria);

            QueryByCriteria query = new QueryByCriteria(AtRiskResult.class, criteria);
            query.addOrderByDescending(AtRiskResult.COL_TIMESTAMP);
            query.addOrderByAscending(AtRiskResult.COL_STUDENT_OID);

            PreparedStatement deleteAlertsStatement = null;
            PreparedStatement deleteResultsStatement = null;
            Connection connection = getBroker().borrowConnection();
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                int deletedAlertsCount = 0;
                int deletedResultsCount = 0;

                deleteAlertsStatement = createAlertsDeleteStatement(connection);
                deleteResultsStatement = createResultsDeleteStatement(connection);

                String lastStudentOid = null;
                AtRiskResult latestResult = null;
                while (iterator.hasNext()) {
                    AtRiskResult result = (AtRiskResult) iterator.next();
                    String studentOid = result.getStudentOid();

                    if (latestResult == null || !ObjectUtils.matchStrict(lastStudentOid, studentOid)) {
                        latestResult = result;
                    }

                    Timestamp currentResultTimestamp = result.getTimestamp();
                    if (!currentResultTimestamp.before(startTime) && !currentResultTimestamp.after(endTime)) {
                        if (latestResult != null && latestResult.getTimestamp() == currentResultTimestamp) {
                            StudentAlert atRiskAlert = m_atRiskAlertByStudentOid.get(studentOid);
                            if (atRiskAlert != null) {
                                deleteAlertsStatement.clearParameters();
                                deleteAlertsStatement.setString(1, atRiskAlert.getOid());
                                deleteAlertsStatement.addBatch();

                                deletedAlertsCount++;
                            }
                        }

                        if (!deleteAlertsOnly) {
                            deleteResultsStatement.clearParameters();
                            deleteResultsStatement.setString(1, result.getOid());
                            deleteResultsStatement.addBatch();

                            deletedResultsCount++;
                        }
                    }

                    lastStudentOid = studentOid;
                }

                deleteAlertsStatement.executeBatch();

                if (!deleteAlertsOnly) {
                    deleteResultsStatement.executeBatch();
                }

                if (deletedResultsCount > 0) {
                    AtRiskDefinition atRiskDef =
                            (AtRiskDefinition) getBroker().getBeanByOid(AtRiskDefinition.class, m_ridOid);
                    logMessage("Deleted " + deletedResultsCount + " At Risk Results from \"" + atRiskDef.getName()
                            + "\" created between " + m_startDate + " and " + m_endDate + ".");
                }

                if (deletedAlertsCount > 0) {
                    logMessage("\nDeleted " + deletedAlertsCount + " related At Risk Alerts.");

                    X2ObjectCache cache = AppGlobals.getCache(getBroker().getPersistenceKey());
                    cache.clear(Student.class.getName());
                    cache.clear(StudentAlert.class.getName());
                }

                if (deletedAlertsCount == 0 && deletedResultsCount == 0) {
                    logMessage("No results or alerts to delete for selected date range.");
                }
            } catch (SQLException sqle) {
                logMessage("SqlException: " + sqle.getMessage());
                sqle.printStackTrace();
            } finally {
                iterator.close();
                try {
                    deleteAlertsStatement.close();
                    deleteResultsStatement.close();
                } catch (SQLException se) {
                    // General statement close.
                }

                getBroker().returnConnection();
            }
        } else {
            logMessage("Start date is after end date! Select valid date range and re-run procedure.");
        }
    }

    /**
     * Gets the organization criteria.
     *
     * @param beanClass Class
     * @param parentAccessType OrganizationAccess
     * @param childAccessType OrganizationAccess
     * @return X 2 criteria
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getOrganizationCriteria(Class,
     *      OrganizationAccess, OrganizationAccess)
     */
    @Override
    protected X2Criteria getOrganizationCriteria(Class beanClass,
                                                 OrganizationAccess parentAccessType,
                                                 OrganizationAccess childAccessType) {
        X2Criteria criteria = new X2Criteria();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(beanClass.getName());

        if (table != null) {
            int level = OrganizationManager.getOrganizationLevel(getOrganization(),
                    getBroker().getPersistenceKey());

            Collection<ModelProperty> organizationPaths = getOrganizationPaths(dictionary, level);

            for (ModelProperty modelProperty : organizationPaths) {
                criteria.addAndCriteria(OrganizationManager.getOrganizationAccessCriteria(
                        getOrganization(), modelProperty, parentAccessType, childAccessType));
            }
        } else {
            addNoMatchCriteria(criteria);
        }

        return criteria;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_ridOid = (String) getParameter(AT_RISK_DEFINITION_OID_PARAM);
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        m_studentCriteria = buildStudentCriteria();
    }

    /**
     * Builds student criteria based on user input.
     * 
     * @return Criteria
     */
    private Criteria buildStudentCriteria() {
        Criteria criteria = new Criteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), AtRiskResult.class,
                SisStudent.class, AtRiskResult.COL_STUDENT_OID);

        // When not running for a current selection of students, scope students based on current
        // view.
        if (!queryBy.contains(CURRENT_KEY)) {
            if (isSchoolContext() && getSchool() != null) {
                criteria.addEqualTo(AtRiskResult.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                        getSchool().getOid());
            } else if (getOrganization() != null) {
                criteria.addAndCriteria(getOrganizationCriteria(AtRiskResult.class));
            }
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    AtRiskResult.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        }

        return criteria;
    }

    /**
     * Returns the prepared statement which will delete existing StudentAlert records.
     *
     * @param connection Connection
     * @return PreparedStatement
     * @throws SQLException exception
     */
    private PreparedStatement createAlertsDeleteStatement(Connection connection) throws SQLException {
        String className = StudentAlert.class.getName();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryTable studentAlertTable =
                dictionary.findDataDictionaryTableByClass(className);

        DataDictionaryField oidField =
                dictionary.findDataDictionaryField(className, X2BaseBean.COL_OID);

        StringBuilder sql = new StringBuilder(256);
        sql.append("DELETE FROM ");
        sql.append(studentAlertTable.getDatabaseName());
        sql.append(" WHERE ");
        sql.append(oidField.getDatabaseName());
        sql.append(" = ?");

        return connection.prepareStatement(sql.toString());
    }

    /**
     * Returns the prepared statement which will delete existing AtRiskResult records.
     *
     * @param connection Connection
     * @return PreparedStatement
     * @throws SQLException exception
     */
    private PreparedStatement createResultsDeleteStatement(Connection connection) throws SQLException {
        String className = AtRiskResult.class.getName();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryTable atRiskresultTable =
                dictionary.findDataDictionaryTableByClass(className);

        DataDictionaryField oidField =
                dictionary.findDataDictionaryField(className, X2BaseBean.COL_OID);

        StringBuilder sql = new StringBuilder(256);
        sql.append("DELETE FROM ");
        sql.append(atRiskresultTable.getDatabaseName());
        sql.append(" WHERE ");
        sql.append(oidField.getDatabaseName());
        sql.append(" = ?");

        return connection.prepareStatement(sql.toString());
    }

    /**
     * Returns list of organization paths for AtRiskResult table. This method is similar
     * to OrganizationManager.getOrganizationPaths(..) except that path is hard-coded here.
     *
     * @param dictionary DataDictionary
     * @param level int
     * @return List&lt;ModelProperty&gt;
     */
    private List<ModelProperty> getOrganizationPaths(DataDictionary dictionary, int level) {
        List<ModelProperty> properties = new ArrayList<ModelProperty>();

        String maskedOidFieldString = AT_RISK_RESULT_STUDENT_ORG_PATH;
        List<String> maskedOidFields = StringUtils.convertDelimitedStringToList(maskedOidFieldString, ',', true);

        for (String maskedOidField : maskedOidFields) {
            String organizationPath = OrganizationManager.resolveOrganizationOidField(maskedOidField, level);
            properties.add(new ModelProperty(organizationPath, dictionary));
        }

        return properties;
    }

    /**
     * Loads a map of AtRisk StudentAlert beans keyed on student OID.
     */
    private void loadAtRiskStudentAlerts() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentAlert.COL_AT_RISK_DEFINITION_OID, m_ridOid);
        criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, Integer.valueOf(StudentAlert.AlertType.ATRISK.ordinal()));

        criteria.addAndCriteria(m_studentCriteria);

        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);

        m_atRiskAlertByStudentOid = getBroker().getMapByQuery(query, StudentAlert.COL_STUDENT_OID, 1000);
    }
}
