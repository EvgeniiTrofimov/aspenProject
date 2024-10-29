/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.widgets.home.AuditHelper;
import com.follett.fsc.core.k12.web.widgets.home.ChangedInfoQuery;
import com.follett.fsc.core.k12.web.widgets.home.DateRange;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.tools.procedures.GraduationHistoryCreation;
import com.x2dev.utils.StringUtils;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;

/**
 * Standard procedure to create graduation history records.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class StandardGraduationHistoryCreation extends GraduationHistoryCreation {

    private static final String ASSESSMENT_IDS_PARAM = "asdIDs";
    private static final String STUDENT_FIELD_IDS_PARAM = "stdFieldIDs";

    /**
     * Overrides the method for Standard Graduation History to add the student assessment tracking
     * if needed
     */
    @Override
    protected Collection<String> getStudentsWithChanges(int days,
                                                        UserDataContainer userData,
                                                        String schoolOid) {
        Collection<String> changedStudents =
                super.getStudentsWithChanges(days, userData, schoolOid);

        School school = getBroker().getBeanByOid(School.class, schoolOid);

        /*
         * Check whether there is any student with assessment changes
         */
        if (!StringUtils.isEmpty((String) getParameter(ASSESSMENT_IDS_PARAM))) {
            String asdOidsAsString = getAssessmentDefinitionOids();

            List<StudentAssessmentChangeQueryImpl> changeList = AuditHelper.findChangedStudent(school,
                    String.valueOf(days),
                    0,
                    null,
                    userData,
                    new ModelBroker(getPrivilegeSet()),
                    new StudentAssessmentChangeQueryImpl(getOrganization(), asdOidsAsString, String.valueOf(days)));

            for (StudentAssessmentChangeQueryImpl info : changeList) {
                String studentOid = info.getStudentOid();

                changedStudents.add(studentOid);
            }
        }

        /*
         * Check whether there is any student with student fields change
         */
        if (!StringUtils.isEmpty((String) getParameter(STUDENT_FIELD_IDS_PARAM))) {

            Collection<String> studentFieldIds =
                    StringUtils.convertDelimitedStringToList((String) getParameter(STUDENT_FIELD_IDS_PARAM), ",", true);

            List<StudentFieldChangeQueryImpl> changeList = AuditHelper.findChangedStudent(school,
                    String.valueOf(days),
                    0,
                    null,
                    userData,
                    new ModelBroker(getPrivilegeSet()),
                    new StudentFieldChangeQueryImpl(getOrganization(), studentFieldIds, String.valueOf(days)));

            for (StudentFieldChangeQueryImpl info : changeList) {
                String studentOid = info.getStudentOid();

                changedStudents.add(studentOid);
            }
        }

        return changedStudents;
    }

    /**
     * Returns the list of assessment definition OIDs in delimited string
     *
     * @return String
     */
    private String getAssessmentDefinitionOids() {
        Collection<String> assessmentDefinitionIds =
                StringUtils.convertDelimitedStringToList((String) getParameter(ASSESSMENT_IDS_PARAM), ",",
                        true);

        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addIn(AssessmentDefinition.COL_ID, assessmentDefinitionIds);

        SubQuery asdSubQuery = new SubQuery(AssessmentDefinition.class, X2BaseBean.COL_OID, asdCriteria);
        Collection<String> asdOids = getBroker().getSubQueryCollectionByQuery(asdSubQuery);

        StringBuffer asdOidsAsString = new StringBuffer();

        for (String oid : asdOids) {
            if (asdOidsAsString.length() > 0) {
                asdOidsAsString.append(", ");
            }
            asdOidsAsString.append("'" + oid + "'");
        }
        return asdOidsAsString.toString();
    }

    /**
     * A container class for information relevant to a changed student assessment.
     * This holds the student, student assessment and data audit information relevant to the change.
     */
    public class StudentAssessmentChangeQueryImpl extends ChangedInfoQuery {

        private String m_assessmentDefinitionOids;
        private String m_assessmentOid;
        private String m_auditOid;
        private Organization m_org;
        private String m_studentOid;
        private DateRange m_dateRange;

        /**
         * Constructor - sets instance variable for organization.
         *
         * @param org Organization
         */
        public StudentAssessmentChangeQueryImpl(Organization org) {
            m_org = org;
        }

        /**
         * Constructor - sets instance variable for organization.
         *
         * @param org Organization
         * @param asdIds String
         * @param days String
         */
        public StudentAssessmentChangeQueryImpl(Organization org, String asdIds, String days) {
            m_org = org;
            m_assessmentDefinitionOids = asdIds;
            m_dateRange = GraduationHistoryCreation.getDateRange(days);
        }

        @Override
        public Object getObject(ResultSet rs) throws SQLException {
            StudentAssessmentChangeQueryImpl changeEventInfo = new StudentAssessmentChangeQueryImpl(m_org);
            changeEventInfo.m_studentOid = rs.getString(1);
            changeEventInfo.m_assessmentOid = rs.getString(2);
            changeEventInfo.m_auditOid = rs.getString(3);
            return changeEventInfo;
        }

        @Override
        public PreparedStatement getQuery(School school, DateRange dateRange, String recordSetOid, Connection conn)
                throws SQLException {

            StringBuffer sql = new StringBuffer();
            sql.append("SELECT STD_OID, ASM_OID, DAU_OID, DAU_TIMESTAMP ");
            sql.append("FROM STUDENT ");
            sql.append("INNER JOIN STUDENT_ASSESSMENT ON ASM_STD_OID=STD_OID ");
            sql.append("INNER JOIN DATA_AUDIT ON DAU_TBL_OID=");
            sql.append("'");
            sql.append("tblStdAssess");
            sql.append("' ");
            sql.append("AND  DAU_OBJ_OID=ASM_OID ");
            sql.append("WHERE  STD_SKL_OID=");
            sql.append("? "); // school.getOid()
            sql.append("AND ASM_ASD_OID IN ( ");

            Collection<String> asdOids =
                    StringUtils.convertDelimitedStringToList(m_assessmentDefinitionOids, ",", true);
            for (int i = 0; i < asdOids.size(); i++) {
                if (i > 0) {
                    sql.append(",");
                }
                sql.append("?");
            }

            sql.append(") ");
            sql.append("AND  DAU_TIMESTAMP >= ?"); // dateRange.lower
            sql.append(" ");
            sql.append(" UNION ALL ");
            sql.append(getSQLForDeletedStudentAssessment(school, m_dateRange));
            sql.append("ORDER BY DAU_TIMESTAMP DESC");

            PreparedStatement stmt = conn.prepareStatement(sql.toString());
            int index = 0;
            stmt.setString(++index, school.getOid());

            for (String asd : asdOids) {
                stmt.setString(++index, asd);
            }

            stmt.setLong(++index, dateRange.lower);

            /*
             * getSQLForDeleletedStduentAssessment()
             */
            stmt.setString(++index, school.getOid());

            for (String asdOidWithQuote : asdOids) {
                String asdOid = asdOidWithQuote.substring(1, asdOidWithQuote.length() - 1);
                stmt.setString(++index, asdOid);
            }

            stmt.setLong(++index, dateRange.lower);

            return stmt;
        }

        /**
         * Returns the auditOid
         *
         * @return String
         */
        public String getAuditOid() {
            return m_auditOid;
        }

        /**
         * Returns the assessment OID
         *
         * @return String
         */
        public String getAssessmentOid() {
            return m_assessmentOid;
        }

        /**
         * Returns the studentOid
         *
         * @return String
         */
        public String getStudentOid() {
            return m_studentOid;
        }

        /**
         * Returns students with assessment deleted. This is parameterized SQL with the following
         * parameters:
         * <ul>
         * <li>1=String, school.getOid()
         * <li>2 through N=String, m_assessmentDefinitionOids
         * <li>N+1=long, dateRange.lower
         * </ul>
         *
         * @param school School
         * @param dateRange DateRange
         *
         * @return String
         */
        private String getSQLForDeletedStudentAssessment(School school, DateRange dateRange) {
            StringBuffer sql = new StringBuffer();
            sql.append("SELECT STD_OID, DAU_OBJ_OID, DAU_OID, DAU_TIMESTAMP ");
            sql.append("FROM STUDENT ");
            sql.append("INNER JOIN DATA_AUDIT ON DAU_TBL_OID=");
            sql.append("'");
            sql.append("tblStdAssess");
            sql.append("' ");
            sql.append("WHERE  STD_SKL_OID=");
            sql.append("? "); // school.getOid()
            sql.append("AND DAU_CHANGE_TYPE = '2' ");

            Collection<String> asdOids =
                    StringUtils.convertDelimitedStringToList(m_assessmentDefinitionOids, ",", true);

            sql.append(" AND ( ");
            for (int i = 0; i < asdOids.size(); i++) {
                if (i > 0) {
                    sql.append(" OR ");
                }
                sql.append(" DAU_CHANGE_DEFINITION LIKE ");
                sql.append("'% ?%'");
            }
            sql.append(" ) ");

            switch (DatabaseOptimizerFactory.getDatabaseOptimizer(
                    school.getPersistenceKey()).getPlatform()) {
                case DatabaseOptimizerFactory.MYSQL:
                    sql.append("AND DAU_CHANGE_DEFINITION LIKE CONCAT('%', STD_OID, '%')");
                    break;

                case DatabaseOptimizerFactory.SQLSERVER:
                    sql.append("AND DAU_CHANGE_DEFINITION LIKE '%' + STD_OID + '%'");
                    break;
            }

            sql.append("AND DAU_TIMESTAMP >= ?"); // dateRange.lower
            sql.append(" ");

            return sql.toString();
        }
    }

    /**
     * A container class for information relevant to a changed student for certain fields.
     * This holds the student and data audit information relevant to the change.
     */
    public class StudentFieldChangeQueryImpl extends ChangedInfoQuery {

        private Collection<String> m_studentFieldIds;
        private String m_auditOid;
        private Organization m_org;
        private String m_studentOid;
        private DateRange m_dateRange;

        /**
         * Constructor - sets instance variable for organization.
         *
         * @param org Organization
         */
        public StudentFieldChangeQueryImpl(Organization org) {
            m_org = org;
        }

        /**
         * Constructor - sets instance variable for organization.
         *
         * @param org Organization
         * @param stdFieldIds Collection<String>
         * @param days
         */
        public StudentFieldChangeQueryImpl(Organization org, Collection<String> stdFieldIds, String days) {
            m_org = org;
            m_studentFieldIds = stdFieldIds;
            m_dateRange = GraduationHistoryCreation.getDateRange(days);
        }

        @Override
        public Object getObject(ResultSet rs) throws SQLException {
            StudentFieldChangeQueryImpl changeInfo = new StudentFieldChangeQueryImpl(m_org);
            changeInfo.m_studentOid = rs.getString(1);
            changeInfo.m_auditOid = rs.getString(2);
            return changeInfo;
        }

        @Override
        public PreparedStatement getQuery(School school, DateRange dateRange, String recordSetOid, Connection conn)
                throws SQLException {

            StringBuffer sql = new StringBuffer();
            sql.append("SELECT STD_OID, DAU_OID, DAU_TIMESTAMP ");
            sql.append("FROM STUDENT ");
            sql.append("INNER JOIN DATA_AUDIT ON DAU_TBL_OID=");
            sql.append("'");
            sql.append("tblStudent");
            sql.append("' ");
            sql.append("AND  DAU_OBJ_OID=STD_OID ");
            sql.append("WHERE  STD_SKL_OID=");
            sql.append("? "); // school.getOid()
            sql.append("AND  DAU_TIMESTAMP >= ?"); // dateRange.lower

            sql.append(" AND ( ");
            for (int i = 0; i < m_studentFieldIds.size(); i++) {
                if (i > 0) {
                    sql.append(" OR ");
                }
                sql.append(" DAU_CHANGE_DEFINITION LIKE ");
                sql.append("'%?%'");
            }
            sql.append(" ) ");

            sql.append(" ");
            sql.append("ORDER BY DAU_TIMESTAMP DESC");

            PreparedStatement stmt = conn.prepareStatement(sql.toString());
            int index = 0;
            stmt.setString(++index, school.getOid());
            stmt.setLong(++index, dateRange.lower);

            for (String studentFieldId : m_studentFieldIds) {
                stmt.setString(++index, studentFieldId);
            }

            return stmt;
        }

        /**
         * Returns the auditOid
         *
         * @return String
         */
        public String getAuditOid() {
            return m_auditOid;
        }

        /**
         * Returns the studentOid
         *
         * @return String
         */
        public String getStudentOid() {
            return m_studentOid;
        }
    }
}
