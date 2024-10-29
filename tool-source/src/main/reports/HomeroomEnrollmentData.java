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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Homeroom Enrollment" report. This report shows enrollment totals
 * across the district broken down by homeroom and gender.
 *
 * @author X2 Development Corporation
 */
public class HomeroomEnrollmentData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String COL_FEMALE_TOTAL = "FEMALE_TOTAL";
    private static final String COL_HOMEROOM_TOTAL = "HOMEROOM_TOTAL";
    private static final String COL_MALE_TOTAL = "MALE_TOTAL";

    private Map m_staffLookup;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid statistics = null;

        try {

            StringBuilder sql = new StringBuilder(1500);

            sql.append("SELECT SKL_SCHOOL_ID, SKL_SCHOOL_NAME, STD_HOMEROOM, PSN_GENDER_CODE, ");
            sql.append("COUNT(PSN_GENDER_CODE) AS TOTAL ");
            sql.append("FROM STUDENT ");
            sql.append("INNER JOIN SCHOOL ON STD_SKL_OID = SKL_OID ");
            sql.append("INNER JOIN PERSON ON STD_PSN_OID = PSN_OID ");
            sql.append("WHERE " + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));
            sql.append("AND (SKL_INACTIVE_IND = '0' OR SKL_INACTIVE_IND IS NULL) ");
            if (isSchoolContext()) {
                sql.append("AND SKL_OID = '" + getSchool().getOid() + "'");
            } else {
                int level = getOrganization().getOrganizationDefinition().getLevel();
                sql.append(" AND STD_ORG_OID_");
                sql.append(level + 1);
                sql.append(" = '");
                sql.append(getOrganization().getOid());
                sql.append("'");

            }

            sql.append("GROUP BY SKL_SCHOOL_ID, SKL_SCHOOL_NAME, STD_HOMEROOM, PSN_GENDER_CODE ");
            sql.append("ORDER BY SKL_SCHOOL_ID, SKL_SCHOOL_NAME, STD_HOMEROOM, PSN_GENDER_CODE ");

            Connection connection = getBroker().borrowConnection();

            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql.toString());

            statistics = formatData(resultSet);
            statistics.beforeTop();

            resultSet.close();
            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return statistics;
    }

    /**
     * Create a DataGrid from the passed ResultSet to use as the report data source. The ResultSet
     * is structured as follows (and is expected to be ordered by school ID and homeroom):
     *
     * <pre>
     * School | Homeroom | Gender | Total
     * -------+----------+--------+-------
     * HS | 100 | F | 12
     * HS | 100 | M | 13
     * HS | 105 | M | 20
     * MS | 213 | F | 15
     * MS | 213 | M | 16
     * MS | 213 | null | 1
     * </pre>
     *
     * The resulting DataGrid is structured as follows:
     *
     * <pre>
     * School | Homeroom | Staff | Males | Females | Total
     * -------+----------+----------------+-------+---------+-------
     * HS | 100 | (staff bean x) | 13 | 12 | 25
     * HS | 105 | (staff bean y) | 20 | 0 | 20
     * MS | 213 | (staff bean z) | 16 | 15 | 32
     * </pre>
     *
     * Note - the "school" column is really two columns: school ID and school name.
     *
     * @param resultSet ResultSet
     * @return ReportDataGrid
     * @throws SQLException exception
     */
    private ReportDataGrid formatData(ResultSet resultSet) throws SQLException {
        ReportDataGrid data = new ReportDataGrid(1000, 7);

        String lastHomeroom = null;
        String lastSchoolId = null;
        while (resultSet.next()) {
            String homeroom = resultSet.getString("STD_HOMEROOM");
            String schoolId = resultSet.getString("SKL_SCHOOL_ID");

            if (schoolId != null) {
                if (homeroom == null || homeroom.length() == 0) {
                    homeroom = "NO HOMEROOM";
                }

                if (!homeroom.equals(lastHomeroom) || !schoolId.equals(lastSchoolId)) {
                    data.append();

                    data.set("SKL_SCHOOL_ID", schoolId);
                    data.set("SKL_SCHOOL_NAME", resultSet.getString("SKL_SCHOOL_NAME"));
                    data.set("STD_HOMEROOM", homeroom);
                    data.set("STAFF", getStaffMember(schoolId, homeroom));

                    data.set(COL_MALE_TOTAL, Integer.valueOf(0));
                    data.set(COL_FEMALE_TOTAL, Integer.valueOf(0));
                    data.set(COL_HOMEROOM_TOTAL, Integer.valueOf(0));
                }

                int total = resultSet.getInt("TOTAL");

                /*
                 * Always update the homeroom total no matter what the gender code is. It is
                 * possible that some students might have a gender code other than "M" or "F".
                 */
                int homeroomTotal = ((Integer) data.get(COL_HOMEROOM_TOTAL)).intValue();
                data.set(COL_HOMEROOM_TOTAL, Integer.valueOf(homeroomTotal + total));

                /*
                 * Conditionally update the male/female totals based on the gender code.
                 */
                String genderCode = resultSet.getString("PSN_GENDER_CODE");
                if ("M".equalsIgnoreCase(genderCode)) {
                    int maleTotal = ((Integer) data.get(COL_MALE_TOTAL)).intValue();
                    data.set(COL_MALE_TOTAL, Integer.valueOf(maleTotal + total));
                } else if ("F".equalsIgnoreCase(genderCode)) {
                    int femaleTotal = ((Integer) data.get(COL_FEMALE_TOTAL)).intValue();
                    data.set(COL_FEMALE_TOTAL, Integer.valueOf(femaleTotal + total));
                }

                lastHomeroom = homeroom;
                lastSchoolId = schoolId;
            }
        }

        return data;
    }

    /**
     * Returns the staff member associated with the passed school/homeroom combination.
     *
     * @param schoolId String
     * @param homeroom String
     * @return Staff
     */
    private SisStaff getStaffMember(String schoolId, String homeroom) {
        if (m_staffLookup == null) {
            m_staffLookup = new HashMap(500);

            /*
             * Only staff members that are associated with a school and have a homeroom need to be
             * added to the map.
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(SisStaff.COL_HOMEROOM, getBroker().getPersistenceKey());

            if (isSchoolContext()) {
                criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                Criteria subCriteria = new Criteria();
                subCriteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
                SubQuery subQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, subCriteria);
                criteria.addIn(SisStaff.COL_SCHOOL_OID, subQuery);
            }

            // D-12294, limit staff names to active only so the report doesn't show inactive staff
            // names
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);

            QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
            QueryIterator staffIterator = getBroker().getIteratorByQuery(query);
            try {
                while (staffIterator.hasNext()) {
                    SisStaff staff = (SisStaff) staffIterator.next();
                    m_staffLookup.put(staff.getSchool().getSchoolId().trim()
                            + staff.getHomeroom().trim(), staff);
                }
            } finally {
                staffIterator.close();
            }
        }

        return (SisStaff) m_staffLookup.get(schoolId.trim() + homeroom.trim());
    }
}
