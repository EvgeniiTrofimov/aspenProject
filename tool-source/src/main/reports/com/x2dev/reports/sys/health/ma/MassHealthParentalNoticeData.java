/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2002-2018 Follett School Solutions.
 * All rights reserved.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.health.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the Mass Health Parental Notice report.
 *
 * @author X2 Development Corporation
 */
public class MassHealthParentalNoticeData extends ReportJavaSourceNet {

    /**
     * Class intended to pass child data to 'MassHealthChildrenTable' sub-report.
     */
    public static class ChildData {
        private String childName;
        private String dateOfBirth;
        private String sasID;

        /**
         * Instantiates a new child data.
         */
        public ChildData() {

        }

        /**
         * Instantiates a new child data.
         *
         * @param childName String
         * @param dateOfBirth String
         * @param sasId String
         */
        public ChildData(String childName, String dateOfBirth, String sasId) {
            this.childName = childName;
            this.dateOfBirth = dateOfBirth;
            this.sasID = sasId;
        }

        /**
         * Gets the child name.
         *
         * @return String
         */
        public String getChildName() {
            return childName;
        }

        /**
         * Gets the date of birth.
         *
         * @return String
         */
        public String getDateOfBirth() {
            return dateOfBirth;
        }

        /**
         * Gets the sas ID.
         *
         * @return String
         */
        public String getSasID() {
            return sasID;
        }

        /**
         * Sets the child name.
         *
         * @param childName void
         */
        public void setChildName(String childName) {
            this.childName = childName;
        }

        /**
         * Sets the date of birth.
         *
         * @param dateOfBirth void
         */
        public void setDateOfBirth(String dateOfBirth) {
            this.dateOfBirth = dateOfBirth;
        }

        /**
         * Sets the sas ID.
         *
         * @param sasID void
         */
        public void setSasID(String sasID) {
            this.sasID = sasID;
        }
    }

    protected static final String ALIAS_DOE_DISTRICT_ID = "DOE District ID";

    protected static final String CHILDREN_LIST_DATA = "childrenList";
    protected static final String CHILDREN_TABLE_FORMAT = "childrenTableFormat";
    protected static final String STRING_DATE_FORMAT = "MM-dd-yyyy";

    // ReportDataGrid column constants for the main report
    protected static final String COL_PARAMETER_MAP = "parameters";
    protected static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    protected static final String COL_SUBREPORT_FORMAT = "format";

    /**
     * Sub-report IDs
     */
    protected static final String MASS_HEALTH_CHILD_LIST_PREFFIX = "MASS-HLT-CHLD-";
    protected static final String MASS_HEALTH_NOTICE_NAME_PREFFIX = "MASS-HLT-NTC-";

    // Report parameter constants
    protected static final String PARAM_DISTRICT_ID = "districtId";
    protected static final String PARAM_DISTRICT_CONTACT_PERSON = "districtContactPerson";
    protected static final String PARAM_DISTRICT_CONTACT_PERSON_EMAIL = "districtContactPersonEmail";
    protected static final String PARAM_PRINT_FOR_ALL_SIBLINGS = "printForAllSiblings";
    protected static final String PARAM_REPORT_LANGUAGE = "reportLanguage";

    /**
     * member variables
     */
    private DateFormat m_dateFormat;
    private SisStudent m_currentStudent;
    private Map<String, byte[]> m_subreportFormats;

    /**
     * Gather data.
     *
     * @return JRDataSource JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        QueryByCriteria query = null;
        ReportDataGrid grid = new ReportDataGrid();

        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();
        if (m_currentStudent != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            // report designed only for current student case
            criteria.addEqualTo(X2BaseBean.COL_OID, "--Invalid--");
        }


        query = new QueryByCriteria(SisStudent.class, criteria);

        SisStudent student = (SisStudent) getBroker().getBeanByQuery(query);

        prepareMassHealthNotice(grid, student);

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize runtime global variables.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    public void initialize() {
        m_dateFormat = new SimpleDateFormat(STRING_DATE_FORMAT, getLocale());
        m_subreportFormats = new HashMap<String, byte[]>();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for
         * just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Takes passed language code and converts it to MassHealth letter language
     * code.
     *
     * @param lang String
     * @return code
     */
    protected String getMassHealthNoticeLangCode(String lang) {
        String code = null;
        String selectedLanguage = (String) getParameter(PARAM_REPORT_LANGUAGE);
        if (null != selectedLanguage && !selectedLanguage.isEmpty()) {
            return selectedLanguage;
        }
        if (lang == null) {
            return "ENG";
        }
        lang = lang.toUpperCase();

        // Find code based on department
        switch (lang) {
            case "ENGLISH":
                code = "ENG";
                break;

            case "CHINESE":
                code = "CHI";
                break;

            case "HAITIAN":
            case "HAITIAN CREOLE":
                code = "CRP";
                break;

            case "PORTUGUESE":
                code = "POR";
                break;

            case "SPANISH":
                code = "SPA";
                break;

        }

        return code;
    }

    /**
     * Return the date formatter.
     */
    protected DateFormat getDateFormat() {
        return m_dateFormat;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     *
     * @return Object
     */
    private byte[] getSubreportFormat(String pageId) {
        if (m_subreportFormats.containsKey(pageId)) {
            return m_subreportFormats.get(pageId);
        }
        Report report = ReportUtils.getReport(pageId, getBroker());
        byte[] compiledReport = report.getCompiledFormat();
        m_subreportFormats.put(pageId, compiledReport);

        return compiledReport;
    }

    /**
     * Returns siblings(student) data of current student.
     *
     * @param student SisStudent
     * @return List
     */
    private List<SisStudent> getSiblingsList(SisStudent student) {
        List<SisStudent> siblings = new ArrayList<SisStudent>();
        X2Criteria criteria = new X2Criteria();
        // Get all students with same address oid as passed student
        String addressOid = student.getPerson().getPhysicalAddressOid();
        criteria.addEqualTo(SisStudent.REL_PERSON + PATH_DELIMITER
                + SisPerson.COL_PHYSICAL_ADDRESS_OID, addressOid);
        criteria.addNotEqualTo(X2BaseBean.COL_OID, student.getOid());

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(SisStudent.COL_GRADE_LEVEL);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisStudent std = (SisStudent) iterator.next();
                if (std != null && "Active".equals(std.getEnrollmentStatus())) {
                    siblings.add(std);
                }
            }
        } finally {
            iterator.close();
        }
        return siblings;
    }

    /**
     * Fills children data to report.
     *
     * @param student SisStudent
     * @param parameters Map
     */
    protected void fillChildrenTable(SisStudent student, Map<String, Object> parameters) {
        List<ChildData> childrenData = new ArrayList<>();
        childrenData.add(new ChildData(student.getNameView(), m_dateFormat.format(student
                .getPerson().getDob()), student.getStateId()));
        Boolean siblingFlag = (Boolean) getParameter(PARAM_PRINT_FOR_ALL_SIBLINGS);
        if (siblingFlag != null && siblingFlag.booleanValue()) {
            List<SisStudent> siblings = getSiblingsList(student);
            for (SisStudent sibling : siblings) {
                childrenData
                        .add(new ChildData(sibling.getNameView(), m_dateFormat.format(sibling
                                .getPerson().getDob()), sibling.getStateId()));
            }
        }

        parameters.put(CHILDREN_LIST_DATA, childrenData);
        parameters.put(CHILDREN_TABLE_FORMAT,
                getSubreportFormat(MASS_HEALTH_CHILD_LIST_PREFFIX
                        + getMassHealthNoticeLangCode(student
                                .getHomeLanguageCode())));
    }

    /**
     * Prepares 'MassHealthNotice' page.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     */
    protected void prepareMassHealthNotice(ReportDataGrid grid, SisStudent student) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                getExtendedDictionary(), getBroker().getPersistenceKey());
        JRDataSource dataSource = new SimpleBeanDataSource(student, dictionary,
                getLocale());
        Map<String, Object> rowParameters = new HashMap<String, Object>(getParameters());
        grid.append();

        if (student.getActiveIep() != null
                && student.getActiveIep().getStaff() != null) {
            rowParameters.put(PARAM_DISTRICT_CONTACT_PERSON, student.getActiveIep().getStaff().getNameView());
            rowParameters.put(PARAM_DISTRICT_CONTACT_PERSON_EMAIL,
                    student.getActiveIep().getStaff().getPerson().getEmail01());
        }
        if (student.getSchool() != null
                && student.getSchool().getOrganization1() != null
                && student.getSchool().getOrganization1().getAddress() != null) {
            String districtId =
                    (String) student.getSchool().getOrganization1().getFieldValueByAlias(ALIAS_DOE_DISTRICT_ID);
            if (!StringUtils.isEmpty(districtId)) {
                rowParameters.put(PARAM_DISTRICT_ID, districtId);
            } else {
                rowParameters.put(PARAM_DISTRICT_ID, "");
            }
        } else {
            rowParameters.put(PARAM_DISTRICT_ID, "");
        }

        String massHealthNoticLangCode = getMassHealthNoticeLangCode(student
                .getHomeLanguageCode());
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT,
                getSubreportFormat(MASS_HEALTH_NOTICE_NAME_PREFFIX
                        + massHealthNoticLangCode));
        fillChildrenTable(student, rowParameters);
        grid.set(COL_PARAMETER_MAP, rowParameters);

    }
}
