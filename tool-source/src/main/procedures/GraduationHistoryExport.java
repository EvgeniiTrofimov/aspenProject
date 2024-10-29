/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationChild;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.GraduationRequirement;
import com.x2dev.sis.model.beans.GraduationRequirementHistory;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java class that export graduation history from the graduation history list in the School view.
 * It relies on the current graduation list.
 *
 * @author Follett Software Company
 */

public class GraduationHistoryExport extends ExportJavaSource {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the enumerated "program studies oid" report parameter. The value is an String.
     */
    public static final String PROGRAM_STUDIES_BY_PARAM = "programStudiesOid";

    private static final String EXPORT_RESULT_AS_OF_DATE_PARAM = "statusAsOfDate";
    private static final String INCLUDE_NESTED_REQUIREMENT_PARAM = "includeNestedRequirement";

    private List<String> m_columns;
    private PlainDate m_dateToExport;
    private boolean m_includeNestedRequirement = false;
    private Collection<ModelProperty> m_studentFieldProperties;
    private UserDataContainer m_userData;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {

        DataGrid grid = new DataGrid(m_columns.size());
        exportAll(grid);
        grid.beforeTop();

        return grid;
    }

    /**
     * Set the student related fields on the grid
     *
     * @param grid DataGrid
     * @param historyList GraduationHistoryList
     * @param student SisStudent
     */
    protected void setStudentFields(DataGrid grid, SisStudent student) {
        for (ModelProperty property : m_studentFieldProperties) {
            String fieldId = WebUtils.getLabel(property, false, Boolean.TRUE, true, getLocale());
            Converter converter =
                    ConverterFactory.getConverterForClass(property.getField().getEffectiveJavaType());
            Object fieldValue = student.getFieldValueByProperty(property);

            grid.set(fieldId, converter != null ? converter.javaToString(fieldValue) : (String) fieldValue);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_userData = userData;
        m_includeNestedRequirement = getParameter(INCLUDE_NESTED_REQUIREMENT_PARAM) != null
                ? ((Boolean) getParameter(INCLUDE_NESTED_REQUIREMENT_PARAM)).booleanValue() : false;
        m_dateToExport = (PlainDate) getParameter(EXPORT_RESULT_AS_OF_DATE_PARAM);

        setStudentPropertyField();
    }

    /**
     * Sets the list of student property field
     * This method is protected so that each district can
     */
    protected void setStudentPropertyField() {
        m_studentFieldProperties = new ArrayList<ModelProperty>();
        m_studentFieldProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.REL_PERSON + "." + Person.COL_FIRST_NAME,
                        m_userData.getPersistenceKey()));
        m_studentFieldProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.REL_PERSON + "." + Person.COL_LAST_NAME,
                        m_userData.getPersistenceKey()));
        m_studentFieldProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.COL_LOCAL_ID, m_userData.getPersistenceKey()));
        m_studentFieldProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, m_userData.getPersistenceKey()));
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setIncludeHeaderRow(true);

        /*
         * Load the columns
         */
        m_columns = new ArrayList<String>();
        /*
         * First, the fields for the student
         */
        for (ModelProperty property : m_studentFieldProperties) {
            m_columns.add(WebUtils.getLabel(property, false, Boolean.TRUE, true, getLocale()));
        }

        /*
         * Second, the columns for the requirement information
         */
        m_columns.add(WebUtils.getLabel(new ModelProperty(GraduationRequirement.class, GraduationRequirement.COL_CODE,
                m_userData.getPersistenceKey()), false, Boolean.FALSE, false));

        m_columns.add(WebUtils.getLabel(
                new ModelProperty(GraduationRequirementHistory.class, GraduationRequirementHistory.COL_UNIT_REQUIRED,
                        m_userData.getPersistenceKey()),
                false, Boolean.FALSE, false));

        m_columns.add(WebUtils.getLabel(
                new ModelProperty(GraduationRequirementHistory.class, GraduationRequirementHistory.COL_UNIT_GAINED,
                        m_userData.getPersistenceKey()),
                false, Boolean.FALSE, false));

        m_columns.add(
                WebUtils.getLabel(new ModelProperty(GraduationRequirementHistory.class,
                        GraduationRequirementHistory.COL_UNIT_IN_PROGRESS,
                        m_userData.getPersistenceKey()), false, Boolean.FALSE, false));

        m_columns.add(
                WebUtils.getLabel(new ModelProperty(GraduationRequirementHistory.class,
                        GraduationRequirementHistory.COL_UNIT_WAIVED,
                        m_userData.getPersistenceKey()), false, Boolean.FALSE, false));

        m_columns.add(
                WebUtils.getLabel(new ModelProperty(GraduationRequirementHistory.class,
                        GraduationRequirementHistory.COL_STATUS,
                        m_userData.getPersistenceKey()), false, Boolean.FALSE, false));

        m_columns.add(
                WebUtils.getLabel(new ModelProperty(GraduationRequirementHistory.class,
                        GraduationRequirementHistory.COL_STATUS_AS_OF_DATE,
                        m_userData.getPersistenceKey()), false, Boolean.FALSE, false));
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @SuppressWarnings("rawtypes")
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
    protected List<String> getColumnUserNames() {
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
     * Exports all data based on the criteria
     *
     * @param grid DataGrid
     */
    private void exportAll(DataGrid grid) {

        X2Criteria historyCriteria = new X2Criteria();
        historyCriteria.addEqualTo(GraduationRequirementHistory.COL_CONTEXT_OID,
                m_userData.getCurrentContextOid());
        historyCriteria.addEqualTo(GraduationRequirementHistory.COL_PROGRAM_OID,
                getParameter(PROGRAM_STUDIES_BY_PARAM));

        // School and Organization scope
        if (getSchool() != null) {
            historyCriteria.addEqualTo(GraduationRequirementHistory.COL_SCHOOL_OID, getSchool().getOid());
        } else if (getOrganization() != null) {
            historyCriteria.addEqualTo(
                    GraduationRequirementHistory.REL_SCHOOL + "."
                            + OrganizationChild.ORGANIZATION_OID_COLS[getOrganization().getOrganizationDefinition()
                                    .getLevel()],
                    getOrganization().getOid());
        }

        // Current or forecast date data
        if (new PlainDate().equals(m_dateToExport)) {
            historyCriteria.addIsNull(GraduationRequirementHistory.COL_STATUS_AS_OF_DATE);
        } else {
            historyCriteria.addEqualTo(GraduationRequirementHistory.COL_STATUS_AS_OF_DATE,
                    m_dateToExport);
        }

        QueryByCriteria historyQuery = new QueryByCriteria(GraduationRequirementHistory.class, historyCriteria);
        Map<String, Collection<GraduationRequirementHistory>> historyByStudent = getBroker()
                .getGroupedCollectionByQuery(historyQuery, GraduationRequirementHistory.COL_STUDENT_OID, 1000);

        for (String studentOid : historyByStudent.keySet()) {

            SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);
            for (GraduationRequirementHistory historyForStudent : historyByStudent.get(studentOid)) {

                if (m_includeNestedRequirement || historyForStudent.getRequirementOid() == null
                        || historyForStudent.getRequirement().getParentRequirement() == null) {
                    grid.append();
                    setStudentFields(grid, student);

                    grid.set(WebUtils
                            .getLabel(new ModelProperty(GraduationRequirement.class, GraduationRequirement.COL_CODE,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            historyForStudent.getRequirement() == null ? ""
                                    : historyForStudent.getRequirement().getCode());

                    grid.set(WebUtils
                            .getLabel(new ModelProperty(GraduationRequirementHistory.class,
                                    GraduationRequirementHistory.COL_UNIT_REQUIRED,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            parseDecimalValue(historyForStudent.getUnitRequired()));

                    grid.set(WebUtils
                            .getLabel(new ModelProperty(GraduationRequirementHistory.class,
                                    GraduationRequirementHistory.COL_UNIT_GAINED,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            parseDecimalValue(historyForStudent.getUnitGained()));

                    grid.set(WebUtils
                            .getLabel(new ModelProperty(GraduationRequirementHistory.class,
                                    GraduationRequirementHistory.COL_UNIT_IN_PROGRESS,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            parseDecimalValue(historyForStudent.getUnitInProgress()));

                    grid.set(WebUtils
                            .getLabel(new ModelProperty(GraduationRequirementHistory.class,
                                    GraduationRequirementHistory.COL_UNIT_WAIVED,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            parseDecimalValue(historyForStudent.getUnitWaived()));

                    grid.set(WebUtils
                            .getLabel(new ModelProperty(GraduationRequirementHistory.class,
                                    GraduationRequirementHistory.COL_STATUS,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            parseDecimalValue(historyForStudent.getStatus()) + "%");

                    String statusDateString = new PlainDate().toString();
                    if (!new PlainDate().equals(m_dateToExport)) {
                    	statusDateString = historyForStudent.getStatusAsOfDate().toString();
                    }
                    grid.set(
                            WebUtils.getLabel(new ModelProperty(GraduationRequirementHistory.class,
                                    GraduationRequirementHistory.COL_STATUS_AS_OF_DATE,
                                    m_userData.getPersistenceKey()), false, Boolean.FALSE, false),
                            statusDateString);
                }
            }
        }

        sortGrid(grid);
    }

    /**
     * Parses the decimal value to String
     *
     * @param decimalValue BigDecimal
     * @return String
     */
    private String parseDecimalValue(BigDecimal decimalValue) {
        return decimalValue == null ? "0" : String.valueOf(decimalValue.doubleValue());
    }

    /**
     * Sorts the grid
     *
     * @param grid
     */
    private void sortGrid(DataGrid grid) {
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        String lastName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class, SisStudent.REL_PERSON + "." + Person.COL_LAST_NAME,
                        m_userData.getPersistenceKey()),
                false, Boolean.TRUE, true, getLocale());

        String firstName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class, SisStudent.REL_PERSON + "." + Person.COL_FIRST_NAME,
                        m_userData.getPersistenceKey()),
                false, Boolean.TRUE, true, getLocale());

        List<String> columnsToSort = new LinkedList<String>();
        switch (sort) {

            case 0: // Name View
                columnsToSort.add(lastName);
                columnsToSort.add(firstName);

                break;

            case 1: // ID
                String studentId = WebUtils.getLabel(
                        new ModelProperty(SisStudent.class, SisStudent.COL_LOCAL_ID,
                                m_userData.getPersistenceKey()),
                        false, Boolean.TRUE, true, getLocale());

                columnsToSort.add(studentId);
                break;

            case 2: // grade level
                String studentGradelevel = WebUtils.getLabel(
                        new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                                m_userData.getPersistenceKey()),
                        false, Boolean.TRUE, true, getLocale());

                columnsToSort.add(studentGradelevel);
                columnsToSort.add(lastName);
                columnsToSort.add(firstName);

                break;

            default:
                // No sort specified
                break;
        }
        columnsToSort.add(WebUtils
                .getLabel(new ModelProperty(GraduationRequirement.class, GraduationRequirement.COL_CODE,
                        m_userData.getPersistenceKey()), false, Boolean.FALSE, false));
        grid.sort(columnsToSort, true);
    }
}
