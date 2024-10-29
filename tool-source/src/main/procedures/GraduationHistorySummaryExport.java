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
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.web.graduation.GraduationHistoryList;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java class that export graduation history from the graduation history list in the School view.
 * It relies on the current graduation list.
 *
 * @author Follett Software Company
 */

public class GraduationHistorySummaryExport extends ExportJavaSource {
    /**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    private List<String> m_columns;
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
        GraduationHistoryList historyList = (GraduationHistoryList) getUserData().getCurrentList();

        exportFromCurrentList(grid, historyList);

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
    private void setStudentFields(DataGrid grid, GraduationHistoryList historyList, SisStudent student) {
        List<ModelProperty> fieldProperties = historyList.getProperties();
        for (ModelProperty property : fieldProperties) {
            String fieldId = WebUtils.getLabel(property, false, Boolean.TRUE, true, getLocale());
            Converter converter =
                    ConverterFactory.getConverterForClass(property.getField().getEffectiveJavaType());
            Object fieldValue = student.getFieldValueByProperty(property);

            grid.set(fieldId, converter != null ? converter.javaToString(fieldValue) : (String) fieldValue);
        }

        String noteColumn = WebUtils.getMessages(m_userData).getMessage("label.schedule.grh.list.note");
        grid.set(noteColumn, student.getGraduationHistoryNotes());

        String totalCreditsColumnn =
                WebUtils.getMessages(m_userData).getMessage("label.schedule.grh.list.totalCredits");
        grid.set(totalCreditsColumnn, String.valueOf(historyList.getTotalCreditsGained(student.getOid())));

        String overallStatusColumn =
                WebUtils.getMessages(m_userData).getMessage("label.graduation.satisfiedStatus");
        grid.set(overallStatusColumn, historyList.getRequirementStatus(student.getOid(), ""));
    }

    /**
     * Exports the results from the current list
     *
     * @param grid DataGrid
     * @param historyList GraduationHistoryList
     */
    private void exportFromCurrentList(DataGrid grid, GraduationHistoryList historyList) {
        X2Criteria studentCriteria = historyList.getCustomCriteria();

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        /*
         * Now sort the staff members based on the user's selection.
         */
        applyUserSort(studentQuery, (String) getParameter(SORT_PARAM));

        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

        for (SisStudent student : students) {

            grid.append();
            setStudentFields(grid, historyList, student);

            Map<String, String> requirementMap = historyList.getRequirementName();
            for (String requirementOid : requirementMap.keySet()) {
                grid.set(requirementMap.get(requirementOid),
                        historyList.getRequirementStatus(student.getOid(), requirementOid));
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_userData = userData;
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
         * First, the fields from the current fieldset
         */
        List<ModelProperty> fieldProperties = getUserData().getCurrentList().getProperties();
        for (ModelProperty property : fieldProperties) {
            m_columns.add(WebUtils.getLabel(property, false, Boolean.TRUE, true, getLocale()));
        }

        /*
         * Second, the note, total credits and the overall status fields
         */
        m_columns.add(WebUtils.getMessages(m_userData).getMessage("label.schedule.grh.list.note"));
        m_columns.add(WebUtils.getMessages(m_userData).getMessage("label.schedule.grh.list.totalCredits"));
        m_columns.add(WebUtils.getMessages(m_userData).getMessage("label.graduation.satisfiedStatus"));

        /*
         * Third, the list of requirement codes
         */
        GraduationHistoryList historyList = (GraduationHistoryList) getUserData().getCurrentList();
        Collection<String> requirementCodes = historyList.getRequirementName().values();
        for (String requirementCode : requirementCodes) {
            m_columns.add(requirementCode);
        }
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
     *
     * Returns the userData.
     *
     * @return UserDataContainer
     */
    private UserDataContainer getUserData() {
        return m_userData;
    }
}
