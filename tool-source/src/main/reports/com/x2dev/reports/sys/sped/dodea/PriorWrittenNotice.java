/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.dodea;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.WorkflowProgressForm;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.Hashtable;
import java.util.logging.Level;

public class PriorWrittenNotice extends BaseFormReportJavaSource {
    /**
     * Name for the "evaluations data source" report parameter. The value is a BeanDataSource
     * object.
     */
    public static final String EVALS_DISPLAY_PARAM = "evalsDisplay";
    public static final String EVALS_DISPLAY_PARAM2 = "evalsDisplay2";
    public static final String EVALS_DISPLAY_PARAM3 = "evalsDisplay3";
    public static final String COL_MEETING_DATE = "meetingDate";
    
    public static final String IEP_MEETING_FORM_ID = "MTG";
    
    /**
     * Name for the "reference description lookup" parameter. The value is a Map of a reference
     * code's
     * long description (field D001) keyed to the code. This map in in a Map keyed to the reference
     * table OID.
     */
    public static final String REFERENCE_LOOKUP_MAP_PARAM = "referenceLookupMap";

    /**
     * Name for the "report" input parameter. The value is an Integer.
     */
    public static final String REPORT_PARAM = "report";

    /**
     * Data for the PWN page, identical to the main report data.
     */
    public static final String ALIAS_ACTION_RESULT = "csc-action-result";
    public static final String ALIAS_DESCRIBE_CHOICE = "csc-describe-choice";
    public static final String ALIAS_DESCRIBE_CHOICE_PWN = "csc-describe-choice-pwn";
    public static final String ALIAS_DESCRIBE_EVAL = "csc-describe-eval";
    public static final String ALIAS_DESCRIBE_EVAL_PWN = "csc-describe-eval-pwn";
    public static final String ALIAS_PARENT_INFO = "csc-meet-parentinfo-pwn";
    public static final String ALIAS_NOTICE_DATE = "csc-iepmtg-notice-date1";

    
    private Hashtable<String, String> m_evalDescription = new Hashtable<String, String>();

    /**
     * @see com.x2dev.sis.tools.reports.MultiPageFormReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        net.sf.jasperreports3.engine.JRDataSource dataSource = null;
        boolean isEmpty = getIsEmpty();
        if (!isEmpty) {
            createactionMap();
            loadEvalsDisplay();
            setMeetingDate();
            dataSource = new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        } else {
            // Return an empty report data grid so the report will be empty.
            dataSource = new ReportDataGrid();
        }
        return dataSource;
    }

    /**
     * Determine if the storage bean is empty.
     * 
     * @return true if the bean is empty (contains no date in all of the data fields).
     */
    private boolean getIsEmpty() {
        X2BaseBean storageBean = getFormStorage();
        String v1 = (String) storageBean.getFieldValueByAlias(ALIAS_ACTION_RESULT, getDictionary());
        String v2 = (String) storageBean.getFieldValueByAlias(ALIAS_DESCRIBE_CHOICE, getDictionary());
        String v3 = (String) storageBean.getFieldValueByAlias(ALIAS_DESCRIBE_CHOICE_PWN, getDictionary());
        String v4 = (String) storageBean.getFieldValueByAlias(ALIAS_DESCRIBE_EVAL, getDictionary());
        String v5 = (String) storageBean.getFieldValueByAlias(ALIAS_DESCRIBE_EVAL_PWN, getDictionary());
        String v6 = (String) storageBean.getFieldValueByAlias(ALIAS_PARENT_INFO, getDictionary());
        String v7 = (String) storageBean.getFieldValueByAlias(ALIAS_NOTICE_DATE, getDictionary());
        return StringUtils.isEmpty(v1) && StringUtils.isEmpty(v2) && StringUtils.isEmpty(v3) && 
               StringUtils.isEmpty(v4) && StringUtils.isEmpty(v5) && StringUtils.isEmpty(v6) &&
               StringUtils.isEmpty(v7);
    }

    /**
     * Get the meeting date, or use Today. 
     */
    private void setMeetingDate() {
        IepMeeting meeting = findMeeting();
        Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
        if (meeting != null && meeting.getDate() != null) {
            addParameter(COL_MEETING_DATE, converter.javaToString(meeting.getDate()));
        } else {
            addParameter(COL_MEETING_DATE, converter.javaToString(new PlainDate()));
        }
    }

    // Returns the SQL query building the Hash Map for Evaluation and Action
    private void createactionMap()
    {
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_DESCRIBE_EVAL);
        if (field != null && field.getReferenceTableOid() != null) {
            
            // AppGlobals.getLog().severe(" Start SQL" );
            String query = new String(
                    "SELECT  " +
                            "RCD_CODE, RCD_CATEGORY, RCD_DESCRIPTION  " +
                            "FROM dbo.REF_CODE as RCD " +
                            "WHERE RCD.RCD_RTB_OID = '" + field.getReferenceTableOid() + "'");
    
            try {
                Connection connection = getBroker().borrowConnection();
                Statement statement = connection.createStatement();
                String evalCode;
                String evalDescription;
                String evalCategory;
    
                try {
                    // AppGlobals.getLog().log(Level.INFO, " Before result set");
                    ResultSet resultSet = statement.executeQuery(query);
    
                    while (resultSet.next()) {
                        evalCode = resultSet.getString("RCD_CODE");
                        evalDescription= resultSet.getString("RCD_DESCRIPTION");
                        evalCategory = resultSet.getString("RCD_CATEGORY");
                        AppGlobals.getLog().log(Level.INFO, " Code:" + evalCode + " Category:" + evalCategory + " Desccription:" + evalDescription);
                        if (evalCategory == null) {
                            evalCategory = "";
                        }
                        if (evalDescription == null) {
                            evalDescription = evalCode;
                        }
                        // AppGlobals.getLog().severe(" Code:" + evalCode + " Category:" + evalCategory);
                        m_evalDescription.put(evalCode, evalDescription);
                    }
    
                    resultSet.close();
                } catch (Exception e) {
                    // Do nothing
                }
    
                statement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                getBroker().returnConnection();
            }
        }
    }

    /**
     * Loads the Evaluations considered into the proper report parameters.
     */
    private void loadEvalsDisplay() {
        StringBuilder descrDisplay = new StringBuilder(500);
        X2BaseBean storageBean = getFormStorage();

        /*
         * Iterate over the selected values building a string of the descriptions
         */
        String fieldValue = (String) storageBean.getFieldValueByAlias(ALIAS_DESCRIBE_EVAL, getDictionary());
        Collection<String> evals = StringUtils.convertDelimitedStringToList(fieldValue, ',', true);

        for (String eval : evals) {
            String evalDescr = m_evalDescription.get(eval);
            descrDisplay.append(evalDescr).append("\r");
        }

        addParameter(EVALS_DISPLAY_PARAM3, descrDisplay.toString());
    }
    
    /**
     * Find the IepMeeting instance that is associated with the same phase as the current PWN form.
     * 
     * @return IepMeeting
     */
    private IepMeeting findMeeting() {
        String oid = getFormStorage().getOid();
        // Track to WorkflowProgress instance then back down to the Meeting form instance.
        
        // Get the WorkflowProgress OID.
        X2Criteria wpCriteria = new X2Criteria();
        wpCriteria.addEqualTo(WorkflowProgress.REL_WORKFLOW_PROGRESS_FORMS + ModelProperty.PATH_DELIMITER +
                            WorkflowProgressForm.REL_FORM_INSTANCE + ModelProperty.PATH_DELIMITER + 
                            FormInstance.COL_STORAGE_OBJECT_OID, oid);
        SubQuery wpQuery = new SubQuery(WorkflowProgress.class, X2BaseBean.COL_OID, wpCriteria);
        
        // Get the meeting form instance from this progress step.
        X2Criteria meetingInstanceCriteria = new X2Criteria();
        meetingInstanceCriteria.addIn(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + ModelProperty.PATH_DELIMITER +
                WorkflowProgressForm.COL_WORKFLOW_PROGRESS_OID, wpQuery);
        meetingInstanceCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                FormDefinition.COL_ID, IEP_MEETING_FORM_ID);
        
        SubQuery meetingInstanceQuery = new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, meetingInstanceCriteria); 

        X2Criteria meetingCriteria = new X2Criteria();
        meetingCriteria.addIn(X2BaseBean.COL_OID, wpQuery);
        BeanQuery meetingQuery = new BeanQuery(IepMeeting.class, meetingCriteria);
        IepMeeting meeting = getBroker().getBeanByQuery(meetingQuery);
        return meeting;
    }
}
