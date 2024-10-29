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

package com.x2dev.procedures.sys.setup.ma;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Class for MA Unused Fields Export.
 *
 * This class is designed to run with SifInfoDriver
 *
 * @author Follett Software Company
 */
public class MAUnusedFieldsExportData extends ExportJavaSource {
    // Boolean parameter passed in: Write the column names header row?
    public static final String PARAM_WRITE_HEADER = "writeHeader";

    /**
     * Grid fields
     */
    private static final String FIELD_ACT_A_UNUSED = "ACT A Unused";
    private static final String FIELD_ACT_B_UNUSED = "ACT B Unused";
    private static final String FIELD_ACT_C_UNUSED = "ACT C Unused";
    private static final String FIELD_ACT_D_UNUSED = "ACT D Unused";
    private static final String FIELD_CND_A_UNUSED = "CND A Unused";
    private static final String FIELD_CND_B_UNUSED = "CND B Unused";
    private static final String FIELD_CND_C_UNUSED = "CND C Unused";
    private static final String FIELD_CND_D_UNUSED = "CND D Unused";
    private static final String FIELD_CND_E_UNUSED = "CND E Unused";

    /**
     * REGEX
     */
    private static final String REGEX_A_FIELDS = "fieldA[0-9]{1,3}";
    private static final String REGEX_B_FIELDS = "fieldB[0-9]{1,3}";
    private static final String REGEX_C_FIELDS = "fieldC[0-9]{1,3}";
    private static final String REGEX_D_FIELDS = "fieldD[0-9]{1,3}";
    private static final String REGEX_E_FIELDS = "fieldE[0-9]{1,3}";

    private String[] COLUMN_NAMES = {FIELD_ACT_A_UNUSED, FIELD_ACT_B_UNUSED, FIELD_ACT_C_UNUSED, FIELD_ACT_D_UNUSED,
            FIELD_CND_A_UNUSED, FIELD_CND_B_UNUSED, FIELD_CND_C_UNUSED, FIELD_CND_D_UNUSED, FIELD_CND_E_UNUSED};

    /**
     * Class members
     */
    private List<String> m_actAFields;
    private List<String> m_actBFields;
    private List<String> m_actCFields;
    private List<String> m_actDFields;
    private List<String> m_cndAFields;
    private List<String> m_cndBFields;
    private List<String> m_cndCFields;
    private List<String> m_cndDFields;
    private List<String> m_cndEFields;


    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        initializeUnusedFieldsArray();

        DataGrid grid = new DataGrid();
        grid.append();
        grid.set(FIELD_ACT_A_UNUSED, Integer.toString(m_actAFields.size()));
        grid.set(FIELD_ACT_B_UNUSED, Integer.toString(m_actBFields.size()));
        grid.set(FIELD_ACT_C_UNUSED, Integer.toString(m_actCFields.size()));
        grid.set(FIELD_ACT_D_UNUSED, Integer.toString(m_actDFields.size()));
        grid.set(FIELD_CND_A_UNUSED, Integer.toString(m_cndAFields.size()));
        grid.set(FIELD_CND_B_UNUSED, Integer.toString(m_cndBFields.size()));
        grid.set(FIELD_CND_C_UNUSED, Integer.toString(m_cndCFields.size()));
        grid.set(FIELD_CND_D_UNUSED, Integer.toString(m_cndDFields.size()));
        grid.set(FIELD_CND_E_UNUSED, Integer.toString(m_cndEFields.size()));

        return grid;
    }

    /**
     * Method to find all unused A,B,C,D,E fields for STUDENT_CONDUCT_ACTION and
     * STUDENT_CONDUCT_INCIDENT.
     */
    private void initializeUnusedFieldsArray() {
        m_actAFields = new ArrayList<String>();
        m_actBFields = new ArrayList<String>();
        m_actCFields = new ArrayList<String>();
        m_actDFields = new ArrayList<String>();
        m_cndAFields = new ArrayList<String>();
        m_cndBFields = new ArrayList<String>();
        m_cndCFields = new ArrayList<String>();
        m_cndDFields = new ArrayList<String>();
        m_cndEFields = new ArrayList<String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        List<DataDictionaryField> actFields = dictionary.getFieldsForContext(ConductAction.class.getName());
        List<DataDictionaryField> cndFields = dictionary.getFieldsForContext(ConductIncident.class.getName());


        for (DataDictionaryField actField : actFields) {

            if (!actField.isEnabled()) {
                String javaName = actField.getJavaName();

                if (javaName.matches(REGEX_A_FIELDS)) {
                    m_actAFields.add(javaName);
                } else if (javaName.matches(REGEX_B_FIELDS)) {
                    m_actBFields.add(javaName);
                } else if (javaName.matches(REGEX_C_FIELDS)) {
                    m_actCFields.add(javaName);
                } else if (javaName.matches(REGEX_D_FIELDS)) {
                    m_actDFields.add(javaName);
                }
            }
        }

        for (DataDictionaryField cndField : cndFields) {
            if (!cndField.isEnabled()) {
                String javaName = cndField.getJavaName();

                if (javaName.matches(REGEX_A_FIELDS)) {
                    m_cndAFields.add(javaName);
                } else if (javaName.matches(REGEX_B_FIELDS)) {
                    m_cndBFields.add(javaName);
                } else if (javaName.matches(REGEX_C_FIELDS)) {
                    m_cndCFields.add(javaName);
                } else if (javaName.matches(REGEX_D_FIELDS)) {
                    m_cndDFields.add(javaName);
                } else if (javaName.matches(REGEX_E_FIELDS)) {
                    m_cndEFields.add(javaName);
                }
            }
        }
    }

    // Below this line is all
    // boilerplate---------------------------------------------------------------------------

    /**
     * Return ordered list of column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        ArrayList<String> columnNames = new ArrayList<>();

        columnNames.addAll(Arrays.asList(COLUMN_NAMES));

        return columnNames;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        boolean writeHeader = ((Boolean) getParameter(PARAM_WRITE_HEADER)).booleanValue();

        // Set exports to use MS/windows end of line character for all exports.
        setLineSeparator(FORMAT_EOL_WINDOWS);

        // Set export parameters from the report data object.
        setEscapeCharacter(Character.valueOf('"'));
        setIncludeHeaderRow(writeHeader);
        setValueDelimiter(Character.valueOf(','));
        setValueWrapper(Character.valueOf('"'));
    }
}

