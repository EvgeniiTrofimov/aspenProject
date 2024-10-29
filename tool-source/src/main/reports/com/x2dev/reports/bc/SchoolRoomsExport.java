/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SchoolRoom;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports school rooms for BC's GDE.
 *
 * @author Follett Software Company
 */
public class SchoolRoomsExport extends GdeExportJavaSource {
    // Grid fields
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_ROOM_ID = "Room ID";
    private static final String FIELD_ROOM_SHORT_NAME = "Short Name";
    private static final String FIELD_ROOM_LONG_NAME = "Long Name";
    private static final String FIELD_ROOM_PHYSICAL_NUMBER = "Physical Number";
    private static final String FIELD_ROOM_CAPACITY = "Capacity";
    private static final String FIELD_ROOM_TYPE = "Room Type";
    private static final String FIELD_ROOM_TYPE_DESC = "Room Type Description";

    // Other constants
    private static final int FIELD_COUNT = 8;

    // Aliases
    private static final String ALIAS_ROOM_SHORT_NAME = "rms-short-name";
    private static final String ALIAS_ROOM_LONG_NAME = "rms-long-name";

    private List<String> m_columns;
    private Map<String, ReferenceCode> m_roomTypeCodesMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(FIELD_COUNT) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(SchoolRoom.class, buildCriteria());
        query.addOrderByAscending(SchoolRoom.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(SchoolRoom.COL_ROOM_NUMBER);

        QueryIterator rooms = getBroker().getIteratorByQuery(query);
        try {
            while (rooms.hasNext()) {
                SchoolRoom room = (SchoolRoom) rooms.next();
                boolean deleteRow = false;
                try {
                    grid.append();
                    deleteRow = true;

                    grid.set(FIELD_SCHOOL_NUMBER, room.getSchool().getSchoolId());
                    grid.set(FIELD_ROOM_ID, room.getOid());
                    grid.set(FIELD_ROOM_SHORT_NAME, room.getFieldValueByAlias(ALIAS_ROOM_SHORT_NAME));
                    grid.set(FIELD_ROOM_LONG_NAME, room.getFieldValueByAlias(ALIAS_ROOM_LONG_NAME));
                    grid.set(FIELD_ROOM_PHYSICAL_NUMBER, room.getRoomNumber());
                    grid.set(FIELD_ROOM_CAPACITY, String.valueOf(room.getMaxCapacity()));
                    grid.set(FIELD_ROOM_TYPE, room.getRoomTypeCode());

                    // Set reference code description instead of field data if reference table
                    // exists.
                    if ((m_roomTypeCodesMap != null) && m_roomTypeCodesMap.containsKey(room.getRoomTypeCode())) {
                        String refDesc = m_roomTypeCodesMap.get(room.getRoomTypeCode()).getDescription();
                        grid.set(FIELD_ROOM_TYPE_DESC, refDesc);
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(room.getOid());
                    strBldr.append("].");

                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            rooms.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_ROOM_ID);
        m_columns.add(FIELD_ROOM_SHORT_NAME);
        m_columns.add(FIELD_ROOM_LONG_NAME);
        m_columns.add(FIELD_ROOM_PHYSICAL_NUMBER);
        m_columns.add(FIELD_ROOM_CAPACITY);
        m_columns.add(FIELD_ROOM_TYPE);
        m_columns.add(FIELD_ROOM_TYPE_DESC);

        // Fill room types reference code map if reference table exists
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(SchoolRoom.class.getName(),
                SchoolRoom.COL_ROOM_TYPE_CODE);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_roomTypeCodesMap = referenceTable.getCodeMap();
            }
        }
    }

    /**
     * Returns the criteria for pulling school rooms into the export.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(SchoolRoom.REL_SCHOOL, SchoolRoom.COL_SCHOOL_OID));

        return criteria;
    }
}
