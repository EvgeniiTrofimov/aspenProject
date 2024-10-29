/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SchoolRoom;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class setFISH.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class setFISH extends ProcedureJavaSource {
    private static final String ALIAS_CLASSROOM_ID = "all-rms-ClassroomIdentificationNo";
    private static final Integer INTEGER_1 = Integer.valueOf(1);

    private Map<String, Integer> m_mapSchoolNumber = new HashMap();
    private int m_roomNumber = 10000;
    private int m_schoolNumber = 10000;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        QueryIterator iterator = getBroker().getIteratorByQuery(new QueryByCriteria(SchoolRoom.class));

        try {
            while (iterator.hasNext()) {
                SchoolRoom room = (SchoolRoom) iterator.next();
                room.setFieldValueByAlias(ALIAS_CLASSROOM_ID, newId(room));
                getBroker().saveBeanForced(room);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Gets the school id.
     *
     * @param sklOid String
     * @return Integer
     */
    private Integer getSchoolId(String sklOid) {
        Integer schoolNumber = m_mapSchoolNumber.get(sklOid);
        if (schoolNumber == null) {
            schoolNumber = Integer.valueOf(m_schoolNumber++);
            m_mapSchoolNumber.put(sklOid, schoolNumber);
        }
        return schoolNumber;
    }

    /**
     * New id.
     *
     * @param room SchoolRoom
     * @return String
     */
    private String newId(SchoolRoom room) {
        String sklOid = room.getSchoolOid();
        Integer facility = getSchoolId(sklOid);
        Integer roomNumber = Integer.valueOf(m_roomNumber++);
        return String.format("%05dA%05d%05d%05d", facility, INTEGER_1, INTEGER_1, roomNumber);
    }
}
