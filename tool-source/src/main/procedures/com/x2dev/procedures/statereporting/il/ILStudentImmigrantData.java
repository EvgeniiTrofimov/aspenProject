/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.x2dev.sis.model.beans.SisStudent;

/**
 * Illinois state export procedure for IL Immigrant Export.
 *
 * @author X2 Development Corporation
 */
public class ILStudentImmigrantData extends RcdtsBasedProcedureData {
    private static final String EXPORT_NAME = "Immigrant";
    private static final String PARAM_REQUEST_UPDATE = "requestUpdate";

    /**
     * Adds the selection criteria.
     *
     * @param criteria X2Criteria
     * @param prefix String
     */
    @Override
    protected void addSelectionCriteria(X2Criteria criteria, String prefix) {
        // check for request (students without state ids) / update (students with state ids)
        int requestUpdateSelection = ((Integer) getParameter(PARAM_REQUEST_UPDATE)).intValue();
        switch (requestUpdateSelection) {
            case 1:
                criteria.addIsNull(prefix + SisStudent.COL_STATE_ID);
                break;
            case 2:
                criteria.addNotNull(prefix + SisStudent.COL_STATE_ID);
                break;
            default:
                break;
        }

    }

    @Override
    protected String getExportName() {
        return EXPORT_NAME;
    }

}
