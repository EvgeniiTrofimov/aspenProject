/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisProcedure;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure that remove the existing exports and reports:
 *
 * Exports:
 * EXP-CRDC-LEA-P1
 * EXP-CRDC-LEA-P2
 * EXP-CRDC-SCH-P1
 * EXP-CRDC-SCH-P2
 * Reports:
 * SYS-CRDC-001
 * SYS-CRDC-002
 * SYS-CRDC-003.
 *
 * @author Follett Software Company
 */
public class DeleteOldCRDCProcedure extends ProcedureJavaSource {
    private static final String[] EXPORTS_ID_LIST =
            {"EXP-CRDC-SCH-P1", "EXP-CRDC-SCH-P2", "EXP-CRDC-LEA-P1", "EXP-CRDC-LEA-P2"};
    private static final String[] PROCEDURES_ID_LIST = {"EXPDATA-CRDC-SCH-P1", "EXPDATA-CRDC-SCH-P2",
            "EXPDATA-CRDC-LEA-P1", "EXPDATA-CRDC-LEA-P2", "CRDC-P2-AP-SETUP", "CRDC-PART2-SETUP"};
    private static final String[] REPORTS_ID_LIST = {"SYS-CRDC-001", "SYS-CRDC-002", "SYS-CRDC-003"};

    private static final String PARAM_EXPORTS = "exportsOids";
    private static final String PARAM_PROCEDURES = "proceduresOids";
    private static final String PARAM_REPORTS = "reportsOids";

    private Collection<X2BaseBean> m_deletedBeans = new ArrayList<X2BaseBean>();
    private String m_exportsOids = null;
    private String m_proceduresOids = null;
    private String m_reportsOids = null;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        for (X2BaseBean deletedBean : m_deletedBeans) {
            if (isSelected(deletedBean)) {
                List validationErrors = getBroker().deleteBeanByOid(deletedBean.getClass(), deletedBean.getOid());
                boolean success = validationErrors == null || validationErrors.isEmpty();

                if (success) {
                    if (deletedBean instanceof ImportExportDefinition) {
                        logMessage("Export " + ((ImportExportDefinition) deletedBean).getId()
                                + " was deleted successfully.");
                    } else if (deletedBean instanceof Report) {
                        logMessage("Report " + ((Report) deletedBean).getId() + " was deleted successfully.");
                    } else if (deletedBean instanceof SisProcedure) {
                        logMessage("Procedure " + ((SisProcedure) deletedBean).getId() + " was deleted successfully.");
                    }
                } else {
                    if (deletedBean instanceof ImportExportDefinition) {
                        logMessage("Cannot delete export " + ((ImportExportDefinition) deletedBean).getId() + ".");
                    } else if (deletedBean instanceof Report) {
                        logMessage("Cannot delete report " + ((Report) deletedBean).getId() + ".");
                    } else if (deletedBean instanceof SisProcedure) {
                        logMessage("Cannot delete procedure " + ((SisProcedure) deletedBean).getId() + ".");
                    }
                }
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_exportsOids = (String) getParameter(PARAM_EXPORTS);
        m_proceduresOids = (String) getParameter(PARAM_PROCEDURES);
        m_reportsOids = (String) getParameter(PARAM_REPORTS);

        X2Criteria exportsCriteria = new X2Criteria();
        exportsCriteria.addIn(ImportExportDefinition.COL_ID, Arrays.asList(EXPORTS_ID_LIST));
        QueryByCriteria exportsQuery = new QueryByCriteria(ImportExportDefinition.class, exportsCriteria);

        X2Criteria proceduresCriteria = new X2Criteria();
        proceduresCriteria.addIn(SisProcedure.COL_ID, Arrays.asList(PROCEDURES_ID_LIST));
        QueryByCriteria proceduresQuery = new QueryByCriteria(SisProcedure.class, proceduresCriteria);

        X2Criteria reportsCriteria = new X2Criteria();
        reportsCriteria.addIn(Report.COL_ID, Arrays.asList(REPORTS_ID_LIST));
        QueryByCriteria reportsQuery = new QueryByCriteria(Report.class, reportsCriteria);

        m_deletedBeans.addAll(getBroker().getCollectionByQuery(exportsQuery));
        m_deletedBeans.addAll(getBroker().getCollectionByQuery(proceduresQuery));
        m_deletedBeans.addAll(getBroker().getCollectionByQuery(reportsQuery));
    }

    /**
     * Checks if is selected.
     *
     * @param bean X2BaseBean
     * @return true, if is selected
     */
    private boolean isSelected(X2BaseBean bean) {
        boolean isSelected = false;

        if (StringUtils.isEmpty(m_exportsOids) && StringUtils.isEmpty(m_reportsOids)
                && StringUtils.isEmpty(m_proceduresOids)) {
            // Nothing selected is default to delete all
            isSelected = true;
        } else if ((!StringUtils.isEmpty(m_exportsOids) && m_exportsOids.contains(bean.getOid())) ||
                (!StringUtils.isEmpty(m_reportsOids) && m_reportsOids.contains(bean.getOid())) ||
                (!StringUtils.isEmpty(m_proceduresOids) && m_proceduresOids.contains(bean.getOid()))) {
            isSelected = true;
        }
        return isSelected;
    }
}
