/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.tools.ToolBroker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ContextDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.nav.Node;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.business.gradebook.restore.SisGradebookRestoreManager;
import com.x2dev.sis.model.business.gradebook.restore.SisGradebookRestoreManager.GradebookRestoreLog;
import com.x2dev.sis.web.gradebook.ScoreGrid;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;

/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

/**
 * Procedure which restores gradebook data back to the state it existed on a previous
 * date (from the input).
 * <p>
 * Must run from within the context of a Gradebook Scores screen.
 */
public class GradebookRestoreProcedure extends ProcedureJavaSource {

    private static final long serialVersionUID = 1L;

    private static final String RESTORE_DATE = "restoreDate";
    private static final String RESTORE_TIME = "restoreTime";

    private MasterSchedule m_section;
    private String m_staffOid;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        if (m_section == null) {
            logMessage("Procedure not configured properly. No section could be determined. Contact a sys admin.");
            return;
        }

        if (m_staffOid == null) {
            logMessage("Procedure not configured properly. No staff could be determined. Contact a sys admin.");
            return;
        }

        PlainDate restoreDate = (PlainDate) getParameter(RESTORE_DATE);
        PlainTime restoreTime = (PlainTime) getParameter(RESTORE_TIME);
        ToolBroker broker = (ToolBroker) getBroker();
        broker.turnAuditOn(getUser());

        SisGradebookRestoreManager manager =
                new SisGradebookRestoreManager(restoreDate, restoreTime, broker);

        GradebookRestoreLog log = manager.restoreData(m_section);
        for (String message : log.getLogs()) {
            logMessage(message);
        }
        manager.destroy();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {

        /*
         * Find the current section in the nav hierarchy. If not found, the current node
         * is a grid, and it is a ScoreGrid, pull the section off the grid.
         */
        m_section = userData.getCurrentRecord(MasterSchedule.class);
        if (m_section == null) {
            Node node = userData.getCurrentNode();
            if (node != null && node.isGrid()) {
                ContextDataGrid grid = userData.getCurrentGrid();
                if (grid instanceof ScoreGrid) {
                    m_section = ((ScoreGrid) grid).getSection();
                }
            }
        }

        m_staffOid = userData.getStaffOid();
        if (m_staffOid == null && m_section != null) {
            m_staffOid = m_section.getPrimaryStaffOid();
        }

        super.saveState(userData);
    }
}
