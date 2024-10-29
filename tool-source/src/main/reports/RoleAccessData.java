/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.Role;
import com.follett.fsc.core.k12.business.PrivilegeGrid;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.struts.util.MessageResources;

/**
 * Prepares the data for the Role Access report. This report displays all privileges granted to the
 * selected role.
 *
 * @author X2 Development Corporation
 */
public class RoleAccessData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "accessible only" report parameter. If true then only records with at least one
     * "on" privilege will be included in the grid. The value is a Boolean.
     */
    public static final String ACCESSIBLE_ONLY_PARAM = "accessibleOnly";

    /**
     * Name for the OID report parameter. The OID represents a Role bean. The value is a String.
     */
    public static final String ROLE_OID_PARAM = "roleOid";

    /**
     * Name for the privileges report parameter. The value is a Set of String objects. The Set
     * contains each privilege ID to which the selected role has access.
     */
    public static final String PRIVILEGES_PARAM = "privilegeIds";

    /**
     * Name for the Role report parameter. The value is a Role bean.
     */
    public static final String ROLE_PARAM = "role";

    private static final String[] BEAN_PRIVILEGE_COLUMNS = new String[] {
            PrivilegeGrid.COLUMN_PRIVILEGE_CREATE,
            PrivilegeGrid.COLUMN_PRIVILEGE_READ,
            PrivilegeGrid.COLUMN_PRIVILEGE_UPDATE,
            PrivilegeGrid.COLUMN_PRIVILEGE_DELETE,
            PrivilegeGrid.COLUMN_PRIVILEGE_GLOBAL,
            PrivilegeGrid.COLUMN_PRIVILEGE_MASSUPDATE
    };

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        boolean accessibleOnly =
                ((Boolean) getParameter(ACCESSIBLE_ONLY_PARAM)).booleanValue();

        Role role = (Role) getBroker().getBeanByOid(Role.class, (String) getParameter(ROLE_OID_PARAM));
        addParameter(ROLE_PARAM, role);

        Set privilegeIds = role.getPrivilegeIds();
        addParameter(PRIVILEGES_PARAM, privilegeIds);

        /*
         * Wrap the PrivilegeGrid in a ReportDataGrid. Note that the initial column size doesn't
         * matter since we won't be adding individual rows (or columns) to the ReportDataGrid.
         */
        PrivilegeGrid privilegeGrid = new PrivilegeGrid(getBroker());
        ReportDataGrid reportGrid = new ReportDataGrid(privilegeGrid.rowCount(), 2);
        reportGrid.append(privilegeGrid);
        reportGrid.beforeTop();

        /*
         * Iterate over each row in the grid and perform the following operations:
         *
         * 1. Translate all accessible business privileges into a single localized messages. We
         * replace the Set with the message String so we don't add another column to the grid.
         * 
         * 2. Eliminate the row if the role doesn't have access to any of the privileges (and the
         * corresponding report parameter is set appropriately).
         */
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());
        Locale locale = getLocale();
        while (reportGrid.next()) {
            boolean hasBusinessAccess = false;
            Collection businessPrivileges = (Collection) reportGrid.get(PrivilegeGrid.COLUMN_OTHER);
            if (businessPrivileges != null) {
                StringBuilder message = new StringBuilder(businessPrivileges.size() * 16);

                Iterator privileges = businessPrivileges.iterator();
                while (privileges.hasNext()) {
                    String privilegeId = (String) privileges.next();
                    if (privilegeIds.contains(privilegeId)) {
                        if (message.length() > 0) {
                            message.append(", ");
                        }

                        message.append(messages.getMessage(locale, "privilege." + privilegeId));
                        hasBusinessAccess = true;
                    }
                }

                reportGrid.set(PrivilegeGrid.COLUMN_OTHER, message.toString());
            }

            if (accessibleOnly && !hasBusinessAccess && !hasBeanAccess(reportGrid, privilegeIds)) {
                reportGrid.deleteRow();
            }
        }
        reportGrid.beforeTop();

        return reportGrid;
    }

    /**
     * Returns true if at least one bean privilege in the current row of the grid is contained in
     * the set of privileges.
     *
     * @param grid ReportDataGrid
     * @param privilegeIds Set
     * @return boolean
     */
    private boolean hasBeanAccess(ReportDataGrid grid, Set privilegeIds) {
        boolean hasBeanAccess = false;

        for (int i = 0; i < BEAN_PRIVILEGE_COLUMNS.length; i++) {
            String privilegeId = (String) grid.get(BEAN_PRIVILEGE_COLUMNS[i]);
            if (privilegeIds.contains(privilegeId)) {
                hasBeanAccess = true;
                break;
            }
        }

        return hasBeanAccess;
    }
}
