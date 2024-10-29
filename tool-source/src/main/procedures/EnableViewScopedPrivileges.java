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
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/**
 * Tool to toggle the hidden system preference for View Scoped Privilege sets.
 *
 * @author X2 Development Corporation
 */
public class EnableViewScopedPrivileges extends ProcedureJavaSource {

    private static final String ENABLE_VIEW_SCOPED_PRIVILEGES = "enableVSPrivileges";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String enableVSPriv = ((Boolean) getParameter(ENABLE_VIEW_SCOPED_PRIVILEGES)).toString();
        Organization root = OrganizationManager.getRootOrganization(getUser().getOrganization1());

        PreferenceManager.setPreferenceValue(root, getBroker(),
                SystemPreferenceDefinition.SYS_VIEW_SCOPED_PRIVILEGES,
                enableVSPriv, true);

    }

}
