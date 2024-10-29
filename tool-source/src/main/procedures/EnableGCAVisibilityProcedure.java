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
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/**
 * Blank Procedure that does nothing. Can be used as a stub for procedures that work solely by
 * using the custom HREF data.
 *
 * @author X2 Development Corporation
 */
public class EnableGCAVisibilityProcedure extends ProcedureJavaSource {

    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), SystemPreferenceDefinition.GCA_VISIBLE,
                "true");
        // PreferenceManager.setPreferenceValue(getOrganization(), getBroker(),
        // SystemPreferenceDefinition.GCA_VISIBLE, "false ");
    }
}
