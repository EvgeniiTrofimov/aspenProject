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
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Map;

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
 * Provides some tests for hot swap deployment and .
 *
 * @author X2 Development Corporation
 */
public class ReloadPreferenceManager extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ArrayList<PersistenceKey> keys = new ArrayList<PersistenceKey>();

        Field field = BeanManager.class.getDeclaredField("s_persistenceKeys");
        field.setAccessible(true);
        Map<String, PersistenceKey> persistenceKeys = (Map<String, PersistenceKey>) field.get(null);

        if (persistenceKeys != null && !persistenceKeys.isEmpty()) {
            keys.addAll(persistenceKeys.values());
        }

        for (PersistenceKey key : keys) {
            PreferenceManager manager = PreferenceManager.getInstance(key);
            manager.clear(true);
            manager.reloadDefinitions(true);
        }

        field.setAccessible(false);

        logMessage("Refreshed preferences for " + keys.size() + " district(s):\n");

        for (PersistenceKey key : keys) {
            logMessage(key.getDeploymentId());
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        runOnApplicationServer();

        super.saveState(userData);
    }
}
