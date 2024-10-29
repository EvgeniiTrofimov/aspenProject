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
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;

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
public class MaxPerPageRefresh extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        PersistenceKey brokerKey = getBroker().getPersistenceKey();

        ArrayList<PersistenceKey> keys = new ArrayList<PersistenceKey>();
        keys.add(brokerKey);

        Method method = AppGlobals.class.getDeclaredMethod("updateMaximumRecordsPerPage", Collection.class);
        method.setAccessible(true);

        method.invoke(null, keys);

        logMessage("Completed.");
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#saveState(UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        runOnApplicationServer();

        super.saveState(userData);
    }
}
