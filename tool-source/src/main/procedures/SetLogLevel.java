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
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import java.util.logging.Level;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 * A wholly owned subsidiary of Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

/**
 * Set System Log Level
 *
 * This procedure allows the user to temporarily redefine the current logging level on a running
 * server without having to stop or restart the server.
 *
 * @author X2 Development Corporation
 */
public class SetLogLevel extends ProcedureJavaSource {

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        logMessage("Aspen logger level: " + AppGlobals.getLog().getLevel().toString());

        String logLevel = (String) getParameter("logLevel");
        AppGlobals.getLog().setLevel(Level.parse(logLevel));

        logMessage("Aspen logger level: " + AppGlobals.getLog().getLevel().toString());
    }

}
