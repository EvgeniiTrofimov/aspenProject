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
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 * A wholly owned subsidiary of Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

/**
 * Set Log4J Log Level
 *
 * This procedure allows the user to temporarily redefine the current logging level for the
 * Destiny data transfer jar (and other jars using log4j instances if added to "keys" below).
 * on a running server without having to stop or restart the server.
 *
 * @author X2 Development Corporation
 */
public class SetLog4jLevel extends ProcedureJavaSource {

    String[] keys =
            {
                    "com.follett.fsc.ws",
                    "com.follett.fsc.destiny.datatransfer"
            };

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String logLevel = (String) getParameter("logLevel");

        Logger rootLogger = Logger.getRootLogger();
        logMessage("Root logger level: " + rootLogger.getLevel().toString());
        logMessage("Root logger name:  " + rootLogger.getName());

        // rootLogger.setLevel(Level.toLevel(logLevel));
        // logMessage("Root logger level: " + rootLogger.getLevel().toString());

        for (String key : keys) {
            Logger logger = rootLogger.getLoggerRepository().getLogger(key);
            if (logger != null) {
                Level oldLevel = logger.getLevel();
                logger.setLevel(Level.toLevel(logLevel));
                logger.addAppender(new ConsoleAppender(
                        new PatternLayout(PatternLayout.TTCC_CONVERSION_PATTERN)));

                logMessage(logger.getName() + " changed from "
                        + (oldLevel == null ? "undefined" : oldLevel.toString())
                        + " to " + logLevel);
            } else {
                logMessage("Logger " + key + " not created.");
            }

        }
    }

}
