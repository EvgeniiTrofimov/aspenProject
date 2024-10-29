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
import com.x2dev.sis.tools.procedures.InitializeSectionsProcedure;
import com.x2dev.utils.X2BaseException;
import java.util.logging.Level;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class StandardInitializeSectionsProcedure extends InitializeSectionsProcedure {

    /**
     * @see com.x2dev.sis.tools.procedures.InitializeSectionsProcedure#bypassSystemLogic()
     */
    @Override
    public boolean bypassSystemLogic() {
        // If bypass returns true then the system process of "Initialize sections" will be skipped
        // and the initializeSections method in this procedure will be run instead.
        logToolMessage(Level.INFO, "StandardInitializeSectionsProcedure - bypass called", true);
        return false;
    }

    /**
     * @see com.x2dev.sis.tools.procedures.InitializeSectionsProcedure#initializeSections()
     */
    @Override
    public void initializeSections() {
        // This is where you would define the logic to initialize the sections.
        logToolMessage(Level.INFO, "StandardInitializeSectionsProcedure - initialize called", true);
    }

    /**
     * @see com.x2dev.sis.tools.procedures.InitializeSectionsProcedure#preInitializeSections()
     */
    @Override
    public void preInitializeSections() {
        // Handle pre-initialization logic.
        logToolMessage(Level.INFO, "StandardInitializeSectionsProcedure - pre called", true);
    }

    /**
     * @throws X2BaseException
     * @see com.x2dev.sis.tools.procedures.InitializeSectionsProcedure#postInitializeSections()
     */
    @Override
    public void postInitializeSections() {
        // Handle post processing logic.
        logToolMessage(Level.INFO, "StandardInitializeSectionsProcedure - post called", true);
    }

}
