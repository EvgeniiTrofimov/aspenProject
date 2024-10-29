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
package com.x2dev.procedures.edfi.tn;

import com.follett.fsc.core.framework.persistence.SisSyncEventHandler;
import com.follett.fsc.core.k12.business.publish.Publisher;
import com.follett.fsc.core.k12.tools.JarPluginManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.JarByteArrayClassLoader;
import com.x2dev.utils.StringUtils;
import java.util.Map;
import java.util.Set;

/**
 * Procedure that displays information about the contents of the JAR plugin cache, and optionally
 * runs clear or removal operations.
 *
 * This procedure is useful when it is necessary to access cache information, or modify the cache,
 * on a report server where monitor.do is not available.
 *
 * @author mmastrangelo
 */
public class EdFiReinitializePublisher extends ProcedureJavaSource {
    // private static final String PARAM_ID = "idString";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Map<String, Set<String>> cacheInfo = JarByteArrayClassLoader.getCacheInfo();

        String jarId = "AspenEdFiPluginTN"; // (String) getParameter(PARAM_ID);
        removeJarFromCache(jarId, cacheInfo);

        // Refresh the cache info after running operations
        cacheInfo = JarByteArrayClassLoader.getCacheInfo();
        logCacheContents(cacheInfo);

        Publisher.getInstance().initialize();
        logMessage("Publisher initialized.");

        // Refresh the cache info after running operations
        cacheInfo = JarByteArrayClassLoader.getCacheInfo();
        logCacheContents(cacheInfo);
    }

    /**
     * Displays information about the contents of the cache.
     *
     * @param cacheInfo Map<String,Set<String>>
     */
    private void logCacheContents(Map<String, Set<String>> cacheInfo) {
        String nodeName = SisSyncEventHandler.UNKNOWN_NODE_NAME;
        if (SisSyncEventHandler.getInstance() != null) {
            nodeName = SisSyncEventHandler.getInstance().getNodeName();
        }
        logMessage("Jar cache information for node " + nodeName + ":");
        logMessage("");

        if (cacheInfo.isEmpty()) {
            logMessage("Cache is empty.");
        } else {
            for (String deploymentId : cacheInfo.keySet()) {
                logMessage("Deployment: " + deploymentId);
                logMessage("-------------------------------------------");
                Set<String> classLoaderIds = cacheInfo.get(deploymentId);

                for (String classLoaderId : classLoaderIds) {
                    logMessage(classLoaderId);
                }

                logMessage("");
            }
        }
    }

    /**
     * Removes a specific JAR from the cache.
     *
     * @param jarId String
     * @param cacheInfo Map<String,Set<String>>
     */
    private void removeJarFromCache(String jarId, Map<String, Set<String>> cacheInfo) {

        if (StringUtils.isEmpty(jarId)) {
            logMessage("Unable to remove JAR from cache; a JAR ID was not provided.");
        } else {
            for (String deploymentId : cacheInfo.keySet()) {
                logMessage("Removing JAR " + jarId + " from cache for deployment " + deploymentId);

                JarPluginManager.removeJarFromCache(jarId, deploymentId);
            }
        }
        logMessage("");
    }
}
