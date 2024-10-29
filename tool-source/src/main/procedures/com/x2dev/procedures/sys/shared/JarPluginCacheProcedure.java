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
package com.x2dev.procedures.sys.shared;

import com.follett.fsc.core.framework.persistence.SisSyncEventHandler;
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
public class JarPluginCacheProcedure extends ProcedureJavaSource {
    private static final String PARAM_OPERATION = "operation";
    private static final String PARAM_ID = "idString";
    private static final String PARAM_ALL_DEPLOYMENTS = "allDeployments";

    private static final String OPERATION_REMOVE_JAR = "removeJar";
    private static final String OPERATION_REMOVE_CLASSLOADER = "removeClassLoader";
    private static final String OPERATION_CLEAR_ALL = "clearAll";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String operation = (String) getParameter(PARAM_OPERATION);
        boolean allDeployments = Boolean.TRUE.equals(getParameter(PARAM_ALL_DEPLOYMENTS));
        String currentDeploymentId = getBroker().getPersistenceKey().getDeploymentId();

        Map<String, Set<String>> cacheInfo = JarByteArrayClassLoader.getCacheInfo();

        if (OPERATION_REMOVE_JAR.equals(operation)) {
            removeJarFromCache(allDeployments, currentDeploymentId, cacheInfo);
        } else if (OPERATION_REMOVE_CLASSLOADER.equals(operation)) {
            removeClassLoaderFromCache(allDeployments, currentDeploymentId, cacheInfo);
        } else if (OPERATION_CLEAR_ALL.equals(operation)) {
            clearCache(allDeployments, currentDeploymentId);
        }

        // Refresh the cache info after running operations
        cacheInfo = JarByteArrayClassLoader.getCacheInfo();

        logCacheContents(allDeployments, currentDeploymentId, cacheInfo);
    }

    /**
     * Resets the cache.
     *
     * @param allDeployments boolean
     * @param currentDeploymentId String
     */
    private void clearCache(boolean allDeployments, String currentDeploymentId) {
        if (allDeployments) {
            logMessage("Clearing cache for all deployments");

            JarByteArrayClassLoader.resetCache();
        } else {
            logMessage("Clearing cache for deployment " + currentDeploymentId);

            JarByteArrayClassLoader.resetCache(currentDeploymentId);
        }

        logMessage("");
    }

    /**
     * Displays information about the contents of the cache.
     *
     * @param allDeployments boolean
     * @param currentDeploymentId String
     * @param cacheInfo Map<String,Set<String>>
     */
    private void logCacheContents(boolean allDeployments,
                                  String currentDeploymentId,
                                  Map<String, Set<String>> cacheInfo) {
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
                if (allDeployments || deploymentId.equals(currentDeploymentId)) {
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
    }

    /**
     * Removes a specific classloader from the cache.
     *
     * @param allDeployments boolean
     * @param currentDeploymentId String
     * @param cacheInfo Map<String,Set<String>>
     */
    private void removeClassLoaderFromCache(boolean allDeployments,
                                            String currentDeploymentId,
                                            Map<String, Set<String>> cacheInfo) {
        String classLoaderId = (String) getParameter(PARAM_ID);

        if (StringUtils.isEmpty(classLoaderId)) {
            logMessage("Unable to remove JAR from cache; a ClassLoader ID was not provided.");
        } else {
            for (String deploymentId : cacheInfo.keySet()) {
                if (allDeployments || deploymentId.equals(currentDeploymentId)) {
                    logMessage("Removing class loader " + classLoaderId + " from cache for deployment " + deploymentId);

                    JarPluginManager.removeClassLoaderFromCache(classLoaderId, deploymentId);
                }
            }
        }
        logMessage("");
    }

    /**
     * Removes a specific JAR from the cache.
     *
     * @param allDeployments boolean
     * @param currentDeploymentId String
     * @param cacheInfo Map<String,Set<String>>
     */
    private void removeJarFromCache(boolean allDeployments,
                                    String currentDeploymentId,
                                    Map<String, Set<String>> cacheInfo) {
        String jarId = (String) getParameter(PARAM_ID);

        if (StringUtils.isEmpty(jarId)) {
            logMessage("Unable to remove JAR from cache; a JAR ID was not provided.");
        } else {
            for (String deploymentId : cacheInfo.keySet()) {
                if (allDeployments || deploymentId.equals(currentDeploymentId)) {
                    logMessage("Removing JAR " + jarId + " from cache for deployment " + deploymentId);

                    JarPluginManager.removeJarFromCache(jarId, deploymentId);
                }
            }
        }
        logMessage("");
    }
}
