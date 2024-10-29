/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.follett.fsc.bundle;

import java.io.File;
import java.io.FilenameFilter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;
import org.apache.log4j.Logger;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class CreateAllBundles {

    private final static Logger logger = Logger.getLogger(CreateAllBundles.class);

    public static void main(String[] args) throws Exception {

        String filePath = args[0];
        Path fullFilePath = new File(filePath, "States").toPath();
        if (!Files.exists(fullFilePath) && !Files.isDirectory(fullFilePath)) {
            logger.error(
                    "PLEASE make sure the path is to <fullpath>/bundle-builder.  You passed in ["
                            + fullFilePath.toString() + "]\n\n");
            System.exit(1);
        }
        logger.debug("\nBegin bundle-builder-ALL");
        new CreateAllBundles().runBuilderForAll(fullFilePath);
    }

    private void runBuilderForAll(Path statePath) {
        int directoryCount = 0;
        int stateCount = 0;
        Set<String> stateList = listAllDirectories(statePath);
        Set<MissingFileException> missingFiles = new TreeSet<MissingFileException>();
        Set<BundleVerificationException> invalidFiles = new HashSet<BundleVerificationException>();

        for (String state : stateList) {
            stateCount = 0;
            Path exportPath = Paths.get(statePath.toString(), state);
            Set<String> exportList = listAllDirectories(exportPath);
            for (String exportDir : exportList) {
                BundleProperties properties;
                try {
                    properties = new BundleProperties(state, exportDir, statePath.getParent());
                } catch (Exception e1) {
                    logger.error(
                            "Exception running tool for [" + state + "][" + exportDir + "]\n\n");
                    e1.printStackTrace();
                    continue;
                }
                try {
                    directoryCount++;
                    stateCount++;
                    Path bundleToCreate = Paths.get(properties.getTargetDir().toString(), properties.getZipFileName());
                    if (Files.exists(bundleToCreate)) {
                        logger.error(bundleToCreate + " ALREADY EXISTS!!!!!");
                    }
                    new CreateBundle(properties);
                    if (!Files.exists(bundleToCreate)) {
                        logger.error(bundleToCreate + " FAILED!!!!!");
                    }
                } catch (MissingFileException e) {
                    missingFiles.add(e);
                } catch (BundleVerificationException e) {
                    e.setIdentifier(properties.getIdentifier());
                    invalidFiles.add(e);
                }catch (Exception e) {
                    logger.error(
                            "Exception running tool for [" + state + "][" + exportDir + "]\n\n");
                    e.printStackTrace();
                    continue;
                }
            }
        }

        for (MissingFileException e : missingFiles) {
            logger.error("Missing Files: " + e.getOutputMessage());
        }
        for (BundleVerificationException e : invalidFiles) {
            logger.error("Verification Error: " + e.getMessage());
        }
        logger.debug("End bundle-builder-ALL, wrote " + directoryCount + " export files");
    }

    private Set<String> listAllDirectories(Path statePath) {
        String[] directories = statePath.toFile().list(new FilenameFilter() {
            @Override
            public boolean accept(File current, String name) {
                return new File(current, name).isDirectory();
            }
        });
        return new TreeSet<String>(Arrays.asList(directories));
    }
}
