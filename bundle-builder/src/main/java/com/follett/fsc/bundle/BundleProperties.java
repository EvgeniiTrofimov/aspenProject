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
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class BundleProperties {
    private static final String BUNDLE_DIR = "output/bundle";
    private static final String TARGET_DIR = "target";

    /*
     * Builder properties
     */
    private static final String PROPERTY_STATE_DIR = "state.dir";
    private static final String PROPERTY_EXPORT_DIR = "export.dir";

    /*
     * State Bundle property file properties
     */
    private static final String PROPERTY_XPATH = "xpath";
    private static final String PROPERTY_SEARCH_PATH = "search-path";
    private static final String PROPERTY_BUNDLE = "bundle-definition";
    private static final String PROPERTY_VERSIONING = "versioning";
    private static final String PROPERTY_SKIP_EXTERNAL = "skip-external";
    private static final String PROPERTY_SKIP_EXTERNAL_IDS = "skip-external-ids";
    private static final String PATH_DELIMETR = ";";

    private final String builderPath;
    private final String state;
    private final String exportDir;
    private final Set<String> allStates;
    private Properties versionProperties = new Properties();
    Properties statePropertyFile = new Properties();
    private final static Logger logger = Logger.getLogger(BundleProperties.class);


    /**
     * @param builderProperties
     * @param builderPathAsPath
     * @throws Exception
     */
    public BundleProperties(Properties builderProperties, Path builderPathAsPath) throws Exception {
        this(builderProperties.getProperty(PROPERTY_STATE_DIR), builderProperties.getProperty(PROPERTY_EXPORT_DIR), builderPathAsPath);
    }

    /**
     * @param state
     * @param exportDir
     * @param builderPathAsPath
     * @throws Exception
     */
    public BundleProperties(String state, String exportDir, Path builderPathAsPath) throws Exception {
        builderPath = builderPathAsPath.toString();
        this.state = state;
        this.exportDir = exportDir;

        Path propertyFile = Paths.get(getBuilderPath().toString(), "States", state, exportDir, "Properties.properties");
        if (propertyFile == null || !Files.exists(propertyFile)) {
            throw new Exception("The property file " + propertyFile.toString() + " does not exist");
        }
        logger.debug("Running the builder tool from ["+builderPath+"]");
        logger.debug("  for ["+state+"] and ["+exportDir+"]");
        try (final InputStream is = Files.newInputStream(propertyFile)) {
            statePropertyFile.load(is);
        }

        try (final InputStream is = BundleProperties.class.getResourceAsStream("/version.properties")) {
            if(is == null) {
                throw new Exception("The property file version.properties did not load properly");
            }
            versionProperties.load(is);
        }

        allStates = gatherAllStates(Paths.get(getBuilderPath().toString(), "States"));

        validatePropertyFile(propertyFile.toString());
    }

    private Set<String> gatherAllStates(Path stateFolder) {
        String[] directories = stateFolder.toFile().list(new FilenameFilter() {
            @Override
            public boolean accept(File current, String name) {
                return new File(current, name).isDirectory();
            }
        });
        Set<String> states = new TreeSet<String>();
        for (String s : directories) {
            states.add(s.toLowerCase());
        }
        states.add("cn");
        states.add("uk");
        states.add("in");
        states.remove("sys");
        states.remove("deploy");
        states.remove("crdc");
        states.remove("core");
        return states;
    }

    public boolean isStateFile() {
        return allStates.contains(state.toLowerCase());
    }

    private void validatePropertyFile(String fileName) throws Exception {
        if(StringUtils.isBlank(statePropertyFile.getProperty(PROPERTY_BUNDLE))) {
            throw new Exception(fileName+" is missing the required field ["+PROPERTY_BUNDLE+"]");
        }
        if(StringUtils.isBlank(statePropertyFile.getProperty(PROPERTY_XPATH))) {
            throw new Exception(fileName+" is missing the required field ["+PROPERTY_XPATH+"]");
        }
        if(StringUtils.isBlank(statePropertyFile.getProperty(PROPERTY_SEARCH_PATH))) {
            throw new Exception(fileName+" is missing the required field ["+PROPERTY_SEARCH_PATH+"]");
        }
        Path stateSourceDir = Paths.get(Paths.get(builderPath).getParent().toString(), "tool-source");

        if (stateSourceDir == null || !Files.exists(stateSourceDir)) {
            throw new Exception("The source dir " + stateSourceDir.toString() + " does not exist");
        }
    }

    public String getZipFileName() {
        return versionProperties.getProperty("project.artifactId")+"-"+versionProperties.getProperty("project.version")+"-"+getIdentifier()+"-bundle.zip";
    }

    public String getIdentifier() {
        return getState()+"-"+getExportDirectory();
    }

    public String getExportDirectory() {
        return exportDir;
    }

    public String getState() {
        return state;
    }

    public String[] getXPaths() {
        return statePropertyFile.getProperty(PROPERTY_XPATH).split(PATH_DELIMETR);
    }

    public Set<String> getAllStates(){
        return allStates;
    }

    public Set<String> getOtherStates(){
        Set<String> otherStates = new TreeSet<String>(getAllStates());
        otherStates.remove(getState().toLowerCase());
        return otherStates;
    }

    public Set<String> getSearchPaths() {
        String[] pathsAsString = statePropertyFile.getProperty(PROPERTY_SEARCH_PATH).split(PATH_DELIMETR);
        Set<String> paths = new TreeSet<String>(Arrays.asList(pathsAsString));
        paths.add(Paths.get("src","main","resources","bundle-resources", state.toLowerCase()).toString());
        paths.add(Paths.get("src","main","resources","bundle-resources").toString());
        return paths;
    }

    public String getBundlePath() throws Exception {
        String bundlePath = statePropertyFile.getProperty(PROPERTY_BUNDLE);

        if (bundlePath.startsWith(".")) {
            bundlePath = getDirectoryPaths(getStateSourcePath(), bundlePath.substring(1));
        }
        return bundlePath;
    }

    public boolean isVersioning() {
        return Boolean.valueOf(statePropertyFile.getProperty(PROPERTY_VERSIONING));
    }

    public boolean isSkipExternal() {
        return Boolean.valueOf(statePropertyFile.getProperty(PROPERTY_SKIP_EXTERNAL));
    }

    public List getSkipExternalIds() {
        String ids = statePropertyFile.getProperty(PROPERTY_SKIP_EXTERNAL_IDS);
        List<String> skipExternalIdsList = ids == null ? Collections.EMPTY_LIST
                : Arrays.asList(ids.split("\\s*,\\s*"));
        return skipExternalIdsList;
    }

    public Path getBuilderPath() {
        return Paths.get(builderPath);
    }


    public Path getStateSourcePath() {
        return Paths.get(getBuilderPath().getParent().toString(), "tool-source");
    }

    public Path getTargetDir() {
        return Paths.get(getBuilderPath().toString(), TARGET_DIR);
    }

    public Path getBundleDir() {
        return Paths.get(getBuilderPath().toString(), BUNDLE_DIR);
    }

    public Path getSharedReportsDir() {
        return Paths.get(getStateSourcePath().toString(), "src", "main", "reports");
    }

    public Path getSharedProceduresDir() {
        return Paths.get(getStateSourcePath().toString(), "src", "main", "procedures");
    }

    public String getDirectoryPaths(Path basePath, String... extraLocations) {
        return getDirectoryPathsAsPath(basePath, extraLocations).toString();
    }
    public Path getDirectoryPathsAsPath(Path basePath, String... extraLocations) {
        return Paths.get(basePath.toString(), extraLocations);
    }

}
