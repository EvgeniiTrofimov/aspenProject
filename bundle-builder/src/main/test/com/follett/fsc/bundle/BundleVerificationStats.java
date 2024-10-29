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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class BundleVerificationStats implements Comparable<BundleVerificationStats> {

    private final Set<String> extraStates = new TreeSet<String>();
    private final List<String> zipContents;
    private final List<String> filesFromOtherStates = new ArrayList<String>();
    private final String myState;
    private final String zipFileName;

    /**
     *
     * @param pathToZip
     * @param allStates
     * @throws IOException
     */
    public BundleVerificationStats(Path pathToZip, Set<String> allStates) throws IOException {
        this.zipContents = runJarCommand(pathToZip);
        this.zipFileName = pathToZip.getFileName().toString();
        this.myState = zipFileName.substring(0, zipFileName.indexOf("-")).toLowerCase();
        Set<String> otherStates = new TreeSet<String>(allStates);
        otherStates.remove(myState);
        validateZip(otherStates);
    }

    public List<String> getContents() {
        return zipContents;
    }

    public List<String> getContentsExcludingWrongStateFiles() {
        List<String> contents = new ArrayList<String>(getContents());
        contents.removeAll(filesFromOtherStates);
        return contents;
    }

    public Set<String> getExtraIncludedStates() {
        return extraStates;
    }

    public String getZipFileName() {
        return zipFileName;
    }

    public String getValidationResults() {
        return getValidationResults(false);
    }

    public String getValidationResults(boolean verbose) {
        if (extraStates.isEmpty()) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        sb.append(zipFileName + " is in error with  ["+extraStates.size()+"] extra states");
        if (verbose) {
            if (!extraStates.isEmpty()) {
                sb.append("\n it contains files from " + extraStates);
            }
        }
        return sb.toString();
    }

    /**
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(BundleVerificationStats o) {
        if (this == o) {
            return 0;
        }
        if (o == null) {
            return 1;
        }
        return this.zipFileName.compareTo(o.zipFileName);
    }

    private void validateZip(Set<String> otherStates) throws IOException {
        // check if we have duplicateFiles
        Set<String> fileNames = new TreeSet<String>();
        for (String zipEntry : zipContents) {
            Path zip = Paths.get(zipEntry);

            for (String state : otherStates) {
                if (zipEntry.contains(File.separator + state) || zipEntry.contains(state + File.separator)) {
                    extraStates.add(state);
                    filesFromOtherStates.add(zipEntry);
                }
            }
        }
    }

    private List<String> runJarCommand(Path pathToZip) throws IOException {
        List<String> jarResults = new ArrayList<String>();
        List<String> commandsList = new ArrayList<String>();
        commandsList.addAll(Arrays.asList(new String[] {"jar", "-tf", pathToZip.toString()}));


        ProcessBuilder processBuilder = new ProcessBuilder(commandsList.toArray(new String[commandsList.size()]));
        processBuilder.redirectErrorStream(true);
        Process process = processBuilder.start();
        BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        process.getOutputStream().flush();
        process.getOutputStream().close();
        while (true) {
            String line = reader.readLine();
            if (line == null) {
                break;
            }
            jarResults.add(line);
        }

        Collections.sort(jarResults);
        return jarResults;
    }

}
