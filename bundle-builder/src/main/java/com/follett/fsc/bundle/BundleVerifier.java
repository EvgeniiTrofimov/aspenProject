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
public class BundleVerifier extends Exception {

    public static void verify(Path pathToZip, String myState, Set<String> otherStates)
            throws BundleVerificationException {

        String zipFileName = pathToZip.getFileName().toString();
        try {
            List<String> zipContents = runJarCommand(pathToZip);
            Set<String> extraStates = validateZip(zipContents, otherStates);

            if (!extraStates.isEmpty()) {
                throw new BundleVerificationException(extraStates, myState, zipFileName);
            }
        } catch (IOException e) {
            throw new BundleVerificationException(myState, zipFileName, e);
        }
    }

    public static boolean entryContainsOtherState(String entry, Set<String> otherStates) {
        for (String state : otherStates) {
            if (entry.contains(File.separator + state + File.separator) || entry.endsWith(File.separator + state)) {
                return true;
            }
        }
        return false;
    }

    private static Set<String> validateZip(List<String> zipContents, Set<String> otherStates) throws IOException {
        // check if we have duplicateFiles
        final Set<String> extraStates = new TreeSet<String>();
        Set<String> fileNames = new TreeSet<String>();
        for (String zipEntry : zipContents) {
            Path zip = Paths.get(zipEntry);

            for (String state : otherStates) {
                if (zipEntry.contains(File.separator + state + File.separator)
                        || zipEntry.endsWith(File.separator + state)) {
                    extraStates.add(state);
                }
            }
        }
        return extraStates;
    }

    private static List<String> runJarCommand(Path pathToZip) throws IOException {
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
