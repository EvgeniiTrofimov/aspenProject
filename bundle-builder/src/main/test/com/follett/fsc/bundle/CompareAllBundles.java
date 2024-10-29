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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class CompareAllBundles {

    final List<String> antInError = new ArrayList<String>();
    final int maxCompare = 1000;
    final boolean debug = true;

    public static void main(String[] args) throws Exception {

        System.out.println("THIS IS A TEMPORARY TEST FILE FOR JENNA"); //TODO println to remove

        Path repoFilePath =
                new File("/Users/jmccormick/dev/test-state-reporting-state-only/bundle-builder/output/bundleOutput")
                .toPath();
        Path developFilePath = new File("/Users/jmccormick/dev/aspen_develop/aspen-bundles/Bundle/zip-files").toPath();
        System.out.println("\nBegin fileCompare");
        new CompareAllBundles().runCompare(developFilePath, repoFilePath);
        System.out.println("\nEnd fileCompare");
    }

    private void runCompare(Path developPath, Path repoPath) {

        Set<String> missingFromRepo = new TreeSet<String>();
        Set<String> missingFromDev = new TreeSet<String>();
        Set<String> identical = new TreeSet<String>();
        Set<MismatchedZipException> mismatched = new TreeSet<MismatchedZipException>();
        Set<MismatchedZipException> mismatchedOnlyForMessageResource = new TreeSet<MismatchedZipException>();
        Set<BundleVerificationStats> developFailedVerification = new TreeSet<BundleVerificationStats>();
        Set<BundleVerificationStats> repoFailedVerification = new TreeSet<BundleVerificationStats>();

        if (!Files.exists(repoPath) || !Files.exists(developPath)) {
            System.out.println("[" + repoPath + "] or [" + developPath + "] doesn't exist");
            System.exit(1);
        }

        Set<String> repoFiles = findAllFileNames(repoPath);
        Set<String> developFiles = findAllFileNames(developPath);

        Set<String> allFiles = repoFiles;
        allFiles.addAll(developFiles);
        Set<String> allStates = gatherStates();

        int meter = 0;
        for (String zipFile : allFiles) {
            if (meter % 50 == 0) {
                System.out.print("\n" + String.format("%03d", meter) + " ");
            }
            meter++;
            System.out.print("."); // TODO println to remove
            Path developZip = Paths.get(developPath.toString(), zipFile);
            Path repoZip = Paths.get(repoPath.toString(), zipFile);
            if (!Files.exists(developZip)) {
                missingFromDev.add(zipFile);
                continue;
            }
            if (!Files.exists(repoZip)) {
                missingFromRepo.add(zipFile);
                continue;
            }
            try {
                BundleVerificationStats developVerification = new BundleVerificationStats(developZip, allStates);
                if (!StringUtils.isBlank(developVerification.getValidationResults())) {
                    developFailedVerification.add(developVerification);
                }
                BundleVerificationStats repoVerification = new BundleVerificationStats(repoZip, allStates);
                if (!StringUtils.isBlank(repoVerification.getValidationResults())) {
                    repoFailedVerification.add(repoVerification);
                }
                if (developVerification.getContentsExcludingWrongStateFiles()
                        .equals(repoVerification.getContentsExcludingWrongStateFiles())) {
                    identical.add(zipFile);
                    continue;
                }
                MismatchedZipException exception = new MismatchedZipException(developVerification, repoVerification);
                if (exception.verificationFailMessageResourceOnly()) {
                    mismatchedOnlyForMessageResource.add(exception);
                } else {
                    mismatched.add(exception);
                }

            } catch (IOException e) {
                e.printStackTrace();
            }
            if (meter >= maxCompare) {
                break;
            }
        }
        System.out.println("\n\n*** STATS *** ");
        System.out.println("There are [" + meter + "] files to compare");
        System.out.println("[" + missingFromDev.size() + "] are missing from develop 6.5");
        System.out.println("[" + missingFromRepo.size() + "] are missing from the new repo");
        System.out.println("[" + developFailedVerification.size() + "] have failed verification in develop 6.5");
        if (debug) {
            for (BundleVerificationStats s : developFailedVerification) {
                System.out.println("  " + s.getValidationResults());
            }
        }
        System.out.println("[" + repoFailedVerification.size() + "] have failed verification in repo");
        if (debug) {
            for (BundleVerificationStats s : repoFailedVerification) {
                System.out.println("  " + s.getValidationResults());
            }
        }
        System.out.println("[" + mismatchedOnlyForMessageResource.size() + "] don't match only because develop doesn't have message-resources");
        System.out.println("[" + mismatched.size() + "] don't match");
        System.out.println("[" + identical.size() + "] are correct");
        System.out.println("*** STATS *** ");

        for (MismatchedZipException m : mismatched) {
            System.out.println(m);
        }

        System.out.println("Missing repo " + missingFromRepo); // TODO println to remove
    }

    private Set<String> gatherStates() {
        Path statePath = Paths.get("/Users/jmccormick/dev/test-state-reporting-state-only/bundle-builder/States");
        String[] directories = statePath.toFile().list(new FilenameFilter() {
            @Override
            public boolean accept(File current, String name) {
                return new File(current, name).isDirectory();
            }
        });
        // System.out.println(Arrays.toString(directories));
        Set<String> states = new TreeSet<String>();
        for (String s : directories) {
            states.add(s.toLowerCase());
        }
        states.add("cn");
        states.remove("sys");
        return states;
    }

    private Set<String> findAllFileNames(Path path) {
        String[] files = path.toFile().list(new FilenameFilter() {
            @Override
            public boolean accept(File current, String name) {
                return new File(current, name).isFile();
            }
        });

        return new TreeSet<String>(Arrays.asList(files));
    }
}

