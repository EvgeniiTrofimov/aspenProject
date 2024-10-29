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

import java.util.ArrayList;
import java.util.List;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class MismatchedZipException extends Exception implements Comparable<MismatchedZipException> {

    final BundleVerificationStats develop;
    final BundleVerificationStats repo;
    final String filename;
    final List<String> filesInRepoNotInDevelop;
    final List<String> filesInDevelopNotInRepo;

    /**
     * @param develop
     * @param repo
     */
    public MismatchedZipException(BundleVerificationStats develop, BundleVerificationStats repo) {
        super();
        this.develop = develop;
        this.repo = repo;
        this.filename = repo.getZipFileName();
        filesInRepoNotInDevelop = new ArrayList<String>(repo.getContentsExcludingWrongStateFiles());
        filesInRepoNotInDevelop.removeAll(develop.getContentsExcludingWrongStateFiles());

        filesInDevelopNotInRepo = new ArrayList<String>(develop.getContentsExcludingWrongStateFiles());
        filesInDevelopNotInRepo.removeAll(repo.getContentsExcludingWrongStateFiles());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n**** Begin " + filename + " ****");
        sb.append("\nFiles in develop only: " + filesInDevelopNotInRepo);
        sb.append("\nFiles in repo only: " + filesInRepoNotInDevelop);
        sb.append("\n**** End " + filename + " ****");
        return sb.toString();
    }

    public boolean verificationFailMessageResourceOnly() {
        if(!filesInDevelopNotInRepo.isEmpty()) {
            return false;
        }
        if(filesInRepoNotInDevelop.size() != 1) {
            return false;
        }
        if(filesInRepoNotInDevelop.get(0).startsWith("message-resource-com.follett.fss.aspen")) {
            return true;
        }
        return false;
    }

    /**
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(MismatchedZipException o) {
        if (this == o) {
            return 0;
        }
        if (o == null) {
            return 1;
        }
        return this.filename.compareTo(o.filename);
    }
}
