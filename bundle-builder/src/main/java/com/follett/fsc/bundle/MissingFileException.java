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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class MissingFileException extends Exception implements Comparable<MissingFileException> {
    private final String state;
    private final String exportDir;
    private final Set<String> missingFiles;

    /**
     * @param state
     * @param exportDir
     * @param missingFiles
     */
    public MissingFileException(String state, String exportDir, Set<String> missingFiles) {
        super("The following files are missing: " + Arrays.toString(missingFiles.toArray(new String[0])));
        this.exportDir = exportDir;
        this.state = state;
        this.missingFiles = missingFiles;
    }

    public MissingFileException(String state, String exportDir, String missingFile) {
        super("The following file is missing: " + missingFile);
        this.exportDir = exportDir;
        this.state = state;
        this.missingFiles = new HashSet<String>();
        missingFiles.add(missingFile);
    }

    public String getOutputMessage() {
        return "  [" + state + "][" + exportDir + "][" + filesAsString() + "]";
    }

    private String filesAsString() {
        return Arrays.toString(missingFiles.toArray(new String[0]));
    }

    /**
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(MissingFileException obj2) {
        if (this == obj2) {
            return 0;
        }
        if (obj2 == null) {
            return 1;
        }

        if (!this.state.equals(obj2.state)) {
            return this.state.compareTo(obj2.state);
        }
        if (!this.exportDir.equals(obj2.exportDir)) {
            return this.exportDir.compareTo(obj2.exportDir);
        }

        return this.filesAsString().compareTo(obj2.filesAsString());
    }

}
