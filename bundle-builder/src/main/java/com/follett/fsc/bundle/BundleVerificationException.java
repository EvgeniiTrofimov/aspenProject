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
import java.util.Set;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class BundleVerificationException extends Exception {

    private String identifier = "";

    /**
     *
     * @param extraStates
     * @param myState
     * @param zipFileName
     */
    public BundleVerificationException(Set<String> extraStates, String myState, String zipFileName) {
        super("The following states are included: " + Arrays.toString(extraStates.toArray(new String[0]))
        + " my state is [" + myState + "] for [" + zipFileName + "]");
    }

    /**
     *
     * @param extraStates
     * @param myState
     * @param zipFileName
     * @param cause
     */
    public BundleVerificationException(String myState, String zipFileName, Throwable cause) {
        super("Exception for:  my state [" + myState + "] for [" + zipFileName + "]  " + cause.getMessage(), cause);
    }

    /**
     *
     * @param message
     */
    public BundleVerificationException(String message) {
        super(message);
    }

    /**
     * @see java.lang.Throwable#getMessage()
     */
    @Override
    public String getMessage() {
        return "["+getIdentifier()+"] "+super.getMessage();
    }

    /**
     * @return the identifier
     */
    public String getIdentifier() {
        return identifier;
    }

    /**
     * @param identifier
     */
    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

}
