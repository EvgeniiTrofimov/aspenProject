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
package com.x2dev.procedures.statereporting.bc;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.Collection;
import java.util.LinkedList;

/**
 * The "Nominal Roll" report developed by Aspen is dependent on calculations performed by a BC
 * custom tool. This class
 * is a placeholder class in the appropriate package so the Java source does not have any compiler
 * errors within
 * Eclipse. The bundle definition defines the BC tool that acts as the external source.
 *
 * @author Follett Software Company
 */
public class BcData1701 extends Object {

    /**
     * Placeholder for the BC Data 1701 constructor.
     *
     * @param broker X2Broker
     * @param org Organization
     * @param orgCriteria X2Criteria
     */
    public BcData1701(X2Broker broker, Organization org, X2Criteria orgCriteria) {
        // Placeholder
    }

    /**
     * Placeholder for the BC Data 1701 get1701Students method.
     *
     * @param schoolOid String
     * @return Collection<Bc1701Student>
     */
    public Collection<Bc1701Student> get1701Students(String schoolOid) {
        return new LinkedList<Bc1701Student>();
    }

    /**
     * Placeholder class for the Student information reported on the 1701.
     */
    public class Bc1701Student {

        /**
         * Instantiates a new bc 1701 student.
         */
        public Bc1701Student() {
            // Placeholder
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return null;
        }

        /**
         * Gets the course count.
         *
         * @return float
         */
        public float getCourseCount() {
            return 0;
        }

        /**
         * Gets the sped designation.
         *
         * @return String
         */
        public String getSpedDesignation() {
            return "";
        }

        /**
         * Gets the support block count.
         *
         * @return int
         */
        public int getSupportBlockCount() {
            return 0;
        }
    }
}
