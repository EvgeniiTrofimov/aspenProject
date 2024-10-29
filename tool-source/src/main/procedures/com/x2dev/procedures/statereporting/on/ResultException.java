/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class ResultException {
    String m_schoolOid;
    String m_msg;

    /**
     * @param m_schoolOid
     * @param m_msg
     */
    public ResultException(String schoolOid, String msg) {
        super();
        m_schoolOid = schoolOid;
        m_msg = msg;
    }

    /**
     * @return the m_schoolOid
     */
    public String getSchoolOid() {
        return m_schoolOid;
    }

    /**
     * @return the m_msg
     */
    public String getMsg() {
        return m_msg;
    }

}
