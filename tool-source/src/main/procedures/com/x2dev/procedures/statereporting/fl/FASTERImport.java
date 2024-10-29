/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.DataRequest;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

/**
 * This import reads incoming FASTER files and determines the proper
 * records types and actions.
 *
 * @author X2 Development Corporation
 */
public class FASTERImport extends TextImportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    SimpleDateFormat m_dobFormat = new SimpleDateFormat("yyyyMMdd");
    SimpleDateFormat m_gradFormat = new SimpleDateFormat("ddMMyyyy");

    /**
     * Current request record, from the most recent header record.
     */
    private DataRequest m_request;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        // Column delimited rows, come in as one field.
        return 1;
    }

    /**
     * Returns one field of length 1020. All record types have a uniform length
     * of 1020 characters. The three character record type code determines the record layout.
     *
     * @return int[]
     */
    @Override
    protected int[] getFieldLengths() {
        int[] fieldLengths = new int[] {1020};

        return fieldLengths;
    }

    /**
     * Interpret the record type from the record type field,
     * then pass to the appropriate record parser.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        String line = record.get(0);
        String recType = line.substring(0, 3);

        /*
         * Check record types:
         *
         */
        if (recType != null && recType.substring(1, 3).equals("00")) {
            importHeader(line);
        } else if (recType != null && m_request != null) {
            String typeCode = recType.substring(1, 3);
            if (typeCode.equals("01")) {
                // importDemographics(line);
            } else if (typeCode.equals("02")) {
                // importHealth(line);
            }
        }
    }


    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setUseEscapes(false);
        setUseValueDelimiters(false);
        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
    }

    /**
     * When the record line is a header record (X00),
     * import the header info into a new DataRequest record.
     *
     * @param line String
     */
    private void importHeader(String line) {
        /*
         * Message type determines the action (request, reply)
         */
        // String messageType = line.substring(35, 38).trim();
        //
        // String toDistrict = line.substring(18, 20).trim();
        // String toSchool = line.substring(20, 24).trim();
        // String fromDistrict = line.substring(29, 31).trim();
        // String fromSchool = line.substring(31, 35).trim();
        // String testProd = line.substring(38, 39).trim();
        //
        // String studentId = line.substring(3, 13).trim();
        String sasid = line.substring(3, 13).trim();
        String localId = line.substring(48, 60).trim();

        String lastName = line.substring(60, 77).trim();
        String firstName = line.substring(80, 92).trim();
        String middleName = line.substring(92, 102).trim();
        String suffix = line.substring(77, 80).trim();
        String altLastName1 = line.substring(102, 119).trim();
        // String altLastName2 = line.substring(119, 136).trim();
        String nickName = line.substring(136, 146).trim();

        String ethnicity = line.substring(146, 147).trim();
        String raceNativeAm = line.substring(147, 148).trim();
        String raceAsian = line.substring(148, 149).trim();
        String raceBlack = line.substring(149, 150).trim();
        String racePacific = line.substring(150, 151).trim();
        String raceWhite = line.substring(151, 152).trim();

        String gender = line.substring(152, 153).trim();
        // String raceCode = line.substring(153, 154).trim();
        String gradDate = line.substring(200, 206).trim();
        String dataOfBirth = line.substring(206, 214).trim();

        // String senderInstId = line.substring(170, 200).trim();
        // String SE_IdType = line.substring(167, 170).trim();
        // String SE_TransControlId = line.substring(214, 223).trim();
        // String SE_InstId = line.substring(223, 238).trim();

        String postDate = line.substring(238, 256).trim();

        X2Broker broker = getBroker();
        m_request = X2BaseBean.newInstance(DataRequest.class, broker.getPersistenceKey());

        /*
         * Set request and batch identification information.
         */
        m_request.setOrganization1Oid(getOrganization().getOid());
        m_request.setType("FASTER");
        m_request.setStatus(DataRequest.RequestStatusCode.INCOMING.ordinal());
        m_request.setId(localId);
        m_request.setBatchId(localId);
        try {
            m_request.setRequestDate(new PlainDate(m_dobFormat.parse(postDate)));
        } catch (ParseException e1) {
            // If it will not parse, leave null
        }

        /*
         * Set student identification fields.
         */
        m_request.setLastName(lastName);
        m_request.setFirstName(firstName);
        m_request.setMiddleName(middleName);
        m_request.setNameSuffixCode(suffix);
        m_request.setAltFirstName(nickName);
        m_request.setAltLastName(altLastName1);
        m_request.setStateId(sasid);
        m_request.setLocalId(localId);
        m_request.setGenderCode(gender);
        try {
            m_request.setBirthdate(new PlainDate(m_dobFormat.parse(dataOfBirth)));
        } catch (ParseException e) {
            // If it will not parse, leave null
        }
        try {
            m_request.setGraduationDate(new PlainDate(m_gradFormat.parse("01" + gradDate)));
        } catch (ParseException e) {
            // If it will not parse, leave null
        }

        /*
         * Set the race codes.
         */
        m_request.setHispanicLatinoIndicator("Y".equalsIgnoreCase(ethnicity));
        m_request.setAsianIndicator("Y".equalsIgnoreCase(raceAsian));
        m_request.setNativeAmericanIndicator("Y".equalsIgnoreCase(raceNativeAm));
        m_request.setPacificIndicator("Y".equalsIgnoreCase(racePacific));
        m_request.setBlackIndicator("Y".equalsIgnoreCase(raceBlack));
        m_request.setWhiteIndicator("Y".equalsIgnoreCase(raceWhite));

        /*
         * Set the entire line contents
         */
        m_request.setRequestRecord(line);

        /*
         * Save the record.
         */
        broker.saveBean(m_request);
        incrementInsertCount();
    }

}
