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
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Florida state export procedure for Student ELL.
 *
 * @author X2 Development Corporation
 */
public class FASTERRequest extends StateReportData {
    protected SimpleDateFormat m_dobFormat = new SimpleDateFormat("yyyyMMdd");
    protected SimpleDateFormat m_gradFormat = new SimpleDateFormat("ddMMyyyy");

    /**
     * Implementation of StateReportEntity to be used by the FL ELL export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class FASTERRequestEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public FASTERRequestEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";

            return name;
        }

        /**
         * After generating all output, write the request record out to a new DataRequest entry.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            /*
             * Create and save a DataRequest record for this request line.
             */
            DataRequest request = X2BaseBean.newInstance(DataRequest.class, getData().getBroker().getPersistenceKey());

            request.setOrganization1Oid(getData().getOrganization().getOid());
            request.setType("FASTER");
            request.setStatus(DataRequest.RequestStatusCode.SENT.ordinal());
            request.setId(getFieldValue("Local Id").trim());
            request.setStudentOid(getBean().getOid());
            request.setRequestDate(new PlainDate());
            request.setLastName(getFieldValue("Last Name").trim());
            request.setFirstName(getFieldValue("First Name").trim());
            request.setMiddleName(getFieldValue("Middle Name").trim());
            request.setNameSuffixCode(getFieldValue("Appendage").trim());
            request.setAltFirstName(getFieldValue("Nick Name").trim());
            request.setAltLastName(getFieldValue("Alt Last Name 1").trim());
            request.setStateId(getFieldValue("Primary Student Id").trim());
            request.setLocalId(getFieldValue("Local Id").trim());
            request.setGenderCode(getFieldValue("Gender").trim());
            try {
                request.setBirthdate(new PlainDate(
                        ((FASTERRequest) getData()).m_dobFormat.parse(getFieldValue("Date of Birth").trim())));
            } catch (ParseException e) {
                // If it will not parse, leave null
            }
            try {
                request.setGraduationDate(new PlainDate(((FASTERRequest) getData()).m_gradFormat
                        .parse("01" + getFieldValue("Graduation Date").trim())));
            } catch (ParseException e) {
                // If it will not parse, leave null
            }

            /*
             * Set the race codes.
             */
            request.setHispanicLatinoIndicator("Y".equalsIgnoreCase(getFieldValue("Ethnicity").trim()));
            request.setAsianIndicator("Y".equalsIgnoreCase(getFieldValue("Asian").trim()));
            request.setNativeAmericanIndicator("Y".equalsIgnoreCase(getFieldValue("AmericaAlaskaIndian").trim()));
            request.setPacificIndicator("Y".equalsIgnoreCase(getFieldValue("Pacific Islander").trim()));
            request.setBlackIndicator("Y".equalsIgnoreCase(getFieldValue("African Black").trim()));
            request.setWhiteIndicator("Y".equalsIgnoreCase(getFieldValue("White").trim()));

            /*
             * Rebuild the export record from all fields.
             * Add it to the request record.
             */
            StringBuilder requestRecord = new StringBuilder(1024);
            for (int i = 0; i < getData().getFieldCount(); i++) {
                String value = getFieldValue(i);
                FieldDefinition field = getData().getFieldDefinition(i);
                value = ExportFormatManager.doPadding(value, field.getResizeMode().ordinal(), field.getPaddingChar(),
                        field.getMaxLength());
                requestRecord.append(value);
            }
            request.setRequestRecord(requestRecord.toString());

            getData().getBroker().saveBean(request);
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        setEntityClass(FASTERRequestEntity.class);

        // Get student active status.
        // Get student indicator and active codes.
        String fasterFlag = translateAliasToJavaName("FASTER Request Type", true);

        // build the query for students to report.
        Criteria studentCriteria = new Criteria();
        // studentCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, activeCode);
        studentCriteria.addNotNull(fasterFlag);

        if (getSchool() != null) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        studentQuery.addOrderBy(SisStudent.COL_NAME_VIEW, true);
        setQuery(studentQuery);
    }
}
