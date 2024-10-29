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
package com.x2dev.procedures.statereporting.on.revised;


import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSalep;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisSalep.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSalep extends OnsisStateReportData {
    /**
     * The Class OnsisSalepEntity.
     */
    public static class OnsisSalepEntity extends OnsisStateReportEntity {
        private OnsisCsvDataRecord m_csvRecord = null;
        private OnsisSalep m_onsisSalep;
        private OnStudentSalep m_salToReport = null;
        private Range<Date> m_dateRange;

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                PlainDate startDate = (m_csvRecord != null)
                        ? getCsvDate(m_csvRecord, OnsisExtractHelper.CsvField.SAL_START_DATE)
                        : getStudentProgram().getStartDate();

                PlainDate endDate = getStudentProgram().getEndDate();
                PlainDate endDatePlusOne = DateUtils.add(m_onsisSalep.getGlobalData().getEndDate(), 1);
                if (endDate != null && endDate.after(endDatePlusOne)) {
                    endDate = null;
                }
                m_dateRange = Range.of(startDate, endDate);
            }
            return m_dateRange;
        }

        /**
         * Gets the end date:
         * - From SAL record, but null if future.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return (PlainDate) getDateRange().getEnd();
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisStudent student = (OnsisStudent) getBean();
            String name = student.getNameView();

            return name;
        }

        /**
         * Gets the exit type.
         *
         * @return String
         */
        public String getExitType() {
            String value = null;
            if (getEndDate() != null) {
                value = getStudentProgram().getExitType();
            }
            return value;
        }

        /**
         * Gets the start date:
         * - From CSV
         * - Else from SAL record.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return (PlainDate) getDateRange().getStart();
        }

        /**
         * Gets the student program participation.
         *
         * @return Graduation student program participation
         */
        public OnStudentSalep getStudentProgram() {
            return m_salToReport;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_onsisSalep = (OnsisSalep) getData();

            /*
             * Export SAL only if:
             * - Primary enrollment school
             * - Age on Dec 31 < 18
             * - SAL in CSV or SAL active on Count Date
             *
             * SAL record to export:
             * - The latest SAL overlapping the submission period
             * whether active on count date or not
             * (must also overlap enrollment span if under Enrolment).
             * - Else when no overlapping SAL exists, normal Delete logic applies
             *
             * SAL Start Date:
             * - From CSV
             * - Else from SAL record
             *
             * SAL End Date:
             * - From SAL record, but null if future
             *
             * CSV keys:
             * - BSID + OEN only. Not Start Date.
             * - Add Start Date to generated Deletes.
             */

            // SALEP should only report in the main school
            OnsisStateReportEntity parentEntity = getReportData().getParentEntity();
            if (parentEntity instanceof OnsisStudentEnrollmentEntity) {
                OnsisAnnualSpan span = ((OnsisStudentEnrollmentEntity) parentEntity).getSpan();
                if (span == null || span.isSecondary()) {
                    setRowCount(0);
                    return;
                }
            }

            OnsisStudent student = (OnsisStudent) getBean();
            int age = student.getAgeAsOfDate(new PlainDate(getGlobalData().getDateDecember31()));
            if (age >= 18) { // MSA0006
                setRowCount(0);
                return;
            }

            /*
             * Get SAL records
             */
            List<OnStudentSalep> descendingOverlappingSals = getStudentPrograms(student);

            /*
             * Filter by enrollment span dates
             */
            if (parentEntity instanceof OnsisStudentEnrollmentEntity) {
                PlainDate enrStartDate = ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentStartDate();
                PlainDate enrEndDate = ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentEndDate();
                Range<Date> dateRange = Range.of(enrStartDate, enrEndDate);

                descendingOverlappingSals = descendingOverlappingSals.stream()
                        .filter(sal -> dateRange.isOverlap(sal.getDateRange()))
                        .collect(Collectors.toList());
            }

            /*
             * Take the latest of the overlapping SAL records
             */
            OnStudentSalep latestSal = descendingOverlappingSals.isEmpty()
                    ? null
                    : descendingOverlappingSals.get(0);

            /*
             * Export SAL only if:
             * - SAL in CSV
             * - OR SAL active on Count Date
             */
            String bsid = deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString());
            String oen = deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.OEN.toString());
            m_csvRecord = getFirstCsvRecord(bsid, oen);

            boolean hasActiveSal =
                    latestSal != null && latestSal.getDateRange().contains(getGlobalData().getEndDate());

            boolean shouldReportSal = hasActiveSal || m_csvRecord != null;

            /*
             * SAL record to export:
             * - Latest SAL overlapping the submission period if it exists
             * (whether active on count date or not)
             * - Else when no overlapping SAL exists, normal Delete logic applies
             */
            if (shouldReportSal) {
                m_salToReport = latestSal;
            }

            setRowCount(m_salToReport == null ? 0 : 1);
        }

        /**
         * After render row fields.
         *
         * @param entityElement Element
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#afterRenderRowFields(org.w3c.dom.Element)
         */
        @Override
        protected void afterRenderRowFields(Element entityElement) {
            super.afterRenderRowFields(entityElement);

            /*
             * Don't send <COMPONENT> that have Action=Update.
             * Because COMPONENT is an all-key object
             * so it's either Add or Delete but never changed via Update.
             *
             * Updates are removed here by the parent <SALEP>
             * AFTER call to OnsisSscOtherCourseInfo.generateDeletes()
             * so that Deletes won't be generated
             * for COMPONENT that were Updates.
             */
            List<Element> componentUpdates = getElementsWithChildValue(ELEMENT_COMPONENT, entityElement,
                    OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_UPDATE);
            for (Element component : componentUpdates) {
                entityElement.removeChild(component);
            }

            if (m_csvRecord != null && getCsvDate(m_csvRecord, OnsisExtractHelper.CsvField.SAL_END_DATE) != null) {
                getElementsWithChildValue(ELEMENT_COMPONENT, entityElement,
                        OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_DELETE).stream()
                                .forEach(component -> entityElement.removeChild(component));
            }

        }

        /**
         * Gets the first csv record.
         *
         * @param bsid String
         * @param oen String
         * @return Onsis csv data record
         */
        private OnsisCsvDataRecord getFirstCsvRecord(String bsid, String oen) {
            Collection<OnsisCsvDataRecord> csvRecords = getReportData().getMatcher().findRecords(
                    Arrays.asList(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                            OnsisExtractHelper.CsvField.OEN.toString()),
                    Arrays.asList(bsid, oen));

            return csvRecords != null && !csvRecords.isEmpty()
                    ? csvRecords.iterator().next()
                    : null;
        }

        /**
         * Gets the student programs.
         *
         * @param student OnsisStudent
         * @return List
         */
        private List<OnStudentSalep> getStudentPrograms(OnsisStudent student) {
            return student.getSalepPrograms(getBroker()).stream()
                    .sorted(new Comparator<OnStudentSalep>() {
                        @Override
                        public int compare(OnStudentSalep o1, OnStudentSalep o2) {
                            int compare = OnsisHelper.compareEndDates(o1.getEndDate(), o2.getEndDate());
                            if (compare == 0) {
                                compare = OnsisHelper.compareStartDates(o1.getStartDate(), o2.getStartDate());
                            }
                            return -compare;
                        }
                    })
                    .collect(Collectors.toList());
        }
    }

    public static final String ELEMENT_COMPONENT = "COMPONENT";
    public static final String ELEMENT_END_DATE = "END_DATE";
    public static final String ELEMENT_START_DATE = "START_DATE";

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans((Arrays.asList((OnsisStudent) getParentEntity().getBean())));
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.follett.fsc.aspensif.framework.SifStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Generate and append delete.
     *
     * @param record OnsisCsvDataRecord
     * @param currentEntityKeySet List<String>
     * @param currentEntityValueSet List<String>
     * @param parentElement Element
     * @return Element
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateAndAppendDelete(com.x2dev.procedures.statereporting.on.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord,
     *      java.util.List, java.util.List, org.w3c.dom.Element)
     */
    @Override
    protected Element generateAndAppendDelete(OnsisCsvDataRecord record,
                                              List<String> currentEntityKeySet,
                                              List<String> currentEntityValueSet,
                                              Element parentElement) {
        Element thisElement =
                super.generateAndAppendDelete(record, currentEntityKeySet, currentEntityValueSet, parentElement);

        // Get additional required fields from the CSV and add onto this Element
        if (record == null) {
            return thisElement;
        }

        String csvStartDate = record.getSingleFieldValue(OnsisExtractHelper.CsvField.SAL_START_DATE);

        appendTextElement(ELEMENT_START_DATE, csvStartDate, thisElement);

        return thisElement;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSalepEntity.class);
    }

}
