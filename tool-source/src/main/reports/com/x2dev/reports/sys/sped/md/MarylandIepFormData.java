/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.md;

import static com.x2dev.sis.model.business.sped.MarylandAliases.*;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryUtils;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.sped.MarylandAliases;
import com.x2dev.sis.web.sped.md.TestingAccommodationAttributes;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Maryland IEP form. This class prepares a ReportDataGrid that contains a row
 * for each section of the IEP. Each row contains a format and a java source for the corresponding
 * section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>1 - Student Info</td>
 * <td>SimpleBeanDataSource and series of parameters for team members and meeting dates</td>
 * </tr>
 * <tr>
 * <td>2a - Initial Eligibility</td>
 * <td>SimpleBeanDataSource and a series of parameters for disabilities</td>
 * </tr>
 * <tr>
 * <td>2b - Continued Eligibility</td>
 * <td>SimpleBeanDataSource and a series of parameters for disabilities</td>
 * </tr>
 * <tr>
 * <td>3 - Assessment/Grad Info</td>
 * <td>SimpleBeanDataSource</td>
 * </tr>
 * <tr>
 * <td>4 - Performance Entries</td>
 * <td>BeanCollectionDataSource</td>
 * </tr>
 * <tr>
 * <td>5 - Performance Questions</td>
 * <td>SimpleBeanDataSource</td>
 * </tr>
 * <tr>
 * <td>6 - Special Considerations</td>
 * <td>ReportDataGrid containing subreport support for each different type of consideration</td>
 * </tr>
 * <tr>
 * <td>7 - Testing Accommodations</td>
 * <td>ReportDataGrid containing a row for each accommodation</td>
 * </tr>
 * <tr>
 * <td>8 - Supplementary Aids</td>
 * <td>BeanCollectionDataSource</td>
 * </tr>
 * <tr>
 * <td>9 - Extended School Year</td>
 * <td>SimpleBeanDataSource</td>
 * </tr>
 * <tr>
 * <td>10 - Transition</td>
 * <td>SimpleBeanDataSource</td>
 * </tr>
 * <tr>
 * <td>11 - Transition Activites</td>
 * <td>ReportDataGrid containing a row for each activity response</td>
 * <tr>
 * <td>12 - Transition Services</td>
 * <td>BeanCollectionDataSource</td>
 * <tr>
 * <td>13 - Goals</td>
 * <td>ReportDataGrid containing subreport support for each section of the goals page (i.e. details,
 * objectives, progress)</td>
 * <tr>
 * <td>14 - Services</td>
 * <td>BeanCollectionDataSource</td>
 * <tr>
 * <td>15 - LRE/Placement</td>
 * <td>SimpleBeanDataSource</td>
 * <tr>
 * <td>16 - Authorizations</td>
 * <td>SimpleBeanDataSource</td>
 * </tr>
 * </table>
 *
 * @author X2 Development Corporation
 */
public class MarylandIepFormData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PARAMETER_MAP = "parameters";

    // ReportDataGrid column constants - common
    private static final String COL_IEP = "iep";

    // ReportDataGrid column constants for section 7 (instructional and testing accommodations)
    private static final String COL_ACCOMMODATION = "accommodation";
    private static final String COL_ATTRIBUTES = "attributes";
    private static final String COL_NAME = "name";
    private static final String COL_GROUP_HEADER = "groupHeader";
    private static final String COL_OTHER = "other";
    private static final String COL_REFERENCE_CODE = "referenceCode";
    private static final String COL_STATE_CODE = "stateCode";

    // ReportDataGrid column constants for section 11 (transition activities)
    private static final String COL_ACTIVITIES = "activities";
    private static final String COL_RESPONSIBLE = "responsible";

    // ReportDataGrid column constants for page 13 (goals)
    private static final String COL_GOAL = "goal";

    private static final String FORMAT_ID_IEP1 = "SYS-SPED-MD-IEP1";
    private static final String FORMAT_ID_IEP2A = "SYS-SPED-MD-IEP2A";
    private static final String FORMAT_ID_IEP2B = "SYS-SPED-MD-IEP2B";
    private static final String FORMAT_ID_IEP3 = "SYS-SPED-MD-IEP3";
    private static final String FORMAT_ID_IEP4 = "SYS-SPED-MD-IEP4";
    private static final String FORMAT_ID_IEP5 = "SYS-SPED-MD-IEP5";
    private static final String FORMAT_ID_IEP6 = "SYS-SPED-MD-IEP6";

    private static final String FORMAT_ID_IEP6_1 = "SYS-SPED-MD-IEP6-1";
    private static final String FORMAT_ID_IEP6_2 = "SYS-SPED-MD-IEP6-2";
    private static final String FORMAT_ID_IEP6_3 = "SYS-SPED-MD-IEP6-3";
    private static final String FORMAT_ID_IEP6_4 = "SYS-SPED-MD-IEP6-4";
    private static final String FORMAT_ID_IEP6_5 = "SYS-SPED-MD-IEP6-5";
    private static final String FORMAT_ID_IEP6_6 = "SYS-SPED-MD-IEP6-6";

    private static final String FORMAT_ID_IEP7 = "SYS-SPED-MD-IEP7";
    private static final String FORMAT_ID_IEP8 = "SYS-SPED-MD-IEP8";
    private static final String FORMAT_ID_IEP9 = "SYS-SPED-MD-IEP9";
    private static final String FORMAT_ID_IEP10 = "SYS-SPED-MD-IEP10";

    private static final String FORMAT_ID_IEP11 = "SYS-SPED-MD-IEP11";
    private static final String FORMAT_ID_IEP12 = "SYS-SPED-MD-IEP12";
    private static final String FORMAT_ID_IEP13 = "SYS-SPED-MD-IEP13";

    private static final String FORMAT_ID_IEP13_1 = "SYS-SPED-MD-IEP13-1";
    private static final String FORMAT_ID_IEP13_2 = "SYS-SPED-MD-IEP13-2";
    private static final String FORMAT_ID_IEP13_3 = "SYS-SPED-MD-IEP13-3";

    private static final String FORMAT_ID_IEP14 = "SYS-SPED-MD-IEP14";
    private static final String FORMAT_ID_IEP15 = "SYS-SPED-MD-IEP15";
    private static final String FORMAT_ID_IEP16 = "SYS-SPED-MD-IEP16";

    private static final String PARAM_DISABILITY_AFFECTED_AREAS = "disabilityAffectedAreas";
    private static final String PARAM_MEETING_DATE_VIEW = "meetingDateView";
    private static final String PARAM_TEAM_MEMBER = "teamMember";
    private static final String PARAM_INITIAL_DISABILITY = "initialDisability";
    private static final String PARAM_CONTINUED_DISABILITY = "continuedDisability";

    private IepData m_currentIep = null;
    private int m_currentPageNumber = 0;
    private DataDictionary m_dictionary = null;
    private Map m_subReports = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        m_dictionary = DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());

        loadSubReports();

        m_currentPageNumber = 0;

        prepareSection1(grid);
        prepareSection2a(grid);
        prepareSection2b(grid);
        prepareSection3(grid);
        prepareSection4(grid);
        prepareSection5(grid);
        prepareSection6(grid);
        prepareSection7(grid);
        prepareSection8(grid);
        prepareSection9(grid);
        prepareSection10(grid);
        prepareSection11(grid);
        prepareSection12(grid);
        prepareSection13(grid);
        prepareSection14(grid);
        prepareSection15(grid);
        prepareSection16(grid);

        addParameter(PARAM_DICTIONARY, m_dictionary);

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Returns the format of subreport for the given report ID constant.
     *
     * @param reportId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String reportId) {
        Report report = (Report) m_subReports.get(reportId);
        return report.getCompiledFormat();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));

            addFormParameters();
        }
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {FORMAT_ID_IEP1,
                FORMAT_ID_IEP2A,
                FORMAT_ID_IEP2B,
                FORMAT_ID_IEP3,
                FORMAT_ID_IEP4,
                FORMAT_ID_IEP5,
                FORMAT_ID_IEP6,
                FORMAT_ID_IEP6_1,
                FORMAT_ID_IEP6_2,
                FORMAT_ID_IEP6_3,
                FORMAT_ID_IEP6_4,
                FORMAT_ID_IEP6_5,
                FORMAT_ID_IEP6_6,
                FORMAT_ID_IEP7,
                FORMAT_ID_IEP8,
                FORMAT_ID_IEP9,
                FORMAT_ID_IEP10,
                FORMAT_ID_IEP11,
                FORMAT_ID_IEP12,
                FORMAT_ID_IEP13,
                FORMAT_ID_IEP13_1,
                FORMAT_ID_IEP13_2,
                FORMAT_ID_IEP13_3,
                FORMAT_ID_IEP14,
                FORMAT_ID_IEP15,
                FORMAT_ID_IEP16}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepares the data source for section 1 - student info.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection1(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        StringBuilder meetingDates = new StringBuilder();

        Converter dateConverter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());

        for (IepMeeting meeting : iep.getIepMeeting(getBroker())) {
            if (meetingDates.length() > 0) {
                meetingDates.append(", ");
            }

            meetingDates.append(dateConverter.javaToString(meeting.getDate()));
        }
        addParameter(PARAM_MEETING_DATE_VIEW, meetingDates.toString());

        int count = 0;
        for (IepTeamMember teamMember : iep.getTeamMembers(getBroker())) {
            addParameter(PARAM_TEAM_MEMBER + count, teamMember);
            count++;
        }

        StringBuilder areasAffected = new StringBuilder();
        for (IepPerformanceLevel performanceLevel : iep.getIepPerformanceLevel(getBroker())) {
            String isAffected =
                    (String) performanceLevel.getFieldValueByAlias("perf-disability-affected", getDictionary());

            if ("Yes".equalsIgnoreCase(isAffected)) {
                if (areasAffected.length() > 0) {
                    areasAffected.append(", ");
                }

                String affectedArea = (String) performanceLevel.getFieldValueByAlias("perf-area", getDictionary());

                if ("Other".equalsIgnoreCase(affectedArea)) {
                    affectedArea = (String) performanceLevel.getFieldValueByAlias("perf-area-other", getDictionary());
                }

                areasAffected.append(affectedArea);
            }
        }
        addParameter(PARAM_DISABILITY_AFFECTED_AREAS, areasAffected.toString());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP1));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 2 - initial eligibility.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection2a(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        int initialCount = 0;

        BooleanAsStringConverter converter = (BooleanAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.BOOLEAN_CONVERTER, getLocale(), true);

        for (IepDisability disability : iep.getIepDisability(getBroker())) {
            Boolean initial = (Boolean) converter.parseSystemString(
                    (String) disability.getFieldValueByAlias(MarylandAliases.DISABILITY_INITIAL, m_dictionary));

            if (initial.booleanValue()) {
                addParameter(PARAM_INITIAL_DISABILITY + initialCount, disability);
                initialCount++;
            }
        }

        int continuedCount = 0;

        for (IepDisability disability : iep.getIepDisability(getBroker())) {
            Boolean initial = (Boolean) converter.parseSystemString(
                    (String) disability.getFieldValueByAlias(MarylandAliases.DISABILITY_INITIAL, m_dictionary));

            if (!initial.booleanValue()) {
                addParameter(PARAM_CONTINUED_DISABILITY + continuedCount, disability);
                continuedCount++;
            }
        }

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP2A));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 2 - continued eligibility.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection2b(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        int initialCount = 0;

        BooleanAsStringConverter converter = (BooleanAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.BOOLEAN_CONVERTER, getLocale(), true);

        for (IepDisability disability : iep.getIepDisability(getBroker())) {
            Boolean initial = (Boolean) converter.parseSystemString(
                    (String) disability.getFieldValueByAlias(MarylandAliases.DISABILITY_INITIAL, m_dictionary));

            if (initial.booleanValue()) {
                addParameter(PARAM_INITIAL_DISABILITY + initialCount, disability);
                initialCount++;
            }
        }

        int continuedCount = 0;

        for (IepDisability disability : iep.getIepDisability(getBroker())) {
            Boolean initial = (Boolean) converter.parseSystemString(
                    (String) disability.getFieldValueByAlias(MarylandAliases.DISABILITY_INITIAL, m_dictionary));

            if (!initial.booleanValue()) {
                addParameter(PARAM_CONTINUED_DISABILITY + continuedCount, disability);
                continuedCount++;
            }
        }

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP2B));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 3 - assessment/graduation info.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection3(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP3));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 4 - performance entries.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection4(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        ArrayList<IepPerformanceLevel> performance;
        if (isBlank()) {
            performance = new ArrayList<IepPerformanceLevel>(8);

            DataDictionaryField typeField = getDictionary().findDataDictionaryField(IepPerformanceLevel.class.getName(),
                    IepPerformanceLevel.COL_TYPE);
            ReferenceTable types = typeField.getReferenceTable();

            for (ReferenceCode code : types.getReferenceCodes(getBroker())) {
                IepPerformanceLevel blankPerformance = new IepPerformanceLevel(getBroker().getPersistenceKey());
                blankPerformance.setType(code.getCode());
                performance.add(blankPerformance);
            }
        } else {
            performance = new ArrayList<IepPerformanceLevel>(iep.getIepPerformanceLevel(getBroker()));
        }

        if (!performance.isEmpty()) {
            sortPerformanceLevels(performance);

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, new BeanCollectionDataSource(performance,
                    m_dictionary,
                    getLocale()));
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP4));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 5 - performance questions.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection5(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP5));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 6 - special considerations.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection6(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        ReportDataGrid accommodations = new ReportDataGrid();

        String[] categories = new String[] {ACCOMMODATION_CATEGORY_COMMUNICATION,
                ACCOMMODATION_CATEGORY_AT,
                ACCOMMODATION_CATEGORY_VISUAL,
                ACCOMMODATION_CATEGORY_HEARING,
                ACCOMMODATION_CATEGORY_BEHAVIORAL,
                ACCOMMODATION_CATEGORY_LEP};

        int formatNumber = 0;
        for (String category : categories) {
            IepAccommodation accommodation = null;
            if (isBlank()) {
                accommodation = new IepAccommodation(getBroker().getPersistenceKey());
                accommodation.setCategory(category);
            } else {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(IepAccommodation.COL_CATEGORY, category);
                criteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, iep.getOid());

                QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);

                accommodation = (IepAccommodation) getBroker().getBeanByQuery(query);
            }

            formatNumber++;

            if (accommodation != null) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                        accommodation.getExtendedDataDictionary(), getBroker().getPersistenceKey());
                JRDataSource dataSource = new SimpleBeanDataSource(accommodation, dictionary, getLocale());

                accommodations.append();
                accommodations.set(COL_IEP, iep);
                accommodations.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
                accommodations.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP6 + "-" + (formatNumber)));
                accommodations.set(COL_PARAMETER_MAP, getParameters());
                accommodations.set(COL_PAGE_NUMBER, Integer.valueOf(m_currentPageNumber));
            }
        }

        if (accommodations.rowCount() > 0) {
            accommodations.beforeTop();

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, accommodations);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP6));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 7 - instructional and testing accommodations.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection7(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        ReportDataGrid accommodations = new ReportDataGrid();

        DataDictionaryField field =
                getDictionary().findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_NAME);

        Criteria referenceCriteria = new Criteria();
        referenceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

        QueryByCriteria referenceQuery = new QueryByCriteria(ReferenceCode.class, referenceCriteria);

        Map<String, Map<String, ReferenceCode>> referenceLookup =
                getBroker().getNestedMapByQuery(referenceQuery, ReferenceCode.COL_DEPENDENCY_CODE,
                        ReferenceCode.COL_CODE, 5, 32);

        if (isBlank()) {
            for (String category : referenceLookup.keySet()) {
                Map<String, ReferenceCode> codesForCategory = referenceLookup.get(category);

                for (ReferenceCode code : codesForCategory.values()) {
                    IepAccommodation accommodation = new IepAccommodation(getBroker().getPersistenceKey());
                    accommodation.setExtendedDataDictionaryOid(getDictionary().getExtendedDictionaryOid());
                    accommodation.setName(code.getCode());
                    accommodation.setCategory(category);
                    accommodation.setType(ACCOMMODATION_TYPE_TESTING);

                    prepareSection7_addAccommodation(accommodations, accommodation, code);

                }
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepAccommodation.COL_TYPE, ACCOMMODATION_TYPE_TESTING);
            criteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    IepAccommodation accommodation = (IepAccommodation) iterator.next();

                    if (accommodation.getExtendedDataDictionary() != null) {
                        String name = accommodation.getName();

                        Map<String, ReferenceCode> codesForCategory = referenceLookup.get(accommodation.getCategory());

                        if (codesForCategory != null) {
                            ReferenceCode code = codesForCategory.get(name);

                            if (code != null) {
                                prepareSection7_addAccommodation(accommodations, accommodation, code);
                            }
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }

        if (accommodations.rowCount() > 0) {
            accommodations.sort(COL_STATE_CODE, false);
            accommodations.beforeTop();

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, accommodations);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP7));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Helper method to <code>prepareSection7</code>. Adds an accommodation entry to the passed
     * ReportDataGrid
     *
     * @param accommodationGrid ReportDataGrid
     * @param accommodation IepAccommodation
     * @param code ReferenceCode
     */
    private void prepareSection7_addAccommodation(ReportDataGrid accommodationGrid,
                                                  IepAccommodation accommodation,
                                                  ReferenceCode code) {
        TestingAccommodationAttributes attributes = new TestingAccommodationAttributes(code);

        accommodationGrid.append();
        accommodationGrid.set(COL_ACCOMMODATION, accommodation);
        accommodationGrid.set(COL_ATTRIBUTES, attributes);
        accommodationGrid.set(COL_IEP, getFormStorage());
        accommodationGrid.set(COL_GROUP_HEADER,
                LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(getLocale(),
                        "label.marylandTestingAccommodations.group." + attributes.getGroup()));
        accommodationGrid.set(COL_OTHER,
                accommodation.getFieldValueByAlias(TESTING_ACCOMMODATION_OTHER, getDictionary()));
        accommodationGrid.set(COL_NAME, code.getCode());
        accommodationGrid.set(COL_REFERENCE_CODE, code);
        accommodationGrid.set(COL_STATE_CODE, code.getStateCode()); // Set explicitly for sorting
                                                                    // purposes
    }

    /**
     * Prepares the data source for section 8 - supplementary aids and services.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection8(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        Collection<IepOtherService> services = null;

        if (isBlank()) {
            int blankServices = 3;

            services = new ArrayList<IepOtherService>(blankServices);

            for (int i = 0; i < blankServices; i++) {
                IepOtherService service = new IepOtherService(getBroker().getPersistenceKey());
                service.setServiceType(SUPPLEMENTARY_SERVICE_TYPE_CODE);

                services.add(service);
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepOtherService.COL_SERVICE_TYPE, SUPPLEMENTARY_SERVICE_TYPE_CODE);
            criteria.addEqualTo(IepOtherService.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria query = new QueryByCriteria(IepOtherService.class, criteria);
            query.addOrderByAscending(IepOtherService.COL_START_DATE);

            services = getBroker().getCollectionByQuery(query);
        }

        if (services != null && !services.isEmpty()) {
            IepOtherService firstService = services.iterator().next();
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(firstService.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, new BeanCollectionDataSource(services,
                    dictionary,
                    getLocale()));
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP8));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 9 - extended school year.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection9(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP9));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 10 - transition.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection10(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP10));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 11 - transition activities.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection11(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        ReportDataGrid responses = new ReportDataGrid();

        String[] aliases = new String[] {TRANSITION_ACTIVITY_ACADEMIC,
                TRANSITION_ACTIVITY_EMPLOYMENT_TRAINING,
                TRANSITION_ACTIVITY_DAILY_LIVING,
                TRANSITION_ACTIVITY_INDEPENDENT_LIVING,
                TRANSITION_ACTIVITY_TRANSPORTATION};

        for (String alias : aliases) {
            responses.append();
            responses.set(COL_IEP, iep);
            responses.set(COL_ACTIVITIES, iep.getFieldValueByAlias(alias, m_dictionary));
            responses.set(COL_RESPONSIBLE,
                    iep.getFieldValueByAlias(alias + TRANSITION_ACTIVITY_RESPONSIBLE_SUFFIX, m_dictionary));
        }

        if (responses.rowCount() > 0) {
            responses.beforeTop();

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, responses);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP11));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 12 - transition services.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection12(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        DataDictionary dictionary =
                DataDictionaryUtils.getExtendedDictionaryById(TRANSITION_SERVICE_DICTIONARY_ID, getBroker());

        Collection<IepOtherService> services = null;

        if (isBlank()) {
            services = new LinkedList<IepOtherService>();

            DataDictionaryField serviceNameField = dictionary.findDataDictionaryFieldByAlias(TRANSITION_SERVICE_NAME);

            ReferenceTable serviceCodeTable = serviceNameField.getReferenceTable();
            Collection<ReferenceCode> serviceCodes = serviceCodeTable.getReferenceCodes(getBroker());
            List<ReferenceCode> sortedCodes = CollectionUtils.sortBeans(serviceCodes, ReferenceCode.COL_CODE, false);
            sortedCodes = CollectionUtils.sortBeans(serviceCodes, ReferenceCode.COL_DEPENDENCY_CODE, false);

            for (ReferenceCode serviceCode : sortedCodes) {
                IepOtherService service = new IepOtherService(getBroker().getPersistenceKey());
                service.setServiceType(TRANSITION_SERVICE_TYPE_CODE);
                service.setExtendedDataDictionaryOid(dictionary.getExtendedDictionaryOid());
                service.setFieldValueByAlias(TRANSITION_SERVICE_NAME, serviceCode.getCode(), dictionary);
                service.setFieldValueByAlias(TRANSITION_SERVICE_CATEGORY, serviceCode.getDependencyCode(), dictionary);

                services.add(service);
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepOtherService.COL_SERVICE_TYPE, TRANSITION_SERVICE_TYPE_CODE);
            criteria.addEqualTo(IepOtherService.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria query = new QueryByCriteria(IepOtherService.class, criteria);
            query.addOrderByAscending(IepOtherService.COL_FIELD_B001);
            query.addOrderByAscending(IepOtherService.COL_FIELD_C001);

            services = getBroker().getCollectionByQuery(query);
        }

        if (!services.isEmpty()) {
            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, new BeanCollectionDataSource(services,
                    dictionary,
                    getLocale()));
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP12));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 13 - goals.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection13(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();

        /*
         * Collect the goals to display on the report. If a blank IEP is being printed, add some
         * empty IepGoal beans to the collection to create blank goal pages.
         */
        Collection<IepGoal> goals = null;

        if (isBlank()) {
            goals = new LinkedList<IepGoal>();

            int blankGoals = 3;

            for (int i = 0; i < blankGoals; i++) {
                goals.add(new IepGoal(getBroker().getPersistenceKey()));
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria query = new QueryByCriteria(IepGoal.class, criteria);
            query.addOrderByAscending(IepGoal.COL_FOCUS);

            goals = getBroker().getCollectionByQuery(query);
        }

        /*
         * For each goal, add 3 subreports:
         *
         * 1) Goal details subreport
         * 2) Objectives subreport
         * 3) Progress subreport
         */
        for (IepGoal goal : goals) {
            ReportDataGrid subreports = new ReportDataGrid();

            /*
             * 1) Goal details
             */
            subreports.append();
            subreports.set(COL_GOAL, goal);
            subreports.set(COL_SUBREPORT_DATA_SOURCE, new SimpleBeanDataSource(goal, m_dictionary, getLocale()));
            subreports.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP13_1));
            subreports.set(COL_PARAMETER_MAP, getParameters());

            /*
             * 2) Goal objectives - if a blank IEP is being printed, add some blank objectives to
             * the
             * collection to create blank objective lines
             */
            Collection<IepGoalObjective> objectives = null;
            if (isBlank()) {
                objectives = new LinkedList<IepGoalObjective>();

                int blankObjectives = 6;
                for (int i = 0; i < blankObjectives; i++) {
                    objectives.add(new IepGoalObjective(getBroker().getPersistenceKey()));
                }
            } else {
                Criteria objectiveCriteria = new Criteria();
                objectiveCriteria.addEqualTo(IepGoalObjective.COL_IEP_GOAL_OID, goal.getOid());

                QueryByCriteria objectiveQuery = new QueryByCriteria(IepGoalObjective.class, objectiveCriteria);
                objectiveQuery.addOrderByAscending(IepGoalObjective.COL_SEQUENCE_NUMBER);

                objectives = getBroker().getCollectionByQuery(objectiveQuery);
            }

            subreports.append();
            subreports.set(COL_GOAL, goal);
            subreports.set(COL_SUBREPORT_DATA_SOURCE,
                    new BeanCollectionDataSource(objectives, m_dictionary, getLocale()));
            subreports.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP13_2));
            subreports.set(COL_PARAMETER_MAP, getParameters());

            /*
             * 3) Goal progress - if a blank IEP is being printed, add some blank progress entries
             * to
             * the collection to create blank progress lines
             */
            Collection<IepGoalProgress> progress = null;
            if (isBlank()) {
                progress = new LinkedList<IepGoalProgress>();

                int blankProgress = 4;
                for (int i = 0; i < blankProgress; i++) {
                    progress.add(new IepGoalProgress(getBroker().getPersistenceKey()));
                }
            } else {
                Criteria progressCriteria = new Criteria();
                progressCriteria.addEqualTo(IepGoalObjective.COL_IEP_GOAL_OID, goal.getOid());

                QueryByCriteria progressQuery = new QueryByCriteria(IepGoalProgress.class, progressCriteria);
                progressQuery.addOrderByAscending(IepGoalProgress.COL_DATE);

                progress = getBroker().getCollectionByQuery(progressQuery);
            }

            subreports.append();
            subreports.set(COL_GOAL, goal);
            subreports.set(COL_SUBREPORT_DATA_SOURCE,
                    new BeanCollectionDataSource(progress, m_dictionary, getLocale()));
            subreports.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP13_3));
            subreports.set(COL_PARAMETER_MAP, getParameters());

            subreports.beforeTop();

            /*
             * Add the subreports grid to the main report data grid
             */
            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, subreports);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP13));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 14 - services.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection14(ReportDataGrid grid) {
        IepData iep = (IepData) getFormStorage();
        Collection<IepService> services = null;

        if (isBlank()) {
            int blanksPerMode = 3;

            services = new ArrayList<IepService>();

            DataDictionaryField modeField =
                    m_dictionary.findDataDictionaryField(IepService.class.getName(), IepService.COL_SERVICE_MODE);
            ReferenceTable modesTable = modeField.getReferenceTable();
            List<ReferenceCode> sortedCodes =
                    CollectionUtils.sortBeans(modesTable.getReferenceCodes(getBroker()), ReferenceCode.COL_CODE, false);

            for (ReferenceCode code : sortedCodes) {
                for (int i = 0; i < blanksPerMode; i++) {
                    IepService service = new IepService(getBroker().getPersistenceKey());
                    service.setServiceMode(code.getCode());

                    services.add(service);
                }
            }
        } else {
            DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(SERVICE_ESY);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepService.COL_IEP_DATA_OID, iep.getOid());
            criteria.addEqualTo(IepService.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria query = new QueryByCriteria(IepService.class, criteria);
            query.addOrderByDescending(IepService.COL_SERVICE_MODE);
            query.addOrderByAscending(field.getJavaName());
            query.addOrderByAscending(IepService.COL_SERVICE_TYPE);

            services = getBroker().getCollectionByQuery(query);
        }

        if (!services.isEmpty()) {
            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, new BeanCollectionDataSource(services, m_dictionary, getLocale()));
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP14));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        }
    }

    /**
     * Prepares the data source for section 15 - LRE/placement.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection15(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP15));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Prepares the data source for section 16 - authorizations.
     *
     * @param grid ReportDataGrid
     */
    private void prepareSection16(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_IEP16));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
    }

    /**
     * Sorts the passed list of IepPerformanceLevel objects based on the sequence numbers of the
     * type reference codes.
     *
     * @param performance ArrayList<IepPerformanceLevel>
     */
    private void sortPerformanceLevels(ArrayList<IepPerformanceLevel> performance) {
        DataDictionaryField performanceTypeField =
                m_dictionary.findDataDictionaryField(IepPerformanceLevel.class.getName(), IepPerformanceLevel.COL_TYPE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, performanceTypeField.getReferenceTableOid());

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

        final Map<String, ReferenceCode> codeLookup = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 16);

        Comparator sortComparator = new Comparator<IepPerformanceLevel>() {
            /**
             * Compares the two performance levels based on the sequence numbers of the type
             * reference code.
             *
             * @param perf1
             * @param perf2
             *
             * @return int
             */
            @Override
            public int compare(IepPerformanceLevel perf1, IepPerformanceLevel perf2) {
                ReferenceCode code1 = codeLookup.get(perf1.getType());
                ReferenceCode code2 = codeLookup.get(perf2.getType());

                Integer sequence1 = code1 != null && StringUtils.isNumeric(code1.getLocalCode())
                        ? Integer.valueOf(code1.getLocalCode()) : Integer.valueOf(999);
                Integer sequence2 = code2 != null && StringUtils.isNumeric(code2.getLocalCode())
                        ? Integer.valueOf(code2.getLocalCode()) : Integer.valueOf(999);

                return sequence1.compareTo(sequence2);
            }
        };

        Collections.sort(performance, sortComparator);
    }
}
