/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.health.md;

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeSet;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Certificate of Immunization report. The data for each health
 * immunization dose category is displayed in a subreport.
 *
 * @author X2 Development Corporation
 */
public class MDCertificateOfImmunizationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Boolean to use student phone
     */
    public static final String PARAM_STUDENT_PHONE = "studentPhone";

    /**
     * Boolean to use physical address.
     */
    public static final String PARAM_USE_STUDENT_PHYSICAL_ADDR = "useStudentAddr";

    /**
     * Name for sort report parameter.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Report columns
     */
    public static final String COL_ADDRESS = "address";
    public static final String COL_CITY = "city";
    public static final String COL_STUDENT = "student";
    public static final String COL_STUDENT_CONTACT = "studentContact";
    public static final String COL_ZIP = "zip";
    private static final String BACK_FIELD_1 = "field1";
    private static final String BACK_FIELD_2 = "field2";
    private static final String BACK_FIELD_3 = "field3";
    private static final String BACK_FIELD_4 = "field4";
    private static final String BACK_FIELD_5 = "field5";
    private static final String BACK_FIELD_6 = "field6";
    private static final String FIELD_DATA = "data";
    private static final String FIELD_FORMAT = "format";


    // subreport format parameters
    private static final String SUBREPORT_ID_BACK = "SYS-HTH-MD-BACK";
    private static final String SUBREPORT_ID_DATA = "SYS-HTH-MD-DATA";
    private static final String SUBREPORT_PARAMETER_DATA = "DATA_FORMAT";
    private static final String SUBREPORT_PARAMETER_BACK = "BACK_FORMAT";

    /**
     * Other constants
     */
    public static final String CONTACT_GUARDIAN = "Guardian";
    public static final String CONTACT_PARENT_F = "Father";
    public static final String CONTACT_PARENT_M = "Mother";

    enum COLUMNS {
        DTP_DTAP_DT("DT,DTP,DTaP", "DTP_DTAP_DT"), POLIO("Polio,Polio: Unknown,IPV,OPV", "POLIO"), HIB("Hib",
                "HIB"), HEPB(
                        "HepB",
                        "HEPB"), PCV("PCV,PCV13,PCV - unknown", "PCV"), ROTAVIRUS("Rotavirus", "ROTAVIRUS"), MCV("MCV",
                                "MCV"), HPV("HPV - unknown,HPV4", "HPV"), HEPA("HepA", "HEPA"), MMR("MMR",
                                        "MMR"), VARICELLA("Varicella", "VARICELLA"), VARICELLA_HIS("Varicella_HIS",
                                                "VARICELLA_HIS"), TD("Td",
                                                        "TD"), TDAP("Tdap",
                                                                "TDAP"), MENB("MenB", "MENB"), OTHER("Other", "OTHER");

        COLUMNS(String seriesId, String column) {
            this.m_seriesId = seriesId;
            this.m_columnName = column;
        }

        String m_columnName;
        String m_seriesId;

        /**
         * @return the column
         */
        public String getColumn() {
            return m_columnName;
        }

        /**
         * @return the value
         */
        public String getSeriesId() {
            return m_seriesId;
        }

        /**
         * Finds the SubscriptionType by its name.
         *
         * @param code
         *
         * @return String
         */
        public static String findColumnByValue(String code) {
            String column = null;
            for (COLUMNS enumColumn : COLUMNS.values()) {
                if (enumColumn.getSeriesId().contains(code)) {
                    column = enumColumn.getColumn();
                    break;
                }
            }
            return column;
        }
    }

    private DataDictionaryField m_fieldHisDiseaseDate;
    private X2Criteria m_studentCriteria;
    private boolean m_useStudentForAddr;

    /**
     * Returns the value for the specified property on the entity bean. This method will log
     * missing values.
     * Some bean paths as specified in the FieldDefinition may not be valid. These will be marked by
     * '$'.
     * In those cases, the bean path itself is a description and not a field.
     *
     * If the path is a typed custom field, use a system string converter to convert it to a usable
     * java type.
     *
     */
    /**
     * @param bean
     * @param field
     * @return
     * @throws X2BaseException
     */
    public Object getPropertyAsJavaType(X2BaseBean bean, DataDictionaryField field) throws X2BaseException {
        Object value = null;
        if (field != null) {
            value = WebUtils.getProperty(bean, field.getJavaName());
            SystemStringConverter converter = null;
            if (value != null && value instanceof String) {
                // Get a SystemStringConverter for the field and convert the value from a string
                // value to java type.
                if (field.isString()) {
                    Converter baseConverter = ConverterFactory.getConverterForClass(
                            field.getEffectiveJavaType(),
                            LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                            field.isString());
                    if (baseConverter instanceof SystemStringConverter) {
                        converter = ((SystemStringConverter) baseConverter);
                    }
                }
                if (converter != null) {
                    value = converter.parseSystemString((String) value);
                }
            }
        }

        return value;
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        /*
         * Set up the grid and parameters.
         */
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_studentCriteria);
        String sortBy = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sortBy);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        addReportFormatParameter(SUBREPORT_ID_DATA, SUBREPORT_PARAMETER_DATA);
        addReportFormatParameter(SUBREPORT_ID_BACK, SUBREPORT_PARAMETER_BACK);
        ReportDataGrid gridMain = new ReportDataGrid();
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                Map<String, SortedSet<PlainDate>> immunizationData = prepareImmunizationData(student);
                if (!immunizationData.isEmpty()) {
                    ReportDataGrid gridData = new ReportDataGrid();
                    gridData.append();
                    gridData.set(COL_STUDENT, student);
                    StudentContact cnt = getStudentContact(student);
                    if (cnt != null) {
                        gridData.set(COL_STUDENT_CONTACT, cnt);
                        gridData.set(COL_ADDRESS, getAddress(cnt));
                        gridData.set(COL_CITY, getCity(cnt));
                        gridData.set(COL_ZIP, getZip(cnt));
                    }
                    for (Entry<String, SortedSet<PlainDate>> entry : immunizationData.entrySet()) {
                        String column = entry.getKey();
                        Collection<PlainDate> dates = entry.getValue();
                        String colToSet = COLUMNS.findColumnByValue(column);
                        if (!StringUtils.isEmpty(colToSet) && dates != null && !dates.isEmpty()) {
                            int dose = 0;
                            for (PlainDate date : dates) {
                                dose++;
                                gridData.set(colToSet + "_" + dose, date);
                            }
                        }
                    }

                    ReportDataGrid gridBack = new ReportDataGrid();
                    gridBack.append();
                    gridBack.set(BACK_FIELD_1, "varicella, measles, mumps, or rubella.");
                    gridBack.set(BACK_FIELD_2, "revaccination may be more expedient.");
                    gridBack.set(BACK_FIELD_3,
                            "\"Minimum Vaccine Requirements for Children Enrolled in Pre-school Programs and in Schools\"");
                    gridBack.set(BACK_FIELD_4,
                            "\"Age- Appropriate Immunizations Requirements for Children Enrolled in Child Care Programs\"");
                    gridBack.set(BACK_FIELD_5,
                            "Age-appropriate immunization requirements for licensed childcare centers and family day care homes are based on the Department of Human Resources COMAR 13A.15.03.02 and COMAR 13A.16.03.04 G and H and the ");
                    gridBack.set(BACK_FIELD_6,
                            "Reconstructed dates for all vaccines must be reviewed and approved by a medical provider or local health department no later than 20 calendar days following the date the student was temporarily admitted or retained.");
                    prepareReportPage(gridMain, gridData, FIELD_DATA, SUBREPORT_PARAMETER_DATA, FIELD_FORMAT);
                    prepareReportPage(gridMain, gridBack, FIELD_DATA, SUBREPORT_PARAMETER_BACK, FIELD_FORMAT);
                }
            }
        } finally {
            iterator.close();
        }
        addParameter(PARAM_STUDENT_PHONE, getParameter(PARAM_STUDENT_PHONE));
        gridMain.beforeTop();
        return gridMain;
    }

    /**
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldHisDiseaseDate = dictionary.findDataDictionaryFieldByAlias("hisDiseaseDate");
        m_useStudentForAddr = ((Boolean) getParameter(PARAM_USE_STUDENT_PHYSICAL_ADDR)).booleanValue();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ReportJavaSource#initialize(com.follett.fsc.core.k12.
     *      web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        SisStudent currentStudent = userData.getCurrentRecord(SisStudent.class);
        buildCriteria(currentStudent);
    }

    /**
     * prepare subreport format.
     *
     * @param reportFormatID String
     * @param formatParameterID String
     */
    private void addReportFormatParameter(String reportFormatID,
                                          String formatParameterID) {
        Report pageSubreportFormat = ReportUtils.getReport(reportFormatID, getBroker());
        if (pageSubreportFormat != null) {
            addParameter(formatParameterID, new ByteArrayInputStream(pageSubreportFormat.getCompiledFormat()));
        }
    }

    /**
     * Builds the student criteria.
     */
    private void buildCriteria(SisStudent student) {
        m_studentCriteria = new X2Criteria();
        if (student != null) {
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, student.getOid());
        } else {
            /*
             * Add the user criteria
             */
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(m_studentCriteria, queryBy, queryString, null, null);
            /*
             * If we are in the school context, filter the list by the current school. This is not
             * necessary if current selection is being used, assuming that the user cannot create an
             * out-of-scope selection.
             */
            if (isSchoolContext()) {
                m_studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
            }
            boolean activeOnly =
                    getParameter(ACTIVE_ONLY_PARAM) != null ? ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue()
                            : false;
            if (activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STUDENT_ACTIVE_CODE);
                m_studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            }
        }
    }

    /**
     * Gets the address.
     *
     * @param cnt StudentContact
     * @return String
     */
    private String getAddress(StudentContact cnt) {
        String address = null;
        if (!m_useStudentForAddr) {
            if (cnt.getPerson().getPhysicalAddress() != null) {
                address = cnt.getPerson().getPhysicalAddress().getAddressLine01();
            }
        } else if (cnt.getStudent().getPerson() != null) {
            address = cnt.getStudent().getAddressView();
        }
        return address;
    }

    /**
     * Gets the city by cnt.
     *
     * @param cnt StudentContact
     * @return String
     */
    private String getCity(StudentContact cnt) {
        String city = null;
        if (!m_useStudentForAddr) {
            if (cnt.getPerson() != null && cnt.getPerson().getPhysicalAddress() != null) {
                city = cnt.getPerson().getPhysicalAddress().getCity();
                if (StringUtils.isEmpty(city)) {
                    String addrLine03 = cnt.getPerson().getPhysicalAddress().getAddressLine03();
                    if (!StringUtils.isEmpty(addrLine03)) {
                        String[] splitteddAddr03 = addrLine03.split(",");
                        if (splitteddAddr03.length > 0) {
                            city = splitteddAddr03[0];
                        }
                    }
                }
            }
        } else if (cnt.getStudent().getPerson().getPhysicalAddress() != null) {
            city = cnt.getStudent().getPerson().getPhysicalAddress().getCity();
            if (StringUtils.isEmpty(city)) {
                String addrLine03 = cnt.getStudent().getPerson().getPhysicalAddress().getAddressLine03();
                if (!StringUtils.isEmpty(addrLine03)) {
                    String[] splitteddAddr03 = addrLine03.split(",");
                    if (splitteddAddr03.length > 0) {
                        city = splitteddAddr03[0];
                    }
                }
            }
        }
        return city;
    }

    /**
     * Calculate student contact by relationship and priority.
     *
     * @return Student contact
     */
    private StudentContact getStudentContact(SisStudent student) {
        StudentContact ctjToReturnParent = null;
        StudentContact ctjToReturnGuardian = null;
        Collection<StudentContact> ctjs = student.getContacts();
        for (StudentContact ctj : ctjs) {
            if (Arrays.asList(CONTACT_PARENT_M, CONTACT_PARENT_F).contains(ctj.getRelationshipCode())) {
                if (ctjToReturnParent == null
                        || ctj.getEmergencyPriority() < ctjToReturnParent.getEmergencyPriority()) {
                    ctjToReturnParent = ctj;
                }
            } else if (CONTACT_GUARDIAN.equals(ctj.getRelationshipCode())) {
                if (ctjToReturnGuardian == null
                        || ctj.getEmergencyPriority() < ctjToReturnGuardian.getEmergencyPriority()) {
                    ctjToReturnGuardian = ctj;
                }
            }
        }
        return ctjToReturnParent != null ? ctjToReturnParent : ctjToReturnGuardian;
    }

    /**
     * Gets the zip by cnt.
     *
     * @param cnt StudentContact
     * @return String
     */
    private String getZip(StudentContact cnt) {
        String zip = null;
        if (!m_useStudentForAddr) {
            if (cnt.getPerson() != null && cnt.getPerson().getPhysicalAddress() != null) {
                zip = cnt.getPerson().getPhysicalAddress().getPostalCode();
                if (StringUtils.isEmpty(zip)) {
                    String addrLine03 = cnt.getPerson().getPhysicalAddress().getAddressLine03();
                    if (!StringUtils.isEmpty(addrLine03)) {
                        String[] splitteddAddr03 = addrLine03.split(",");
                        if (splitteddAddr03.length > 1) {
                            zip = splitteddAddr03[1];
                        }
                    }
                }
            }
        } else if (cnt.getStudent().getPerson().getPhysicalAddress() != null) {
            zip = cnt.getStudent().getPerson().getPhysicalAddress().getPostalCode();
            if (StringUtils.isEmpty(zip)) {
                String addrLine03 = cnt.getStudent().getPerson().getPhysicalAddress().getAddressLine03();
                if (!StringUtils.isEmpty(addrLine03)) {
                    String[] splitteddAddr03 = addrLine03.split(",");
                    if (splitteddAddr03.length > 1) {
                        zip = splitteddAddr03[1];
                    }
                }
            }
        }
        return zip;
    }

    /**
     * Prepare the immunities ReportDataGrid.
     *
     * @throws X2BaseException
     */
    private Map<String, SortedSet<PlainDate>> prepareImmunizationData(SisStudent student) throws X2BaseException {
        Map<String, SortedSet<PlainDate>> immunizationData = new HashMap<>();
        Collection<HealthImmunizationDose> studentDoses = student.getImmunizationDoses();
        X2Criteria cr = new X2Criteria();
        cr.addEqualTo(HealthImmunizationSeries.COL_STUDENT_OID, student.getOid());
        cr.addEqualTo(
                HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + "." + HealthImmunizationDefinition.COL_SERIES_ID,
                COLUMNS.VARICELLA.getSeriesId());
        HealthImmunizationSeries series = (HealthImmunizationSeries) getBroker()
                .getBeanByQuery(new QueryByCriteria(HealthImmunizationSeries.class, cr));
        if (series != null && m_fieldHisDiseaseDate != null) {
            SortedSet<PlainDate> datesHistVaricella = new TreeSet<>();
            PlainDate hisDate =
                    (PlainDate) getPropertyAsJavaType(series, m_fieldHisDiseaseDate);
            if (hisDate != null) {
                datesHistVaricella.add(hisDate);
            }
            immunizationData.put(COLUMNS.VARICELLA_HIS.getSeriesId(), datesHistVaricella);
        }
        for (HealthImmunizationDose dose : studentDoses) {
            HealthImmunizationDefinition def = dose.getImmunizationSeries().getImmunizationDefinition();
            if (def != null) {
                String code = def.getSeriesId();
                SortedSet<PlainDate> dates = immunizationData.get(code);
                if (dates != null && dose.getDate() != null) {
                    dates.add(dose.getDate());
                } else {
                    dates = new TreeSet<>();
                    if (dose.getDate() != null) {
                        dates.add(dose.getDate());
                        immunizationData.put(code, dates);
                    }
                }
            }
        }
        return immunizationData;
    }

    /**
     * prepare subreport datasources and formats.
     *
     * @param reportGrid ReportDataGrid
     * @param subreportGrid ReportDataGrid
     * @param dataFieldID String
     * @param reportFormatType String
     * @param formatFieldID String
     */
    private void prepareReportPage(ReportDataGrid reportGrid,
                                   ReportDataGrid subreportGrid,
                                   String dataFieldID,
                                   String reportFormatType,
                                   String formatFieldID) {
        reportGrid.append();
        subreportGrid.beforeTop();
        reportGrid.set(dataFieldID, subreportGrid);
        reportGrid.set(formatFieldID, reportFormatType);
    }

}
