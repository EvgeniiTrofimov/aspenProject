/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Student Demographic/Enrollment.
 *
 * @author X2 Development Corporation
 */
public class ILServiceProviderData extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RcdtsEntity extends StateReportEntity {
        private ArrayList<OutplacementData> m_outplacementData = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RcdtsEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize and increment counter
         *
         * If there is no recent entry enrollment record, ignore it.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            ILServiceProviderData spData = (ILServiceProviderData) data;

            SisStudent student = (SisStudent) bean;

            m_outplacementData = spData.getStudentOutplacementsData(student.getOid());
            // If student hasn't outplacement secondary school, skip him.
            if (m_outplacementData == null || m_outplacementData.size() == 0) {
                setRowCount(0);
                return;
            }

            // Do not report students not active on report date.
            StudentEnrollment enrollment =
                    spData.m_helper.getEnrollmentForDate(student.getOid(), spData.m_reportDate, "EW");

            if (enrollment == null ||
                    !spData.m_activeCode.equals(enrollment.getStatusCode()) ||
                    spData.m_currectContextStartDate.after(enrollment.getEnrollmentDate())) {
                setRowCount(0);
                return;
            }

            // keep count of records
            spData.m_totalStudentCount = spData.m_totalStudentCount + m_outplacementData.size();

            setRowCount(m_outplacementData.size());
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
         * Gets the current ouplacement data.
         *
         * @return object that aggregates calculated data.
         */
        public OutplacementData getCurrentOuplacementData() {
            return m_outplacementData.get(getCurrentRow());
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
     * Class providing values of calculated fields.
     */
    public class OutplacementData {
        private String m_defaultSchool = null;
        private String m_home = null;
        private String m_provider = null;
        private String m_serving = null;

        /**
         * Instantiates a new outplacement data.
         *
         * @param outplacement String
         * @param provider String
         * @param studentSchoolId String
         */
        public OutplacementData(String outplacement, String provider, String studentSchoolId) {
            if (!StringUtils.isEmpty(outplacement)) {
                m_serving = lookupStateValue(StudentEnrollment.class, m_fieldServiceSchoolCode, outplacement);
            }

            if (!StringUtils.isEmpty(provider)) {
                m_provider = lookupStateValue(StudentEnrollment.class, m_fieldServiceSchoolCode, provider);
            }

            m_home = studentSchoolId;
        }

        /**
         * Instantiates a new outplacement data.
         *
         * @param outplacement String
         * @param provider String
         * @param studentSchoolId String
         * @param defaultSchool String
         */
        public OutplacementData(String outplacement, String provider, String studentSchoolId, String defaultSchool) {
            this(outplacement, provider, studentSchoolId);
            m_defaultSchool = defaultSchool;
        }

        /**
         * Gets the default school.
         *
         * @return String
         */
        public String getDefaultSchool() {
            return m_defaultSchool;
        }

        /**
         * Gets the home.
         *
         * @return String
         */
        public String getHome() {
            return m_home;
        }

        /**
         * Gets the provider.
         *
         * @return String
         */
        public String getProvider() {
            return m_provider;
        }

        /**
         * Gets the service.
         *
         * @return String
         */
        public String getService() {
            return m_serving;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_DATE_ENTRY_US = "DOE DATE ENTRY US";
    protected static final String ALIAS_IEP = "DOE IEP IND";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_PROVIDER_CODE = "DOE SERVICE PROVIDER";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_LUNCH_STATUS = "DOE LUNCH STATUS";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";

    /*
     * Parameters
     */
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_REQUEST_UPDATE = "requestUpdate";

    /*
     * Other internal constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z ]";

    /*
     * Instance variables
     */
    protected String m_activeCode;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_excludeStdField;
    protected String m_excludeSklField;
    protected String m_exportName;
    protected String m_fieldDateEntryUS;
    protected String m_fieldDistrictCode;
    protected String m_fieldIep;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceProviderCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldLunchStatusCode;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected PlainDate m_startDate;
    protected PlainDate m_reportDate;
    protected PlainDate m_currectContextStartDate;
    protected String m_fieldRcdtsForServingSchool;
    protected Organization m_rootOrganization;

    /**
     * Helper class:
     * For student selection by enrollment.
     * For Student schedule span.
     */
    protected StudentHistoryHelper m_helper;
    protected StudentHistoryHelper m_helperSched;

    /**
     * Keep track of number of students
     */
    protected int m_totalStudentCount;

    private Map<String, ArrayList<OutplacementData>> m_studentWithOutplacementsMap =
            new HashMap<String, ArrayList<OutplacementData>>();

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS
     * - "SP" for service provider RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            RcdtsEntity rcdtsEntity = (RcdtsEntity) entity;
            String rcdts = null;
            if (param.equals("H")) {
                rcdts = rcdtsEntity.getCurrentOuplacementData().getHome();
            } else if (param.equals("S")) {
                rcdts = rcdtsEntity.getCurrentOuplacementData().getService();
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = rcdtsEntity.getCurrentOuplacementData().getDefaultSchool();
                }
            } else if (param.equals("SP")) {
                rcdts = rcdtsEntity.getCurrentOuplacementData().getProvider();
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = rcdtsEntity.getCurrentOuplacementData().getDefaultSchool();
                }
            }

            return rcdts;
        }
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_rootOrganization = getOrganization();
        if (m_rootOrganization == null) {
            m_rootOrganization = OrganizationManager.getRootOrganization(getBroker());
        }

        initializeFields();
        setEntityClass(RcdtsEntity.class);

        m_startDate = m_rootOrganization.getCurrentContext().getStartDate();
        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        m_helperSched = new StudentHistoryHelper(this);
        m_helperSched.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        X2Criteria schedCriteria = m_helperSched.getStudentScheduleCriteria();

        schedCriteria.addGreaterThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_CREDIT,
                BigDecimal.ZERO);

        // Set the query to be used for student selection.
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        setQuery(studentQuery);

        // check for request (students without state ids) / update (students with state ids)
        int requestUpdateSelection = ((Integer) getParameter(PARAM_REQUEST_UPDATE)).intValue();
        switch (requestUpdateSelection) {
            case 1:
                studentCriteria.addIsNull(SisStudent.COL_STATE_ID);
                schedCriteria.addIsNull(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);
                break;
            case 2:
                studentCriteria.addNotNull(SisStudent.COL_STATE_ID);
                schedCriteria.addNotNull(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);
                break;
            default:
                break;
        }

        Calendar currectContextStartDate = Calendar.getInstance();
        currectContextStartDate.set(2014, Calendar.AUGUST, 1);
        currectContextStartDate.set(Calendar.YEAR, currectContextStartDate.get(Calendar.YEAR) - 3);
        m_currectContextStartDate = new PlainDate(currectContextStartDate.getTime());

        initializeOutplacementData();

        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("DEMO-RCDTS", new RetrieveRcdts());
        addCalcs(calcs);

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        addValidators(validators);
    }

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        if (m_exportName != null) {
            heading.append(m_exportName).append(',');
        }
        heading.append(m_totalStudentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(m_rootOrganization.getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Gets the student outplacements data.
     *
     * @param studentOid String
     * @return Array list
     */
    public ArrayList<OutplacementData> getStudentOutplacementsData(String studentOid) {
        return m_studentWithOutplacementsMap.get(studentOid);
    }

    /**
     * If outplacement school hasn't Serving or Provider codes, try to get it from schedule spans.
     * If there aren't schedule spans, or they don't contain school code, then try to get it from
     * student's enrollment.
     *
     * @param iter QueryIterator
     */
    private void addDataForOutplacementSchool(QueryIterator iter) {
        StudentSchool item = (StudentSchool) iter.next();

        String studentSchoolId = null;
        StudentEnrollment enrollment = m_helper.getEnrollmentForDate(item.getStudentOid(), m_reportDate, "E");
        if (enrollment != null && enrollment.getSchool() != null &&
        // Add record only if home school is not excluded.
                !BooleanAsStringConverter.TRUE
                        .equals(enrollment.getSchool().getFieldValueByBeanPath(m_excludeSklField))) {
            studentSchoolId = (String) enrollment.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);

            OutplacementData outData =
                    new OutplacementData((String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool),
                            (String) item.getFieldValueByBeanPath(m_fieldServiceProviderCode),
                            studentSchoolId);

            if (StringUtils.isEmpty(outData.getProvider()) || StringUtils.isEmpty(outData.getService())) {
                ArrayList<String> spanSchools = new ArrayList<String>();

                Collection<StudentScheduleSpan> studentSpans =
                        m_helperSched.getStudentScheduleSpans((SisStudent) item.getStudent());

                for (StudentScheduleSpan span : studentSpans) {
                    if (!span.getEntryDate().after(m_reportDate) &&
                            !span.getExitDate().before(m_reportDate)) {
                        SisSchool spanSchool = span.getSection().getSchedule().getSchool();
                        String spanSchoolId = (String) spanSchool.getFieldValueByBeanPath(m_fieldSchoolCode);
                        if (!StringUtils.isEmpty(spanSchoolId) && !spanSchools.contains(spanSchoolId)) {
                            spanSchools.add(spanSchoolId);
                        }
                    }
                }
                if (spanSchools.size() > 0) {
                    for (String spanSchoolId : spanSchools) {
                        outData = new OutplacementData(
                                (String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool),
                                (String) item.getFieldValueByBeanPath(m_fieldServiceProviderCode),
                                studentSchoolId,
                                spanSchoolId);

                        putOutplacementData(item, outData);
                    }
                } else {
                    outData = new OutplacementData((String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool),
                            (String) item.getFieldValueByBeanPath(m_fieldServiceProviderCode),
                            studentSchoolId,
                            studentSchoolId);

                    putOutplacementData(item, outData);
                }
            } else {
                putOutplacementData(item, outData);
            }
        }
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(m_rootOrganization.getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_activeCode = PreferenceManager.getPreferenceValue(m_rootOrganization,
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldServiceProviderCode = translateAliasToJavaName(ALIAS_SERVICE_PROVIDER_CODE, true);
        m_fieldDateEntryUS = translateAliasToJavaName(ALIAS_DATE_ENTRY_US, true);
        m_fieldIep = translateAliasToJavaName(ALIAS_IEP, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
    }

    /**
     * Initializes secondary schools maps.
     */
    private void initializeOutplacementData() {
        // Additional rule for secondary OUTPLACEMENT school
        X2Criteria secondaryOutplacementCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                "OUTPLACEMENT");
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

        // Get only student's secondary outplacement schools where the student was within
        // overlapping with selected date range.
        X2Criteria overlapCriteria = new X2Criteria();

        overlapCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);

        X2Criteria endDateCriteria = new X2Criteria();
        X2Criteria endDateCriteriaNull = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, m_startDate);
        endDateCriteriaNull.addIsNull(StudentSchool.COL_END_DATE);
        endDateCriteria.addOrCriteria(endDateCriteriaNull);

        overlapCriteria.addAndCriteria(endDateCriteria);

        secondaryOutplacementCriteria.addAndCriteria(overlapCriteria);

        QueryByCriteria secondaryOutplacementQuery =
                new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
        QueryIterator iter = getBroker().getIteratorByQuery(secondaryOutplacementQuery);

        try {
            while (iter.hasNext()) {
                addDataForOutplacementSchool(iter);
            }
        } finally {
            iter.close();
        }
    }

    /**
     * Put outplacement data.
     *
     * @param item StudentSchool
     * @param outData OutplacementData
     */
    private void putOutplacementData(StudentSchool item, OutplacementData outData) {
        ArrayList<OutplacementData> outplacementData = m_studentWithOutplacementsMap.get(item.getStudentOid());
        if (outplacementData == null) {
            outplacementData = new ArrayList<OutplacementData>();
            m_studentWithOutplacementsMap.put(item.getStudentOid(), outplacementData);
        }

        outplacementData.add(outData);
    }
}
