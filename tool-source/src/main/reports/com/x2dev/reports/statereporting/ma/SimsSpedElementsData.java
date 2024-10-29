/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.ACTIVE;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.EXITED;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.INELIGIBLE;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.REFERRED;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_EDUCATIONAL_ENVIRONMENT;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_EDUCATIONAL_ENVIRONMENT_EC;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_LEVEL_OF_NEED;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPA_SPECIAL_INSTRUCTION_CONTENT;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPA_SPECIAL_INSTRUCTION_METHODOLOGY;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPA_SPECIAL_INSTRUCTION_PERFORMANCE;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPB_SPECIAL_INSTRUCTION_CONTENT;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPB_SPECIAL_INSTRUCTION_METHODOLOGY;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPB_SPECIAL_INSTRUCTION_PERFORMANCE;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.ViewTemplate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.CoreDataType;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.business.sped.IepLookup;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2SAXBuilder;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.JDOMException;

/**
 * Report that displays the calculated values of the MA SIMS sped elements and provides the
 * option to update student fields.
 *
 * @author mmastrangelo
 */
public class SimsSpedElementsData extends StudentReportJavaSource {

    /**
     * Parameters
     */
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_UPDATE_STUDENTS = "updateStudents";
    private static final String INPUT_PARAM_SPED_ONLY = "spedOnly";
    private static final String INPUT_PARAM_TEMPLATE_TO_UPDATE = "templateToUpdate";
    private static final String OUTPUT_PARAM_DOE32_VALUES = "doe32Values";
    private static final String OUTPUT_PARAM_DOE34_VALUES = "doe34Values";
    private static final String OUTPUT_PARAM_DOE36_VALUES = "doe36Values";
    private static final String OUTPUT_PARAM_DOE38_VALUES = "doe38Values";
    private static final String OUTPUT_PARAM_DOE40_VALUES = "doe40Values";
    private static final String OUTPUT_PARAM_DOE32_FIELD = "doe32Field";
    private static final String OUTPUT_PARAM_DOE34_FIELD = "doe34Field";
    private static final String OUTPUT_PARAM_DOE36_FIELD = "doe36Field";
    private static final String OUTPUT_PARAM_DOE38_FIELD = "doe38Field";
    private static final String OUTPUT_PARAM_DOE40_FIELD = "doe40Field";
    private static final String OUTPUT_PARAM_MESSAGES = "messages";
    private static final String OUTPUT_PARAM_STUDENTS_TO_EXCLUDE = "studentsToExclude";

    /**
     * Aliases
     */
    private static final String ALIAS_DOE_32_SPED_PLACEMENT = "DOE 32";
    private static final String ALIAS_DOE_34_SPED_PLACEMENT = "DOE 34";
    private static final String ALIAS_DOE_36_SPED_DISABILITY = "DOE 36";
    private static final String ALIAS_DOE_38_SPED_LEVEL = "DOE 38";
    private static final String ALIAS_DOE_40_SPED_EVALUATION_RESULTS = "DOE 40";
    private static final String ALIAS_LOCK_SPED_VALUES = "DOE LOCK SPED";

    /**
     * Templates
     */
    private static final String TEMPLATE_CONTEXT = "student.std.list.detail";
    private static final String TEMPLATE_XML_TO_INSERT =
            "\n    <!-- DOE LOCK SPED auto-inserted by SIMS Sped Elements Report -->\n    <row>\n       <column>\n          <property alias=\"DOE LOCK SPED\" />\n       </column>\n    </row>\n";
    private static final String TEMPLATE_INSERT_LOCATION_SEARCH_1 = "property\\s+?alias=\"DOE 31\".+?\\</row\\>";
    private static final String TEMPLATE_INSERT_LOCATION_SEARCH_2 = "\\</tab\\>";

    /**
     * Other constants
     */
    private static final List<String> DOE40_BLANKING_VALUES = Arrays.asList("02", "08", "09");
    private static final List<String> GRADES_KINDERGARTEN = Arrays.asList("KF", "KP", "KT");
    private static final String LOCK_STUDENTS_FIELD_NAME = "Lock SPED values";

    /**
     * Members
     */
    protected IepLookup m_iepLookup;
    protected Map<String, Collection<IepDisability>> m_iepDisabilityLookup;
    protected DataDictionary m_iepDictionary;
    protected Map<String, Integer> m_iepMeetingType;
    protected DataDictionary m_districtDictionary;
    private DataDictionaryField m_doe32SpedPlacementField;
    private DataDictionaryField m_doe34SpedPlacementField;
    private DataDictionaryField m_doe36SpedDisabilityField;
    private DataDictionaryField m_doe38SpedLevelField;
    private DataDictionaryField m_doe40SpedEvalResultsField;
    private Map<String, String> m_doe32Values;
    private Map<String, String> m_doe34Values;
    private Map<String, String> m_doe36Values;
    private Map<String, String> m_doe38Values;
    private Map<String, String> m_doe40Values;
    private DataDictionaryField m_lockSpedField;
    private Map<String, String> m_messages;
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
    private PlainDate m_reportDate;
    private boolean m_spedOnly = false;
    private Set<String> m_studentsToExclude;
    private boolean m_updateStudents = false;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.StudentReportJavaSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = buildCriteria();

        loadIepData(criteria);
        calculateSpedValues(criteria);

        addParameter(OUTPUT_PARAM_DOE32_VALUES, m_doe32Values);
        addParameter(OUTPUT_PARAM_DOE34_VALUES, m_doe34Values);
        addParameter(OUTPUT_PARAM_DOE36_VALUES, m_doe36Values);
        addParameter(OUTPUT_PARAM_DOE38_VALUES, m_doe38Values);
        addParameter(OUTPUT_PARAM_DOE40_VALUES, m_doe40Values);

        addParameter(OUTPUT_PARAM_MESSAGES, m_messages);
        addParameter(OUTPUT_PARAM_STUDENTS_TO_EXCLUDE, m_studentsToExclude);

        addParameter(OUTPUT_PARAM_DOE32_FIELD, m_doe32SpedPlacementField.getJavaName());
        addParameter(OUTPUT_PARAM_DOE34_FIELD, m_doe34SpedPlacementField.getJavaName());
        addParameter(OUTPUT_PARAM_DOE36_FIELD, m_doe36SpedDisabilityField.getJavaName());
        addParameter(OUTPUT_PARAM_DOE38_FIELD, m_doe38SpedLevelField.getJavaName());
        addParameter(OUTPUT_PARAM_DOE40_FIELD, m_doe40SpedEvalResultsField.getJavaName());

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        /*
         * Build the sort based on user input
         *
         * If we are not in the context of a school, sort by the school first to support school
         * grouping on the format.
         */
        if (!isSchoolContext()) {
            query.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        }

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        /*
         * Execute the query and return the results
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()), true, getLocale());
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_doe32Values = new HashMap<>(256);
        m_doe34Values = new HashMap<>(256);
        m_doe36Values = new HashMap<>(256);
        m_doe38Values = new HashMap<>(256);
        m_doe40Values = new HashMap<>(256);
        m_messages = new HashMap<>(256);

        m_studentsToExclude = new HashSet<>(256);

        ExtendedDataDictionary extendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        m_iepDictionary = DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());

        m_districtDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_doe32SpedPlacementField = m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_32_SPED_PLACEMENT);
        m_doe34SpedPlacementField = m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_34_SPED_PLACEMENT);
        m_doe36SpedDisabilityField = m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_36_SPED_DISABILITY);
        m_doe38SpedLevelField = m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_38_SPED_LEVEL);
        m_doe40SpedEvalResultsField =
                m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_40_SPED_EVALUATION_RESULTS);

        m_lockSpedField = m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_LOCK_SPED_VALUES);
        if (m_lockSpedField == null) {
            setupLockSpedField();
            m_lockSpedField = m_districtDictionary.findDataDictionaryFieldByAlias(ALIAS_LOCK_SPED_VALUES);
        }

        m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        m_updateStudents = Boolean.TRUE.equals(getParameter(INPUT_PARAM_UPDATE_STUDENTS));
        m_spedOnly = Boolean.TRUE.equals(getParameter(INPUT_PARAM_SPED_ONLY));

        String templateToUpdate = (String) getParameter(INPUT_PARAM_TEMPLATE_TO_UPDATE);
        if (!StringUtils.isEmpty(templateToUpdate)) {
            updateTemplate(templateToUpdate);
        }
    }

    /**
     * Returns true if the passed student is evaluated for special education this year.
     *
     * @param student SisStudent
     * @return boolean
     */
    protected boolean isSpedEvaluatedThisYear(SisStudent student) {
        return student.getSpedLastEvaluationDate() != null &&
                student.getSpedLastEvaluationDate().after(getOrganization().getCurrentContext().getStartDate());
    }

    /**
     * Returns true if the passed student exited special education this year. A student is
     * considered exited if their status is either EXITED or INELIGIBLE, and the exit date on the
     * student record is within the current school year.
     *
     * @param student SisStudent
     * @return boolean
     */
    protected boolean isSpedExitedThisYear(SisStudent student) {
        return (SpedStatusCode.EXITED.equals(student.getSpedStatusCodeEnum()) ||
                SpedStatusCode.INELIGIBLE.equals(student.getSpedStatusCodeEnum())) &&
                student.getSpedExitDate() != null &&
                student.getSpedExitDate().after(getOrganization().getCurrentContext().getStartDate());
    }

    /**
     * Returns true if the passed IEP designates the student for specially designed instruction.
     *
     * @param studentOid String
     * @return boolean
     */
    protected boolean isSpedSpecialInstruction(String studentOid) {
        String plepASpecialContent =
                (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                        IEP_PLEPA_SPECIAL_INSTRUCTION_CONTENT);
        String plepASpecialMethodology =
                (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                        IEP_PLEPA_SPECIAL_INSTRUCTION_METHODOLOGY);
        String plepASpecialPerformance =
                (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                        IEP_PLEPA_SPECIAL_INSTRUCTION_PERFORMANCE);
        String plepBSpecialContent =
                (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                        IEP_PLEPB_SPECIAL_INSTRUCTION_CONTENT);
        String plepBSpecialMethodology =
                (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                        IEP_PLEPB_SPECIAL_INSTRUCTION_METHODOLOGY);
        String plepBSpecialPerformance =
                (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                        IEP_PLEPB_SPECIAL_INSTRUCTION_PERFORMANCE);

        return !StringUtils.isEmpty(plepASpecialContent) ||
                !StringUtils.isEmpty(plepASpecialMethodology) ||
                !StringUtils.isEmpty(plepASpecialPerformance) ||
                !StringUtils.isEmpty(plepBSpecialContent) ||
                !StringUtils.isEmpty(plepBSpecialMethodology) ||
                !StringUtils.isEmpty(plepBSpecialPerformance);
    }

    /**
     * Calculates SPED values for each student found by the passed criteria. Values are captured in
     * the
     * DOE values maps and optionally saved to the student record if m_updateStudents is true.
     * Students with
     * the lock sped field set to "1" are skipped
     *
     * @param studentCriteria Criteria
     */
    private void calculateSpedValues(Criteria studentCriteria) {
        String doe32NonSpedValue = lookupUserValue(SisStudent.class, m_doe32SpedPlacementField.getJavaName(), "00");
        String doe34NonSpedValue = lookupUserValue(SisStudent.class, m_doe34SpedPlacementField.getJavaName(), "00");
        String doe36NonSpedValue = lookupUserValue(SisStudent.class, m_doe36SpedDisabilityField.getJavaName(), "500");
        String doe38NonSpedValue = lookupUserValue(SisStudent.class, m_doe38SpedLevelField.getJavaName(), "500");
        String doe40NonSpedValue = lookupUserValue(SisStudent.class, m_doe40SpedEvalResultsField.getJavaName(), "00");
        QueryByCriteria query = new QueryByCriteria(Student.class, studentCriteria);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(query);
        if (students != null && !students.isEmpty()) {
            for (SisStudent student : students) {
                if (student.getPerson() == null || StringUtils.isEmpty(student.getPersonOid())) {
                    continue;
                }
                String doe32SpedPlacement;
                try {
                    doe32SpedPlacement = retrieveSpedPlacement(student, m_doe32SpedPlacementField);
                } catch (X2BaseException e) {
                    continue;
                }
                String doe34SpedPlacement;
                try {
                    doe34SpedPlacement = retrieveSpedPlacement(student, m_doe34SpedPlacementField);
                } catch (X2BaseException e) {
                    continue;
                }
                String doe36SpedDisability = retrieveSpedDisability(student);
                String doe38SpedLevel = retrieveSpedLevel(student);
                String doe40SpedEvalResults = retrieveEvaluationResults(student);
                if (DOE40_BLANKING_VALUES.contains(doe40SpedEvalResults.substring(0, 2))) {
                    doe32SpedPlacement = "";
                    doe34SpedPlacement = "";
                    doe36SpedDisability = "";
                    doe38SpedLevel = "";
                }
                if (!m_spedOnly ||
                        ((!StringUtils.isEmpty(doe32SpedPlacement) && !doe32SpedPlacement.equals(doe32NonSpedValue))
                                ||
                                (!StringUtils.isEmpty(doe34SpedPlacement)
                                        && !doe34SpedPlacement.equals(doe34NonSpedValue))
                                ||
                                (!StringUtils.isEmpty(doe36SpedDisability)
                                        && !doe36SpedDisability.equals(doe36NonSpedValue))
                                ||
                                (!StringUtils.isEmpty(doe38SpedLevel) && !doe38SpedLevel.equals(doe38NonSpedValue))
                                ||
                                (!StringUtils.isEmpty(doe40SpedEvalResults)
                                        && !doe40SpedEvalResults.equals(doe40NonSpedValue)))) {
                    String doe32SpedPlacementState = lookupStateValue(SisStudent.class,
                            m_doe32SpedPlacementField.getJavaName(), doe32SpedPlacement);
                    String doe34SpedPlacementState = lookupStateValue(SisStudent.class,
                            m_doe34SpedPlacementField.getJavaName(), doe34SpedPlacement);
                    if (Arrays.asList("00", "05").contains(doe32SpedPlacementState)
                            && "00".equals(doe34SpedPlacementState)) {
                        doe36SpedDisability = "";
                        doe38SpedLevel = "";
                        doe40SpedEvalResults =
                                lookupUserValue(SisStudent.class, m_doe40SpedEvalResultsField.getJavaName(), "00");
                    }
                    captureSpedValue(doe32SpedPlacement, m_doe32Values, student, m_doe32SpedPlacementField);
                    captureSpedValue(doe34SpedPlacement, m_doe34Values, student, m_doe34SpedPlacementField);
                    captureSpedValue(doe36SpedDisability, m_doe36Values, student, m_doe36SpedDisabilityField);
                    captureSpedValue(doe38SpedLevel, m_doe38Values, student, m_doe38SpedLevelField);
                    captureSpedValue(doe40SpedEvalResults, m_doe40Values, student, m_doe40SpedEvalResultsField);
                } else {
                    m_studentsToExclude.add(student.getOid());
                }

            }
        }
    }

    /**
     * Captures a single sped value.
     *
     * @param value String
     * @param map Map<String,String>
     * @param student Student
     * @param studentField DataDictionaryField
     */
    private void captureSpedValue(String value,
                                  Map<String, String> map,
                                  Student student,
                                  DataDictionaryField studentField) {
        if (!StringUtils.isBlank(value)) {
            map.put(student.getOid(), value);

            if (m_updateStudents) {
                String lockValue = (String) student.getFieldValueByAlias(ALIAS_LOCK_SPED_VALUES);

                if (!"1".equalsIgnoreCase(lockValue)) {
                    student.setFieldValueByBeanPath(studentField.getJavaName(), value);
                    getBroker().saveBeanForced(student);
                }
            }
        }
    }

    /**
     * Gets the cutoff date.
     *
     * @return Plain date
     */
    private PlainDate getCutoffDate() {
        Calendar calendar = Calendar.getInstance();

        int schoolYear = getOrganization().getCurrentContext().getSchoolYear() - 1;

        calendar.set(Calendar.YEAR, schoolYear);
        calendar.set(Calendar.MONTH, Calendar.JULY);
        calendar.set(Calendar.DAY_OF_MONTH, 1);

        return new PlainDate(calendar.getTime());
    }

    /**
     * Loads the IEP and IEP disability lookup objects for students included in the passed criteria.
     *
     * @param studentCriteria Criteria
     */
    private void loadIepData(Criteria studentCriteria) {
        String[] columns = new String[] {X2BaseBean.COL_OID,
                IepData.COL_LAST_EVALUATION_DATE,
                IepData.COL_EXIT_REASON};

        String[] aliases = new String[] {IEP_EDUCATIONAL_ENVIRONMENT,
                IEP_EDUCATIONAL_ENVIRONMENT_EC,
                IEP_LEVEL_OF_NEED,
                IEP_PLEPA_SPECIAL_INSTRUCTION_CONTENT,
                IEP_PLEPA_SPECIAL_INSTRUCTION_METHODOLOGY,
                IEP_PLEPA_SPECIAL_INSTRUCTION_PERFORMANCE,
                IEP_PLEPB_SPECIAL_INSTRUCTION_CONTENT,
                IEP_PLEPB_SPECIAL_INSTRUCTION_METHODOLOGY,
                IEP_PLEPB_SPECIAL_INSTRUCTION_PERFORMANCE};

        m_iepLookup = new IepLookup(studentCriteria, columns, aliases, m_iepDictionary, getBroker());

        loadMeetingType(studentCriteria);

        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria disabilityCriteria = new Criteria();
        disabilityCriteria.addIn(IepDisability.REL_IEP_DATA + "." + IepData.COL_STUDENT_OID, subQuery);

        QueryByCriteria disabilityQuery = new QueryByCriteria(IepDisability.class, disabilityCriteria);
        disabilityQuery.addOrderByAscending(IepDisability.COL_IEP_DATA_OID);
        disabilityQuery.addOrderByDescending(IepDisability.COL_PRIMARY_INDICATOR);

        m_iepDisabilityLookup =
                getBroker().getGroupedCollectionByQuery(disabilityQuery, IepDisability.COL_IEP_DATA_OID, 1024);
    }

    /**
     * Load meeting type.
     *
     * @param studentCriteria Criteria
     */
    private void loadMeetingType(Criteria studentCriteria) {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(IepData.COL_STUDENT_OID, new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria));
        criteria.addGreaterOrEqualThan(IepData.COL_MEETING_DATE, getCutoffDate());
        criteria.addIn(IepData.COL_MEETING_TYPE_CODE,
                Arrays.asList(Integer.valueOf(IepMeeting.TypeCode.INITIAL.ordinal()),
                        Integer.valueOf(IepMeeting.TypeCode.REEVAL.ordinal())));

        m_iepMeetingType = new HashMap();
        String[] columns = new String[] {IepData.COL_STUDENT_OID, IepData.COL_MEETING_TYPE_CODE};
        ColumnQuery query = new ColumnQuery(IepData.class, columns, criteria);
        query.addOrderByAscending(IepData.COL_MEETING_DATE);
        try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                // save last record - most recent date
                m_iepMeetingType.put((String) row[0], Integer.valueOf(row[1].toString()));
            }
        }
    }

    /**
     * Captures a log message associated with the passed student.
     *
     * @param student SisStudent
     * @param messageToAdd String
     */
    private void logStudentMessage(SisStudent student, String messageToAdd) {
        String message = m_messages.get(student.getOid());
        if (message == null) {
            message = messageToAdd;
        } else {
            message += " " + messageToAdd;
        }

        m_messages.put(student.getOid(), message);
    }

    /**
     * Retrieves the value for DOE 36 (sped disability) from IEP data.
     *
     * @param student SisStudent
     * @return String
     */
    private String retrieveSpedDisability(SisStudent student) {
        String disability = null;

        String iepOid = null;
        if (SpedStatusCode.ACTIVE.equals(student.getSpedStatusCodeEnum())) {
            iepOid = (String) m_iepLookup.getIepValue(student.getOid(), IepData.StatusCode.ACTIVE, X2BaseBean.COL_OID);
        } else if (isSpedExitedThisYear(student)) {
            iepOid = (String) m_iepLookup.getIepValue(student.getOid(), IepData.StatusCode.PREVIOUS,
                    X2BaseBean.COL_OID);
        }

        if (iepOid != null) {
            Collection<IepDisability> disabilities = m_iepDisabilityLookup.get(iepOid);

            if (!CollectionUtils.isEmpty(disabilities)) {
                disability = disabilities.iterator().next().getDisabilityCode();
            }
        }

        return disability;
    }

    /**
     * Retrieves the value for DOE 40 (evaluation results) from IEP data.
     *
     * @param student SisStudent
     * @return String
     */
    private String retrieveEvaluationResults(SisStudent student) {
        String stateCode = "00";

        IepData.StatusCode iepStatusCode = null;
        if (student.getSpedStatusCodeEnum() == ACTIVE) {
            iepStatusCode = IepData.StatusCode.ACTIVE;
        } else if (student.getSpedStatusCodeEnum() == EXITED && isSpedExitedThisYear(student)) {
            iepStatusCode = IepData.StatusCode.PREVIOUS;
        }

        if (iepStatusCode != null) {
            if (m_iepLookup.hasIep(student.getOid(), iepStatusCode)) {
                /*
                 * Check the signed date of the IEP. If it happened in a previous school
                 * year, the stateCode should be reported as '01' because the special status
                 * would have been reported in a previous send. If the signed date happened
                 * this school year, use the correct special code depended upon the student
                 * IEP's last meeting type.
                 */
                Date lastEvaluationDate = (Date) m_iepLookup.getIepValue(student.getOid(), iepStatusCode,
                        IepData.COL_LAST_EVALUATION_DATE);
                if (isLastEvaluationAfterCutoffDate(
                        lastEvaluationDate != null ? new PlainDate(lastEvaluationDate) : null)) {
                    /*
                     * First check the exit reason. If it has a state code use that value. Otherwise
                     * use previous logic.
                     */
                    String exitStateCode = lookupStateValue(IepData.class, IepData.COL_EXIT_REASON,
                            (String) m_iepLookup.getIepValue(student.getOid(), iepStatusCode, IepData.COL_EXIT_REASON));
                    if (!StringUtils.isEmpty(exitStateCode)) {
                        stateCode = exitStateCode;
                    } else {
                        Integer meetingTypeCode = m_iepMeetingType.get(student.getOid());
                        if (meetingTypeCode == null) {
                            stateCode = "01";
                        } else {
                            switch (IepMeeting.TypeCode.values()[meetingTypeCode.intValue()]) {
                                case REEVAL:
                                    if (isSpedSpecialInstruction(student.getOid())) {
                                        stateCode = "06";
                                    } else {
                                        stateCode = "07";
                                    }
                                    break;

                                case INITIAL:
                                    if (isSpedSpecialInstruction(student.getOid())) {
                                        stateCode = "04";
                                    } else {
                                        stateCode = "05";
                                    }
                                    break;

                                case AMENDMENT:
                                case OTHER:
                                case REVIEW:
                                default:
                                    throw new IllegalStateException(
                                            "Only REEVAL and INITIAL should be contained in the map");
                            }
                        }
                    }
                } else {
                    String exitStateCode = lookupStateValue(IepData.class, IepData.COL_EXIT_REASON,
                            (String) m_iepLookup.getIepValue(student.getOid(), iepStatusCode, IepData.COL_EXIT_REASON));
                    if ("09".equals(exitStateCode)) {
                        stateCode = exitStateCode;
                    } else {
                        stateCode = "01";
                    }
                }
            }
        } else if (student.getSpedStatusCodeEnum() == INELIGIBLE && isSpedEvaluatedThisYear(student)) {
            if (m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.DISCARDED)) {
                Integer meetingTypeCode = m_iepMeetingType.get(student.getOid());
                if (meetingTypeCode == null) {
                    logStudentMessage(student,
                            "Meeting type must be Initial Referral or Re-evaluation for ineligible students.");
                } else {
                    switch (IepMeeting.TypeCode.values()[meetingTypeCode.intValue()]) {
                        case REEVAL:
                            stateCode = "03";
                            break;

                        case INITIAL:
                            stateCode = "02";
                            break;

                        default:
                            throw new IllegalStateException(
                                    "Only REEVAL and INITIAL should be contained in the map");
                    }
                }
            } else {
                logStudentMessage(student,
                        "Student is ineligible and exited program this year but does not have a discarded IEP");
            }
        } else if (student.getSpedStatusCodeEnum() == REFERRED) {
            stateCode = "08";

            /*
             * If the student's status is referred and their most recent IEP is REJECTED, then
             * use code 09 -
             * "Student evaluated and found eligible for services but parent/guardian declined."
             */
            if (m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.REJECTED) &&
                    !m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.DRAFT)) {
                stateCode = "09";
            }
        }
        return lookupUserValue(SisStudent.class, m_doe40SpedEvalResultsField.getJavaName(), stateCode);
    }

    /**
     * Checks if is last evaluation after cutoff date.
     *
     * @param lastEvaluationDate PlainDate
     * @return true, if is last evaluation after cutoff date
     */
    private boolean isLastEvaluationAfterCutoffDate(PlainDate lastEvaluationDate) {
        return lastEvaluationDate == null || lastEvaluationDate.after(getCutoffDate());
    }

    /**
     * Retrieves the value for DOE 38 (level of need) from IEP data.
     *
     * @param student SisStudent
     * @return String
     */
    private String retrieveSpedLevel(SisStudent student) {
        String level = null;

        boolean active = SpedStatusCode.ACTIVE.equals(student.getSpedStatusCodeEnum());

        if (active) {
            level = (String) m_iepLookup.getIepValueByAlias(student.getOid(), IepData.StatusCode.ACTIVE,
                    IEP_LEVEL_OF_NEED);
        } else if (isSpedExitedThisYear(student)) {
            level = (String) m_iepLookup.getIepValueByAlias(student.getOid(), IepData.StatusCode.PREVIOUS,
                    IEP_LEVEL_OF_NEED);
        }

        return level;
    }

    /**
     * Retrieves the value for DOE 32/34 (sped placement) from IEP data.
     *
     * @param student SisStudent
     * @param placementField DataDictionaryField
     * @return String
     * @throws X2BaseException exception
     */
    private String retrieveSpedPlacement(SisStudent student, DataDictionaryField placementField)
            throws X2BaseException {
        String placementInfo = null;

        String iepAlias;
        if (placementField.getAlias() != null && placementField.getAlias().contains(ALIAS_DOE_34_SPED_PLACEMENT)) {
            iepAlias = IEP_EDUCATIONAL_ENVIRONMENT;
        } else {
            iepAlias = IEP_EDUCATIONAL_ENVIRONMENT_EC;
        }

        boolean notSpecialEd = false;
        int ageAsOf = 0;
        if (m_reportDate != null && student.getPerson() != null) {
            ageAsOf = student.getPerson().getAgeAsOfDate(m_reportDate);
        }

        /*
         * DOE 32 is used for any child under 5 and 5 year olds that are not in kindergarten
         */
        boolean useDOE34 = true;
        if (ageAsOf == 0) {
            logStudentMessage(student,
                    "Unable to calculate sped placement (DOE32/DOE34). The student does not have a valid date of birth.");
            notSpecialEd = true;
        } else if (ageAsOf == 5 && !GRADES_KINDERGARTEN.contains(student.getGradeLevel())) {
            useDOE34 = false;
        } else if (ageAsOf < 5) {
            useDOE34 = false;
        }

        if (useDOE34 && ALIAS_DOE_32_SPED_PLACEMENT.equals(placementField.getAlias())
                || !useDOE34 && ALIAS_DOE_34_SPED_PLACEMENT.equals(placementField.getAlias())) {
            placementInfo = lookupUserValue(SisStudent.class, placementField.getJavaName(), "00");
            notSpecialEd = true;
        } else {
            switch (student.getSpedStatusCodeEnum()) {
                case ACTIVE:
                    placementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(), IepData.StatusCode.ACTIVE,
                            iepAlias);
                    break;

                case EXITED:
                    if (isSpedExitedThisYear(student)) {
                        if (student.getSpedExitDate().after(m_reportDate)) {
                            // If the student exited AFTER report date, display what would have been
                            // their DOE32/34 status prior to exiting.
                            placementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(),
                                    IepData.StatusCode.PREVIOUS, iepAlias);
                        } else {
                            // If the student exited on or before report date, report the "exited
                            // this year" code "01".
                            placementInfo = lookupUserValue(SisStudent.class, placementField.getJavaName(), "01");
                        }
                    } else {
                        notSpecialEd = true;
                    }
                    break;

                case INELIGIBLE:
                    placementInfo = lookupUserValue(SisStudent.class, placementField.getJavaName(), "00");
                    notSpecialEd = true;

                    if (isSpedExitedThisYear(student)) {
                        String iepPlacementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(),
                                IepData.StatusCode.PREVIOUS, iepAlias);
                        if (!StringUtils.isEmpty(iepPlacementInfo)) {
                            if (student.getSpedExitDate().after(m_reportDate)) {
                                // The student was deemed in-eligible this year after report date.
                                // Report what the Previous IEP had for placement.
                                placementInfo = iepPlacementInfo;
                            } else {
                                // The student was deemed in-eligible this year before report date.
                                // If they were not sped before, they are still not sped.
                                // If they were placed in sped in any way before, they are now
                                // Exited.
                                String currentPlacementCode = lookupStateValue(SisStudent.class,
                                        placementField.getJavaName(), iepPlacementInfo);
                                if (!"00".equals(currentPlacementCode)) {
                                    placementInfo =
                                            lookupUserValue(SisStudent.class, placementField.getJavaName(), "01");
                                    notSpecialEd = false;
                                }
                            }
                        }
                    }
                    break;

                default:
                    placementInfo = lookupUserValue(SisStudent.class, placementField.getJavaName(), "00");
                    notSpecialEd = true;
            }
        }

        /*
         * The special case below handles the "role model" code, which is a value in DOE32 for
         * regular education students. If the student is not a special education student, and we
         * are processing DOE32, maintain code values of 05 for role models.
         */
        if (notSpecialEd && placementField.getAlias() != null
                && placementField.getAlias().contains(ALIAS_DOE_32_SPED_PLACEMENT)) {
            String roleModelCode = lookupUserValue(SisStudent.class, placementField.getJavaName(), "05");
            if (roleModelCode != null) {
                String currentCode = (String) WebUtils.getProperty(student, placementField.getJavaName());
                if (roleModelCode.equals(currentCode)) {
                    placementInfo = roleModelCode;
                }
            }
        }

        return placementInfo;
    }

    /**
     * Configures the "lock sped values" field on the student table and reloads the data dictionary.
     *
     * @throws X2BaseException exception
     */
    private void setupLockSpedField() throws X2BaseException {
        ModelBroker broker = new ModelBroker(getPrivilegeSet());

        DataDictionaryTable studentTable = m_districtDictionary.findDataDictionaryTableByClass(Student.class.getName());

        DataFieldConfig availableField = DataDictionary.getAvailableField(studentTable.getSystemOid(), 1, broker);
        if (availableField != null) {
            availableField.setAlias(ALIAS_LOCK_SPED_VALUES);
            availableField.setUserType(CoreDataType.LOGICAL.getName());
            availableField.setEnabledIndicator(true);
            availableField.setListEditIndicator(true);
            availableField.setUpdateIndicator(true);
            availableField.setUserLongName(LOCK_STUDENTS_FIELD_NAME);
            availableField.setUserShortName(LOCK_STUDENTS_FIELD_NAME);

        }

        broker.saveBeanForced(availableField);

        DataDictionaryCache.clearDictionaries(broker.getPersistenceKey(), true);

        UpdateQuery query =
                new UpdateQuery(SisStudent.class, new Criteria(), availableField.getDataField().getJavaName(), null);
        broker.executeUpdateQuery(query);
    }

    /**
     * Attempts to update the template with the passed name with the DOE LOCK SPED field.
     * No update is performed if:
     * - No matching name is found within the student detail page context
     * - The template already contains the lock sped field
     * - The XML is not well formed after inserting the field
     *
     * @param templateName String
     */
    private void updateTemplate(String templateName) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ViewTemplate.COL_CONTEXT, TEMPLATE_CONTEXT);
        criteria.addEqualTo(ViewTemplate.COL_NAME, templateName);

        BeanQuery query = new BeanQuery(ViewTemplate.class, criteria);

        ViewTemplate template = (ViewTemplate) getBroker().getBeanByQuery(query);

        if (template == null) {
            AppGlobals.getLog().log(Level.INFO,
                    "SIMS Sped procedure: unable to update template; a template with the name '" + templateName
                            + "' was not found.");
        } else {
            String templateDef = template.getViewDefinition();
            if (templateDef.contains(ALIAS_LOCK_SPED_VALUES)) {
                AppGlobals.getLog().log(Level.INFO, "SIMS Sped procedure: skipped template update; the template '"
                        + templateName + "' already contains DOE LOCK SPED.");
            } else {
                boolean updated = false;
                Pattern search1Pattern =
                        Pattern.compile(TEMPLATE_INSERT_LOCATION_SEARCH_1, Pattern.DOTALL | Pattern.MULTILINE);
                Matcher search1Matcher = search1Pattern.matcher(templateDef);
                if (search1Matcher.find()) {
                    int insertPosition = search1Matcher.end();
                    updated = updateTemplate(template, insertPosition);
                } else {
                    Pattern search2Pattern =
                            Pattern.compile(TEMPLATE_INSERT_LOCATION_SEARCH_2, Pattern.DOTALL | Pattern.MULTILINE);
                    Matcher search2Matcher = search2Pattern.matcher(templateDef);
                    if (search2Matcher.find()) {
                        int insertPosition = search2Matcher.start() - 1;
                        updated = updateTemplate(template, insertPosition);
                    }
                }

                if (!updated) {
                    AppGlobals.getLog().log(Level.WARNING,
                            "SIMS Sped procedure: template update failed; unable to successfully insert DOE LOCK SPED into template '"
                                    + templateName + "'.");
                }
            }
        }
    }

    /**
     * Inserts the LOCK SPED field into the passed template at the passed position.
     * The operation is aborted if the resulting XML is not well formed and false is
     * returned.
     *
     * @param template ViewTemplate
     * @param insertPosition int
     * @return boolean
     */
    private boolean updateTemplate(ViewTemplate template, int insertPosition) {
        boolean updated = false;

        String updatedTemplate =
                StringUtils.insertString(template.getViewDefinition(), insertPosition, TEMPLATE_XML_TO_INSERT);
        if (validateTemplateXml(updatedTemplate)) {
            template.setViewDefinition(updatedTemplate);
            getBroker().saveBeanForced(template);
            updated = true;
        }

        return updated;
    }

    /**
     * Returns true if the passed XML is well formed and the root element name is "template".
     *
     * @param xml String
     * @return true, if successful
     */
    private boolean validateTemplateXml(String xml) {
        boolean valid = false;

        X2SAXBuilder builder = new X2SAXBuilder();
        org.jdom.Document document;
        try {
            document = builder.build(new ByteArrayInputStream(xml.getBytes()));
            valid = "template".equals(document.getRootElement().getName());
        } catch (JDOMException | IOException e) {
            // valid will be false
        }

        return valid;

    }

    // -------------------------- Adapted from StateReportData -------------------------- //

    /**
     * Returns the user lookup code for a state reference code value.
     * Look up based on bean path. Enabled codes are prioritized but if a match cannot be found,
     * disabled codes are considered.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupUserValue(Class beanClass, String beanPath, String value) {
        String userValue = lookupUserValue(beanClass, beanPath, value, false);

        if (userValue == null) {
            userValue = lookupUserValue(beanClass, beanPath, value, true);
        }

        return userValue;
    }

    /**
     * Returns the user lookup code for a state reference code value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     * @param includeDisabled boolean
     * @return String - state code for input value.
     */
    public String lookupUserValue(Class beanClass, String beanPath, String value, boolean includeDisabled) {
        String userValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            Map<String, ReferenceCode> refcodes = getReferenceCodes(dictionaryField.getReferenceTableOid());
            for (ReferenceCode code : refcodes.values()) {
                if ((includeDisabled || !code.getDisabledIndicator()) &&
                        !StringUtils.isEmpty(code.getStateCode()) && code.getStateCode().equals(value)) {
                    userValue = code.getCode();
                    break;
                }
            }
        }

        return userValue;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupStateValue(Class beanClass, String beanPath, String value) {
        String stateValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, value,
                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        return stateValue;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - reference map type (ExportFormatField.ReferenceMapTypeCode.*.ordinal())
     *        of the lookup.
     *
     * @return String - state code for input value.
     */
    public String lookupReferenceCodeByBeanPath(Class beanClass, String beanPath, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        }

        return stateValue;
    }

    /**
     * Returns the lookup code value for field value.
     * Look up based on the reference table.
     *
     * @param referenceTableOid String
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - the reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
     * @return String - reference code lookup value for input value.
     */
    public String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
        String returnValue = null;
        Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
        ReferenceCode code = refCodes.get(value);
        if (code != null) {
            if (referenceMap == ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) {
                returnValue = code.getStateCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal()) {
                returnValue = code.getFederalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal()) {
                returnValue = code.getLocalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.SYSTEM.ordinal()) {
                returnValue = code.getSystemCode();
            }
        }

        return returnValue;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     *
     * @param beanClass Class
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
        ModelProperty prop = new ModelProperty(beanClass, path, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = m_districtDictionary.findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * Lookup a map of reference codes for a reference table oid.
     * Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable =
                    (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    codeMap.put(code.getCode(), code);
                }
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }
}
