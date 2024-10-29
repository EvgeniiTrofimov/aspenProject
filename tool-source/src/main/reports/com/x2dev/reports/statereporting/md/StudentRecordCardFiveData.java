/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class StudentRecordCardFiveData.
 *
 * @author X2 Development Corporation
 */
public class StudentRecordCardFiveData extends ReportJavaSourceNet {

    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String ADDITIONAL_FORMAT = "AdditionalFormat";
    private static final String ADDITIONAL_GRID = "AdditionalGrid";
    private static final String ADDITIONAL_TYPE = "general";
    private static final String ALLERGIES = "allergies";
    private static final String ALLERGY_CONDITION = "Allergy";

    private static final String BDAY = "bday";
    private static final String BMONTH = "bmonth";
    private static final String BYEAR = "byear";

    private static final String CODE_504 = "504";
    private static final String COLOR = "color";
    private static final String COMMENT = "comment";

    private static final String DATE = "date";
    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final String DATE_NOTIFIED = "dateNotified";


    private static final String DENTAL_FORMAT = "DentalFormat";
    private static final String DENTAL_GRID = "DentalGrid";
    private static final String DENTAL_TYPE = "dental";

    private static final String EMPTY = "empty";

    private static final String FAIL = "fail";
    private static final String FAIL_STATUS = "fail";
    private static final String FEET_SYMBOL = "' ";
    private static final String FOLLOW_UP_N = "FollowUpN";
    private static final String FOLLOW_UP_Y = "FollowUpY";

    private static final String GRADE = "grade";

    private static final String HEARING_FORMAT = "HearingFormat";
    private static final String HEARING_GRID = "HearingGrid";
    private static final String HEARING_L = "hearingL";
    private static final String HEARING_R = "hearingR";
    private static final String HEARING_TYPE = "hearing";
    private static final String HEIGHT = " Height: ";

    private static final String IEP = "IEP";
    private static final String IEP_ENTRY = "iep";
    private static final String INCHES_SYMBOL = "\"";
    private static final String INPUT_ALIAS_HEIGHT = "height";
    private static final String INPUT_ALIAS_SPED504IEP = "SPED504IEP";
    private static final String INPUT_ALIAS_WEIGHT = "weight";
    private static final String INPUT_BEAN_PATH_EXAMINER = "name";
    private static final String INPUT_BEAN_PATH_PROVIDER = "provider";
    private static final String INPUT_BEAN_PATH_REF_DATE = "date";
    private static final String INPUT_BEAN_PATH_TITLE = "title";

    private static final String LOCID = "locid";

    private static final String MEDICAL_CONDITION = "medicalCondition";
    private static final String MEDICAL_CONDITION_TYPES_TABLE = "Medical Condition Types";
    private static final String MEDICAL_CONDITIONS_TABLE = "Medical Conditions";
    private static final String MUSCLE_F = "muscleF";
    private static final String MUSCLE_N = "muscleN";

    private static final String NAME = "name";
    private static final String NAME_TITLE = "nameTitle";
    private static final String NO_CODE = " ";
    private static final String NO_GLASS_L = "noGlassL";
    private static final String NO_GLASS_R = "noGlassR";
    private static final String NONE = "None";
    private static final String NOT_AVAILABLE = "Not Available";

    private static final String OTHER_FORMAT = "OtherFormat";
    private static final String OTHER_GRID = "OtherGrid";
    private static final String OTHER_TYPE = "lice";

    private static final String PASS = "pass";
    private static final String PASS_STATUS = "pass";
    private static final String PHYSICAL_FORMAT = "PhysicalFormat";
    private static final String PHYSICAL_GRID = "PhysicalGrid";
    private static final String PHYSICAL_TYPE = "physical";
    private static final String POUNDS = "lb";

    private static final String QUERY_BY = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";

    private static final String SASID = "sasid";

    private static final String SLASH = "/";

    private static final String VISION_FORMAT = "VisionFormat";
    private static final String VISION_GRID = "VisionGrid";
    private static final String VISION_TYPE = "vision";

    private static final String WEIGHT = " Weight: ";
    private static final String WITH_GLASS_L = "withGlassL";
    private static final String WITH_GLASS_R = "withGlassR";

    private static final String YES_CODE = "Y";

    private String m_additionalScreeningId;

    private SisStudent m_currentStudent;
    private QueryByCriteria m_conditionsQuery;
    private Map<String, DataDictionary> m_ddxMap = new HashMap<String, DataDictionary>();
    private String m_dentalScreeningId;
    private ReportDataGrid m_grid;
    private String m_hearingScreeningId;
    private Map<String, ReferenceCode> m_medConditionsLookup;
    private Map<String, ReferenceCode> m_medCondTypesLookup;
    private String m_otherScreeningId;
    private String m_physicalScreeningId;
    private Map<String, Collection<HealthCondition>> m_studentsToConditions;
    private Criteria m_studentCriteria;
    private X2Criteria m_screeningCriteria;
    private String m_visionScreeningId;

    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_grid = new ReportDataGrid();
        ReportDataGrid current;

        m_visionScreeningId = (String) getParameter(VISION_FORMAT);
        m_hearingScreeningId = (String) getParameter(HEARING_FORMAT);
        m_physicalScreeningId = (String) getParameter(OTHER_FORMAT);
        m_dentalScreeningId = (String) getParameter(OTHER_FORMAT);
        m_otherScreeningId = (String) getParameter(OTHER_FORMAT);
        m_additionalScreeningId = (String) getParameter(ADDITIONAL_FORMAT);

        buildCriteria();

        m_studentsToConditions = getBroker().getGroupedCollectionByQuery(m_conditionsQuery,
                HealthCondition.COL_STUDENT_OID, 150);

        QueryByCriteria screeningQuery = new QueryByCriteria(HealthScreening.class, m_screeningCriteria);
        screeningQuery.addOrderBy(HealthScreening.COL_STUDENT_OID, true);
        GradeLevelHistory gradeLevel = new GradeLevelHistory(m_studentCriteria, 18, getOrganization(), getBroker());

        ReportDataGrid vision = new ReportDataGrid();
        ReportDataGrid hearing = new ReportDataGrid();
        ReportDataGrid physical = new ReportDataGrid();
        ReportDataGrid dental = new ReportDataGrid();
        ReportDataGrid other = new ReportDataGrid();
        ReportDataGrid additional = new ReportDataGrid();

        SisStudent oldstudent = null;

        QueryIterator screenings = getBroker().getIteratorByQuery(screeningQuery);
        try {
            while (screenings.hasNext()) {
                HealthScreening screening = (HealthScreening) screenings.next();
                ExtendedDictionaryAttributes ddxAttr = screening.getExtendedDataDictionary();
                boolean newstudent = !screening.getStudent().equals(oldstudent);
                oldstudent = screening.getStudent();
                if (newstudent) {
                    if (!m_grid.isEmpty()) {
                        doSubreports(vision, hearing, physical, dental, other, additional);
                    }
                    vision = new ReportDataGrid();
                    hearing = new ReportDataGrid();
                    physical = new ReportDataGrid();
                    dental = new ReportDataGrid();
                    other = new ReportDataGrid();
                    additional = new ReportDataGrid();
                    doHeader(oldstudent);
                }

                String screetingType = screening.getType();
                String type = null;
                // try fix issue if screetingType is null
                if (StringUtils.isEmpty(screetingType)) {

                    if (ddxAttr != null) {
                        String oid = ddxAttr.getOid();
                        String ddxAttrName = ddxAttr.getName();
                        if (oid.equals("ddxHscLice")) {
                            type = OTHER_TYPE;
                        } else if (!StringUtils.isEmpty(ddxAttrName)) {
                            type = ddxAttrName.toLowerCase();
                        }
                    }
                } else {
                    type = screetingType.toLowerCase();

                }


                // pulling comment early so I can append in general

                String rawComment = screening.getComment();
                if (rawComment == null) {
                    rawComment = "";
                }
                StringBuffer comment = new StringBuffer(rawComment);
                if (type.equalsIgnoreCase(VISION_TYPE))// Vision
                {

                    current = vision;
                    current.append();

                    current.set(NO_GLASS_R, getFieldValueByAlias(screening, NO_GLASS_R, ddxAttr));
                    current.set(NO_GLASS_L, getFieldValueByAlias(screening, NO_GLASS_L, ddxAttr));
                    current.set(WITH_GLASS_R, getFieldValueByAlias(screening, WITH_GLASS_R, ddxAttr));
                    current.set(WITH_GLASS_L, getFieldValueByAlias(screening, WITH_GLASS_L, ddxAttr));
                    current.set(MUSCLE_N, getFieldValueByAlias(screening, MUSCLE_N, ddxAttr));
                    current.set(MUSCLE_F, getFieldValueByAlias(screening, MUSCLE_F, ddxAttr));
                    current.set(COLOR, getFieldValueByAlias(screening, COLOR, ddxAttr));
                } else if (type.equals(PHYSICAL_TYPE))// Physical
                {
                    current = physical;
                    current.append();
                } else if (type.equals(DENTAL_TYPE)) {
                    current = dental;
                    current.append();
                } else if (type.equals(OTHER_TYPE))// Lice
                {
                    current = other;
                    current.append();
                } else if (type.equals(ADDITIONAL_TYPE))// General
                {
                    current = additional;
                    current.append();
                    doHeightWeightParse(screening, comment);
                } else if (type.equals(HEARING_TYPE))// Hearing
                {
                    current = hearing;
                    current.append();
                    current.set(HEARING_L, getFieldValueByAlias(screening, HEARING_L, ddxAttr));
                    current.set(HEARING_R, getFieldValueByAlias(screening, HEARING_R, ddxAttr));
                } else {
                    current = m_grid;
                    current.append();
                }

                current.set(GRADE, gradeLevel.getGradeLevel(screening.getStudentOid(), screening.getDate()));
                doMisc(current, screening, type, comment);
            }
            doSubreports(vision, hearing, physical, dental, other, additional);

        } finally {
            screenings.close();
        }

        m_grid.beforeTop();

        return m_grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(final UserDataContainer userData) {
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Adds a subreport. Could have done inline, but was longer with debugging. Compiler can inline
     * it if it's that
     * important
     *
     * @param id The identity of the report
     * @param format The parameter for putting in the format
     * @param gridParam Where the subreport goes (the name passed to ireport)
     * @param rubreportGrid The data grid
     */
    private void addSubreport(final String id,
                              final String format,
                              final String gridParam,
                              final ReportDataGrid rubreportGrid) {
        rubreportGrid.beforeTop();
        m_grid.set(gridParam, rubreportGrid);
        Report report = ReportUtils.getReport(id, getBroker());
        byte[] stream = report.getCompiledFormat();
        addParameter(format, new ByteArrayInputStream(stream));
    }

    /**
     * Determines what to pull based on user input.
     */
    private void buildCriteria() {
        m_studentCriteria = new Criteria();

        if (m_currentStudent != null) {
            /*
             * Running for one student
             */
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = getParameter(QUERY_BY).toString();
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            if (queryString == null) {
                queryString = "";
            }

            addUserCriteria(m_studentCriteria, queryBy, queryString, SisStudent.class, X2BaseBean.COL_OID);

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly && !queryBy.contains(CURRENT_KEY)) {
                StudentContextReportHelper helper =
                        new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());
                m_studentCriteria.addAndCriteria(helper.getActiveStudentCriteria());
            }

            if (isSchoolContext() && !queryBy.contains(CURRENT_KEY)) {
                m_studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            }


        }


        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
        m_screeningCriteria = new X2Criteria();

        m_screeningCriteria.addIn(HealthScreening.COL_STUDENT_OID, studentSubQuery);

        X2Criteria conditionsCriteria = new X2Criteria();
        conditionsCriteria.addIn(HealthCondition.COL_STUDENT_OID, studentSubQuery);
        m_conditionsQuery = new QueryByCriteria(HealthCondition.class, conditionsCriteria);
        m_conditionsQuery.addOrderByAscending(HealthCondition.COL_STUDENT_OID);

    }

    /**
     * Sets up the header information for the report.
     *
     * @param oldstudent the student about to be processed
     */
    private void doHeader(SisStudent oldstudent) {
        m_grid.append();
        m_grid.set(NAME, oldstudent.getNameView());
        m_grid.set(LOCID, oldstudent.getLocalId());
        m_grid.set(SASID, oldstudent.getStateId());
        PlainDate date = oldstudent.getPerson().getDob();
        String[] dateParts = date.toString().split("-");
        String month = dateParts[1];
        String day = dateParts[2];
        String year = dateParts[0];

        m_grid.set(BMONTH, month);
        m_grid.set(BDAY, day);
        m_grid.set(BYEAR, year);
        Boolean fiveOhFour = Boolean.valueOf(false);
        Boolean iep = Boolean.valueOf(false);
        String iepOr504code = (String) getFieldValueByAlias(oldstudent, INPUT_ALIAS_SPED504IEP);
        if ((iepOr504code != null) && iepOr504code.contains(CODE_504)) {
            fiveOhFour = Boolean.valueOf(true);
        }
        if ((iepOr504code != null) && iepOr504code.contains(IEP)) {
            iep = Boolean.valueOf(true);
        }
        // usually an IEP eliminates the 504. Can do that in logic, but drawing from data for now
        m_grid.set(CODE_504, fiveOhFour);
        m_grid.set(IEP_ENTRY, iep);
        Collection<HealthCondition> conditions = m_studentsToConditions.get(oldstudent.getOid());
        if (conditions == null) {
            conditions = new ArrayList<HealthCondition>();
        }
        StringBuffer medicalConditions = new StringBuffer();
        StringBuffer allergyConditions = new StringBuffer();

        m_medConditionsLookup = getReferenceTableMap(MEDICAL_CONDITIONS_TABLE);
        m_medCondTypesLookup = getReferenceTableMap(MEDICAL_CONDITION_TYPES_TABLE);
        for (HealthCondition condition : conditions) {

            String conditionName = lookUp(condition.getConditionCode(), m_medConditionsLookup);
            String conditionType = lookUp(condition.getConditionType(), m_medCondTypesLookup);

            if (conditionType.contains(ALLERGY_CONDITION)) {
                allergyConditions.append(condition.getComment());
            } else {
                medicalConditions.append(conditionName);
            }
        }
        if (medicalConditions.toString().trim().isEmpty()) {
            medicalConditions.append(NONE);
        }
        if (allergyConditions.toString().trim().isEmpty()) {
            allergyConditions.append(NONE);
        }
        m_grid.set(MEDICAL_CONDITION, medicalConditions.toString());
        m_grid.set(ALLERGIES, allergyConditions.toString());
    }

    /**
     * Handles parsing the height and weight .
     *
     * @param screening the data source
     * @param comment where client wants the height and weight reported
     */
    private void doHeightWeightParse(HealthScreening screening, StringBuffer comment) {
        ExtendedDictionaryAttributes ddxAtr = screening.getExtendedDataDictionary();
        String height = (String) getFieldValueByAlias(screening, INPUT_ALIAS_HEIGHT, ddxAtr);
        if ((height != null) && StringUtils.isNumeric(height)) {
            BigDecimal inches = new BigDecimal(height);
            comment.append(HEIGHT + (inches.intValue() / 12) + FEET_SYMBOL + (inches.doubleValue() % 12)
                    + INCHES_SYMBOL);
        }
        String weight = (String) getFieldValueByAlias(screening, INPUT_ALIAS_WEIGHT, ddxAtr);
        if (weight != null) {
            comment.append(WEIGHT + weight + POUNDS);
        }
    }

    /**
     * Takes care of the various bits that needed to be done, but were just ... assorted
     *
     * @param current grid being operated on
     * @param screening the screening with the data
     * @param type what type it is
     * @param comment the comment string, for manipulation
     */
    private void doMisc(ReportDataGrid current, HealthScreening screening, String type, StringBuffer comment) {
        current.set(DATE, screening.getDate());
        String pass, fail;
        String screenStatus = screening.getResultCode().toLowerCase();

        if (screenStatus.equals(PASS_STATUS)) {
            pass = YES_CODE;
            fail = NO_CODE;
        } else if (screenStatus.equals(FAIL_STATUS)) {
            pass = NO_CODE;
            fail = YES_CODE;
        } else {
            pass = NO_CODE;
            fail = NO_CODE;
        }

        current.set(PASS, pass);
        current.set(FAIL, fail);
        String date = (String) getFieldValueByBeanPath(screening, INPUT_BEAN_PATH_REF_DATE);
        SimpleDateFormat formatter = new SimpleDateFormat(DATE_FORMAT);
        PlainDate dateStr = null;
        if (date != null) {
            try {
                dateStr = new PlainDate(formatter.parse(date));
            } catch (ParseException e) {
                dateStr = null;
            }
        }
        current.set(DATE_NOTIFIED, dateStr);

        if (screening.getFollowUpIndicator()) {
            pass = YES_CODE;
            fail = NO_CODE;
        } else {
            pass = NO_CODE;
            fail = YES_CODE;
        }
        current.set(FOLLOW_UP_Y, pass);
        current.set(FOLLOW_UP_N, fail);
        String name = (String) getFieldValueByBeanPath(screening, INPUT_BEAN_PATH_EXAMINER);
        if (name == null) {
            name = NOT_AVAILABLE;
        }
        String title = (String) getFieldValueByBeanPath(screening, INPUT_BEAN_PATH_TITLE);
        screening.getFieldB047();
        if (title == null) {
            title = NOT_AVAILABLE;
        }
        String provider = (String) getFieldValueByBeanPath(screening, INPUT_BEAN_PATH_PROVIDER);
        if (provider == null) {
            provider = NOT_AVAILABLE;
        }
        if (!type.equals(ADDITIONAL_TYPE))// General
        {
            // general displays this information in another field
            comment.append(" " + name + SLASH + provider);
        }

        current.set(NAME_TITLE, (name + SLASH + title));
        current.set(COMMENT, comment.toString());
        current.set(EMPTY, Boolean.FALSE);
    }

    /**
     * Does all the rubreports for this. Considered storing them in an array for my sanity, but
     * sanity seems constant without it
     *
     * @param vision one of the grids to be subreported
     * @param hearing one of the grids to be subreported
     * @param physical one of the grids to be subreported
     * @param dental one of the grids to be subreported
     * @param other one of the grids to be subreported
     * @param additional one of the grids to be subreported
     */
    private void doSubreports(final ReportDataGrid vision,
                              final ReportDataGrid hearing,
                              final ReportDataGrid physical,
                              final ReportDataGrid dental,
                              final ReportDataGrid other,
                              final ReportDataGrid additional) {

        if (vision.isEmpty()) {
            vision.append();
            vision.set(EMPTY, Boolean.TRUE);
        }
        vision.beforeTop();

        if (hearing.isEmpty()) {
            hearing.append();
            hearing.set(EMPTY, Boolean.TRUE);
        }
        hearing.beforeTop();

        if (physical.isEmpty()) {
            physical.append();
            physical.set(EMPTY, Boolean.TRUE);
        }
        physical.beforeTop();

        if (dental.isEmpty()) {
            dental.append();
            dental.set(EMPTY, Boolean.TRUE);
        }
        dental.beforeTop();

        if (other.isEmpty()) {
            other.append();
            other.set(EMPTY, Boolean.TRUE);
        }
        other.beforeTop();

        if (additional.isEmpty()) {
            additional.append();
            additional.set(EMPTY, Boolean.TRUE);
        }
        additional.beforeTop();
        addSubreport(m_visionScreeningId, VISION_FORMAT, VISION_GRID, vision);
        addSubreport(m_hearingScreeningId, HEARING_FORMAT, HEARING_GRID, hearing);
        addSubreport(m_physicalScreeningId, PHYSICAL_FORMAT, PHYSICAL_GRID, physical);
        addSubreport(m_dentalScreeningId, DENTAL_FORMAT, DENTAL_GRID, dental);
        addSubreport(m_otherScreeningId, OTHER_FORMAT, OTHER_GRID, other);
        addSubreport(m_additionalScreeningId, ADDITIONAL_FORMAT, ADDITIONAL_GRID, additional);
    }

    /**
     * Gets the data dictionary.
     *
     * @return district Dictionary
     */
    private DataDictionary getDataDictionary() {
        return DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Gets the data dictionary by attributes.
     *
     * @param ddxAttr ExtendedDictionaryAttributes
     * @return DataDictionary extend by <code>ddxAttr</code>
     */
    private DataDictionary getDataDictionaryByAttributes(ExtendedDictionaryAttributes ddxAttr) {
        String key = ddxAttr.getName();
        DataDictionary ddx = m_ddxMap.get(key);
        if (ddx == null) {
            ddx = DataDictionary.getDistrictDictionary(ddxAttr, getBroker().getPersistenceKey());
            m_ddxMap.put(key, ddx);
        }
        return ddx;
    }

    /**
     * return field value by alias<br>
     * try find alias in input definition by <code>alias</code><br>
     * if doesn't found - using <code>alias</code> like alias .
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return Object
     */
    private Object getFieldValueByAlias(X2BaseBean bean, String alias) {
        return getFieldValueByAlias(bean, alias, getDataDictionary());
    }

    /**
     * return field value by alias<br>
     * try find alias in input definition by <code>alias</code><br>
     * if doesn't found - using <code>alias</code> like alias .
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param ddx DataDictionary
     * @return Object
     */
    private Object getFieldValueByAlias(X2BaseBean bean, String alias, DataDictionary ddx) {
        String aliasFromInput = (String) getParameter(alias);
        if (!StringUtils.isEmpty(aliasFromInput)) {
            alias = aliasFromInput;
        }
        return bean.getFieldValueByAlias(alias, ddx);
    }

    /**
     * return field value by alias<br>
     * try find alias in input definition by <code>alias</code><br>
     * if doesn't found - using <code>alias</code> like alias .
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param ddxAttr ExtendedDictionaryAttributes
     * @return Object
     */
    private Object getFieldValueByAlias(X2BaseBean bean, String alias, ExtendedDictionaryAttributes ddxAttr) {
        DataDictionary ddx = getDataDictionaryByAttributes(ddxAttr);
        String aliasFromInput = (String) getParameter(alias);
        if (!StringUtils.isEmpty(aliasFromInput)) {
            alias = aliasFromInput;
        }
        return bean.getFieldValueByAlias(alias, ddx);
    }

    /**
     * return field value by BeanPath<br>
     * try find BeanPath in input definition by <code>inputKey</code><br>
     * .
     *
     * @param bean X2BaseBean
     * @param inputKey String
     * @return Object
     */
    private Object getFieldValueByBeanPath(X2BaseBean bean, String inputKey) {
        String beanPathFormInput = (String) getParameter(inputKey);
        if (!StringUtils.isEmpty(beanPathFormInput)) {
            inputKey = beanPathFormInput;
        }
        return bean.getFieldValueByBeanPath(inputKey);
    }


    /**
     * ReferanceTable.getCodesMap for custom references.
     *
     * @param tablename
     *        Name of the table we're getting the map for.
     * @return The map, just like getCodesMap
     */
    private Map<String, ReferenceCode> getReferenceTableMap(final String tablename) {
        Criteria assessmentImpressionCodeCriteria = new Criteria();
        assessmentImpressionCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER
                + ReferenceTable.COL_USER_NAME, tablename);
        QueryByCriteria assessmentImpressionCrtiteria = new QueryByCriteria(ReferenceCode.class,
                assessmentImpressionCodeCriteria);

        return getBroker().getMapByQuery(assessmentImpressionCrtiteria, ReferenceCode.COL_CODE, 16);
    }

    /**
     * Look up.
     *
     * @param field The code you want to lookup delimited
     * @param codeLookup The map yow want to use
     * @return The description that goes with those codes code, blank if null
     */
    private String lookUp(final String field, final Map<String, ReferenceCode> codeLookup) {
        String result = "";
        if (field != null) {
            ReferenceCode code = codeLookup.get(field);
            if (code != null) {
                result = code.getDescription();
            }
        }

        return result;
    }

}
