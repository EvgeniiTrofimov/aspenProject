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

package com.x2dev.procedures.statereporting.on.register.original;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.ParameterSelectionHandler;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.FileDownloadAction;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.sis.web.gradebook.ScoreGrid;
import com.x2dev.utils.ByteArrayUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.awt.Image;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.stream.Collectors;
import javax.imageio.ImageIO;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Container class Ontario tools for customizations.
 *
 * @author Follett Software Company
 */
public class OntarioToolHelper {
    private static final long serialVersionUID = 1L;
    private School m_school;

    /**
     * Instantiates a new Ontario tool helper.
     */
    public OntarioToolHelper() {
        // Empty constructor.
    }

    /**
     * Instantiates a new Ontario tool helper.
     *
     * @param school School
     */
    public OntarioToolHelper(School school) {
        super();
        this.m_school = school;
    }

    /**
     * Returns a list of all active schools for the site.
     *
     * <p>
     * <b>NOTE:</b> You may need to exclude CTCC schools from returned list.
     *
     * @param broker the broker
     * @return the all active school oids
     */
    public static Collection<String> getAllActiveSchoolOids(X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, true);
        criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, true);

        BeanQuery query = new BeanQuery(SisSchool.class, criteria);
        List activeSchoolOids = new LinkedList();
        try (QueryIterator iterator = broker.getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                SisSchool school = (SisSchool) iterator.next();
                activeSchoolOids.add(school.getOid());
            }
        }

        return activeSchoolOids;
    }

    /**
     * Get logo image in base64 string format.
     *
     * @param imageCode RCD_CODE for image
     * @param broker the broker
     * @return String - base64 value
     */
    public static String getBase64ImageString(String imageCode, X2Broker broker) {
        String base64Image = null;
        Map<String, ReferenceCode> referenceCodeMap = getBase64ImageCodeMap(broker);
        ReferenceCode rcdBean = referenceCodeMap.get(imageCode);
        if (rcdBean != null) {
            base64Image = (String) rcdBean.getFieldValueByAlias(OntarioAlias.ALIAS_RCD_IMAGE_BASE64);
        }
        return base64Image;
    }

    /**
     * Get logo image in base64 string format.
     *
     * @param imageCode RCD_CODE for image
     * @param referenceCodeMap the reference code map
     * @return String - base64 value
     */
    public static String getBase64ImageString(String imageCode, Map<String, ReferenceCode> referenceCodeMap) {
        String base64Image = null;
        ReferenceCode rcdBean = referenceCodeMap.get(imageCode);
        if (rcdBean != null) {
            base64Image = (String) rcdBean.getFieldValueByAlias(OntarioAlias.ALIAS_RCD_IMAGE_BASE64);
        }
        return base64Image;
    }

    /**
     * Get map of logo image in base64 string format to image codes Will return null
     * for any invalid key.
     *
     * @param imageCodes list of RCD_CODE for image
     * @param broker the broker
     * @return Map<String, String> - Values to Keys map
     */
    public static Map<String, String> getBase64ImageString(List<String> imageCodes, X2Broker broker) {
        Map<String, String> resultImagesMap = new HashMap<>();
        Map<String, ReferenceCode> referenceCodeMap;
        ReferenceCode tempRcdBean;
        if (imageCodes != null) {
            referenceCodeMap = getBase64ImageCodeMap(broker);
            for (String imageCode : imageCodes) {
                tempRcdBean = referenceCodeMap.get(imageCode);
                if (tempRcdBean != null) {
                    resultImagesMap.put(imageCode,
                            (String) tempRcdBean.getFieldValueByAlias(OntarioAlias.ALIAS_RCD_IMAGE_BASE64));
                }
            }
        }
        return resultImagesMap;
    }

    /**
     * Get map of ReferenceCode bean keyed on ReferenceCode.COL_CODE for
     * ON_SIS_IMAGES reference
     *
     * @param broker the broker
     * @return Map<String, ReferenceCode>
     */
    private static Map<String, ReferenceCode> getBase64ImageCodeMap(X2Broker broker) {
        Map<String, ReferenceCode> referenceCodeMap;
        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(SisBeanPaths.REF_CODE.referenceTableOid().getPath(),
                OntarioAlias.REF_OID_ON_SIS_IMAGES);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        referenceCodeMap = broker.getMapByQuery(imageQuery, SisBeanPaths.REF_CODE.code().getPath(), 10);
        return referenceCodeMap;
    }

    /**
     * Initializes for localization Language is set at the school level and can be
     * English or French
     *
     * Populates the Valid Locales map Returns value for the localization parameters.
     *
     * @param org the org
     * @param skl the skl
     * @param broker the broker
     * @return MessageResouces
     */
    public static MessageResources initializeLocalized(Organization org, SisSchool skl, X2Broker broker) {
        Collection<OrganizationLocale> locales = org.getRootOrganization().getLocales();
        MessageResources resourcesEnglish = null;
        MessageResources resourcesFrench = null;
        MessageResources resourcesOut = null;

        String sklLang = OntarioAlias.CONST_EMPTY;
        if (skl != null) {
            sklLang = (String) skl.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_LANGUAGE);
        } else {
            sklLang = (String) org.getFieldValueByAlias(OntarioAlias.ALIAS_ORG_LANGUAGE);
        }

        for (OrganizationLocale loc : locales) {
            if (loc.getLocale().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                resourcesEnglish = LocalizationCache.getMessages(broker.getPersistenceKey(), loc.getSystemLocale());
            } else if (loc.getLocale().contains("fr")) {
                resourcesFrench = LocalizationCache.getMessages(broker.getPersistenceKey(), loc.getSystemLocale());
            }
        }

        resourcesOut = resourcesEnglish;
        if ((sklLang != null) && (sklLang.equals(OntarioAlias.CONST_LANG_FRENCH)) && (resourcesFrench != null)) {
            resourcesOut = resourcesFrench;
        }

        return resourcesOut;
    }

    /**
     * Initializes for localization Language is set at the school level and can be
     * English or French
     *
     * Populates the Valid Locales map Returns value for the localization parameters
     * Updates locale based on language.
     *
     * @param org the org
     * @param skl the skl
     * @param locVarMap - updated, pass empty map in. Update with
     *        OrganizationLocale(also key)
     * @param broker the broker
     * @return MessageResouces - OrganizationLocale with key lol
     */
    public static MessageResources initializeLocalized(Organization org,
                                                       SisSchool skl,
                                                       Map<String, Object> locVarMap,
                                                       X2Broker broker) {
        Collection<OrganizationLocale> locales = org.getRootOrganization().getLocales();
        MessageResources resourcesEnglish = null;
        MessageResources resourcesFrench = null;
        MessageResources resourcesOut = null;
        OrganizationLocale locForEnglish = null;
        OrganizationLocale locForFrench = null;

        OrganizationLocale locForLang = null;

        String sklLang = OntarioAlias.CONST_EMPTY;
        if (skl != null) {
            sklLang = (String) skl.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_LANGUAGE);
        } else {
            sklLang = (String) org.getFieldValueByAlias(OntarioAlias.ALIAS_ORG_LANGUAGE);
        }

        for (OrganizationLocale loc : locales) {
            if (loc.getLocale().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                resourcesEnglish = LocalizationCache.getMessages(broker.getPersistenceKey(), loc.getSystemLocale());
                locForEnglish = loc;
            } else if (loc.getLocale().contains("fr")) {
                resourcesFrench = LocalizationCache.getMessages(broker.getPersistenceKey(), loc.getSystemLocale());
                locForFrench = loc;
            }
        }

        resourcesOut = resourcesEnglish;
        locForLang = locForEnglish;
        if ((sklLang != null) && (sklLang.equals(OntarioAlias.CONST_LANG_FRENCH)) && (resourcesFrench != null)) {
            resourcesOut = resourcesFrench;
            locForLang = locForFrench;
        }
        locVarMap.put(OrganizationLocale.class.getName(), locForLang);

        return resourcesOut;
    }

    /**
     * Get the most recent enrolment record
     * - returns PlainDate if dateOnly, enrolment record if not dateOnly
     * - null values returned for std oid if Entry record not found for any reason.
     *
     * @param stdOids the std oids
     * @param selectionStd the selection std
     * @param asOfDate - optional (null if not sent)
     * @param broker the broker
     * @param dateOnly the date only
     * @return Map<String, Object>
     */
    public static Map<String, Object> getEnrMostRecentEntry(List<String> stdOids,
                                                            Selection selectionStd,
                                                            PlainDate asOfDate,
                                                            X2Broker broker,
                                                            boolean dateOnly) {
        /*
         * Load BSIDs for the board
         */
        Map<String, ReferenceCode> schoolBsidsForBoardMap =
                getReferenceCodesMap(broker, OntarioAlias.REF_OID_BSID_SCHOOLS);

        /*
         * Get and process enrolment records
         */
        // create enrolment criteria
        X2Criteria enrCriteria = new X2Criteria();

        // add condition for entry or withdrawal records only
        Collection<String> enrTypeForCrit = new ArrayList<String>(
                Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));
        enrCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrTypeForCrit);

        // add student condition
        if ((stdOids != null) && (!stdOids.isEmpty())) {
            // based on list of student oids
            enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, stdOids);
        } else if (selectionStd != null) {
            // based on selection of student oids
            SubQuery subQUery = getSubQueryFromSelection(StudentEnrollment.COL_STUDENT_OID, selectionStd);
            enrCriteria.addExists(subQUery);
        } else {
            enrCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, OntarioAlias.CONST_EMPTY);
        }

        // add as of date condition
        if (asOfDate != null) {
            enrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, asOfDate);
        }

        // get student enrolment records in descending order
        QueryByCriteria enrQuery = new QueryByCriteria(StudentEnrollment.class, enrCriteria);
        enrQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        enrQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);

        // load map of enrolment info fields to std oid
        Map<String, Object> enrMostRecentEntry = new HashMap<String, Object>();
        String stdOidPrev = OntarioAlias.CONST_EMPTY;
        StudentEnrollment stdEntryMostRecentEnr = null;
        PlainDate stdEntryMostRecentDt = null;

        try (QueryIterator enrIterator = broker.getIteratorByQuery(enrQuery)) {
            while (enrIterator.hasNext()) {
                StudentEnrollment enr = (StudentEnrollment) enrIterator.next();
                String stdOid = enr.getStudentOid();
                String enrType = enr.getEnrollmentType();
                String enrBsid = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_ENTRY_DEMIT_BSID);

                // save values for prev student
                if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOidPrev.equals(stdOid))) {
                    if (dateOnly) {
                        enrMostRecentEntry.put(stdOidPrev, stdEntryMostRecentDt);
                    } else {
                        enrMostRecentEntry.put(stdOidPrev, stdEntryMostRecentEnr);
                    }

                    stdEntryMostRecentEnr = null;
                    stdEntryMostRecentDt = null;
                }
                stdOidPrev = stdOid;

                // if entry set entry info and student is withdrawn or not arrived
                if ((enrType.equals(StudentEnrollment.ENTRY)) && (stdEntryMostRecentEnr == null)) {
                    stdEntryMostRecentEnr = enr;
                    stdEntryMostRecentDt = enr.getEnrollmentDate();
                } else
                    // if withdraw process withdrawal as per business rules
                    if (enrType.equals(StudentEnrollment.WITHDRAWAL)) {
                        // If the School BSID field (all-enr-EntryDemitBsid) on the Student Enrolment
                        // table is empty or null then exit the loop.
                        if ((enrBsid == null)
                                // We need to find out if this withdrawal record indicates that the
                                // student left the board.
                                // Look up the value from the School BSID field in the BSID Schools
                                // reference table.
                                // If the record is not found then exit the loop.
                                || (!schoolBsidsForBoardMap.keySet().contains(enrBsid))) {
                            stdEntryMostRecentEnr = null;
                            stdEntryMostRecentDt = null;

                            continue;
                        }

                        // for cases where enrBsid is in schools for board (not processed above)
                        // get dependent code for BSID (next board student attends)
                        // If the Dependent code field from the BSID - Schools reference table does not
                        // equal the Identifier field, then exit the loop.
                        ReferenceCode rcdNextBsid = schoolBsidsForBoardMap.get(enrBsid);
                        String enrBsidNextDependentCode = rcdNextBsid.getDependencyCode();
                        if ((enrBsidNextDependentCode != null)
                                && (!enrBsidNextDependentCode.equals(enr.getSchool().getOrganization1().getId()))) {
                            stdEntryMostRecentEnr = null;
                            stdEntryMostRecentDt = null;

                            continue;
                        }

                        // else If the Dependent code field from the BSID - Schools reference table
                        // equals the Identifier field from the Organizations table then repeat the
                        // loop.
                        // so read next enrolment record without withdrawing the student
                    }
            }
        }

        // save values for last student
        if (!StringUtils.isEmpty(stdOidPrev)) {
            if (dateOnly) {
                enrMostRecentEntry.put(stdOidPrev, stdEntryMostRecentDt);
            } else {
                enrMostRecentEntry.put(stdOidPrev, stdEntryMostRecentEnr);
            }
        }

        return enrMostRecentEntry;
    }

    /**
     * Gets the student OP indicator (Y/N), enrolment register type (FT/PT), counts
     * by enrolment month/gender
     *
     * - This gets register type changes from the FTE Monthly (UDD) table instead of FTE Records
     * (UDC)
     *
     * - Returns map of indicator to student oid + CONST_ENR_OP_IND_SUFFIX
     * - And enrolment register type to student oid + CONST_ENR_REG_TYP_SUFFIX +
     * CONST_STD_FTE_REG_OCT
     * - And enrolment register type to student oid + CONST_ENR_REG_TYP_SUFFIX +
     * CONST_STD_FTE_REG_MAR
     * - And first date of enrolment in the date range to student oid + CONST_ENR_DT_FIRST
     *
     * Takes list of student oids OR student oid selection object.
     *
     * Get fields based on student enrolment records and Student FTE Monthly (UDD)
     * register changes.
     *
     * @param stdOids the std oids
     * @param selectionStd the selection std
     * @param fromDate - optional (null if not sent)
     * @param asOfDate - optional (null if not sent)
     * @param dictionary the dictionary
     * @param broker the broker
     * @param locale the locale
     * @return Map<String, String>
     */
    public static Map<String, String> getEnrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit(List<String> stdOids,
                                                                                       Selection selectionStd,
                                                                                       PlainDate fromDate,
                                                                                       PlainDate asOfDate,
                                                                                       DataDictionary dictionary,
                                                                                       X2Broker broker,
                                                                                       Locale locale) {
        Calendar calendar = Calendar.getInstance(locale);
        final String DATE_DB_PATTERN = "yyyy-MM-dd";

        /*
         * Get and process UDD records for student reg changes
         */
        ExtendedDataDictionary ddxStudentFte = broker.getBeanByOid(ExtendedDataDictionary.class,
                OntarioAlias.EXT_OID_STD_FTE);
        DataDictionary dictionaryStudentFte = DataDictionary.getDistrictDictionary(ddxStudentFte,
                broker.getPersistenceKey());

        String fieldStdFteUddDate = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_DATE, false,
                dictionaryStudentFte);
        String fieldStdFteUddRegister = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_REG, false,
                dictionaryStudentFte);

        // define empty udd info map (here in case no Student fte extension defined)
        Map<String, Map<Integer, String>> regTypeToSklMonthByStdOid = new HashMap<String, Map<Integer, String>>();

        // process student fte records if defined
        if (fieldStdFteUddDate != null) {
            // create criteria
            X2Criteria uddCriteria = new X2Criteria();

            // add student condition
            if ((stdOids != null) && (!stdOids.isEmpty())) {
                // based on list of student oids
                uddCriteria.addIn(UserDefinedTableD.COL_STUDENT_OID, stdOids);
            } else if (selectionStd != null) {
                // based on selection of student oids
                SubQuery subQUery = getSubQueryFromSelection(UserDefinedTableD.COL_STUDENT_OID, selectionStd);
                uddCriteria.addExists(subQUery);
            } else {
                uddCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, OntarioAlias.CONST_EMPTY);
            }

            // make sure dates are not null
            uddCriteria.addNotNull(fieldStdFteUddDate);

            // dates were invalid for some udd records so checked in code

            // create query and sort by student oid, month
            QueryByCriteria uddQuery = new QueryByCriteria(UserDefinedTableD.class, uddCriteria);
            uddQuery.addOrderByAscending(UserDefinedTableD.COL_STUDENT_OID);
            uddQuery.addOrderByAscending(fieldStdFteUddDate);

            // query iterator for loop through records
            QueryIterator<X2BaseBean> udds = broker.getIteratorByQuery(uddQuery);

            // create and initialize student level variables
            String stdOidPrevUdd = null;
            String stdUddOct = OntarioAlias.CONST_EMPTY;
            String stdUddMar = OntarioAlias.CONST_EMPTY;

            // loop through records
            // declare variable used in catch exception
            UserDefinedTableD udd = null;

            try {
                while (udds.hasNext()) {
                    // Get UserDefinedTableD
                    udd = (UserDefinedTableD) udds.next();

                    // get variables
                    String stdOid = udd.getStudentOid();
                    PlainDate regChgDate = null;
                    String dateDb = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_DATE,
                            dictionaryStudentFte);
                    regChgDate = getDateIfValid(dateDb, DATE_DB_PATTERN);
                    String reg = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_REG,
                            dictionaryStudentFte);

                    // get or create output map for student
                    if ((!StringUtils.isEmpty(stdOidPrevUdd)) && (!stdOid.equals(stdOidPrevUdd))) {
                        Map<Integer, String> regTypeToSklMonthForStdOid = new HashMap<Integer, String>();
                        regTypeToSklMonthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_OCT, stdUddOct);
                        regTypeToSklMonthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_MAR, stdUddMar);
                        regTypeToSklMonthByStdOid.put(stdOidPrevUdd, regTypeToSklMonthForStdOid);

                    }
                    stdOidPrevUdd = stdOid;

                    // continue if dates are in range, checked here as UDD dates were invalid in
                    // some cases
                    if ((regChgDate != null) && (!regChgDate.before(fromDate)) && (!regChgDate.after(asOfDate))) {
                        // process udd
                        // set reg type
                        calendar.setTime(regChgDate);
                        Integer regChgCalMonth = Integer.valueOf(calendar.get(Calendar.MONTH) + 1);
                        Integer regChgSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(regChgCalMonth);
                        if (regChgSklMonth.intValue() <= OntarioAlias.CONST_SKL_MONTH_OCT) {
                            stdUddOct = reg;
                            stdUddMar = reg;
                        } else if (regChgSklMonth.intValue() <= OntarioAlias.CONST_SKL_MONTH_MAR) {
                            stdUddMar = reg;
                        }
                    }
                }
            } catch (Exception e) {
                // Catch the exception into the report output message.
                AppGlobals.getLog().log(Level.WARNING,
                        "Error reading UDD records for enrolment register changes: " + e);
            } finally {
                udds.close();
            }

            // process last student
            if (!StringUtils.isEmpty(stdOidPrevUdd)) {
                Map<Integer, String> regTypeToSklMonthForStdOid = new HashMap<Integer, String>();
                regTypeToSklMonthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_OCT, stdUddOct);
                regTypeToSklMonthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_MAR, stdUddMar);
                regTypeToSklMonthByStdOid.put(stdOidPrevUdd, regTypeToSklMonthForStdOid);
            }
        }

        /*
         * Get and process enrolment records -will only get reg type values from udd
         * records
         */
        // create enrolment criteria
        X2Criteria enrCriteria = new X2Criteria();

        // add condition for entry records only
        enrCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, OntarioAlias.CONST_ENR_TYPE_ENTRY);

        // add student condition
        if ((stdOids != null) && (!stdOids.isEmpty())) {
            // based on list of student oids
            enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, stdOids);
        } else if (selectionStd != null) {
            // based on selection of student oids
            SubQuery subQUery = getSubQueryFromSelection(StudentEnrollment.COL_STUDENT_OID, selectionStd);
            enrCriteria.addExists(subQUery);
        } else {
            enrCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, OntarioAlias.CONST_EMPTY);
        }

        // add from date condition
        if (fromDate != null) {
            enrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, fromDate);
        }

        // add as of date condition
        if (asOfDate != null) {
            enrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, asOfDate);
        }

        // get bean paths for udfs
        String fieldEnrOp = getBeanPathFromAlias(OntarioAlias.ALIAS_ENR_OTHER_PUPIL, false, dictionary);
        String fieldEnrRegister = getBeanPathFromAlias(OntarioAlias.ALIAS_ENR_REGISTER, false, dictionary);
        String fieldEnrOpPayer = getBeanPathFromAlias(OntarioAlias.ALIAS_ENR_FUNDING_PAYER, false, dictionary);

        // get student enrolment records in descending order
        String[] columns = new String[] {StudentEnrollment.COL_STUDENT_OID, fieldEnrOp, fieldEnrRegister,
                StudentEnrollment.COL_ENROLLMENT_DATE, fieldEnrOpPayer};
        ColumnQuery enrQuery = new ColumnQuery(StudentEnrollment.class, columns, enrCriteria);
        enrQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        enrQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);

        // load map of enrolment info fields to std oid
        Map<String, String> enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit = new HashMap<String, String>();
        String stdOidPrevEnr = OntarioAlias.CONST_EMPTY;
        PlainDate stdEnrDtFirst = null;
        String stdEnrOtherPupilCode = OntarioAlias.CONST_EMPTY;
        String stdEnrOtherPupilPayer = OntarioAlias.CONST_EMPTY;
        Map<Integer, String> regTypeToSklMonthForStdOid = null;

        try (ReportQueryIterator enrIterator = broker.getReportQueryIteratorByQuery(enrQuery)) {
            while (enrIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) enrIterator.next();
                String stdOid = data[0].toString();
                String enrOtherPupilCode = OntarioAlias.CONST_EMPTY;
                if (data[1] != null) {
                    enrOtherPupilCode = data[1].toString();
                }
                String enrOtherPupilPayer = OntarioAlias.CONST_EMPTY;
                if (data[4] != null) {
                    enrOtherPupilPayer = data[4].toString();
                }
                Timestamp enrDateTs = (Timestamp) data[3];
                PlainDate enrDate = PlainDate.fromString(enrDateTs.toString().substring(0, 10));

                // if new stdOid, write enrolment info for stdOid
                if ((!StringUtils.isEmpty(stdOidPrevEnr)) && (stdEnrDtFirst == null)) {
                    // write default enrolment date from date if not found in range for prev student
                    if ((!StringUtils.isEmpty(stdOidPrevEnr)) && (stdEnrDtFirst == null)) {
                        enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit
                        .put(stdOidPrevEnr + OntarioAlias.CONST_ENR_DT_FIRST, fromDate.toString());
                    }
                }
                if (!stdOidPrevEnr.equals(stdOid)) {
                    // write other pupil indicator if enrolment found in range, default empty
                    String stdOtherPupilInd = OntarioAlias.CONST_EMPTY;
                    if ((!StringUtils.isEmpty(stdEnrOtherPupilCode))
                            && (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(stdEnrOtherPupilCode))) {
                        stdOtherPupilInd = OntarioAlias.CONST_ENR_OP_TXT;
                    }
                    enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit
                    .put(stdOidPrevEnr + OntarioAlias.CONST_ENR_OP_IND_SUFFIX, stdOtherPupilInd);
                    enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit
                    .put(stdOidPrevEnr + OntarioAlias.CONST_ENR_OP_PAYER_SUFFIX, stdEnrOtherPupilPayer);

                    // for new student - reset student variables
                    stdEnrDtFirst = null;
                    stdEnrOtherPupilCode = OntarioAlias.CONST_EMPTY;
                    stdEnrOtherPupilPayer = OntarioAlias.CONST_EMPTY;
                    regTypeToSklMonthForStdOid = null;
                }
                stdOidPrevEnr = stdOid;

                // set op indicator, rules maybe changed later
                stdEnrOtherPupilCode = enrOtherPupilCode;
                stdEnrOtherPupilPayer = enrOtherPupilPayer;

                // check and write first enrolment date for each record
                if ((stdEnrDtFirst == null) || (stdEnrDtFirst.compareTo(enrDate) > 0)) {
                    enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit.put(stdOid + OntarioAlias.CONST_ENR_DT_FIRST,
                            enrDate.toString());
                    stdEnrDtFirst = enrDate;
                }

                // set reg type from udd
                if (regTypeToSklMonthForStdOid == null) {
                    if (regTypeToSklMonthByStdOid.get(stdOid) != null) {
                        regTypeToSklMonthForStdOid = regTypeToSklMonthByStdOid.get(stdOid);
                        // if in UDD would be after enrolment so get from there first
                        if (regTypeToSklMonthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_OCT) != null) {
                            String stdEnrRegisterOct = regTypeToSklMonthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_OCT);

                            enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit.put(stdOidPrevEnr
                                    + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_OCT,
                                    stdEnrRegisterOct);
                        }
                        if (regTypeToSklMonthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_MAR) != null) {
                            String stdEnrRegisterMar = regTypeToSklMonthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_MAR);

                            enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit.put(stdOidPrevEnr
                                    + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_MAR,
                                    stdEnrRegisterMar);
                        }
                    }
                }
            }
        }

        // for last stdOid, write enrolment info for stdOid
        if ((!StringUtils.isEmpty(stdOidPrevEnr)) && (stdEnrDtFirst == null)) {
            // write default enrolment date from date if not found in range for prev student
            if ((!StringUtils.isEmpty(stdOidPrevEnr)) && (stdEnrDtFirst == null)) {
                enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit.put(stdOidPrevEnr + OntarioAlias.CONST_ENR_DT_FIRST,
                        fromDate.toString());
            }
        }
        if (!StringUtils.isEmpty(stdOidPrevEnr)) {
            // write other pupil indicator if enrolment found in range, default empty
            String stdOtherPupilInd = OntarioAlias.CONST_EMPTY;
            if ((!StringUtils.isEmpty(stdEnrOtherPupilCode))
                    && (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(stdEnrOtherPupilCode))) {
                stdOtherPupilInd = OntarioAlias.CONST_ENR_OP_TXT;
            }
            enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit.put(stdOidPrevEnr + OntarioAlias.CONST_ENR_OP_IND_SUFFIX,
                    stdOtherPupilInd);
            enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit
            .put(stdOidPrevEnr + OntarioAlias.CONST_ENR_OP_PAYER_SUFFIX, stdEnrOtherPupilPayer);
        }

        return enrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit;
    }

    /**
     * Gets the student OP indicator
     *
     * Returns map of indicator to school month to school oid to student oid
     *
     * Takes list of student oids OR student oid selection object.
     *
     * Get fields based on student school association records
     *
     * @param stdOids the std oids
     * @param selectionStd the selection std
     * @param ctx the ctx
     * @param dictionary the dictionary
     * @param broker the broker
     * @param locale the locale
     * @return Map<String, String>
     */
    public static Map<String, Map<String, Map<Integer, String>>> getSskOpIndToSklMthToSklOidToStdOidMap(List<String> stdOids,
                                                                                                        Selection selectionStd,
                                                                                                        DistrictSchoolYearContext ctx,
                                                                                                        DataDictionary dictionary,
                                                                                                        X2Broker broker,
                                                                                                        Locale locale) {
        Calendar calendar = Calendar.getInstance(locale);

        /*
         * Get and process student school association records
         */
        // create criteria
        X2Criteria sskCriteria = new X2Criteria();

        // add condition for school year context
        sskCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, ctx.getOid());

        // add condition for secondary records only
        sskCriteria.addEqualTo(StudentSchool.COL_TYPE, StudentSchool.SECONDARY);

        // add student condition
        if ((stdOids != null) && (!stdOids.isEmpty())) {
            // based on list of student oids
            sskCriteria.addIn(StudentSchool.COL_STUDENT_OID, stdOids);
        } else if (selectionStd != null) {
            // based on selection of student oids
            SubQuery subQUery = getSubQueryFromSelection(StudentSchool.COL_STUDENT_OID, selectionStd);
            sskCriteria.addExists(subQUery);
        } else {
            sskCriteria.addEqualTo(StudentSchool.COL_STUDENT_OID, OntarioAlias.CONST_EMPTY);
        }

        // get bean paths for udfs
        String fieldBoardResidentStatus =
                getBeanPathFromAlias(OntarioAlias.ALIAS_SSK_BOARD_RESIDENT_STATUS, false, dictionary);

        // get in descendign order
        String[] columns = new String[] {StudentSchool.COL_STUDENT_OID, StudentSchool.COL_SCHOOL_OID,
                StudentSchool.COL_START_DATE, StudentSchool.COL_END_DATE, fieldBoardResidentStatus};
        ColumnQuery sskQuery = new ColumnQuery(StudentSchool.class, columns, sskCriteria);
        sskQuery.addOrderByAscending(StudentSchool.COL_STUDENT_OID);
        sskQuery.addOrderByAscending(StudentSchool.COL_START_DATE);

        // load map to std oid
        Map<String, Map<String, Map<Integer, String>>> sskOpIndToSklMthToSklOidToStdOidMap =
                new HashMap<String, Map<String, Map<Integer, String>>>();
        String stdOidPrev = OntarioAlias.CONST_EMPTY;
        Map<String, Map<Integer, String>> sskOpIndToSklMthToSklOidForStdOid =
                new HashMap<String, Map<Integer, String>>();

        try (ReportQueryIterator sskIterator = broker.getReportQueryIteratorByQuery(sskQuery)) {
            while (sskIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) sskIterator.next();
                String stdOid = data[0].toString();
                String sskSklOid = data[1].toString();
                Timestamp sskStartDateTs = (Timestamp) data[2];
                PlainDate sskStartDate = PlainDate.fromString(sskStartDateTs.toString().substring(0, 10));
                Timestamp sskEndDateTs = (Timestamp) data[3];
                PlainDate sskEndDate = PlainDate.fromString(sskEndDateTs.toString().substring(0, 10));

                String sskOtherPupilCode = OntarioAlias.CONST_EMPTY;
                if (data[4] != null) {
                    sskOtherPupilCode = data[4].toString();
                }

                // if new stdOid, write info for stdOid
                if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOidPrev.equals(stdOid))) {
                    // write to outputmap
                    sskOpIndToSklMthToSklOidToStdOidMap.put(stdOidPrev, sskOpIndToSklMthToSklOidForStdOid);

                    // for new student - reset student variables
                    sskOpIndToSklMthToSklOidForStdOid = new HashMap<String, Map<Integer, String>>();
                }
                stdOidPrev = stdOid;

                // get school months for current record
                calendar.setTime(sskStartDate);
                Integer sskStartCalMonth = Integer.valueOf(calendar.get(Calendar.MONTH) + 1);
                Integer sskStartSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(sskStartCalMonth);
                Integer sskEndCalMonth = Integer.valueOf(calendar.get(Calendar.MONTH) + 1);
                Integer sskEndSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(sskEndCalMonth);

                // initialize and get any existing info for student/school
                Map<Integer, String> sskOpIndToSklMthForSklOid = new HashMap<Integer, String>();
                if (sskOpIndToSklMthToSklOidForStdOid.get(sskSklOid) != null) {
                    sskOpIndToSklMthForSklOid = sskOpIndToSklMthToSklOidForStdOid.get(sskSklOid);
                }

                // save op indicator for each school month in range for the record
                // overlay if exists and populated as is ordered by start date
                for (int sklMth = sskStartSklMonth.intValue(); sklMth <= sskEndSklMonth; sklMth++) {
                    String sskOpIndForSklMth = null;// null if override not set
                    if (!StringUtils.isEmpty(sskOtherPupilCode)) {
                        if (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(sskOtherPupilCode)) {
                            sskOpIndForSklMth = OntarioAlias.CONST_ENR_OP_TXT;
                        } else {
                            sskOpIndForSklMth = OntarioAlias.CONST_EMPTY;
                        }
                    }
                    sskOpIndToSklMthForSklOid.put(Integer.valueOf(sklMth), sskOpIndForSklMth);
                }

                // save info for student/school
                sskOpIndToSklMthToSklOidForStdOid.put(sskSklOid, sskOpIndToSklMthForSklOid);
            }

            // for last stdOid, write enrolment info for stdOid
            if (!StringUtils.isEmpty(stdOidPrev)) {
                // write to outputmap
                sskOpIndToSklMthToSklOidToStdOidMap.put(stdOidPrev, sskOpIndToSklMthToSklOidForStdOid);
            }
        }

        return sskOpIndToSklMthToSklOidToStdOidMap;
    }

    /**
     * Create a selection object to query handle Many oidList - Copied from core
     * ParameterSelectionHandler as that method is private.
     *
     * @param broker X2Broker
     * @param oidList Collection<String>
     * @return Selection Object
     */
    public static Selection saveSelectionQuery(X2Broker broker, Collection<String> oidList) {
        Selection selection = X2BaseBean.newInstance(Selection.class, broker.getPersistenceKey());
        Iterator<String> selectedOids = oidList.iterator();
        while (selectedOids.hasNext()) {
            SelectionObject selectedObject = X2BaseBean.newInstance(SelectionObject.class, broker.getPersistenceKey());
            selectedObject.setObjectOid(selectedOids.next());
            selection.addToSelectionObjects(selectedObject);
        }
        selection.setTimestamp(System.currentTimeMillis());
        broker.saveBean(selection);
        return selection;
    }

    /**
     * Delete selection and associated selection objects.
     *
     * @param broker X2Broker
     * @param selection Selection
     * @return int selectionObjects deleted
     */
    public static int deleteSelectionQuery(X2Broker broker, Selection selection) {
        int selectionObjectsDeleted = 0;
        if (selection != null) {
            String selectionOid = selection.getOid();
            // delete selection objects
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selectionOid);
            BeanQuery query = new BeanQuery(SelectionObject.class, criteria);
            selectionObjectsDeleted = broker.deleteByQuery(query);
            // delete selection
            broker.deleteBeanByOid(Selection.class, selectionOid);
        }
        return selectionObjectsDeleted;
    }

    /**
     * Add condition to criteria based on selection (if exists) or list Will not
     * meet condition if both are null.
     *
     * @param criteriaIn - Criteria
     * @param listValues - Collection
     * @param selection - Selection bean
     * @param beanPath target bean oid column path
     * @return Criteria - updated
     */
    public static Criteria addInListOrSelectionCondition(Criteria criteriaIn,
                                                         Collection<String> listValues,
                                                         Selection selection,
                                                         String beanPath) {
        Criteria criteriaOut = criteriaIn.copy(true, true, true);
        if (selection != null) {
            // add to criteria to add condition for selection
            criteriaOut.addExists(OntarioToolHelper.getSubQueryFromSelection(beanPath, selection));
        } else if (listValues != null) {
            // add to criteria to add condition for collection
            criteriaOut.addIn(beanPath, listValues);
        } else {
            // add condition for empty list - in order to not match any if empty
            listValues = new ArrayList<String>();
            criteriaOut.addIn(beanPath, listValues);
        }
        return criteriaOut;
    }

    /**
     * Returns sub query of bean oids.
     *
     * @param beanPath target bean oid column path
     * @param selection the selection
     * @return sub query
     */
    public static SubQuery getSubQueryFromSelection(String beanPath, Selection selection) {
        SubQuery subQuery;

        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + beanPath);

        subQuery = new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria);

        return subQuery;
    }

    /**
     * Add staff view criteria for reports - return student oids.
     *
     * @param userData the user data
     * @param currentCriteria the current criteria
     * @param broker the broker
     * @return stdOids - will be updated
     */
    public static List<String> addStaffViewSelectStdOids(UserDataContainer userData,
                                                         X2Criteria currentCriteria,
                                                         X2Broker broker) {
        List<String> stdOids = new ArrayList<String>();
        ScoreGrid grid = (ScoreGrid) userData.getCurrentGrid();
        MasterSchedule section = grid.getSection();
        Collection<Student> students = grid.getStudents();
        String currentGridStr = userData.getCurrentGrid().toString();

        // if from grade book class list, run for students for all classes on page
        if (userData.getCurrentNode().getId().equalsIgnoreCase(OntarioAlias.CONST_STF_VW_NAV_CLASS_LIST)) {
            stdOids = getStudentsByTeacher(currentCriteria, broker);
        } else
            // if from grade book class list, run for students from one class student scores
            if (userData.getCurrentNode().getId().equalsIgnoreCase(OntarioAlias.CONST_STF_VW_NAV_CLASS_LIST_IN)) {

                for (Student s : students) {
                    stdOids.add(s.getOid());
                }
            } else
                // if from grade book class list, run for student selected from one class
                // student scores
                if (userData.getCurrentNode().getId().equalsIgnoreCase(OntarioAlias.CONST_STF_VW_NAV_CLASS_LIST_IN_DTL)) {
                    // TODO need to review after
                    String mstOid = OntarioAlias.CONST_EMPTY;
                    if (currentGridStr.contains("MasterSchedule")) {
                        String mstOidFirst = currentGridStr.split("MasterSchedule ")[1];
                        mstOid = mstOidFirst.split(" ")[0];
                    }
                    if (!StringUtils.isEmpty(mstOid)) {
                        section = broker.getBeanByOid(MasterSchedule.class, mstOid);
                        stdOids = getStudentsBySection(section, broker);
                    }
                }

        return stdOids;
    }

    /**
     * Returns a list of all students for a teacher across it's sections from the
     * Staff View --> Class list.
     *
     * @param currentCriteria the current criteria
     * @param broker the broker
     * @return List
     */
    protected static List<String> getStudentsByTeacher(X2Criteria currentCriteria, X2Broker broker) {
        List<String> stdOidList = new LinkedList<String>();
        X2Criteria criteria = currentCriteria;
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);
        QueryIterator iterator = broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                ScheduleTeacher scheduleTeacher = (ScheduleTeacher) iterator.next();
                for (StudentSection stdSection : scheduleTeacher.getSection().getStudentSections()) {
                    String stdOid = stdSection.getStudent().getOid();
                    if (!stdOidList.contains(stdOid)) {
                        stdOidList.add(stdSection.getStudent().getOid());
                    }
                }
            }
        } finally {
            iterator.close();
        }

        return stdOidList;
    }

    /**
     * Returns a list of all students for a teacher across it's sections from the
     * Staff View --> Class list.
     *
     * @param section the section
     * @param broker the broker
     * @return List
     */
    protected static List<String> getStudentsBySection(MasterSchedule section, X2Broker broker) {
        List<String> stdOidList = new LinkedList<String>();
        for (StudentSection stdSection : section.getStudentSections()) {
            String stdOid = stdSection.getStudent().getOid();
            if (!stdOidList.contains(stdOid)) {
                stdOidList.add(stdSection.getStudent().getOid());
            }
        }

        return stdOidList;
    }

    /**
     * Gets a map of course description (Ministry) to ministry course codes.
     *
     * @param broker the broker
     * @return Map (desc to code)
     */
    public static Map<String, String> getCourseDescMinistryMap(X2Broker broker) {
        return getLocalizedCourseDescMinistryMap(broker, null);
    }

    /**
     * Gets a map of course description (Ministry) to ministry course codes for a
     * specific language. When College Course Title is present (DDX defined field)
     * this value will be used instead of message resource.
     *
     * @param broker the broker
     * @param language the language
     * @return Map (desc to code)
     */
    public static Map<String, String> getLocalizedCourseDescMinistryMap(X2Broker broker, String language) {
        Map<String, String> crsDescMinistryMap = new HashMap();
        X2Criteria rcdCriteria = new X2Criteria();
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, OntarioAlias.REF_OID_CRS_MINISTRY_CODES);
        Boolean isLangaugeProvided = true;
        if (StringUtils.isBlank(language)) {
            // If the value is NOT passed, then we simply default back to English
            language = "English";
            isLangaugeProvided = false;
        }
        ExtendedDataDictionary onSisMinistryCourseCodesDDX =
                broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_ONSIS_MINISTRY_COURSE_CODE);
        DataDictionaryField collegeCourseTitleField =
                DataDictionary.getDistrictDictionary(onSisMinistryCourseCodesDDX, broker.getPersistenceKey())
                .findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_EXT_COLLEGE_COURSE_TITLE);

        String[] columns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_DESCRIPTION, X2BaseBean.COL_OID,
                collegeCourseTitleField.getJavaName()};
        ColumnQuery rcdQuery = new ColumnQuery(ReferenceCode.class, columns, rcdCriteria);

        try (ReportQueryIterator rcdIterator = broker.getReportQueryIteratorByQuery(rcdQuery)) {

            while (rcdIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) rcdIterator.next();
                String code = data[0].toString();
                String collegeCourseTitle = StringUtils.isBlank((String) data[3]) ? null : (String) data[3];
                // We start with the actual rcdDescription
                String desc = data[1].toString();
                if (collegeCourseTitle != null) {
                    desc = collegeCourseTitle;
                }
                if (isLangaugeProvided) {
                    for (LocalizationResource locResource : LocalizationCache
                            .getLocalizedResources(broker.getPersistenceKey(), (String) data[2])) {
                        String[] localeAttributes = locResource.getLocale().split("_");
                        Locale resourceLocale = new Locale(localeAttributes[0], localeAttributes[1]);
                        if (resourceLocale.getDisplayLanguage().contains(language)) {
                            desc = locResource.getValue();
                        }
                    }
                }
                crsDescMinistryMap.put(code, desc);

            }
        }

        return crsDescMinistryMap;
    }

    /**
     * Gets a map of reference table codes to reference table descriptions for a
     * specific language (example: "French").
     *
     * @param broker X2Broker
     * @param rtbOid String
     * @param language String
     *
     * @return Map
     */
    public static Map<String, String> getLocalizedRefDescMap(X2Broker broker, String rtbOid, String language) {
        Map<String, String> refMap = new HashMap();
        X2Criteria rcdCriteria = new X2Criteria();
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        Boolean isLanguageProvided = true;
        if (StringUtils.isBlank(language)) {
            // If the value is NOT passed, then we simply default back to English
            language = "English";
            isLanguageProvided = false;
        }

        String[] columns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_DESCRIPTION, X2BaseBean.COL_OID};
        ColumnQuery rcdQuery = new ColumnQuery(ReferenceCode.class, columns, rcdCriteria);

        try (ReportQueryIterator rcdIterator = broker.getReportQueryIteratorByQuery(rcdQuery)) {

            while (rcdIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) rcdIterator.next();
                String code = data[0].toString();
                // We start with the actual rcdDescription
                String desc = data[1] != null ? data[1].toString() : (String) data[0];
                if (isLanguageProvided) {
                    for (LocalizationResource locResource : LocalizationCache
                            .getLocalizedResources(broker.getPersistenceKey(), (String) data[2])) {

                        String[] localeAttributes = locResource.getLocale().split("_");
                        Locale resourceLocale = new Locale(localeAttributes[0], localeAttributes[1]);
                        if (resourceLocale.getDisplayLanguage().contains(language)
                                && !StringUtils.isBlank(locResource.getValue())) {
                            desc = locResource.getValue();
                        }
                    }

                }
                refMap.put(code, desc);

            }
        }

        return refMap;
    }

    /**
     * Gets the high school grade level based on course code.
     *
     * @param courseNumber the course number
     * @return String
     */
    public static String getCourseHsGradeLevel(String courseNumber) {
        String courseGradeLevelOnt = courseNumber.substring(OntarioAlias.CONST_CRS_GRD_LVL_POS - 1,
                OntarioAlias.CONST_CRS_GRD_LVL_POS);

        String hsGradeLevel = OntarioAlias.CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.get(courseGradeLevelOnt);

        return hsGradeLevel;
    }

    /**
     * Gets the high school grade level based on cskCourse.
     *
     * @param cskCourse the csk course
     * @return String
     */
    public static String getCourseHsGradeLevel(SchoolCourse cskCourse) {
        String courseGradeLevelOnt = cskCourse.getNumber().substring(OntarioAlias.CONST_CRS_GRD_LVL_POS - 1,
                OntarioAlias.CONST_CRS_GRD_LVL_POS);

        String hsGradeLevel = "00";
        if (StringUtils.isNumeric(courseGradeLevelOnt)) {
            hsGradeLevel = OntarioAlias.CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.get(courseGradeLevelOnt);
        } else {
            hsGradeLevel = cskCourse.getGradeLevel();
        }

        return hsGradeLevel;
    }

    /**
     * Checks if grade level is for grade 9 or 10 Input is converted hs grade level
     * (to 09 or 10).
     *
     * @param gradeLevel the grade level
     * @return boolean
     */
    public static boolean isGradeLevelIn0910(String gradeLevel) {
        List<String> grdLvl0910List = new ArrayList<String>(Arrays.asList("09", "10"));

        if (grdLvl0910List.contains(gradeLevel)) {
            return true;
        }

        // else -
        return false;
    }

    /**
     * Checks if grade level is for grade 11 or 12 Input is converted course grade
     * level (to 11 or 12).
     *
     * @param gradeLevel the grade level
     * @return boolean
     */
    public static boolean isGradeLevelIn1112(String gradeLevel) {
        List<String> grdLvl1112List = new ArrayList<String>(Arrays.asList("11", "12"));

        if (grdLvl1112List.contains(gradeLevel)) {
            return true;
        }

        // else -
        return false;
    }

    /**
     * determines the gradeLevel for specific language courses.
     *
     * @param courseNumber the course number
     * @param dateCompleted the date completed
     * @return String gradeLevel
     * @throws Exception the exception
     */
    public static String determineLanguageCrsGradeLevel(String courseNumber, String dateCompleted) throws Exception {
        String gradeLevel = "";

        Date compareDate = null;
        Date completedDate = null;
        Date compareDate2 = null;

        String dateCompare = "2015-08-31";
        String dateCompare2 = "2016-08-31";

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            compareDate = dateFormat.parse(dateCompare.toString());
            completedDate = dateFormat.parse(dateCompleted.toString());
            compareDate2 = dateFormat.parse(dateCompare2.toString());
        } catch (ParseException e) {
            AppGlobals.getLog().log(Level.WARNING, "determineLanguageCrsGradeLevel: " + e);
        }

        String fourthCharacter = Character.toString(courseNumber.charAt(3));
        String crsPrefix = StringUtils.substring(courseNumber, 3);
        if (compareDate != null && completedDate != null && completedDate.before(compareDate)) {
            if ("ESL".equals(crsPrefix) || "ELD".equals(crsPrefix) || courseNumber.toUpperCase().startsWith("LN")) {
                switch (fourthCharacter) {
                    case "A":
                        gradeLevel = "1";
                        break;
                    case "B":
                        gradeLevel = "2";
                        break;
                    case "C":
                        gradeLevel = "3";
                        break;
                    case "D":
                        gradeLevel = "4";
                        break;
                    case "E":
                        gradeLevel = "5";
                        break;
                }
            } else if (courseNumber.toUpperCase().startsWith("L")) {
                switch (fourthCharacter) {
                    case "A":
                        gradeLevel = "1";
                        break;
                    case "B":
                        gradeLevel = "2";
                        break;
                    case "C":
                        gradeLevel = "3";
                        break;
                    case "D":
                        gradeLevel = "4";
                        break;
                    case "E":
                        gradeLevel = "5";
                        break;
                }
            }
        } // Specific logic for year 2015-2016
        else if (compareDate2 != null && completedDate != null && completedDate.before(compareDate2)) {
            if ("ESL".equals(crsPrefix) || "ELD".equals(crsPrefix) || courseNumber.toUpperCase().startsWith("LN")) {
                switch (fourthCharacter) {
                    case "A":
                        gradeLevel = "1";
                        break;
                    case "B":
                        gradeLevel = "2";
                        break;
                    case "C":
                        gradeLevel = "3";
                        break;
                    case "D":
                        gradeLevel = "4";
                        break;
                    case "E":
                        gradeLevel = "5";
                        break;
                }
            } else if (courseNumber.toUpperCase().startsWith("L")) {
                switch (fourthCharacter) {
                    case "A":
                        gradeLevel = "1";
                        break;
                    case "B":
                        gradeLevel = "2";
                        break;
                    case "C":
                        gradeLevel = "3";
                        break;
                }
            }
        } else {
            if ("ESL".equals(crsPrefix) || "ELD".equals(crsPrefix) || courseNumber.toUpperCase().startsWith("LN")) {
                switch (fourthCharacter) {
                    case "A":
                        gradeLevel = "1";
                        break;
                    case "B":
                        gradeLevel = "2";
                        break;
                    case "C":
                        gradeLevel = "3";
                        break;
                    case "D":
                        gradeLevel = "4";
                        break;
                    case "E":
                        gradeLevel = "5";
                        break;
                }
            } else if (courseNumber.toUpperCase().startsWith("L")) {
                switch (fourthCharacter) {
                    case "B":
                        gradeLevel = "1";
                        break;
                    case "C":
                        gradeLevel = "2";
                        break;
                    case "D":
                        gradeLevel = "3";
                        break;
                }
            }
        }

        return gradeLevel;
    }

    /**
     * . loads reference code-description map for OnSIS Dual Credit
     *
     * @param refTableOid reference table oid
     * @param broker the broker
     * @return map
     */
    public static Map<String, String> loadOnsisDualCreditMap(String refTableOid, X2Broker broker) {
        String[] requestColumns = {ReferenceCode.COL_CODE, ReferenceCode.COL_DESCRIPTION};
        Map<String, String> onsisDualCreditMap = new HashMap<String, String>();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + X2BaseBean.COL_OID, refTableOid);
        criteria.addIn(ReferenceCode.COL_STATE_CODE, OntarioAlias.CONST_CRS_DELIVERY_TYPE_DUAL_CRED_LIST);

        ColumnQuery query = new ColumnQuery(ReferenceCode.class, requestColumns, criteria);

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String code = (String) row[0];
                String description = (String) row[1];

                onsisDualCreditMap.put(code, description);
            }
        }

        return onsisDualCreditMap;
    }

    /**
     * . loads reference code-description map for OnSIS Co-op
     *
     * @param refTableOid reference table oid
     * @param broker the broker
     * @return map
     */
    public static Map<String, String> loadOnsisCoopMap(String refTableOid, X2Broker broker) {
        String[] requestColumns = {ReferenceCode.COL_CODE, ReferenceCode.COL_DESCRIPTION};
        Map<String, String> onsisCoopMap = new HashMap<String, String>();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + X2BaseBean.COL_OID, refTableOid);
        criteria.addIn(ReferenceCode.COL_STATE_CODE, OntarioAlias.CONST_CRS_DELIVERY_TYPE_COOP_LIST);

        ColumnQuery query = new ColumnQuery(ReferenceCode.class, requestColumns, criteria);

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String code = (String) row[0];
                String description = (String) row[1];

                onsisCoopMap.put(code, description);
            }
        }

        return onsisCoopMap;
    }

    /**
     * determine the notes for a transcript record.
     *
     * @param trn the trn
     * @param onsisDualCreditMap the onsis dual credit map
     * @param onsisCoopMap the onsis coop map
     * @return the list
     */
    public static List<String> determineNotesValues(Transcript trn,
                                                    Map<String, String> onsisDualCreditMap,
                                                    Map<String, String> onsisCoopMap) {
        List<String> crsNotesList = new ArrayList<String>();
        Course crs = trn.getSchoolCourse().getCourse();
        SisOrganization org = crs.getOrganization1();
        String boardLang = (String) org.getFieldValueByAlias(OntarioAlias.ALIAS_ORG_LANGUAGE);
        SchoolCourse csk = trn.getSchoolCourse();
        MasterSchedule mst = trn.getMasterSchedule();
        String crsPrefix = StringUtils.substring(crs.getNumber(), 3);
        String crsDeliveryType = (String) trn
                .getFieldValueByAlias(OntarioAlias.ALIAS_TRN_COURSE_DELIVERY_TYPE_OVERRIDE);
        if (StringUtils.isEmpty(crsDeliveryType)) {
            crsDeliveryType = (String) csk.getFieldValueByAlias(OntarioAlias.ALIAS_CSK_COURSE_DELIVERY_TYPE);
        }
        if (StringUtils.isEmpty(crsDeliveryType)) {
            crsDeliveryType = (String) crs.getFieldValueByAlias(OntarioAlias.ALIAS_CRS_DELIVERY_TYPE);
        }
        String subsIndicator = (String) trn.getFieldValueByAlias(OntarioAlias.ALIAS_TRN_SUBSTITUTE_CREDIT_IND);
        if (!StringUtils.isBlank(subsIndicator)
                && (subsIndicator.equalsIgnoreCase("b") || subsIndicator.equalsIgnoreCase("p"))) {
            crsNotesList.add("X");
        }
        if (!StringUtils.isEmpty(crsDeliveryType)) {
            if (onsisDualCreditMap.containsKey(crsDeliveryType)) {
                crsNotesList.add("T");
            } else if (onsisCoopMap.containsKey(crsDeliveryType)) {
                crsNotesList.add("C");
            }
        }
        // add SHSM requirement met value to Notes
        String meetsShsmRequirement = (String) trn.getFieldValueByAlias(OntarioAlias.ALIAS_TRN_SHSM_REQUIREMENT);
        if (!StringUtils.isEmpty(meetsShsmRequirement) && meetsShsmRequirement.equals("1")) {
            crsNotesList.add("H");
        }

        // add special indicator notes
        String specialIndicator = (String) trn.getFieldValueByAlias(OntarioAlias.ALIAS_TRN_SPECIAL_INDICATOR);
        if (!StringUtils.isEmpty(specialIndicator) && specialIndicator.equals("1")) {
            crsNotesList.add("S");
        }

        // add Private school requirement met value to Notes
        String meetsPrivateSchoolRequirement = (String) trn
                .getFieldValueByAlias(OntarioAlias.ALIAS_TRN_PRIVATE_SCHOOL_REQUIREMENT);
        if (!StringUtils.isEmpty(meetsPrivateSchoolRequirement) && meetsPrivateSchoolRequirement.equals("1")) {
            crsNotesList.add("P");
        }

        // add Interdisciplinary Studies notes
        if ("IDC".equals(crsPrefix) || "IDP".equals(crsPrefix)) {
            crsNotesList.add("I");
        }

        // add M notes
        String modifiedCurriculumExp = (String) trn.getFieldValueByAlias(OntarioAlias.ALIAS_TRN_MODIFIED_EXPECTATIONS);
        if (!StringUtils.isEmpty(modifiedCurriculumExp) && modifiedCurriculumExp.equals("1")) {
            crsNotesList.add("M");
        }

        // add A notes
        String alternativeExpectation = (String) trn
                .getFieldValueByAlias(OntarioAlias.ALIAS_TRN_ALTERNATE_EXPECTATIONS);
        if (!StringUtils.isEmpty(alternativeExpectation) && alternativeExpectation.equals("1")) {
            // French boards use note "D" instead of "A"
            if ((boardLang != null) && (boardLang.equals(OntarioAlias.CONST_LANG_FRENCH))) {
                crsNotesList.add("D");
            } else {
                crsNotesList.add("A");
            }
        }

        // add Language of Instruction Override notes

        if ((boardLang != null) && (boardLang.equals(OntarioAlias.CONST_LANG_FRENCH))) {
            // This note does note get added to French Boards
        } else {
            String languageOfInstruction = (String) trn
                    .getFieldValueByAlias(OntarioAlias.ALIAS_TRN_LANGUAGE_OF_INSTR_OVERRIDE);
            if (StringUtils.isBlank(languageOfInstruction) && mst != null) {
                languageOfInstruction = (String) mst.getFieldValueByAlias(OntarioAlias.ALL_MST_LANGUAGE_OF_INSTRUCTION);
            }
            if (StringUtils.isBlank(languageOfInstruction)) {
                languageOfInstruction = (String) csk
                        .getFieldValueByAlias(OntarioAlias.ALIAS_CSK_LANGUAGE_OF_INSTRUCTION);
            }
            if (StringUtils.isBlank(languageOfInstruction)) {
                languageOfInstruction = (String) crs
                        .getFieldValueByAlias(OntarioAlias.ALIAS_CRS_LANGUAGE_OF_INSTRUCTION);
            }
            if (!StringUtils.isEmpty(languageOfInstruction) && languageOfInstruction.equals("French")) {
                crsNotesList.add("F");
            }
        }

        return crsNotesList;
    }

    /**
     * Gets a nested map of compulsory and elective certificates maps for SHSM
     * programs.
     * <li>certificateType compulsory:
     * OntarioAlias.CONST_SHSM_CERTIFICATE_COMPULSORY
     * <li>certificateType elective: OntarioAlias.CONST_SHSM_CERTIFICATE_ELECTIVE
     *
     * @param gradStdProgram GraduationStudentProgram
     * @return HashMap&lt;certificaateType, HashMap&lt;reqCode,
     *         GraduationRequirement&gt;&gt;
     */
    public static HashMap<String, HashMap<String, GraduationRequirement>> getCompulsoryAndElectiveCertificatesMaps(
                                                                                                                   GraduationStudentProgram gradStdProgram) {
        HashMap<String, HashMap<String, GraduationRequirement>> certificatesByType =
                new HashMap<String, HashMap<String, GraduationRequirement>>();

        HashMap<String, GraduationRequirement> compulsoryCertificatesByCodeMap =
                new HashMap<String, GraduationRequirement>();
        GraduationProgram compulsoryCertificatesRequirementProgram = gradStdProgram.getProgramStudies()
                .getRequirements().stream().map(gr -> gr)
                .filter(req -> req.getCode().equals(OntarioAlias.CONST_SHSM_CERTIFICATE_COMPULSORY))
                .map(gradReq -> gradReq.getSubProgramStudies()).findFirst().get();
        compulsoryCertificatesRequirementProgram.getRequirements().stream()
        .filter(req -> req.getType() == GraduationRequirement.TYPE_OTHER)
        .forEachOrdered(r -> compulsoryCertificatesByCodeMap.put(r.getCode(), r));
        certificatesByType.put(OntarioAlias.CONST_SHSM_CERTIFICATE_COMPULSORY, compulsoryCertificatesByCodeMap);

        HashMap<String, GraduationRequirement> electiveCertificatesByCodeMap =
                new HashMap<String, GraduationRequirement>();
        GraduationProgram electiveCertificatesRequirementProgram = gradStdProgram.getProgramStudies().getRequirements()
                .stream().map(gr -> gr)
                .filter(req -> req.getCode().equals(OntarioAlias.CONST_SHSM_CERTIFICATE_ELECTIVE))
                .map(gradReq -> gradReq.getSubProgramStudies()).findFirst().get();
        electiveCertificatesRequirementProgram.getRequirements().stream()
        .filter(req -> req.getType() == GraduationRequirement.TYPE_OTHER)
        .forEachOrdered(r -> electiveCertificatesByCodeMap.put(r.getCode(), r));
        certificatesByType.put(OntarioAlias.CONST_SHSM_CERTIFICATE_ELECTIVE, electiveCertificatesByCodeMap);

        return certificatesByType;
    }

    /**
     * Determine additional notes for a school course record. Note descriptions
     * located on Graduation Summary and OST reports. French immersion note only
     * applies to English boards
     *
     * @param csk the csk
     * @param onsisDualCreditMap the onsis dual credit map
     * @param onsisCoopMap the onsis coop map
     * @return the list
     */
    public static List<String> determineNotesValues(SchoolCourse csk,
                                                    Map<String, String> onsisDualCreditMap,
                                                    Map<String, String> onsisCoopMap) {
        SisOrganization org = csk.getSchool().getOrganization1();
        String boardLang = (String) org.getFieldValueByAlias(OntarioAlias.ALIAS_ORG_LANGUAGE);
        List<String> crsNotesList = new ArrayList<String>();
        if (csk != null) {
            String crsPrefix = "";
            if (!StringUtils.isBlank(csk.getNumber())) {
                crsPrefix = StringUtils.substring(csk.getNumber(), 3);
            }
            String crsDeliveryType = (String) csk.getFieldValueByAlias(OntarioAlias.ALIAS_CSK_COURSE_DELIVERY_TYPE);
            if (StringUtils.isBlank(crsDeliveryType)) {
                crsDeliveryType = (String) csk.getCourse().getFieldValueByAlias(OntarioAlias.ALIAS_CRS_DELIVERY_TYPE);
            }

            if (!StringUtils.isEmpty(crsDeliveryType)) {
                if (onsisDualCreditMap.containsKey(crsDeliveryType)) {
                    crsNotesList.add("T");
                } else if (onsisCoopMap.containsKey(crsDeliveryType)) {
                    crsNotesList.add("C");
                }
            }

            // add Interdisciplinary Studies notes
            if ("IDC".equals(crsPrefix) || "IDP".equals(crsPrefix)) {
                crsNotesList.add("I");
            }

            // Language of Instruction
            String courseLanguageOfInstruction = (String) csk
                    .getFieldValueByAlias(OntarioAlias.ALIAS_CSK_LANGUAGE_OF_INSTRUCTION);
            if (StringUtils.isEmpty(courseLanguageOfInstruction)) {

                courseLanguageOfInstruction = (String) csk.getCourse()
                        .getFieldValueByAlias(OntarioAlias.ALIAS_CRS_LANGUAGE_OF_INSTRUCTION);
            }
            if ((boardLang != null) && (boardLang.equals(OntarioAlias.CONST_LANG_FRENCH))) {
                // This note does note get added to French Boards
            } else if (!StringUtils.isEmpty(courseLanguageOfInstruction)
                    && courseLanguageOfInstruction.equals("French")) {
                crsNotesList.add("F");
            }

        }

        return crsNotesList;
    }

    /**
     * Determine additional notes for a course record. Note descriptions located on
     * Graduation Summary and OST reports.
     *
     * @param crs the crs
     * @param onsisDualCreditMap the onsis dual credit map
     * @param onsisCoopMap the onsis coop map
     * @return the list
     */
    public static List<String> determineNotesValues(Course crs,
                                                    Map<String, String> onsisDualCreditMap,
                                                    Map<String, String> onsisCoopMap) {
        List<String> crsNotesList = new ArrayList<String>();
        SisOrganization org = crs.getOrganization1();
        String boardLang = (String) org.getFieldValueByAlias(OntarioAlias.ALIAS_ORG_LANGUAGE);
        if (crs != null) {
            String crsPrefix = "";
            if (!StringUtils.isBlank(crs.getNumber())) {
                crsPrefix = StringUtils.substring(crs.getNumber(), 3);
            }
            String crsDeliveryType = (String) crs.getFieldValueByAlias(OntarioAlias.ALIAS_CRS_DELIVERY_TYPE);

            if (!StringUtils.isEmpty(crsDeliveryType)) {
                if (onsisDualCreditMap.containsKey(crsDeliveryType)) {
                    crsNotesList.add("T");
                } else if (onsisCoopMap.containsKey(crsDeliveryType)) {
                    crsNotesList.add("C");
                }
            }

            // add Interdisciplinary Studies notes
            if ("IDC".equals(crsPrefix) || "IDP".equals(crsPrefix)) {
                crsNotesList.add("I");
            }

            // Language of Instruction
            if ((boardLang != null) && (boardLang.equals(OntarioAlias.CONST_LANG_FRENCH))) {
                // This note does note get added to French Boards
            } else {
                String courseLanguageOfInstruction = (String) crs
                        .getFieldValueByAlias(OntarioAlias.ALIAS_CRS_LANGUAGE_OF_INSTRUCTION);
                if (!StringUtils.isEmpty(courseLanguageOfInstruction) && courseLanguageOfInstruction.equals("French")) {
                    crsNotesList.add("F");
                }
            }
        }

        return crsNotesList;
    }

    /**
     * Gets a map of valid waivers for GradDiploma OSSD (1999) keyed on:
     * OntarioAlias.WAIVER_OSSD_1999_COMMUNITY_SERVICE_INVOLVEMENT_REQ_CODE
     * OntarioAlias.WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT
     * <br>
     * Expected parameter is the collection of waivers for each student graduation
     * student program: gsrObject.getStudentWaivers().
     *
     * @param studentWaivers Collection<GraduationStudentWaiver>
     * @return Map&lt;StringConstant, boolean&gt;
     */
    public static Map<String, Boolean> getOSSD199WaiverInfo(Collection<GraduationStudentWaiver> studentWaivers) {
        Map<String, Boolean> waiverInfo = new HashMap<String, Boolean>();
        for (GraduationStudentWaiver gsw : studentWaivers) {
            if (gsw.getRequirement().getCode()
                    .equalsIgnoreCase(OntarioAlias.WAIVER_OSSD_1999_COMMUNITY_SERVICE_INVOLVEMENT_REQ_CODE)) { // Constant
                // -
                // Community
                // Involvement
                // Hours
                waiverInfo.put(OntarioAlias.WAIVER_OSSD_1999_COMMUNITY_SERVICE_INVOLVEMENT_REQ_CODE, true);
            }
            if (gsw.getRequirement().getCode()
                    .equalsIgnoreCase(OntarioAlias.WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT)) { // Constant
                // -
                // Provincial
                // Secondary
                // School
                // Literacy
                // Requirement
                waiverInfo.put(OntarioAlias.WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT, true);
            }
        }
        return waiverInfo;
    }

    /**
     * Returns map where key is student's oid and value is a list of his
     * exceptionalities.
     *
     * @param broker the broker
     * @return the exceptionality map
     */
    public static Map<String, List<String>> getExceptionalityMap(X2Broker broker) {
        DataDictionary extendedDictionary = DataDictionary.getDistrictDictionary(
                broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_SPECIAL_ED),
                broker.getPersistenceKey());
        Map<String, List<String>> exceptionalityMap = new HashMap<>();
        String[] column = new String[] {SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().studentOid().getPath(),
                extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_EXCEPTIONALITY)
                .getDataFieldConfig().getDataField().getJavaName()};

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.STUDENT_PROGRAM_DETAIL.type().getPath(), "Exception");

        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addGreaterThan(SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().endDate().getPath(), new PlainDate());

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIsNull(SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().endDate().getPath());

        subCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(subCriteria);

        subCriteria = new X2Criteria();
        subCriteria.addGreaterThan(
                extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_END_DATE)
                .getDataFieldConfig().getDataField().getJavaName(),
                new PlainDate());

        orCriteria = new X2Criteria();
        orCriteria.addIsNull(extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_END_DATE)
                .getDataFieldConfig().getDataField().getJavaName());

        subCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(subCriteria);

        ColumnQuery query = new ColumnQuery(StudentProgramDetail.class, column, criteria, true);

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                List<String> excepts = exceptionalityMap.get(row[0]);
                if (excepts == null) {
                    excepts = new ArrayList<>();
                }

                excepts.add((String) row[1]);
                exceptionalityMap.put((String) row[0], excepts);
            }
        }

        return exceptionalityMap;
    }

    /**
     * Returns map where key is student's oid and value is his primary
     * exceptionality.
     *
     * @param broker the broker
     * @return the primary exceptionality map
     */
    public static Map<String, String> getPrimaryExceptionalityMap(X2Broker broker) {
        DataDictionary extendedDictionary = DataDictionary.getDistrictDictionary(
                broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_SPECIAL_ED),
                broker.getPersistenceKey());
        Map<String, String> exceptionalityMap = new HashMap<>();
        String[] column = new String[] {SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_EXCEPTIONALITY)
                .getDataFieldConfig().getDataField().getJavaName()};

        X2Criteria criteria = new X2Criteria();
        criteria.addNotNull(
                extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_EXCEPTIONALITY)
                .getDataFieldConfig().getDataField().getJavaName());
        criteria.addEqualTo(extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_IS_PRIMARY)
                .getDataFieldConfig().getDataField().getJavaName(), "1");

        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addGreaterThan(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate().getPath(), new PlainDate());

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIsNull(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate().getPath());

        subCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(subCriteria);

        ColumnQuery query = new ColumnQuery(StudentProgramParticipation.class, column, criteria, true);

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                exceptionalityMap.put((String) row[0], (String) row[1]);
            }
        }

        return exceptionalityMap;
    }

    /**
     * Returns map where key is student's oid and value is a list of
     * exceptionalities. End dated exceptionalities are not included.
     *
     * @param broker X2Broker
     * @return Map
     */
    public static Map<String, List<String>> getExceptionalitiesMap(X2Broker broker) {
        DataDictionary extendedDictionary = DataDictionary.getDistrictDictionary(
                broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_SPECIAL_ED),
                broker.getPersistenceKey());
        Map<String, List<String>> exceptionalityMap = new HashMap<>();
        String[] column = new String[] {SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_EXCEPTIONALITY)
                .getDataFieldConfig().getDataField().getJavaName()};

        X2Criteria criteria = new X2Criteria();
        criteria.addNotNull(
                extendedDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_EXCEPTIONALITY)
                .getDataFieldConfig().getDataField().getJavaName());
        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addGreaterThan(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate().getPath(), new PlainDate());

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIsNull(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate().getPath());

        subCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(subCriteria);

        ColumnQuery query = new ColumnQuery(StudentProgramParticipation.class, column, criteria, true);
        query.addOrderByAscending(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath());
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                if (exceptionalityMap.containsKey(row[0])) {
                    exceptionalityMap.get(row[0]).add((String) row[1]);
                } else if (row[1] != null) {
                    List exceptionalitesList = new LinkedList();
                    exceptionalitesList.add(row[1]);
                    exceptionalityMap.put((String) row[0], exceptionalitesList);
                }
            }
        }

        return exceptionalityMap;
    }

    /**
     * Returns map where key is student's oid and value is a list of SLP program
     * info.
     *
     * @param asOfDate - PlainDate
     * @param schoolOid - optional, if null will process entire district
     * @param subQueryStdOids - optional, if null will process all students for
     *        other criteria
     * @param stdOidsList - optional, if null will process all students for
     *        other criteria
     * @param broker - X2Broker
     * @return Map<String, List<String>> - slp info map by student oid. Info = Code
     *         + '-' + Type
     */
    public static Map<String, List<String>> getSlpInfoAsOfDateMap(PlainDate asOfDate,
                                                                  String schoolOid,
                                                                  SubQuery subQueryStdOids,
                                                                  List<String> stdOidsList,
                                                                  X2Broker broker) {
        // initialize variables
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
        Map<String, List<String>> slpInfoByStdOidMap = new HashMap<String, List<String>>();

        // get columns for student aliases
        String fieldSlpCodeJava = getBeanPathFromAlias(OntarioAlias.ALIAS_PGM_SLP_CODE, true, dictionary);
        String fieldSlpTypeJava = getBeanPathFromAlias(OntarioAlias.ALIAS_PGM_SLP_TYPE, true, dictionary);

        String[] column = new String[] {StudentProgramParticipation.COL_STUDENT_OID, fieldSlpCodeJava,
                fieldSlpTypeJava};

        // conditin for SLP
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, OntarioAlias.PROGRAM_TYPE_SLP);
        criteria.addLessThan(StudentProgramParticipation.COL_START_DATE, asOfDate);

        // condition for program end date
        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE, asOfDate);

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        subCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(subCriteria);

        // condition for school oid
        if (!StringUtils.isEmpty(schoolOid)) {
            criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    schoolOid);
        }

        // condition for student oids sub query
        if (subQueryStdOids != null) {
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQueryStdOids);
        }

        // condition for student oids list
        if (stdOidsList != null) {
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, stdOidsList);
        }

        ColumnQuery query = new ColumnQuery(StudentProgramParticipation.class, column, criteria, true);

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                List<String> slps = slpInfoByStdOidMap.get(row[0]);
                if (slps == null) {
                    slps = new ArrayList<String>();
                }

                String stdOid = (String) row[0];
                String slpCode = OntarioAlias.CONST_EMPTY;
                if (row[1] != null) {
                    slpCode = (String) row[1];
                }
                String slpType = OntarioAlias.CONST_EMPTY;
                if (row[2] != null) {
                    slpType = (String) row[2];
                }

                String slpInfo = slpCode + "," + slpType;

                slps.add(slpInfo);
                slpInfoByStdOidMap.put(stdOid, slps);
            }
        }

        return slpInfoByStdOidMap;
    }

    /**
     * If there is a slp record in between term start date and end date, need to be
     * processed. Can not only consider random date.
     *
     * 14JUN2022: Enhanced logic implemented to account for students who left their
     * school board before the end or the term.
     *
     * 14DEC2022: Modifying to allow for both French and Native language SLP
     * records. Was not done for inactive students.
     *
     * @param termStartDate the term start date
     * @param termEndDate the term end date
     * @param schoolOid the school oid
     * @param subQueryStdOids the sub query std oids
     * @param stdOidsList the std oids list
     * @param broker the broker
     * @param dictionary the dictionary
     * @return the slp map
     */
    public Map<String, String[]> getSlpMap(PlainDate termStartDate,
                                           PlainDate termEndDate,
                                           String schoolOid,
                                           SubQuery subQueryStdOids,
                                           List<String> stdOidsList,
                                           X2Broker broker,
                                           DataDictionary dictionary) {
        // initialize variables
        Map<String, String[]> slpInfoByStdOidMap = new HashMap<String, String[]>();

        // get columns for student aliases
        String fieldSlpCodeJava = OntarioToolHelper.getBeanPathFromAlias(OntarioAlias.ALIAS_PGM_SLP_CODE, true,
                dictionary);
        String fieldSlpTypeJava = OntarioToolHelper.getBeanPathFromAlias(OntarioAlias.ALIAS_PGM_SLP_TYPE, true,
                dictionary);

        String[] column = new String[] { StudentProgramParticipation.COL_STUDENT_OID, fieldSlpCodeJava,
                fieldSlpTypeJava };

        // condition for SLP
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, OntarioAlias.PROGRAM_TYPE_SLP);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, termEndDate);

        // condition for program end date
        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, termEndDate);

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        subCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(subCriteria);

        // condition for school oid
        if (!StringUtils.isEmpty(schoolOid)) {
            criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    schoolOid);
        }
        // condition for student oids sub query
        if (subQueryStdOids != null) {
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQueryStdOids);
        }

        // condition for student oids list
        if (stdOidsList != null) {
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, stdOidsList);
        }

        ColumnQuery query = new ColumnQuery(StudentProgramParticipation.class, column, criteria, true);
        query.addOrderByAscending(StudentProgramParticipation.COL_START_DATE);
        query.addOrderByAscending(fieldSlpCodeJava);
        query.addOrderByAscending(fieldSlpTypeJava);

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String stdOid = (String) row[0];
                String[] slps = slpInfoByStdOidMap.get(stdOid);
                if (slps == null) {
                    // slps[0] = fsl, slps[1] = native language
                    slps = new String[2];
                }

                String slpCode = OntarioAlias.CONST_EMPTY;
                if (row[1] != null) {
                    slpCode = (String) row[1];
                }
                String slpType = OntarioAlias.CONST_EMPTY;
                if (row[2] != null) {
                    slpType = (String) row[2];
                }

                String slpInfo = slpCode + "," + slpType;

                if ((!StringUtils.isEmpty(slpType)) && (slpType.equals(OntarioAlias.CONST_PGM_SLP_TYPE_FRENCH))) {
                    slps[0] = slpInfo;
                }
                if ((!StringUtils.isEmpty(slpType)) && (slpType.equals(OntarioAlias.CONST_PGM_SLP_TYPE_NATIVE_LANG))) {
                    slps[1] = slpInfo;
                }
                slpInfoByStdOidMap.put(stdOid, slps);
            }
        }

        // Enhanced logic to properly address students who left the board before the end of the term
        X2Criteria inactiveStudentsCriteria = new X2Criteria();

        inactiveStudentsCriteria.addNotIn(Student.COL_ENROLLMENT_STATUS,
                Arrays.asList("Active", "Active No Primary", "Excluded"));

        // condition for school oid
        if (!StringUtils.isEmpty(schoolOid)) {
            inactiveStudentsCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID,
                    schoolOid);
        }
        // condition for student oids sub query
        if (subQueryStdOids != null) {
            inactiveStudentsCriteria.addIn(X2BaseBean.COL_OID, subQueryStdOids);
        }

        // condition for student oids list
        if (stdOidsList != null) {
            inactiveStudentsCriteria.addIn(X2BaseBean.COL_OID, stdOidsList);
        }

        BeanQuery inactiveStudentsQuery = new BeanQuery(SisStudent.class, inactiveStudentsCriteria);

        Map<String, SisStudent> inactiveStudentsMap =
                broker.getMapByQuery(inactiveStudentsQuery, X2BaseBean.COL_OID, 100);

        if (!inactiveStudentsMap.isEmpty()) {
            X2Criteria inactiveStudentsLatestEnrolmentCriteria = new X2Criteria();
            inactiveStudentsLatestEnrolmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID,
                    inactiveStudentsMap.keySet());
            BeanQuery inactiveStudentsLatestEnrolmentQuery =
                    new BeanQuery(StudentEnrollment.class, inactiveStudentsLatestEnrolmentCriteria);
            inactiveStudentsLatestEnrolmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);

            Map<String, StudentEnrollment> inactiveStudentsLatestEnrolmentMap =
                    broker.getMapByQuery(inactiveStudentsLatestEnrolmentQuery, StudentEnrollment.COL_STUDENT_OID, 100);

            List<String> inactiveStdOidsProcessed = new ArrayList<String>();

            for (SisStudent inactiveStudent : inactiveStudentsMap.values()) {
                X2Criteria slpForInactiveStdsCriteria = new X2Criteria();
                slpForInactiveStdsCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID,
                        inactiveStudent.getOid());
                slpForInactiveStdsCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE,
                        OntarioAlias.PROGRAM_TYPE_SLP);
                slpForInactiveStdsCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, termEndDate);

                // condition for school oid
                if (!StringUtils.isEmpty(schoolOid)) {
                    slpForInactiveStdsCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                            schoolOid);
                }

                // condition for modified program end date to be the last enrolment date
                X2Criteria subInactiveCriteria = new X2Criteria();
                subInactiveCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                        inactiveStudentsLatestEnrolmentMap.get(inactiveStudent.getOid()).getEnrollmentDate());

                X2Criteria orInactiveCriteria = new X2Criteria();
                orInactiveCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

                subInactiveCriteria.addOrCriteria(orCriteria);
                slpForInactiveStdsCriteria.addAndCriteria(subInactiveCriteria);

                ColumnQuery slpForInactiveStdsQuery =
                        new ColumnQuery(StudentProgramParticipation.class, column, slpForInactiveStdsCriteria, true);
                try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(slpForInactiveStdsQuery)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();

                        String stdOid = (String) row[0];
                        // slps[0] = fsl, slps[1] = native language
                        String[] slps = new String[2];
                        // do not fetch stored values which did not separate inactive students
                        if (inactiveStdOidsProcessed.contains(stdOid)) {
                            slps = slpInfoByStdOidMap.get(stdOid);
                        } else {
                            inactiveStdOidsProcessed.add(stdOid);
                        }

                        String slpCode = OntarioAlias.CONST_EMPTY;
                        if (row[1] != null) {
                            slpCode = (String) row[1];
                        }
                        String slpType = OntarioAlias.CONST_EMPTY;
                        if (row[2] != null) {
                            slpType = (String) row[2];
                        }

                        String slpInfo = slpCode + "," + slpType;

                        if ((!StringUtils.isEmpty(slpType))
                                && (slpType.equals(OntarioAlias.CONST_PGM_SLP_TYPE_FRENCH))) {
                            slps[0] = slpInfo;
                        }
                        if ((!StringUtils.isEmpty(slpType))
                                && (slpType.equals(OntarioAlias.CONST_PGM_SLP_TYPE_NATIVE_LANG))) {
                            slps[1] = slpInfo;
                        }
                        slpInfoByStdOidMap.put(stdOid, slps);
                    }
                }
            }
        }

        return slpInfoByStdOidMap;
    }

    /**
     * Returns map where key is student's oid and value is a boolean representing IEP
     * on the effective date passed.
     *
     * @param effectiveDate PlainDate
     * @param stdOidsSubQuery SubQuery
     * @param broker X2Broker
     * @return Map
     */
    public static Map<String, String> getSpecEdMap(PlainDate effectiveDate, SubQuery stdOidsSubQuery, X2Broker broker) {
        Map specEdMap = new HashMap<>();
        DataDictionary m_specEdDictionary = DataDictionary.getDistrictDictionary(
                broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_SPECIAL_ED),
                broker.getPersistenceKey());

        String[] columns = {
                SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().studentOid().getPath(), // 0
                m_specEdDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_IEP_REQUIRED)
                .getDataFieldConfig().getDataField().getJavaName(), // 1
        };

        X2Criteria criteria = new X2Criteria();
        // IEP has to be checked on the actual Placement record
        criteria.addEqualTo(
                m_specEdDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_IEP_REQUIRED)
                .getDataFieldConfig().getDataField().getJavaName(),
                "1");
        // Has to be reported to OnSIS to count
        criteria.addEqualTo(SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().getPath() + PATH_DELIMITER +
                m_specEdDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGM_SPECED_REPORT_INDICATOR)
        .getDataFieldConfig().getDataField().getJavaName(),
        "1");
        criteria.addIn(SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().studentOid().getPath(), stdOidsSubQuery);
        criteria.addEqualTo(SisBeanPaths.STUDENT_PROGRAM_DETAIL.type().getPath(), "Placement");
        // Exceptionality has to be valid on effective date
        X2Criteria exceptionalityDateCriteria = new X2Criteria();
        exceptionalityDateCriteria.addLessOrEqualThan(
                SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().startDate().getPath(), effectiveDate);
        X2Criteria andCriteria = new X2Criteria();
        andCriteria.addGreaterThan(SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().endDate().getPath(), effectiveDate);
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().endDate().getPath(),
                broker.getPersistenceKey());
        andCriteria.addOrCriteria(orCriteria);
        exceptionalityDateCriteria.addAndCriteria(andCriteria);
        criteria.addAndCriteria(exceptionalityDateCriteria);
        // Placement has to be valid on effective date
        X2Criteria placementDateCriteria = new X2Criteria();
        placementDateCriteria.addLessOrEqualThan(
                m_specEdDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_START_DATE)
                .getDataFieldConfig().getDataField().getJavaName(),
                effectiveDate);
        X2Criteria placementAndCriteria = new X2Criteria();
        placementAndCriteria.addGreaterThan(
                m_specEdDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_END_DATE)
                .getDataFieldConfig().getDataField().getJavaName(),
                effectiveDate);
        X2Criteria placementOrCriteria = new X2Criteria();
        placementOrCriteria
        .addEmpty(m_specEdDictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_PGD_SPECED_END_DATE)
                .getDataFieldConfig().getDataField().getJavaName(), broker.getPersistenceKey());
        placementAndCriteria.addOrCriteria(placementOrCriteria);
        placementDateCriteria.addAndCriteria(placementAndCriteria);
        criteria.addAndCriteria(placementDateCriteria);
        ColumnQuery query = new ColumnQuery(StudentProgramDetail.class, columns, criteria, true);
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                specEdMap.put(row[0], row[1]);
            }
        }
        return specEdMap;
    }

    /**
     * Convenient method to get hours required for a particular student, if
     * the method returns null, then the student does not need any community
     * hours and consulting tool should report requirement as N/A.
     *
     * @param stdOid String
     * @param currentYearContext DistrictSchoolYearContext
     * @param broker X2Broker
     * @return Big decimal
     * @throws Exception the exception
     */
    public static BigDecimal getCommunityHourNeededForStudent(String stdOid,
                                                              DistrictSchoolYearContext currentYearContext,
                                                              X2Broker broker)
                                                                      throws Exception {

        BigDecimal hoursNeededForStudent = null;

        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addEqualTo(X2BaseBean.COL_OID, stdOid);
        SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

        hoursNeededForStudent = getCommunityHoursByStdOid(stdSubQuery, currentYearContext, broker).get(stdOid);

        return hoursNeededForStudent;
    }

    /**
     * Gets a Map of community hours needed keyed studentOid.
     * The method needs the current year context to determine properly the hours needed
     * for students that left the board but didn't graduate.
     * <br/>
     * <br/>
     *
     * NOTE: Only students with OSSD (1999) or DESO (1999) are returned on results map
     * if a student is not present on the returning map, then comm. hours are not applicable,
     * particularly for students graduated on 2020, this method will return a value or not
     * depending on the rules applicable.
     *
     * @param stdOidsSubQuery SubQuery
     * @param currentYearContext DistrictSchoolYearContext
     * @param broker X2Broker
     * @return Map
     * @throws Exception the exception
     */
    public static Map<String, BigDecimal> getCommunityHoursByStdOid(SubQuery stdOidsSubQuery,
                                                                    DistrictSchoolYearContext currentYearContext,
                                                                    X2Broker broker)
                                                                            throws Exception {
        int mapsInitialSize = broker.getCount(stdOidsSubQuery);
        BigDecimal defaultHours = new BigDecimal(40.0);
        Map<String, BigDecimal> hoursNeededPerStudentOid = new HashMap();
        Map<String, Integer> yogPerStudentOid = new HashMap();
        Map<String, GraduationStudentProgram> mainDiplomaByStdOid = new HashMap();
        Map<String, GraduationStudentWaiver> hoursWaiverByStdOid = new HashMap();
        Map<Integer, BigDecimal> modifiedHoursByYearMap = new HashMap();

        DataDictionary districtDictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());

        ExtendedDataDictionary ctxDdx =
                broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_RCD_SCHOOL_YEARS);
        DataDictionary ctxRefTableDictionary = DataDictionary.getDistrictDictionary(ctxDdx, broker.getPersistenceKey());
        String schoolYearsRtbCommHoursField =

                ctxRefTableDictionary
                .findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_RCD_MANDATED_COMM_INVOLMENT_HOURS)
                .getJavaName();

        X2Criteria rcdSchoolYearsCriteria = new X2Criteria();
        rcdSchoolYearsCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbContextYear");
        rcdSchoolYearsCriteria.addNotEmpty(schoolYearsRtbCommHoursField, broker.getPersistenceKey());
        BeanQuery rcdSchoolYearsQuery = new BeanQuery(ReferenceCode.class, rcdSchoolYearsCriteria);
        try (QueryIterator iterator = broker.getIteratorByQuery(rcdSchoolYearsQuery)) {
            while (iterator.hasNext()) {
                ReferenceCode rcdBean = (ReferenceCode) iterator.next();
                if (rcdBean.getFieldValueByBeanPath(schoolYearsRtbCommHoursField) != null) {
                    // Sequence orders are negative for front end ordering
                    modifiedHoursByYearMap.put(rcdBean.getSequenceNumber() * -1,
                            BigDecimal.valueOf(Double
                                    .valueOf((String) rcdBean.getFieldValueByBeanPath(schoolYearsRtbCommHoursField))));

                }
            }
        }

        X2Criteria gsrCriteria = new X2Criteria();
        gsrCriteria.addIn(GraduationStudentProgram.COL_STUDENT_OID, stdOidsSubQuery);
        gsrCriteria.addEqualTo(GraduationStudentProgram.COL_PRIMARY_INDICATOR, true);
        gsrCriteria.addIn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.programStudies().name().getPath(),
                Arrays.asList("OSSD (1999)", "DESO (1999)"));
        BeanQuery gsrQuery = new BeanQuery(GraduationStudentProgram.class, gsrCriteria);
        mainDiplomaByStdOid = broker.getMapByQuery(gsrQuery, GraduationStudentProgram.COL_STUDENT_OID, mapsInitialSize);

        X2Criteria gswCriteria = new X2Criteria();
        gswCriteria.addIn(SisBeanPaths.GRADUATION_STUDENT_WAIVER.studentProgram().studentOid().getPath(),
                stdOidsSubQuery);

        gswCriteria.addIn(getBeanPathFromAlias(OntarioAlias.ALIAS_GSW_WAIVER_REASON, true, districtDictionary),
                Arrays.asList("CIH", "MatureCIH"));
        BeanQuery gswQuery = new BeanQuery(GraduationStudentWaiver.class, gswCriteria);
        hoursWaiverByStdOid = broker.getMapByQuery(gswQuery,
                SisBeanPaths.GRADUATION_STUDENT_WAIVER.studentProgram().studentOid().getPath(), 100);


        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addIn(X2BaseBean.COL_OID, stdOidsSubQuery);
        String[] stdColumns = {X2BaseBean.COL_OID,
                SisStudent.COL_YOG,
                getBeanPathFromAlias(OntarioAlias.ALIAS_STD_COMMUNITY_HOURS_ACTUAL, true, districtDictionary)};
        ColumnQuery stdColumnQuery = new ColumnQuery(SisStudent.class, stdColumns, stdCriteria);
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(stdColumnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String stdOid = (String) row[0];
                int yog = ((BigDecimal) row[1]).intValue();
                BigDecimal commHoursEarned =
                        row[2] != null ? BigDecimal.valueOf(Double.valueOf((String) row[2])) : BigDecimal.ZERO;

                if (mainDiplomaByStdOid.containsKey(stdOid)) {
                    boolean issuedDiploma = !StringUtils.isBlank(
                            (String) mainDiplomaByStdOid.get(stdOid)
                            .getFieldValueByAlias(OntarioAlias.ALIAS_GSR_ISSUED_DATE));
                    yogPerStudentOid.put(stdOid, yog);
                    // When waiver is present we use this value, otherwise we go with default hours
                    if (hoursWaiverByStdOid.containsKey(stdOid)) {
                        hoursNeededPerStudentOid.put(stdOid, hoursWaiverByStdOid.get(stdOid).getCreditWaived());
                    } else {
                        if (modifiedHoursByYearMap.containsKey(yog)) {
                            if (yog == 2020 && issuedDiploma) {
                                if (commHoursEarned.compareTo(defaultHours) >= 0) {
                                    hoursNeededPerStudentOid.put(stdOid, defaultHours);
                                } else {
                                    // We won't include YOG=2020 students to give them an N/A on OST
                                    continue;
                                }
                            } else {
                                if (issuedDiploma) {
                                    hoursNeededPerStudentOid.put(stdOid, modifiedHoursByYearMap.get(yog));
                                } else {
                                    if (yog < currentYearContext.getSchoolYear()) {
                                        hoursNeededPerStudentOid.put(stdOid, defaultHours);
                                    } else {
                                        hoursNeededPerStudentOid.put(stdOid, modifiedHoursByYearMap.get(yog));
                                    }
                                }
                            }
                        } else {
                            if (issuedDiploma) {
                                if (commHoursEarned.compareTo(defaultHours) <= 0) {
                                    hoursNeededPerStudentOid.put(stdOid, commHoursEarned);
                                } else {
                                    hoursNeededPerStudentOid.put(stdOid, defaultHours);
                                }
                            } else {
                                hoursNeededPerStudentOid.put(stdOid, defaultHours);
                            }
                        }

                    }
                }
            }
        } catch (Exception e) {
            throw e;
        }

        return hoursNeededPerStudentOid;
    }

    /**
     * Convenient method to get online credits required for a particular student,
     * if the method returns null, then the student does not need any online
     * credits and consulting tool should report requirement as N/A.
     *
     * @param stdOid String
     * @param broker X2Broker
     * @return Big decimal
     * @throws Exception the exception
     */
    public static BigDecimal getOnlineCreditsRequiredForStudent(String stdOid,
                                                                X2Broker broker)
                                                                        throws Exception {

        BigDecimal creditsNeededForStudent = null;

        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addEqualTo(X2BaseBean.COL_OID, stdOid);
        SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

        creditsNeededForStudent = getOnlineCreditsRequiredByStdOid(stdSubQuery, broker).get(stdOid);

        return creditsNeededForStudent;
    }


    /**
     * Gets a map of online credits required keyed by stdOid based on Grade9Cohort.
     * As of May, 2022: The hours expected by student are between 0 and 2. <br/>
     * <br/>
     *
     * <b>NOTE:</b> Only students with OSSD (1999) or DESO (1999) are returned on results map
     * if a student is not present on the returning map, then online credits are not applicable
     *
     *
     * @param stdOidsSubQuery SubQuery
     * @param broker X2Broker
     * @return Map
     * @throws Exception exception
     */
    public static Map<String, BigDecimal> getOnlineCreditsRequiredByStdOid(SubQuery stdOidsSubQuery,
                                                                           X2Broker broker)
                                                                                   throws Exception {
        int mapsInitialSize = broker.getCount(stdOidsSubQuery);
        BigDecimal defaultCredits = new BigDecimal(2.0);
        Map<String, BigDecimal> creditsNeededPerStudentOid = new HashMap();
        Map<String, GraduationStudentProgram> mainDiplomaByStdOid = new HashMap();
        Map<String, GraduationStudentWaiver> creditsWaiverByStdOid = new HashMap();

        DataDictionary districtDictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());

        Map<String, ReferenceCode> schoolYearsMap = getReferenceCodesMap(broker, "rtbContextYear");

        X2Criteria gsrCriteria = new X2Criteria();
        gsrCriteria.addIn(GraduationStudentProgram.COL_STUDENT_OID, stdOidsSubQuery);
        gsrCriteria.addEqualTo(GraduationStudentProgram.COL_PRIMARY_INDICATOR, true);
        gsrCriteria.addIn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.programStudies().name().getPath(),
                Arrays.asList("OSSD (1999)", "DESO (1999)"));
        BeanQuery gsrQuery = new BeanQuery(GraduationStudentProgram.class, gsrCriteria);
        mainDiplomaByStdOid = broker.getMapByQuery(gsrQuery, GraduationStudentProgram.COL_STUDENT_OID, mapsInitialSize);

        X2Criteria gswCriteria = new X2Criteria();
        gswCriteria.addIn(SisBeanPaths.GRADUATION_STUDENT_WAIVER.studentProgram().studentOid().getPath(),
                stdOidsSubQuery);

        gswCriteria.addIn(getBeanPathFromAlias(OntarioAlias.ALIAS_GSW_WAIVER_REASON, true, districtDictionary),
                Arrays.asList("OptOutPar", "OptOutPri", "OptOutStu"));
        BeanQuery gswQuery = new BeanQuery(GraduationStudentWaiver.class, gswCriteria);
        creditsWaiverByStdOid = broker.getMapByQuery(gswQuery,
                SisBeanPaths.GRADUATION_STUDENT_WAIVER.studentProgram().studentOid().getPath(), 100);

        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addIn(X2BaseBean.COL_OID, stdOidsSubQuery);
        String[] stdColumns = {X2BaseBean.COL_OID,
                SisStudent.COL_YOG,
                getBeanPathFromAlias(OntarioAlias.ALIAS_STD_GRADE9_COHORT, true, districtDictionary)};
        ColumnQuery stdColumnQuery = new ColumnQuery(SisStudent.class, stdColumns, stdCriteria);
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(stdColumnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String stdOid = (String) row[0];
                int yog = ((BigDecimal) row[1]).intValue();
                String grade9Cohort = (String) row[2];
                int grade9CohortAsInt = 0;
                // Only students on OSSD/DESO 1999 need online credits
                if (mainDiplomaByStdOid.containsKey(stdOid)) {
                    // Assigning online credits needed based on Grade9Cohort
                    if (!StringUtils.isBlank(grade9Cohort)) {
                        grade9CohortAsInt = Math.abs(schoolYearsMap.get(grade9Cohort).getSequenceNumber());
                        if (grade9CohortAsInt < 2021) {
                            // Students with Grade9Cohort before 2021 don't need online credits
                            continue;
                        }
                        if (creditsWaiverByStdOid.containsKey(stdOid)
                                && creditsWaiverByStdOid.get(stdOid).getCreditWaived()
                                .compareTo(BigDecimal.ZERO) >= 0) {
                            // On the front end, we store actual credits required on gswCredWaived
                            creditsNeededPerStudentOid.put(stdOid, creditsWaiverByStdOid.get(stdOid).getCreditWaived());
                        } else {
                            // Students without a waiver need 2 credits
                            creditsNeededPerStudentOid.put(stdOid, defaultCredits);
                        }
                    } // uncomment code below if fallback logic proves to be needed
                    /*
                     * else {// fallback logic in case Grade9Cohort is not present
                     * if (yog < 2024) {
                     * // Students graduating before 2024 don't need online credits
                     * continue;
                     * }
                     * if (creditsWaiverByStdOid.containsKey(stdOid)
                     * && creditsWaiverByStdOid.get(stdOid).getCreditWaived().compareTo(BigDecimal.
                     * ZERO) >= 0) {
                     * // On the front end, we store actual credits required on gswCredWaived
                     * creditsNeededPerStudentOid.put(stdOid,
                     * creditsWaiverByStdOid.get(stdOid).getCreditWaived());
                     * } else {
                     * // Students without a waiver need 2 credits
                     * creditsNeededPerStudentOid.put(stdOid, defaultCredits);
                     * }
                     * }
                     */
                }
            }
        }

        return creditsNeededPerStudentOid;
    }

    /**
     * Convenient method to get Grade9CohortCalculatedMap for a particular student,
     * if the method returns a map with ONLY yog present, then the student does
     * not have a Grade9Cohort defined and consulting tool should address it accordingly.
     *
     * <br/>
     * <li>yog: same as std_yog</li>
     * <li>grade9Cohort: school year as int to match std_yog</li>
     * <li>yearsInHighSchool: an integer 0+</li>
     * <li>yearsInHighSchoolSimplified: a integer between 0 and 4</li>
     *
     * yearsInHighSchoolSimplified combines all students that have been in HS for over 4 years
     * into a single group.
     *
     * @param stdOid the std oid
     * @param currentYearContext SisDistrictSchoolYearContext
     * @param broker X2Broker
     * @return Map&lt;stdOid, Map&lt;key, int&gt;&gt;
     * @throws Exception the exception
     */
    public static Map<String, Integer> getGrade9CohortCalculatedMapForStudent(String stdOid,
                                                                              DistrictSchoolYearContext currentYearContext,
                                                                              X2Broker broker)
                                                                                      throws Exception {

        Map<String, Integer> calculatedMap = new HashMap();

        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addEqualTo(X2BaseBean.COL_OID, stdOid);
        SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

        calculatedMap = getGrade9CohortCalculatedMap(stdSubQuery, currentYearContext, broker).get(stdOid);

        return calculatedMap;
    }


    /**
     * Gets a map keyed by stdOid.
     * There are 4 keys: yog, grade9Cohort, yearsInHighSchool and yearsInHighSchoolSimplified.
     * Each of the values of the inner map will be an integer, if the key is not present, it
     * means that the Grade9Cohort was not populated.
     * <br/>
     * <li>yog: same as std_yog</li>
     * <li>grade9Cohort: school year as int to match std_yog</li>
     * <li>yearsInHighSchool: an integer 0+</li>
     * <li>yearsInHighSchoolSimplified: a integer between 0 and 4</li>
     *
     * yearsInHighSchoolSimplified combines all students that have been in HS for over 4 years
     * into a single group. These internal map key values are located on the OntarioAlias.
     *
     * @param stdOidsSubQuery SubQuery
     * @param currentYearContext SisDistrictSchoolYearContext
     * @param broker X2Broker
     * @return Map&lt;stdOid, Map&lt;key, int&gt;&gt;
     * @throws Exception the exception
     */
    public static Map<String, Map<String, Integer>> getGrade9CohortCalculatedMap(SubQuery stdOidsSubQuery,
                                                                                 DistrictSchoolYearContext currentYearContext,
                                                                                 X2Broker broker)
                                                                                         throws Exception {
        Map<String, Map<String, Integer>> grade9CohortCalculatedByStdOidMap = new HashMap();
        DataDictionary districtDictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
        Map<String, ReferenceCode> schoolYearsMap = getReferenceCodesMap(broker, "rtbContextYear");

        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addIn(X2BaseBean.COL_OID, stdOidsSubQuery);
        String[] stdColumns = {X2BaseBean.COL_OID,
                SisStudent.COL_YOG,
                getBeanPathFromAlias(OntarioAlias.ALIAS_STD_GRADE9_COHORT, true, districtDictionary)};
        ColumnQuery stdColumnQuery = new ColumnQuery(SisStudent.class, stdColumns, stdCriteria);
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(stdColumnQuery)) {
            while (iterator.hasNext()) {
                Map<String, Integer> calculatedMap = new HashMap();
                Object[] row = (Object[]) iterator.next();
                String stdOid = (String) row[0];
                int yog = ((BigDecimal) row[1]).intValue();
                String grade9Cohort = (String) row[2];
                int grade9CohortAsInt;
                int yearsInHighSchool;
                int yearsInHighSchoolSimplified;
                calculatedMap.put(OntarioAlias.CONST_G9C_CALC_MAP_YOG, yog);
                if (!StringUtils.isBlank(grade9Cohort)) {
                    grade9CohortAsInt = Math.abs(schoolYearsMap.get(grade9Cohort).getSequenceNumber());
                    calculatedMap.put(OntarioAlias.CONST_G9C_CALC_MAP_GRADE9COHORT, grade9CohortAsInt);
                    yearsInHighSchool = (currentYearContext.getSchoolYear() - grade9CohortAsInt) < 0 ? 0
                            : currentYearContext.getSchoolYear() - grade9CohortAsInt;
                    // all students that have been in HS for over 4 years are compounded into one
                    // simplified group.
                    yearsInHighSchoolSimplified = yearsInHighSchool < 4 ? yearsInHighSchool : 4;
                    calculatedMap.put(OntarioAlias.CONST_G9C_CALC_MAP_YEARS_HS, yearsInHighSchool);
                    calculatedMap.put(OntarioAlias.CONST_G9C_CALC_MAP_YEARS_HS_SIMP, yearsInHighSchoolSimplified);
                }
                grade9CohortCalculatedByStdOidMap.put(stdOid, calculatedMap);
            }
        }

        return grade9CohortCalculatedByStdOidMap;
    }

    /**
     * Builds a map by school of students enrolled on an effective date It will
     * count students with an enrollment record of E or S for a particular school
     * that do not have a W record after the E or S record.
     *
     * "Y" (YOG_CHANGE) records are not considered.
     *
     * Arrival Status = "Arrival" is required to be accounted on the final returning
     * list.
     *
     * It won't take in consideration a student current school association.
     *
     * @param broker X2Broker
     * @param effectiveDate PlainDate
     * @param schoolOids ArrayList&lt;String&gt;
     * @param context DistrictSchoolYearContext
     * @return Map&lt;sklOid, HashMap&lt;stdOid, StudentEnrollment&gt;&gt;
     */
    public static Map<String, HashMap<String, StudentEnrollment>> getStudentsEnrolledOnEffectiveDateBySchool(
                                                                                                             X2Broker broker,
                                                                                                             PlainDate effectiveDate,
                                                                                                             ArrayList<String> schoolOids,
                                                                                                             DistrictSchoolYearContext context) {
        Map<String, HashMap<String, StudentEnrollment>> schoolOidToStudentOidForEffectiveDate =
                new HashMap<String, HashMap<String, StudentEnrollment>>();
        for (String schoolOid : schoolOids) {
            X2Criteria latestEnrolmentCriteria = new X2Criteria();
            latestEnrolmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, schoolOid);
            // ignore "Y" records as they cannot exist without a proper E or S record
            latestEnrolmentCriteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.YOG_CHANGE);
            // add condition for context dates
            latestEnrolmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    context.getStartDate());
            // use end date, 1 has been subtracted if last day of school as that should not
            // be
            // included
            latestEnrolmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, effectiveDate);
            BeanQuery latestEnrolmentQuery = new BeanQuery(StudentEnrollment.class, latestEnrolmentCriteria, true,
                    true);
            latestEnrolmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            latestEnrolmentQuery.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

            Map<String, StudentEnrollment> latestEnrolmentMap = broker.getMapByQuery(latestEnrolmentQuery,
                    StudentEnrollment.COL_STUDENT_OID, 1000);
            HashMap<String, StudentEnrollment> ontarioEnrolmentMap = new HashMap<String, StudentEnrollment>();
            for (String stdOid : latestEnrolmentMap.keySet()) {
                if (latestEnrolmentMap.get(stdOid).getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    // skip
                } else if (!StringUtils.isBlank(latestEnrolmentMap.get(stdOid).getStudentOid())) {
                    // Skip students with arrival status = No Show, Expected or NULL.
                    if ((StringUtils.isEqualsIgnoreCase(
                            (String) latestEnrolmentMap.get(stdOid)
                            .getFieldValueByAlias(OntarioAlias.ALIAS_ENR_ARRIVAL_STATUS),
                            OntarioAlias.CONST_ARRIVAL_STATUS_ARRIVED))) {
                        ontarioEnrolmentMap.put(stdOid, latestEnrolmentMap.get(stdOid));
                    }
                }
            }
            schoolOidToStudentOidForEffectiveDate.put(schoolOid, ontarioEnrolmentMap);
        }
        return schoolOidToStudentOidForEffectiveDate;
    }

    /**
     * Populate map with grade scale.
     *
     * @param broker the broker
     * @return Map
     */
    public static Map<String, BigDecimal> getGradeScaleMap(X2Broker broker) {
        Map<String, BigDecimal> gradeScaleMap = new HashMap<>();
        String[] columns = new String[] {SisBeanPaths.GRADE_SCALE_GRADE_DEFINITION.gradeScaleOid().getPath(),
                "min(" + SisBeanPaths.GRADE_SCALE_GRADE_DEFINITION.gradeCutoffValue().getPath() + ")"};

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.GRADE_SCALE_GRADE_DEFINITION.creditIndicator().getPath(), "1");

        ColumnQuery columnQuery = new ColumnQuery(GradeScaleGradeDefinition.class, columns, criteria);
        columnQuery.addGroupBy(SisBeanPaths.GRADE_SCALE_GRADE_DEFINITION.gradeScaleOid().getPath());
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(columnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                gradeScaleMap.put((String) row[0], (BigDecimal) row[1]);
            }
        }
        return gradeScaleMap;
    }

    /**
     * Gets a map based on OLC/CCL transcripts by student. Each student will have an
     * entry with a singleton map detailing success or failure (if not transcript
     * found) plus the language the student is proficient on based on the course
     * prefix.
     * <li>1 = English
     * <li>2 = French <br>
     * <b>DO NOT CALL THIS METHOD WITHIN A LOOP</b>
     *
     * @param stdOidsSubQuery SubQuery
     * @param dictionary DataDictionary
     * @param broker X2Broker
     * @return Map
     */
    public static Map<String, Map<Boolean, String>> gestStudentOlcCclDataMap(SubQuery stdOidsSubQuery,
                                                                             DataDictionary dictionary,
                                                                             X2Broker broker) {
        Map<String, Map<Boolean, String>> studentOLC_DataMap = new HashMap();
        Map<String, BigDecimal> gradeScaleMap = getGradeScaleMap(broker);
        final String ENGLISH_COURUSES_PREFIX_CONST = "OLC";
        final String FRENCH_COURUSES_PREFIX_CONST = "CCL";
        DataDictionaryField transcriptCourseRepeated = dictionary
                .findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_TRN_COURSE_REPEATED);
        DataDictionaryField transcriptDateCompleted = dictionary
                .findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_TRN_DATE_COMPLETED);
        String[] column = new String[] {SisBeanPaths.STUDENT_TRANSCRIPT.studentOid().getPath(), // 0
                SisBeanPaths.STUDENT_TRANSCRIPT.finalGrade().getPath(), // 1
                SisBeanPaths.STUDENT_TRANSCRIPT.transcriptDefinition().transcriptColumnDefinitions().gradeScaleOid()
                .getPath(), // 2
                transcriptCourseRepeated.getJavaName(), // 3
                transcriptDateCompleted.getJavaName(), // 4
                SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().number().getPath(), // 5
        };

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisBeanPaths.STUDENT_TRANSCRIPT.studentOid().getPath(), stdOidsSubQuery);
        criteria.addEqualTo(SisBeanPaths.STUDENT_TRANSCRIPT.transcriptDefinition().transcriptColumnDefinitions()
                .dataFieldConfig().dataFieldOid().getPath(), "trnFinalGrade");
        criteria.addGreaterThan(SisBeanPaths.STUDENT_TRANSCRIPT.totalCredit().getPath(), BigDecimal.ZERO);
        X2Criteria bothCoursesCriteria = new X2Criteria();
        X2Criteria olcCoursesCriteria = new X2Criteria();
        olcCoursesCriteria.addBeginsWithIgnoreCase(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().number().getPath(),
                ENGLISH_COURUSES_PREFIX_CONST);
        X2Criteria cclCoursesCriteria = new X2Criteria();
        cclCoursesCriteria.addBeginsWithIgnoreCase(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().number().getPath(),
                FRENCH_COURUSES_PREFIX_CONST);
        bothCoursesCriteria.addAndCriteria(olcCoursesCriteria);
        bothCoursesCriteria.addOrCriteria(cclCoursesCriteria);
        criteria.addAndCriteria(bothCoursesCriteria);
        ColumnQuery columnQuery = new ColumnQuery(Transcript.class, column, criteria);
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(columnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                String cskCourseNumber = (String) row[5];
                // String finalGradeString = (String) row[1];
                BigDecimal finalGrade = BigDecimal.ZERO;
                if (StringUtils.isNumeric((String) row[1])) {
                    finalGrade = BigDecimal
                            .valueOf(Double.parseDouble(!StringUtils.isBlank((String) row[1]) ? (String) row[1] : "0"));
                }
                BigDecimal minCreditGrade = gradeScaleMap.get(row[2]);

                // bypass repeated transcript courses
                String courseRepeated = null;
                if (!StringUtils.isBlank((String) row[3])) {
                    courseRepeated = row[3].toString();
                }
                if ((!StringUtils.isEmpty(courseRepeated)) && (courseRepeated.equals("R"))) {
                    continue;// skip to next iterator row
                }

                if (finalGrade != null && minCreditGrade != null && finalGrade.compareTo(minCreditGrade) >= 0) {
                    studentOLC_DataMap.put(studentOid, Collections.singletonMap(Boolean.TRUE,
                            cskCourseNumber.startsWith(ENGLISH_COURUSES_PREFIX_CONST) ? "1" : "2"));
                } else {
                    studentOLC_DataMap.put(studentOid, Collections.singletonMap(Boolean.FALSE, ""));
                }
            }
        }
        return studentOLC_DataMap;
    }

    /**
     * Gets a map with OSSLT Assessment results by student. Each student will have
     * an entry with a singleton map detailing success or failure of the assessment
     * plus the language the student if proficient on when present.
     * <li>1 = English
     * <li>2 = French
     * <p>
     * <b>DO NOT CALL THIS METHOD WITHIN A LOOP</b>
     *
     * @param stdOidsSubQuery SubQuery
     * @param broker X2Broker
     * @return Map&lt;String(stdOid), Map&lt;Boolean(successful),
     *         String(language)&gt;&gt;
     */
    public static Map<String, Map<Boolean, String>> getStudentOSSLTDataMap(SubQuery stdOidsSubQuery, X2Broker broker) {
        Map<String, Map<Boolean, String>> studentOSSLT_DataMap = new HashMap();
        AssessmentDefinition assessmentDefinition = broker.getBeanByOid(AssessmentDefinition.class,
                OntarioAlias.CONST_ASD_OSSLT_OID);
        DataDictionary assessDataDictionary = DataDictionary.getDistrictDictionary(assessmentDefinition,
                broker.getPersistenceKey());
        String ossltLanguageJavaName = assessDataDictionary
                .findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_ASM_OSSLT_LANGUAGE).getJavaName();
        String asmOssltOutComeJavaName = assessDataDictionary
                .findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_ASM_OSSLT_RESULT).getJavaName();
        String[] columns = new String[] {SisBeanPaths.STUDENT_ASSESSMENT.studentOid().getPath(),
                SisBeanPaths.STUDENT_ASSESSMENT.getPath() + PATH_DELIMITER + asmOssltOutComeJavaName,
                SisBeanPaths.STUDENT_ASSESSMENT.getPath() + PATH_DELIMITER + ossltLanguageJavaName};
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.STUDENT_ASSESSMENT.assessmentDefinitionOid().getPath(),
                OntarioAlias.CONST_ASD_OSSLT_OID);
        List<String> studentOidList = (List<String>) broker.getSubQueryCollectionByQuery(stdOidsSubQuery);
        ParameterSelectionHandler.addParameterSafeOIDList(criteria, broker, studentOidList, 0,
                SisBeanPaths.STUDENT_ASSESSMENT.studentOid().getPath());
        ColumnQuery columnQuery = new ColumnQuery(StudentAssessment.class, columns, criteria);
        columnQuery.addOrderByAscending(SisBeanPaths.STUDENT_ASSESSMENT.date().getPath());
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(columnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                String ossltStatus = (String) row[1];
                String ossltLanguage = (String) row[2];
                if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(ossltStatus)) {
                    studentOSSLT_DataMap.put(studentOid,
                            Collections.singletonMap(Boolean.TRUE, row[2] != null ? ossltLanguage : ""));
                } else {
                    studentOSSLT_DataMap.put(studentOid, Collections.singletonMap(Boolean.FALSE, ""));
                }
            }
        }
        return studentOSSLT_DataMap;
    }

    /**
     * Gets the students primary diploma. <br/>
     * <b>DO NOT CALL THIS METHOD WITHIN A LOOP</b>
     *
     * @param stdOidsSubQuery SubQuery
     * @param broker X2Broker
     * @return Map&lt;studentOid, GraduationStudentProgram&gt;
     */
    public static Map<String, GraduationStudentProgram> getStudentsPrimaryDiploma(SubQuery stdOidsSubQuery,
                                                                                  X2Broker broker) {
        Map<String, GraduationStudentProgram> studentToMainGradDiplomaMap = new HashMap();

        X2Criteria stdGradProgramCriteria = new X2Criteria();
        stdGradProgramCriteria.addEqualTo(GraduationStudentProgram.COL_PRIMARY_INDICATOR, true);
        List<String> studentOidList = (List<String>) broker.getSubQueryCollectionByQuery(stdOidsSubQuery);
        ParameterSelectionHandler.addParameterSafeOIDList(stdGradProgramCriteria, broker, studentOidList, 0,
                GraduationStudentProgram.COL_STUDENT_OID);
        BeanQuery stdGradProgQuery = new BeanQuery(GraduationStudentProgram.class, stdGradProgramCriteria, true);
        studentToMainGradDiplomaMap = broker.getMapByQuery(stdGradProgQuery, GraduationStudentProgram.COL_STUDENT_OID,
                2048);

        return studentToMainGradDiplomaMap;
    }

    /**
     * Return a lookup map to identify the columns that are setup for a particular
     * transcript based on:
     * <li>relTrnMstOid.relMstTrmOid
     * <li>trnGtdOID
     *
     * <p>
     * Sample use:
     * <code><br>String foundGradeFieldOnTranscriptTable = myLookupMap.get(transcriptObj.getTranscriptDefinitionOid(), transcriptObj.getMasterSchedule().getGradeTermDefinitionOid())
     * <br>transcriptObj.getFieldValueByBeanPath(foundGradeFieldOnTranscriptTable)</code>
     *
     * <br/>
     * <br>
     * <b>Returned map structure:</b>
     * Map&lt;trmOid,Map&lt;gtdOid,Map&lt;ternNumber,trn_javaFieldPath>>> <br>
     * <b>Note:</b> This method could be easily used for one school or all schools
     * as the primary key is trmOid, therefore it is unique for the school, context
     * year.
     *
     * @param context the context
     * @param schoolOids the school oids
     * @param activeSchOids the active sch oids
     * @param broker the broker
     * @return the transcript friendly grades location lookup map
     * @throws NumberFormatException the number format exception
     */
    public static Map<String, Map<String, Map<Integer, String>>> getTranscriptFriendlyGradesLocationLookupMap(
                                                                                                              DistrictSchoolYearContext context,
                                                                                                              Collection<String> schoolOids,
                                                                                                              Collection<String> activeSchOids,
                                                                                                              X2Broker broker)
                                                                                                                      throws NumberFormatException {

        Map<Integer, Map<String, Map<String, Map<Integer, String>>>> termsPerYear_GtdOid_GtmOid_FldJavaNameMap =
                new HashMap();
        loadDistrictSetupTranscriptConfigLookupMap(termsPerYear_GtdOid_GtmOid_FldJavaNameMap, broker);

        Map<String, Map<String, List<String>>> trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap = new HashMap();

        loadTrmOidToGtdOidToGradeTermsCoverMap(termsPerYear_GtdOid_GtmOid_FldJavaNameMap, schoolOids, activeSchOids,
                trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap, context, broker);

        Map<String, Map<String, Map<Integer, String>>> finalLookupMap = new HashMap();
        for (String trmOid : trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap.keySet()) {
            Map<String, Map<Integer, String>> gtdOidToFldNameByTermNumber = new HashMap();
            for (String gtdOid : trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap.get(trmOid).keySet()) {
                Map<Integer, String> ternNumberToJavaFieldMap = new HashMap();
                for (Integer termsPerYear : termsPerYear_GtdOid_GtmOid_FldJavaNameMap.keySet()) {
                    if (termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(termsPerYear).containsKey(gtdOid)) {
                        for (String gtmOid : termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(termsPerYear).get(gtdOid)
                                .keySet()) {
                            List<String> gradingTermsCoveredMapString =
                                    trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap
                                    .get(trmOid).get(gtdOid);

                            for (String checkmarkToTermNumber : gradingTermsCoveredMapString) {
                                if (termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(termsPerYear).get(gtdOid).get(gtmOid)
                                        .containsKey(Integer.valueOf(checkmarkToTermNumber))) {
                                    String trnJavaField = termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(termsPerYear)
                                            .get(gtdOid).get(gtmOid).get(Integer.valueOf(checkmarkToTermNumber));
                                    ternNumberToJavaFieldMap.put(Integer.valueOf(checkmarkToTermNumber), trnJavaField);
                                }
                            }
                        }
                    }

                }
                gtdOidToFldNameByTermNumber.put(gtdOid, ternNumberToJavaFieldMap);
            }
            finalLookupMap.put(trmOid, gtdOidToFldNameByTermNumber);
        }

        return finalLookupMap;
    }

    /**
     * Load trm oid to gtd oid to grade terms cover map.
     *
     * @param termsPerYear_GtdOid_GtmOid_FldJavaNameMap the terms per year gtd oid gtm oid fld java
     *        name map
     * @param schoolOids the school oids
     * @param activeSchOids the active sch oids
     * @param trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap the trm oid T O gtd oid tograde
     *        terms cover map position of ones map
     * @param context the context
     * @param broker the broker
     */
    public static void loadTrmOidToGtdOidToGradeTermsCoverMap(
                                                              Map<Integer, Map<String, Map<String, Map<Integer, String>>>> termsPerYear_GtdOid_GtmOid_FldJavaNameMap,
                                                              Collection<String> schoolOids,
                                                              Collection<String> activeSchOids,
                                                              Map<String, Map<String, List<String>>> trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap,
                                                              DistrictSchoolYearContext context,
                                                              X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.SCHEDULE_TERM.schedule().districtContextOid().getPath(), context.getOid());
        criteria.addIn(SisBeanPaths.SCHEDULE_TERM.schedule().schoolOid().getPath(), schoolOids);
        criteria.addEqualTo(
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().districtContextOid().getPath(),
                context.getOid());
        criteria.addIn(SisBeanPaths.SCHEDULE_TERM.scheduleOid().getPath(), activeSchOids);

        String[] columns = {SisBeanPaths.SCHEDULE_TERM.oid().getPath(), // 0
                SisBeanPaths.SCHEDULE_TERM.code().getPath(), // 1
                SisBeanPaths.SCHEDULE_TERM.gradeTermMap().getPath(), // 2
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTermOid().getPath(), // 3
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTerm().gradeTermNum().getPath(), // 4
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTerm().gradeTermDefinition()
                .gradeTermsPerYear().getPath(), // 5
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTerm().gradeTermDefinitionOid()
                .getPath(), // 6
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTerm().gradeTermId().getPath(), // 7
        };

        Map<String, List<String>> gtdOidTogradeTermsCoverMapPositionOfOnesMap = new HashMap();

        ColumnQuery columnQuery = new ColumnQuery(ScheduleTerm.class, columns, criteria);
        columnQuery.addOrderByAscending(
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTermOid().getPath());
        columnQuery.addOrderByAscending(
                SisBeanPaths.SCHEDULE_TERM.schedule().school().gradeTermDates().gradeTerm().gradeTermNum().getPath());
        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(columnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String trm_oid = (String) row[0];
                String trm_grade_term_cover_map = (String) row[2];
                String gtm_oid = (String) row[3];
                int gtf_grade_terms_per_year = ((BigDecimal) row[5]).intValue();

                ArrayList<String> gradeTermsCoverMapList = StringUtils
                        .convertDelimitedStringToList(trm_grade_term_cover_map, ',');
                for (String gradeTermsCoverMapString : gradeTermsCoverMapList) {
                    if (gradeTermsCoverMapString.length() == gtf_grade_terms_per_year) {
                        int i = 1;
                        List<String> gradeTermsCoverMapPositionOfOnesList = new LinkedList();
                        for (String checkmark : siplitStringIntoSingleCharacters(gradeTermsCoverMapString)) {
                            if (checkmark.equalsIgnoreCase("1")) {
                                gradeTermsCoverMapPositionOfOnesList.add(String.valueOf(i));
                            }
                            i++;
                        }

                        if (termsPerYear_GtdOid_GtmOid_FldJavaNameMap.containsKey(gtf_grade_terms_per_year)) {
                            String gtdOidFromMap = "";
                            for (String gtdOid : termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(gtf_grade_terms_per_year)
                                    .keySet()) {
                                gtdOidFromMap = gtdOid;
                            }

                            for (Map<String, Map<Integer, String>> gtmOidTO_gtm_grade_term_number_TO_FldJavaNameMap : termsPerYear_GtdOid_GtmOid_FldJavaNameMap
                                    .get(gtf_grade_terms_per_year).values()) {
                                if (gtmOidTO_gtm_grade_term_number_TO_FldJavaNameMap.containsKey(gtm_oid)) {
                                    for (Entry<Integer, String> gtmGradetermNumber_To_FldJavaNameMap : gtmOidTO_gtm_grade_term_number_TO_FldJavaNameMap
                                            .get(gtm_oid).entrySet()) {
                                        if (trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap
                                                .containsKey(trm_oid)) {
                                            trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap.get(trm_oid)
                                            .put(gtdOidFromMap, gradeTermsCoverMapPositionOfOnesList);
                                        } else {
                                            gtdOidTogradeTermsCoverMapPositionOfOnesMap = new HashMap();

                                            gtdOidTogradeTermsCoverMapPositionOfOnesMap.put(gtdOidFromMap,
                                                    gradeTermsCoverMapPositionOfOnesList);

                                            trmOid_TO_gtdOidTogradeTermsCoverMapPositionOfOnesMap.put(trm_oid,
                                                    gtdOidTogradeTermsCoverMapPositionOfOnesMap);
                                        }
                                    }
                                }
                            }
                        }

                    }
                }
            }
        }
    }

    /**
     * Split a string into single characters.
     *
     * @param s String
     * @return String[]
     */
    public static String[] siplitStringIntoSingleCharacters(String s) {
        return s.split("(?!^)");
    }

    /**
     * Gets the district setup transcript config lookup map.
     *
     * <br>
     * Returning map structure: <br>
     * <code>&lt;termsPerYear, &lt;gtdOid, &lt;gtmOid, &lt;ternNumber, trnJavaFieldPath>>>></code>
     *
     * @param termsPerYear_GtdOid_GtmOid_FldJavaNameMap
     *        Map<Integer,Map<String,Map<String,Map<Integer,String>>>>
     * @param broker the broker
     * @return void
     */
    private static void loadDistrictSetupTranscriptConfigLookupMap(
                                                                   Map<Integer, Map<String, Map<String, Map<Integer, String>>>> termsPerYear_GtdOid_GtmOid_FldJavaNameMap,
                                                                   X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().columnTypeCode().getPath(),
                TranscriptColumnDefinition.COLUMN_TYPE_TERM_AVERAGE);
        criteria.addEqualTo(SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().reportType().getPath(),
                TranscriptColumnDefinition.GRADE_TYPE_TERM);

        String[] columns = {SisBeanPaths.DATA_FIELD_CONFIG.dataField().javaName().getPath(), // 0
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().transcriptDefinition()
                .transcriptDefinitionDesc().getPath(), // 1
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm().gradeTermId().getPath(), // 2
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm().gradeTermNum().getPath(), // 3
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm().gradeTermDefinition()
                .gradeTermsPerYear().getPath(), // 4
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().transcriptDefinition().oid().getPath(), // 5
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm().oid().getPath(), // 6
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm().gradeTermId().getPath(), // 7
        };
        ColumnQuery columnQuery = new ColumnQuery(DataFieldConfig.class, columns, criteria);
        columnQuery.addOrderByAscending(SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm()
                .gradeTermDefinition().gradeTermsPerYear().getPath());
        columnQuery.addOrderByAscending(SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions()
                .transcriptDefinition().transcriptDefinitionDesc().getPath());
        columnQuery.addOrderByAscending(
                SisBeanPaths.DATA_FIELD_CONFIG.transcriptColumnDefinitions().gradeTerm().gradeTermId().getPath());

        try (ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(columnQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                int gtf_grade_terms_per_year = ((BigDecimal) row[4]).intValue();
                String gtd_oid = (String) row[5];
                int gtm_grade_term_number = ((BigDecimal) row[3]).intValue();
                String gtm_oid = (String) row[6];
                String fld_java_name = (String) row[0];

                if (termsPerYear_GtdOid_GtmOid_FldJavaNameMap.containsKey(gtf_grade_terms_per_year)
                        && termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(gtf_grade_terms_per_year)
                        .containsKey(gtd_oid)) {
                    Map<Integer, String> termNumberToFldJavaNameMap = new HashMap();
                    termNumberToFldJavaNameMap.put(gtm_grade_term_number, fld_java_name);
                    termsPerYear_GtdOid_GtmOid_FldJavaNameMap.get(gtf_grade_terms_per_year).get(gtd_oid).put(gtm_oid,
                            termNumberToFldJavaNameMap);
                } else {
                    Map<String, Map<String, Map<Integer, String>>> gtdOid_GtmOid_FldJavaNameMap = new HashMap();
                    Map<String, Map<Integer, String>> gtmOid_fldJavaNameMap = new HashMap();
                    Map<Integer, String> ternNumberToFldJavaNameMap = new HashMap();
                    ternNumberToFldJavaNameMap.put(gtm_grade_term_number, fld_java_name);
                    gtmOid_fldJavaNameMap.put(gtm_oid, ternNumberToFldJavaNameMap);
                    gtdOid_GtmOid_FldJavaNameMap.put(gtd_oid, gtmOid_fldJavaNameMap);
                    termsPerYear_GtdOid_GtmOid_FldJavaNameMap.put(gtf_grade_terms_per_year,
                            gtdOid_GtmOid_FldJavaNameMap);
                }
            }
        }
    }

    /**
     * Gets waiver Map with two keys:
     * <li>Community Involvement Hours
     * <li>Provincial Secondary School Literacy Requirement.
     *
     * @param studentWaivers the student waivers
     * @return the waiver info
     */
    public static Map<String, Boolean> getWaiverInfo(Collection<GraduationStudentWaiver> studentWaivers) {
        Map<String, Boolean> waiversMap = new HashMap();

        for (GraduationStudentWaiver gsw : studentWaivers) {
            if (gsw.getRequirement().getCode()
                    .equalsIgnoreCase(OntarioAlias.WAIVER_OSSD_1999_COMMUNITY_SERVICE_INVOLVEMENT_REQ_CODE)) {
                waiversMap.put(OntarioAlias.WAIVER_OSSD_1999_COMMUNITY_SERVICE_INVOLVEMENT_REQ_CODE, true);
            }
            if (gsw.getRequirement().getCode()
                    .equalsIgnoreCase(OntarioAlias.WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT)) {
                waiversMap.put(OntarioAlias.WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT, true);
            }
        }

        return waiversMap;
    }

    /**
     * Gets students Provincial Secondary School Literacy Requirement This method
     * analyzes the student OSSLT Assessment, OLC/CCL course, Graduation DIploma and
     * Waivers, based on this it could return one of the following three results by
     * student matching the simplified classification found on the OST.
     * <li>Completed (enrolled OSSD 1999 and has successfully completed req.)
     * <li>Not Completed (enrolled OSSD 1999 and has not completed requirement)
     * <li>N/A (student is not enrolled on OSSD 1999) <br/>
     * <p/>
     * <b>DO NOT CALL THIS METHOD WITHIN A LOOP</b>
     *
     * @param stdOidsSubQuery SubQuery
     * @param dictionary DataDictionary
     * @param broker X2Broker
     * @return Map&lt;stdOid, String&gt;
     */
    public static Map<String, String> getStudentsOSSLReqStatusSimplified(SubQuery stdOidsSubQuery,
                                                                         DataDictionary dictionary,
                                                                         X2Broker broker) {
        Map<String, String> studentOSSLTMap = new HashMap();
        Map<String, Map<Boolean, String>> ossltMap = OntarioToolHelper.getStudentOSSLTDataMap(stdOidsSubQuery, broker);
        Map<String, Map<Boolean, String>> olcCclMap = OntarioToolHelper.gestStudentOlcCclDataMap(stdOidsSubQuery,
                dictionary, broker);
        Map<String, GraduationStudentProgram> gradProgramByStdOidMap = OntarioToolHelper
                .getStudentsPrimaryDiploma(stdOidsSubQuery, broker);

        Collection<String> studentsInSubquery = broker.getSubQueryCollectionByQuery(stdOidsSubQuery);
        for (String stdOid : studentsInSubquery) {
            if (ossltMap.containsKey(stdOid) && ossltMap.get(stdOid).containsKey(true)) {
                studentOSSLTMap.put(stdOid, "Completed");
            } else if (olcCclMap.containsKey(stdOid) && olcCclMap.get(stdOid).containsKey(true)) {
                studentOSSLTMap.put(stdOid, "Completed");
            } else if (gradProgramByStdOidMap.containsKey(stdOid) && !OntarioAlias.OSSD_1999_GPR_OID
                    .equals(gradProgramByStdOidMap.get(stdOid).getProgramStudiesOid())) {
                studentOSSLTMap.put(stdOid, "N/A");
            } else if (gradProgramByStdOidMap.containsKey(stdOid)
                    && getWaiverInfo(gradProgramByStdOidMap.get(stdOid).getStudentWaivers()).containsKey(
                            OntarioAlias.WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT)) {
                studentOSSLTMap.put(stdOid, "Completed");
            } else {
                studentOSSLTMap.put(stdOid, "Not Completed");
            }
        }

        return studentOSSLTMap;
    }

    /**
     * Returns a collection of student's SLP program participation info.
     *
     * @param stdOid student's OID
     * @param getSlpInfoAsOfDateMap - created by prior method
     * @return slpInfo
     */
    public static String[] getStudentSlpInfo(String stdOid, Map<String, String[]> getSlpInfoAsOfDateMap) {
        String[] slpInfoList = new String[2];

        if (getSlpInfoAsOfDateMap.get(stdOid) != null) {
            slpInfoList = getSlpInfoAsOfDateMap.get(stdOid);
        }

        return slpInfoList;
    }

    /**
     * Returns student native language from SLP if it exists.
     *
     * @param slpInfoList - created by prior method
     * @return nativeLang - from SLP
     */
    public static String getStudentSlpNativeLang(String[] slpInfoList) {
        String nativeLang = "";
        String slpInfo = slpInfoList[1];

        if (!StringUtils.isEmpty(slpInfo)) {
            String[] slpInfoSplit = slpInfo.split(",");
            String slpCode = slpInfoSplit[0];
            String slpType = OntarioAlias.CONST_EMPTY;
            if (slpInfoSplit.length > 1) {
                slpType = slpInfoSplit[1];
            }

            if ((!StringUtils.isEmpty(slpType)) && (slpType.equals(OntarioAlias.CONST_PGM_SLP_TYPE_NATIVE_LANG))
                    && (!StringUtils.isEmpty(slpCode))) {
                nativeLang = slpCode;
            }
        }

        return nativeLang;
    }

    /**
     * Returns student indicators (true or false) from SLP (false if do not exist).
     *
     * @param slpInfoList - created by prior method
     * @return List<Boolean> - Core, Immersion, Extended, Exempted
     */
    public static List<Boolean> getStudentSlpIndicators(String[] slpInfoList) {
        List<Boolean> slpIndicators = new ArrayList<Boolean>();

        // initialize output values
        Boolean slpCore = Boolean.FALSE;
        Boolean slpImmersion = Boolean.FALSE;
        Boolean slpExtended = Boolean.FALSE;
        Boolean slpExempted = Boolean.FALSE;

        // loop through slpInfo values
        String slpInfo = slpInfoList[0];

        if (!StringUtils.isEmpty(slpInfo)) {
            String[] slpInfoSplit = slpInfo.split(",");
            String slpCode = slpInfoSplit[0];
            if (!StringUtils.isEmpty(slpCode)) {
                if (slpCode.equals(OntarioAlias.CONST_PGM_SLP_CODE_CORE)) {
                    slpCore = Boolean.TRUE;
                } else if (slpCode.equals(OntarioAlias.CONST_PGM_SLP_CODE_IMMERSION)) {
                    slpImmersion = Boolean.TRUE;
                } else if (slpCode.equals(OntarioAlias.CONST_PGM_SLP_CODE_EXTENDED)) {
                    slpExtended = Boolean.TRUE;
                } else if (slpCode.equals(OntarioAlias.CONST_PGM_SLP_CODE_EXEMPTED)) {
                    slpExempted = Boolean.TRUE;
                }
            }
        }
        // set output list
        slpIndicators.add(slpCore);
        slpIndicators.add(slpImmersion);
        slpIndicators.add(slpExtended);
        slpIndicators.add(slpExempted);

        return slpIndicators;
    }

    /**
     * Returns student indicators (true or false) from SLP (false if do not exist).
     *
     * @param slpInfoList - created by prior method
     * @param secLangCodes the sec lang codes
     * @return List<Boolean> - Core, Immersion, Extended, Exempted
     */
    public static List<Boolean> getStudentSlpIndicators(String[] slpInfoList, Map<String, ReferenceCode> secLangCodes) {
        List<Boolean> slpIndicators = new ArrayList<Boolean>();

        // initialize output values
        Boolean slpCore = Boolean.FALSE;
        Boolean slpImmersion = Boolean.FALSE;
        Boolean slpExtended = Boolean.FALSE;
        Boolean slpExempted = Boolean.FALSE;

        // loop through slpInfo values
        String slpInfo = slpInfoList[0];
        if (!StringUtils.isEmpty(slpInfo)) {
            String[] slpInfoSplit = slpInfo.split(",");
            String slpCode = slpInfoSplit[0];

            if (!StringUtils.isEmpty(slpCode)) {
                String stateCode = secLangCodes.get(slpCode).getStateCode();
                if (!StringUtils.isEmpty(stateCode)) {
                    switch (stateCode) {
                        case "001":
                        case "006":
                            slpCore = Boolean.TRUE;
                            break;
                        case "002":
                            slpExtended = Boolean.TRUE;
                            break;
                        case "003":
                            slpImmersion = Boolean.TRUE;
                            break;
                    }
                }
            }
        }
        // set output list
        slpIndicators.add(slpCore);
        slpIndicators.add(slpImmersion);
        slpIndicators.add(slpExtended);
        slpIndicators.add(slpExempted);

        return slpIndicators;
    }

    /**
     * Get active schedule map for input list of school oids and context oid -
     * context oid.
     *
     * @param contextOid the context oid
     * @param broker the broker
     * @return the active schedule map
     */
    public static Map<String, Schedule> getActiveScheduleMap(String contextOid, X2Broker broker) {
        return getActiveScheduleMap(getAllActiveSchoolOids(broker), contextOid, null, null, broker);
    }

    /**
     * Get active schedule map for input list of school oids and context oid -
     * school oids list should have at least one oid - context oid.
     *
     * @param schoolOids the school oids
     * @param contextOid the context oid
     * @param broker the broker
     * @return the active schedule map
     */
    public static Map<String, Schedule> getActiveScheduleMap(Collection<String> schoolOids,
                                                             String contextOid,
                                                             X2Broker broker) {
        return getActiveScheduleMap(schoolOids, contextOid, null, null, broker);
    }

    /**
     * Get active schedule map for input list of school oids and start/end dates -
     * school oids list should have at least one oid - start/end dates.
     *
     * @param schoolOids the school oids
     * @param startDate the start date
     * @param endDate the end date
     * @param broker the broker
     * @return the active schedule map
     */
    public static Map<String, Schedule> getActiveScheduleMap(Collection<String> schoolOids,
                                                             PlainDate startDate,
                                                             PlainDate endDate,
                                                             X2Broker broker) {
        return getActiveScheduleMap(schoolOids, null, startDate, endDate, broker);
    }

    /**
     * Get active schedule map for input list of school oids and context oid or
     * start/end dates Called from either of a couple of public methods - school
     * oids list should have at least one oid - either context oid or start/end
     * dates must be populated.
     *
     * @param schoolOids the school oids
     * @param contextOid the context oid
     * @param startDate the start date
     * @param endDate the end date
     * @param broker the broker
     * @return the active schedule map
     */
    private static Map<String, Schedule> getActiveScheduleMap(Collection<String> schoolOids,
                                                              String contextOid,
                                                              PlainDate startDate,
                                                              PlainDate endDate,
                                                              X2Broker broker) {
        // initialize return map
        Map<String, Schedule> activeScheduleToSchoolMap = new HashMap<String, Schedule>();

        // validation return if no school oids
        if (((schoolOids == null) || (schoolOids.isEmpty()))
                // or either start/end dates are null
                || (((startDate == null) || (endDate == null))
                        // or context oid is not populated
                        && (StringUtils.isEmpty(contextOid)))) {
            return activeScheduleToSchoolMap;
        }

        // create criteria for active schedules
        X2Criteria skxCriteria = new X2Criteria();

        // add condition for school oids
        skxCriteria.addIn(SchoolScheduleContext.COL_SCHOOL_OID, schoolOids);

        // add condition for context oid
        if (!StringUtils.isEmpty(contextOid)) {
            skxCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, contextOid);
        }

        // add condition for start/end dates
        if ((startDate != null) && (endDate != null)) {
            skxCriteria.addLessOrEqualThan(
                    SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER + Schedule.COL_START_DATE, startDate);
            skxCriteria.addGreaterOrEqualThan(
                    SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER + Schedule.COL_END_DATE, endDate);
        }

        // create query
        QueryByCriteria query = new QueryByCriteria(SchoolScheduleContext.class, skxCriteria);
        query.addOrderByDescending(SchoolScheduleContext.COL_SCHOOL_OID);
        query.addOrderByDescending(
                SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER + Schedule.COL_START_DATE);

        // loop through schedules in descending order by date, get context from first
        // schedule
        QueryIterator iterator = broker.getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SchoolScheduleContext skx = (SchoolScheduleContext) iterator.next();
                String skxSchoolOid = skx.getSchoolOid();
                Schedule skxActiveSchedule = skx.getActiveSchedule();

                if (activeScheduleToSchoolMap.get(skxSchoolOid) == null) {
                    activeScheduleToSchoolMap.put(skxSchoolOid, skxActiveSchedule);
                }
            }
        } finally {
            iterator.close();
        }

        return activeScheduleToSchoolMap;
    }

    /**
     * Loads bell periods to bel oid, period name map.
     *
     * @param broker the broker
     * @return the map
     */
    public static Map<String, ScheduleBellPeriod> loadSchedBpeToBelOidPeriodNm(X2Broker broker) {
        Map<String, ScheduleBellPeriod> bpeTimeToBelOidPeriodNmMap = new HashMap<String, ScheduleBellPeriod>();

        // constants for only this method
        final String CONST_HYPHEN = "-";

        // create criteria/query to get bell schedule information - fetches for all
        // schools though
        // may not be needed
        X2Criteria bpeCriteria = new X2Criteria();

        QueryByCriteria bpeQuery = new QueryByCriteria(ScheduleBellPeriod.class, bpeCriteria);
        bpeQuery.addOrderByAscending(ScheduleBellPeriod.REL_BELL_SCHEDULE + PATH_DELIMITER + ScheduleBell.COL_OID);
        bpeQuery.addOrderByAscending(ScheduleBellPeriod.COL_NAME);

        // load map of schedule bell period to schedule bell oid/period name
        try (QueryIterator bpeIterator = broker.getIteratorByQuery(bpeQuery)) {
            // iterates through and saves into map
            while (bpeIterator.hasNext()) {
                ScheduleBellPeriod bpe = (ScheduleBellPeriod) bpeIterator.next();

                // get new values
                String belOid = bpe.getBellScheduleOid();
                String bpePeriodNm = bpe.getName();

                // load into map
                String keyField = belOid + CONST_HYPHEN + bpePeriodNm;
                bpeTimeToBelOidPeriodNmMap.put(keyField, bpe);
            }
        }

        return bpeTimeToBelOidPeriodNmMap;
    }

    /**
     * Loads class dates/days/time lists in order of class number to master schedule
     * maps.
     *
     * @param ctxOid the ctx oid
     * @param schoolOid - optional, if null will process entire
     *        district
     * @param subQueryMstOids - optional, if null will process all
     *        sections for other criteria
     * @param mstOidsList - optional, if null will process all
     *        students for other criteria
     * @param courseIncludeDays the course include days
     * @param courseIncludeTime the course include time
     * @param courseTrmDtToTrmOidMap - must be populated
     * @param gradeTermStartDate the grade term start date
     * @param gradeTermEndDate the grade term end date
     * @param bpeTimeToBelOidPeriodNmMap - must be populated
     * @param broker - X2Broker
     * @param locale the locale
     * @return List<Map> 1. classDtListToMstOidMap 2. classDayListToMstOidMap 3.
     *         classStartEndTimeToMstOidMap
     */
    public static List<Map> loadSchedClassDateDayTimeListsToMstOid(String ctxOid,
                                                                   String schoolOid,
                                                                   SubQuery subQueryMstOids,
                                                                   List<String> mstOidsList,
                                                                   boolean courseIncludeDays,
                                                                   boolean courseIncludeTime,
                                                                   Map<String, ScheduleTermDate> courseTrmDtToTrmOidMap,
                                                                   PlainDate gradeTermStartDate,
                                                                   PlainDate gradeTermEndDate,
                                                                   Map<String, ScheduleBellPeriod> bpeTimeToBelOidPeriodNmMap,
                                                                   X2Broker broker,
                                                                   Locale locale) {
        // initialize output maps
        Map<String, List<PlainDate>> classDtListToMstOidMap = new HashMap<String, List<PlainDate>>();
        Map<String, List<String>> classDayListToMstOidMap = new HashMap<String, List<String>>();
        Map<String, List<PlainTime>> classStartEndTimeToMstOidMap = new HashMap<String, List<PlainTime>>();

        // constants for only this method
        // Number of csd dates looked at to see days courses meets
        final int CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP = 10;
        Calendar calendar = Calendar.getInstance(locale);
        final String CONST_HYPHEN = "-";
        final String CONST_COMMA = ",";

        /*
         * get the master schedules to ScheduleDays that selected sections meet also
         * lists of school oids, day numbers to use in next query
         */
        Map<String, List<String>> mstOidsToSklOidDayNumMap = new HashMap<String, List<String>>();
        Map<String, List<String>> periodNmsToMstOidMap = new HashMap<String, List<String>>();
        List<String> mstSklOidsToRestrictSelection = new ArrayList<String>();
        List<Integer> mstDayNumsToRestrictSelection = new ArrayList<Integer>();

        // create criteria/query to get schedule days for mstOids
        X2Criteria mtxCriteria = new X2Criteria();

        // condition for school year
        mtxCriteria.addEqualTo(MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE
                + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID, ctxOid);

        // condition for school oid
        if (!StringUtils.isEmpty(schoolOid)) {
            mtxCriteria.addEqualTo(MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER
                    + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID, schoolOid);
        }

        // condition for section oids sub query
        if (subQueryMstOids != null) {
            mtxCriteria.addIn(
                    MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID,
                    subQueryMstOids);
        }

        // condition for section oids list
        if (mstOidsList != null) {
            mtxCriteria.addIn(
                    MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID,
                    mstOidsList);
        }

        String[] mtxColumns = new String[] {
                // school oid
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                // schedule day number
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.REL_SCHEDULE_DAY
                + PATH_DELIMITER + ScheduleDay.COL_NUMBER,
                // schedule period name
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.REL_SCHEDULE_PERIOD
                + PATH_DELIMITER + SchedulePeriod.COL_NAME,
                // master schedule oid
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID};
        ColumnQuery mtxQuery = new ColumnQuery(MasterScheduleMatrix.class, mtxColumns, mtxCriteria);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_DAY + PATH_DELIMITER + ScheduleDay.COL_NUMBER);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_PERIOD + PATH_DELIMITER + SchedulePeriod.COL_NAME);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID);

        // load map of schedule days to mstOids
        String mtxQuerySklOidDayNumPrev = null;
        List<String> mstOidsForSklOidDayNum = new ArrayList<String>();
        try (ReportQueryIterator mtxIterator = broker.getReportQueryIteratorByQuery(mtxQuery)) {
            // iterates through using sort key which is school/day/period/mstOid
            while (mtxIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) mtxIterator.next();
                String sklOid = data[0].toString();
                Integer dayNum = Integer.valueOf(((BigDecimal) data[1]).intValue());
                String periodNm = ((String) data[2]);
                String mstOid = data[3].toString();
                String sklOidDayNum = sklOid + dayNum.toString();

                // if new key school/day, write section list for prev key
                if ((!(mtxQuerySklOidDayNumPrev == null)) && (!sklOidDayNum.equals(mtxQuerySklOidDayNumPrev))) {
                    mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);

                    // initialize list
                    mstOidsForSklOidDayNum = new ArrayList<String>();
                }
                mtxQuerySklOidDayNumPrev = sklOidDayNum;

                // save sklOids, day numbers in list
                if (!mstSklOidsToRestrictSelection.contains(sklOid)) {
                    mstSklOidsToRestrictSelection.add(sklOid);
                }
                if (!mstDayNumsToRestrictSelection.contains(dayNum)) {
                    mstDayNumsToRestrictSelection.add(dayNum);
                }

                // add section to list for school/day
                // may be duplicates including if multiple periods so saved only once
                if (!mstOidsForSklOidDayNum.contains(mstOid)) {
                    mstOidsForSklOidDayNum.add(mstOid);
                }

                // add period names by mstOid
                List<String> periodNmsForMst = periodNmsToMstOidMap.get(mstOid);
                if (periodNmsForMst == null) {
                    periodNmsForMst = new ArrayList<String>();
                }
                if (!periodNmsForMst.contains(periodNm)) {
                    periodNmsForMst.add(periodNm);
                }
                periodNmsToMstOidMap.put(mstOid, periodNmsForMst);
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            AppGlobals.getLog().log(Level.WARNING,
                    "Error getting section dates/days/times, MasterScheduleMatrix lookup: " + e);
        }

        // write section list for last school/day
        if (!(mtxQuerySklOidDayNumPrev == null)) {
            mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);
        }

        /*
         * get SchoolCalendarDates(full year)/days-of-week lists to selected sections
         * also get bell schedule by school/day number (assuming 1 to 1 relation)
         */
        Map<String, List<PlainDate>> classDtFullYrToMstOidMap = new HashMap<String, List<PlainDate>>();
        Map<String, String> belOidToMstOidMap = new HashMap<String, String>();

        // create criteria/query to get school calendar dates for mstOids
        X2Criteria csdCriteria = new X2Criteria();

        // condition for school year/schools/day numbers for selected sections
        csdCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                ctxOid);
        csdCriteria.addIn(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                mstSklOidsToRestrictSelection);
        csdCriteria.addIn(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER, mstDayNumsToRestrictSelection);

        String[] csdColumns = new String[] {
                // school oid
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                // schedule day number
                SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER,
                // date
                SisSchoolCalendarDate.COL_DATE,
                // schedule bell oid
                SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID};
        ColumnQuery csdQuery = new ColumnQuery(SisSchoolCalendarDate.class, csdColumns, csdCriteria);
        csdQuery.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        // load map of schedule days to mstOids
        String csdQuerySklOidDayNumPrev = null;
        Collection<PlainDate> csdDatesForSklOidDayNum = new ArrayList<PlainDate>();
        Collection<String> csdDaysForSklOidDayNum = new ArrayList<String>();
        String belOidForSklOidDayNum = OntarioAlias.CONST_EMPTY;
        try (ReportQueryIterator csdIterator = broker.getReportQueryIteratorByQuery(csdQuery)) {
            while (csdIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) csdIterator.next();
                String sklOid = data[0].toString();
                Integer dayNum = Integer.valueOf(((BigDecimal) data[1]).intValue());
                PlainDate csdDate = new PlainDate((Timestamp) data[2]);
                String sklOidDayNum = sklOid + dayNum.toString();
                String belOid = OntarioAlias.CONST_EMPTY;
                if (data[3] != null) {
                    belOid = data[3].toString();
                }

                // if new key school/day, write class date list for prev key
                if ((!(csdQuerySklOidDayNumPrev == null)) && (!sklOidDayNum.equals(csdQuerySklOidDayNumPrev))) {
                    // add school dates/days to lists to mstOid
                    List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
                    for (String mstOid : mstOidsForSklOidDayNumLoop) {
                        // add school dates (for full year)
                        List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                        if (csdDates == null) {
                            csdDates = new ArrayList<PlainDate>();
                        }
                        csdDates.addAll(csdDatesForSklOidDayNum);
                        classDtFullYrToMstOidMap.put(mstOid, csdDates);

                        // add days of week
                        if (courseIncludeDays) {
                            List<String> csdDays = classDayListToMstOidMap.get(mstOid);
                            if (csdDays == null) {
                                csdDays = new ArrayList<String>();
                            }
                            csdDays.addAll(csdDaysForSklOidDayNum);
                            classDayListToMstOidMap.put(mstOid, csdDays);
                        }

                        // add bell schedule oid if not populated already
                        // - taking first, only one set of times on report
                        if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                            belOidToMstOidMap.put(mstOid, belOidForSklOidDayNum);
                        }
                    }

                    // initialize lists
                    csdDatesForSklOidDayNum = new ArrayList<PlainDate>();
                    csdDaysForSklOidDayNum = new ArrayList<String>();
                    belOidForSklOidDayNum = OntarioAlias.CONST_EMPTY;
                }
                csdQuerySklOidDayNumPrev = sklOidDayNum;

                // add class date to list for school/day
                if (!csdDatesForSklOidDayNum.contains(csdDate)) {
                    csdDatesForSklOidDayNum.add(csdDate);

                    // save the day-of-week if several have not been been saved for the day number
                    if ((courseIncludeDays)
                            && (csdDatesForSklOidDayNum.size() < CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP)) {
                        calendar.setTime(csdDate);
                        int csdDateDayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
                        csdDaysForSklOidDayNum.add(Integer.valueOf(csdDateDayOfWeek).toString());
                    }
                }

                // save bell schedule oid for school/day
                if ((courseIncludeTime) && (!StringUtils.isEmpty(belOid))) {
                    belOidForSklOidDayNum = belOid;
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            AppGlobals.getLog().log(Level.WARNING,
                    "Error getting section dates/days/times, SisSchoolCalendarDate lookup: " + e);
            throw e;
        }

        // write class date list for last school/day
        if (!(csdQuerySklOidDayNumPrev == null)) {
            // add school dates to lists to mstOid
            List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
            for (String mstOid : mstOidsForSklOidDayNumLoop) {
                // add school dates (for full year)
                List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                if (csdDates == null) {
                    csdDates = new ArrayList<PlainDate>();
                }
                csdDates.addAll(csdDatesForSklOidDayNum);
                classDtFullYrToMstOidMap.put(mstOid, csdDates);

                // add days of week
                if (courseIncludeDays) {
                    List<String> csdDays = classDayListToMstOidMap.get(mstOid);
                    if (csdDays == null) {
                        csdDays = new ArrayList<String>();
                    }
                    csdDays.addAll(csdDaysForSklOidDayNum);
                    classDayListToMstOidMap.put(mstOid, csdDays);
                }

                // add bell schedule oid if not populated already
                // - taking first, only one set of times on report
                if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                    belOidToMstOidMap.put(mstOid, belOidForSklOidDayNum);
                }
            }
        }

        /*
         * clean class date lists to only leave for dates within schedule term sort
         * class date lists saved to mstOids save class time (start/end) by mstOids
         */
        // get sections
        Set<String> mstOids = classDtFullYrToMstOidMap.keySet();

        // get class date list
        // get unsorted list, sort for each section and add to output map
        // for class time
        // get period names for mst, get bell schedule times
        for (String mstOid : mstOids) {
            // get class date list
            MasterSchedule mst = broker.getBeanByOid(MasterSchedule.class, mstOid);
            List<PlainDate> classDtsFullYr = classDtFullYrToMstOidMap.get(mstOid);
            String trmOid = mst.getScheduleTermOid();
            ScheduleTermDate tmd = courseTrmDtToTrmOidMap.get(trmOid);
            PlainDate classStartDate = tmd.getStartDate();
            PlainDate classEndDate = tmd.getEndDate();
            if (tmd.getScheduleTerm().getCode().equals("FY")) {
                classStartDate = gradeTermStartDate;
                classEndDate = gradeTermEndDate;
            }

            // process class date list for each section
            List<PlainDate> classDts = new ArrayList<PlainDate>();
            if (classDtsFullYr != null) {
                // create class date list for section's schedule term
                for (PlainDate classDt : classDtsFullYr) {
                    if ((classDt.compareTo(classStartDate) >= 0) && (classDt.compareTo(classEndDate) <= 0)) {
                        classDts.add(classDt);
                    }
                }

                // sort list of dates
                Collections.sort(classDts, new Comparator<PlainDate>() {
                    @Override
                    public int compare(PlainDate o1, PlainDate o2) {
                        return o1.compareTo(o2);
                    }
                });
            }

            // save sorted list
            classDtListToMstOidMap.put(mstOid, classDts);

            if (courseIncludeTime) {
                // get period names list
                List<String> periodNms = periodNmsToMstOidMap.get(mstOid);

                // sort period names
                Collections.sort(periodNms);

                // get class start/end time
                PlainTime startTimeMst = null;
                PlainTime endTimeMst = null;
                if (belOidToMstOidMap.get(mstOid) != null) {
                    String belOid = belOidToMstOidMap.get(mstOid);
                    // look for lowest start time and highest end time for class periods
                    // -(if bell schedule is found)
                    if (!StringUtils.isEmpty(belOid)) {
                        for (String periodNm : periodNms) {
                            String keyField = belOid + CONST_HYPHEN + periodNm;
                            PlainTime startTimeBpe = null;
                            PlainTime endTimeBpe = null;

                            ScheduleBellPeriod bpe = bpeTimeToBelOidPeriodNmMap.get(keyField);
                            if (bpe != null) {
                                startTimeBpe = bpe.getStartTime();
                                endTimeBpe = bpe.getEndTime();
                            }

                            if ((startTimeBpe != null)
                                    && ((startTimeMst == null) || (startTimeBpe.before(startTimeMst)))) {
                                startTimeMst = startTimeBpe;
                            }
                            if ((endTimeBpe != null) && ((endTimeMst == null) || (endTimeBpe.after(endTimeMst)))) {
                                endTimeMst = endTimeBpe;
                            }
                        }
                    }
                }
                // move start time, end time to output map
                List<PlainTime> startEndTime = new ArrayList<PlainTime>();
                startEndTime.add(startTimeMst);
                startEndTime.add(endTimeMst);

                // save result by mstOid
                classStartEndTimeToMstOidMap.put(mstOid, startEndTime);
            }
        }

        // return list of maps
        List<Map> returnMaps = new ArrayList<Map>();
        returnMaps.add(classDtListToMstOidMap);
        returnMaps.add(classDayListToMstOidMap);
        returnMaps.add(classStartEndTimeToMstOidMap);

        return returnMaps;
    }

    /**
     * Loads class dates/days/time lists in order of class number to master schedule
     * maps.
     *
     * @param ctxOid the ctx oid
     * @param schoolOid - optional, if null will process entire
     *        district
     * @param subQueryMstOids - optional, if null will process all
     *        sections for other criteria
     * @param mstOidsList - optional, if null will process all
     *        students for other criteria
     * @param courseIncludeDays the course include days
     * @param courseIncludeTime the course include time
     * @param courseTrmDtToTrmOidMap - must be populated
     * @param bpeTimeToBelOidPeriodNmMap - must be populated
     * @param broker - X2Broker
     * @param locale the locale
     * @return List&lt;Map&gt;<br>
     *         1. classDtListToMstOidMap<br>
     *         2. classDayListToMstOidMap<br>
     *         3. classStartEndTimeToMstOidMap 4. periodCountByDayNumberToMstOidMap
     *         5. classDtToPeriodsPerDayByMstOidMap
     */
    public static List<Map> loadSchedClassDateDayTimeListsToMstOid(String ctxOid,
                                                                   String schoolOid,
                                                                   SubQuery subQueryMstOids,
                                                                   List<String> mstOidsList,
                                                                   boolean courseIncludeDays,
                                                                   boolean courseIncludeTime,
                                                                   Map<String, ScheduleTermDate> courseTrmDtToTrmOidMap,
                                                                   Map<String, ScheduleBellPeriod> bpeTimeToBelOidPeriodNmMap,
                                                                   X2Broker broker,
                                                                   Locale locale) {
        // initialize output maps
        Map<String, List<PlainDate>> classDtListToMstOidMap = new HashMap<String, List<PlainDate>>();
        Map<String, List<String>> classDayListToMstOidMap = new HashMap<String, List<String>>();
        Map<String, List<PlainTime>> classStartEndTimeToMstOidMap = new HashMap<String, List<PlainTime>>();
        Map<String, Map<Integer, Integer>> periodCountByDayNumberToMstOidMap = new HashMap();
        Map<String, Map<PlainDate, Integer>> classDtToPeriodsPerDayByMstOidMap = new HashMap();

        // constants for only this method
        // Number of csd dates looked at to see days courses meets
        final int CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP = 10;
        Calendar calendar = Calendar.getInstance(locale);
        final String CONST_HYPHEN = "-";
        final String CONST_COMMA = ",";

        /*
         * get the master schedules to ScheduleDays that selected sections meet also
         * lists of school oids, day numbers to use in next query
         */
        Map<String, List<String>> mstOidsToSklOidDayNumMap = new HashMap<String, List<String>>();
        Map<String, List<String>> periodNmsToMstOidMap = new HashMap<String, List<String>>();
        List<String> mstSklOidsToRestrictSelection = new ArrayList<String>();
        List<Integer> mstDayNumsToRestrictSelection = new ArrayList<Integer>();

        // create criteria/query to get schedule days for mstOids
        X2Criteria mtxCriteria = new X2Criteria();

        // condition for school year
        mtxCriteria.addEqualTo(MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE
                + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID, ctxOid);

        // condition for school oid
        if (!StringUtils.isEmpty(schoolOid)) {
            mtxCriteria.addEqualTo(MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER
                    + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID, schoolOid);
        }

        // condition for section oids sub query
        if (subQueryMstOids != null) {
            mtxCriteria.addIn(
                    MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID,
                    subQueryMstOids);
        }

        // condition for section oids list
        if (mstOidsList != null) {
            mtxCriteria.addIn(
                    MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID,
                    mstOidsList);
        }

        String[] mtxColumns = new String[] {
                // school oid
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                // schedule day number
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.REL_SCHEDULE_DAY
                + PATH_DELIMITER + ScheduleDay.COL_NUMBER,
                // schedule period name
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.REL_SCHEDULE_PERIOD
                + PATH_DELIMITER + SchedulePeriod.COL_NAME,
                // master schedule oid
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID};
        ColumnQuery mtxQuery = new ColumnQuery(MasterScheduleMatrix.class, mtxColumns, mtxCriteria);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_DAY + PATH_DELIMITER + ScheduleDay.COL_NUMBER);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_PERIOD + PATH_DELIMITER + SchedulePeriod.COL_NAME);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID);

        // load map of schedule days to mstOids
        String mtxQuerySklOidDayNumPrev = null;
        List<String> mstOidsForSklOidDayNum = new ArrayList<String>();
        try (ReportQueryIterator mtxIterator = broker.getReportQueryIteratorByQuery(mtxQuery)) {
            // iterates through using sort key which is school/day/period/mstOid
            while (mtxIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) mtxIterator.next();
                String sklOid = data[0].toString();
                Integer dayNum = Integer.valueOf(((BigDecimal) data[1]).intValue());
                String periodNm = ((String) data[2]);
                String mstOid = data[3].toString();
                String sklOidDayNum = sklOid + dayNum.toString();

                // if new key school/day, write section list for prev key
                if ((!(mtxQuerySklOidDayNumPrev == null)) && (!sklOidDayNum.equals(mtxQuerySklOidDayNumPrev))) {
                    mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);

                    // initialize list
                    mstOidsForSklOidDayNum = new ArrayList<String>();
                }
                mtxQuerySklOidDayNumPrev = sklOidDayNum;

                // save sklOids, day numbers in list
                if (!mstSklOidsToRestrictSelection.contains(sklOid)) {
                    mstSklOidsToRestrictSelection.add(sklOid);
                }
                if (!mstDayNumsToRestrictSelection.contains(dayNum)) {
                    mstDayNumsToRestrictSelection.add(dayNum);
                }

                // add section to list for school/day
                // may be duplicates including if multiple periods so saved only once
                if (!mstOidsForSklOidDayNum.contains(mstOid)) {
                    mstOidsForSklOidDayNum.add(mstOid);
                }

                // add period names by mstOid
                List<String> periodNmsForMst = periodNmsToMstOidMap.get(mstOid);

                int periodCountForMst = 0;
                if (!periodCountByDayNumberToMstOidMap.containsKey(mstOid)) {
                    Map<Integer, Integer> dayToPeriodCount = new HashMap();
                    periodCountByDayNumberToMstOidMap.put(mstOid, dayToPeriodCount);
                }
                if (periodCountByDayNumberToMstOidMap.containsKey(mstOid)
                        && periodCountByDayNumberToMstOidMap.get(mstOid).containsKey(dayNum)) {
                    periodCountForMst = periodCountByDayNumberToMstOidMap.get(mstOid).get(dayNum);
                } else {

                    Map<Integer, Integer> dayToPeriodCount = periodCountByDayNumberToMstOidMap.get(mstOid);
                    dayToPeriodCount.put(dayNum, 0);
                    periodCountByDayNumberToMstOidMap.put(mstOid, dayToPeriodCount);
                }

                if (periodNmsForMst == null) {
                    periodNmsForMst = new ArrayList<String>();
                }

                if (!periodNmsForMst.contains(periodNm)) {
                    periodNmsForMst.add(periodNm);
                }
                periodCountForMst++;
                periodCountByDayNumberToMstOidMap.get(mstOid).put(dayNum, periodCountForMst);

                periodNmsToMstOidMap.put(mstOid, periodNmsForMst);

            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            AppGlobals.getLog().log(Level.WARNING,
                    "Error getting section dates/days/times, MasterScheduleMatrix lookup: " + e);
        }

        // write section list for last school/day
        if (!(mtxQuerySklOidDayNumPrev == null)) {
            mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);
        }

        /*
         * get SchoolCalendarDates(full year)/days-of-week lists to selected sections
         * also get bell schedule by school/day number (assuming 1 to 1 relation)
         */
        Map<String, List<PlainDate>> classDtFullYrToMstOidMap = new HashMap<String, List<PlainDate>>();
        Map<String, String> belOidToMstOidMap = new HashMap<String, String>();

        // create criteria/query to get school calendar dates for mstOids
        X2Criteria csdCriteria = new X2Criteria();

        // condition for school year/schools/day numbers for selected sections
        csdCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                ctxOid);
        csdCriteria.addIn(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                mstSklOidsToRestrictSelection);
        csdCriteria.addIn(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER, mstDayNumsToRestrictSelection);

        String[] csdColumns = new String[] {
                // school oid
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                // schedule day number
                SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER,
                // date
                SisSchoolCalendarDate.COL_DATE,
                // schedule bell oid
                SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID};
        ColumnQuery csdQuery = new ColumnQuery(SisSchoolCalendarDate.class, csdColumns, csdCriteria);
        csdQuery.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        // load map of schedule days to mstOids
        String csdQuerySklOidDayNumPrev = null;
        Collection<PlainDate> csdDatesForSklOidDayNum = new ArrayList<PlainDate>();
        Collection<String> csdDaysForSklOidDayNum = new ArrayList<String>();
        String belOidForSklOidDayNum = OntarioAlias.CONST_EMPTY;
        Map<PlainDate, Integer> calendarDateToDayNumberMap = new HashMap();
        try (ReportQueryIterator csdIterator = broker.getReportQueryIteratorByQuery(csdQuery)) {
            while (csdIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) csdIterator.next();
                String sklOid = data[0].toString();
                Integer dayNum = Integer.valueOf(((BigDecimal) data[1]).intValue());
                PlainDate csdDate = new PlainDate((Timestamp) data[2]);
                String sklOidDayNum = sklOid + dayNum.toString();
                String belOid = OntarioAlias.CONST_EMPTY;
                if (data[3] != null) {
                    belOid = data[3].toString();
                }

                calendarDateToDayNumberMap.put(csdDate, dayNum);

                // if new key school/day, write class date list for prev key
                if ((!(csdQuerySklOidDayNumPrev == null)) && (!sklOidDayNum.equals(csdQuerySklOidDayNumPrev))) {
                    // add school dates/days to lists to mstOid
                    List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
                    for (String mstOid : mstOidsForSklOidDayNumLoop) {
                        // add school dates (for full year)
                        List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                        if (csdDates == null) {
                            csdDates = new ArrayList<PlainDate>();
                        }
                        csdDates.addAll(csdDatesForSklOidDayNum);
                        classDtFullYrToMstOidMap.put(mstOid, csdDates);

                        // add days of week
                        if (courseIncludeDays) {
                            List<String> csdDays = classDayListToMstOidMap.get(mstOid);
                            if (csdDays == null) {
                                csdDays = new ArrayList<String>();
                            }
                            csdDays.addAll(csdDaysForSklOidDayNum);
                            classDayListToMstOidMap.put(mstOid, csdDays);
                        }

                        // add bell schedule oid if not populated already
                        // - taking first, only one set of times on report
                        if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                            belOidToMstOidMap.put(mstOid, belOidForSklOidDayNum);
                        }
                    }

                    // initialize lists
                    csdDatesForSklOidDayNum = new ArrayList<PlainDate>();
                    csdDaysForSklOidDayNum = new ArrayList<String>();
                    belOidForSklOidDayNum = OntarioAlias.CONST_EMPTY;
                }
                csdQuerySklOidDayNumPrev = sklOidDayNum;

                // add class date to list for school/day
                if (!csdDatesForSklOidDayNum.contains(csdDate)) {
                    csdDatesForSklOidDayNum.add(csdDate);

                    // save the day-of-week if several have not been been saved for the day number
                    if ((courseIncludeDays)
                            && (csdDatesForSklOidDayNum.size() < CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP)) {
                        calendar.setTime(csdDate);
                        int csdDateDayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
                        csdDaysForSklOidDayNum.add(Integer.valueOf(csdDateDayOfWeek).toString());
                    }
                }

                // save bell schedule oid for school/day
                if ((courseIncludeTime) && (!StringUtils.isEmpty(belOid))) {
                    belOidForSklOidDayNum = belOid;
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            AppGlobals.getLog().log(Level.WARNING,
                    "Error getting section dates/days/times, SisSchoolCalendarDate lookup: " + e);
            throw e;
        }

        // write class date list for last school/day
        if (!(csdQuerySklOidDayNumPrev == null)) {
            // add school dates to lists to mstOid
            List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
            for (String mstOid : mstOidsForSklOidDayNumLoop) {
                // add school dates (for full year)
                List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                if (csdDates == null) {
                    csdDates = new ArrayList<PlainDate>();
                }
                csdDates.addAll(csdDatesForSklOidDayNum);
                classDtFullYrToMstOidMap.put(mstOid, csdDates);

                // add days of week
                if (courseIncludeDays) {
                    List<String> csdDays = classDayListToMstOidMap.get(mstOid);
                    if (csdDays == null) {
                        csdDays = new ArrayList<String>();
                    }
                    csdDays.addAll(csdDaysForSklOidDayNum);
                    classDayListToMstOidMap.put(mstOid, csdDays);
                }

                // add bell schedule oid if not populated already
                // - taking first, only one set of times on report
                if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                    belOidToMstOidMap.put(mstOid, belOidForSklOidDayNum);
                }
            }
        }

        /*
         * clean class date lists to only leave for dates within schedule term sort
         * class date lists saved to mstOids save class time (start/end) by mstOids
         */
        // get sections
        Set<String> mstOids = classDtFullYrToMstOidMap.keySet();

        // get class date list
        // get unsorted list, sort for each section and add to output map
        // for class time
        // get period names for mst, get bell schedule times
        for (String mstOid : mstOids) {
            // get class date list
            MasterSchedule mst = broker.getBeanByOid(MasterSchedule.class, mstOid);
            List<PlainDate> classDtsFullYr = classDtFullYrToMstOidMap.get(mstOid);
            String trmOid = mst.getScheduleTermOid();
            ScheduleTermDate tmd = courseTrmDtToTrmOidMap.get(trmOid);
            PlainDate classStartDate = tmd.getStartDate();
            PlainDate classEndDate = tmd.getEndDate();
            int periodsOnDay = 0;

            // process class date list for each section
            List<PlainDate> classDts = new ArrayList<PlainDate>();
            Map<PlainDate, Integer> dateToPeriods = new HashMap();
            if (classDtsFullYr != null) {
                // create class date list for section's schedule term
                for (PlainDate classDt : classDtsFullYr) {
                    if ((classDt.compareTo(classStartDate) >= 0) && (classDt.compareTo(classEndDate) <= 0)) {
                        classDts.add(classDt);
                        // Keeping track of calendar date to number of periods on the day for the
                        // class
                        dateToPeriods.put(classDt, periodCountByDayNumberToMstOidMap.get(mstOid)
                                .get(calendarDateToDayNumberMap.get(classDt)));
                    }
                }

                // sort list of dates
                Collections.sort(classDts, new Comparator<PlainDate>() {
                    @Override
                    public int compare(PlainDate o1, PlainDate o2) {
                        return o1.compareTo(o2);
                    }
                });
            }

            // save sorted list
            classDtListToMstOidMap.put(mstOid, classDts);
            classDtToPeriodsPerDayByMstOidMap.put(mstOid, dateToPeriods);

            if (courseIncludeTime) {
                // get period names list
                List<String> periodNms = periodNmsToMstOidMap.get(mstOid);

                // sort period names
                Collections.sort(periodNms);

                // get class start/end time
                PlainTime startTimeMst = null;
                PlainTime endTimeMst = null;
                if (belOidToMstOidMap.get(mstOid) != null) {
                    String belOid = belOidToMstOidMap.get(mstOid);
                    // look for lowest start time and highest end time for class periods
                    // -(if bell schedule is found)
                    if (!StringUtils.isEmpty(belOid)) {
                        for (String periodNm : periodNms) {
                            String keyField = belOid + CONST_HYPHEN + periodNm;
                            PlainTime startTimeBpe = null;
                            PlainTime endTimeBpe = null;

                            ScheduleBellPeriod bpe = bpeTimeToBelOidPeriodNmMap.get(keyField);
                            if (bpe != null) {
                                startTimeBpe = bpe.getStartTime();
                                endTimeBpe = bpe.getEndTime();
                            }

                            if ((startTimeBpe != null)
                                    && ((startTimeMst == null) || (startTimeBpe.before(startTimeMst)))) {
                                startTimeMst = startTimeBpe;
                            }
                            if ((endTimeBpe != null) && ((endTimeMst == null) || (endTimeBpe.after(endTimeMst)))) {
                                endTimeMst = endTimeBpe;
                            }
                        }
                    }
                }
                // move start time, end time to output map
                List<PlainTime> startEndTime = new ArrayList<PlainTime>();
                startEndTime.add(startTimeMst);
                startEndTime.add(endTimeMst);

                // save result by mstOid
                classStartEndTimeToMstOidMap.put(mstOid, startEndTime);
            }
        }

        // return list of maps
        List<Map> returnMaps = new ArrayList<Map>();
        returnMaps.add(classDtListToMstOidMap);
        returnMaps.add(classDayListToMstOidMap);
        returnMaps.add(classStartEndTimeToMstOidMap);
        returnMaps.add(periodCountByDayNumberToMstOidMap);
        returnMaps.add(classDtToPeriodsPerDayByMstOidMap);
        return returnMaps;
    }

    /**
     * Gets the designated school from BSID for passed in school School Types are
     * defined in OntarioAlias as CONST_SKL_TYPE_.. for Elementary, Secondary
     *
     * @param school the school
     * @param schoolType the school type
     * @param dictionary the dictionary
     * @param broker the broker
     * @return SisSchool - Designated School
     */
    public static SisSchool getDesignatedBsidForSchool(SisSchool school,
                                                       String schoolType,
                                                       DataDictionary dictionary,
                                                       X2Broker broker) {
        SisSchool designatedSchool = school;
        String designatedBSID = OntarioAlias.CONST_EMPTY;

        if ((schoolType.equals(OntarioAlias.CONST_SKL_TYPE_SEC)) && (!StringUtils
                .isBlank((String) school.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_DESIGNATED_SECONDARY_BSID)))) {
            designatedBSID = (String) school.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_DESIGNATED_SECONDARY_BSID);
        } else if ((schoolType.equals(OntarioAlias.CONST_SKL_TYPE_ELEM)) && (!StringUtils
                .isBlank((String) school.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_DESIGNATED_ELEMENTARY_BSID)))) {
            designatedBSID = (String) school.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_DESIGNATED_ELEMENTARY_BSID);
        }

        if (!StringUtils.isEmpty(designatedBSID)) {
            X2Criteria designatedBsidSchoolCriteria = new X2Criteria();
            DataDictionaryField schoolBsid = dictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_SKL_BSID);
            designatedBsidSchoolCriteria.addContains(schoolBsid.getJavaName(), designatedBSID);
            BeanQuery designatedSchoolQuery = new BeanQuery(SisSchool.class, designatedBsidSchoolCriteria);
            designatedSchool = broker.getBeanByQuery(designatedSchoolQuery);
        }

        return designatedSchool;
    }

    /**
     * Takes an alias of field and returns either the java name or the database name
     * that field depending on the status of returnBbName.
     *
     * @param alias - String,
     * @param returnJavaName - boolean
     * @param dictionary - DataDictionary
     *
     * @return String the java name or field name of the object that is aliased
     */
    public static String getBeanPathFromAlias(String alias, boolean returnJavaName, DataDictionary dictionary) {
        String foundField = null;

        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);

        if (field != null) {
            if (returnJavaName) {
                foundField = field.getJavaName();
            } else {
                foundField = field.getDatabaseName();
            }
        }

        return foundField;
    }

    /**
     * Validate certificate completion date.
     *
     * @param req GraduationRequirement
     * @param stdAssess StudentAssessment
     * @return true, if successful
     */
    public static boolean validateCertificateDate(GraduationRequirement req, StudentAssessment stdAssess) {
        boolean isValidCertificate = false;
        try {
            if (req.getFieldValueByAlias(OntarioAlias.ALIAS_GRQ_START_DATE) != null) {
                isValidCertificate = stdAssess.getDate().after(
                        PlainDate.fromString((String) req.getFieldValueByAlias(OntarioAlias.ALIAS_GRQ_START_DATE)));
            }
            if (isValidCertificate && req.getFieldValueByAlias(OntarioAlias.ALIAS_GRQ_END_DATE) != null) {
                isValidCertificate = stdAssess.getDate().before(
                        PlainDate.fromString((String) req.getFieldValueByAlias(OntarioAlias.ALIAS_GRQ_END_DATE)));
            }
        } catch (NullPointerException npe) {
            // skip
        }
        return isValidCertificate;
    }

    /**
     * Gets the gta general messages.
     *
     * @param ctxOid String
     * @param gtmOid String
     * @param broker X2Broker
     * @return Map
     */
    public Map getGtaGeneralMessages(String ctxOid, String gtmOid, X2Broker broker) {
        Map<String, String> gtaGeneralMessagesMap = new HashMap<String, String>();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(GradeTermDate.REL_SCHOOL + PATH_DELIMITER + School.COL_OID, m_school.getOid());
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, ctxOid);
        criteria.addEqualTo(GradeTermDate.COL_GRADE_TERM_OID, gtmOid);

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
        try (QueryIterator iterator = broker.getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                GradeTermDate gta = (GradeTermDate) iterator.next();
                gtaGeneralMessagesMap.put("1",
                        gta.getFieldValueByAlias(OntarioAlias.ALIAS_GTA_GENERAL_MSG_1) != null
                        ? gta.getFieldValueByAlias(OntarioAlias.ALIAS_GTA_GENERAL_MSG_1).toString()
                                : "");
                gtaGeneralMessagesMap.put("2",
                        gta.getFieldValueByAlias(OntarioAlias.ALIAS_GTA_GENERAL_MSG_2) != null
                        ? gta.getFieldValueByAlias(OntarioAlias.ALIAS_GTA_GENERAL_MSG_2).toString()
                                : "");
                gtaGeneralMessagesMap.put("3",
                        gta.getFieldValueByAlias(OntarioAlias.ALIAS_GTA_GENERAL_MSG_3) != null
                        ? gta.getFieldValueByAlias(OntarioAlias.ALIAS_GTA_GENERAL_MSG_3).toString()
                                : "");
            }
        }
        return gtaGeneralMessagesMap;
    }

    /**
     * Utility - Get rounded value from input string if its a number otherwise
     * return number.
     *
     * @param inputNumStr the input num str
     * @param places the places
     * @return String - roundedNumStr
     */
    public static String getRounded(String inputNumStr, int places) {
        String roundedNumStr = inputNumStr;

        if ((inputNumStr != null) && (StringUtils.isNumeric(inputNumStr))) {
            BigDecimal bd = new BigDecimal(inputNumStr);
            bd = bd.setScale(places, RoundingMode.HALF_UP);
            roundedNumStr = bd.toString();
        }

        return roundedNumStr;
    }

    /**
     * Utility - Get rounded value from input double if its a number otherwise
     * return number.
     *
     * @param inputNumDbl the input num dbl
     * @param places the places
     * @return Double - roundedNumDbl
     */
    public static Double getRounded(Double inputNumDbl, int places) {
        Double roundedNumDbl = inputNumDbl;

        if (inputNumDbl != null) {
            BigDecimal bd = new BigDecimal(inputNumDbl);
            bd = bd.setScale(places, RoundingMode.HALF_UP);
            roundedNumDbl = Double.valueOf(bd.doubleValue());
        }

        return roundedNumDbl;
    }

    /**
     * Build teacher name list from MTC records.
     *
     * @param sectionTeacherList List<ScheduleTeacher>
     * @param gradeTerm GradeTerm
     * @param context SisDistrictSchoolYearContext
     * @return String
     */
    public String processSectionTeachers(List<ScheduleTeacher> sectionTeacherList,
                                         GradeTerm gradeTerm,
                                         SisDistrictSchoolYearContext context) {
        StringBuilder teacherNames = new StringBuilder();
        PlainDate gtaStartDate = null;
        PlainDate gtaEndDate = null;
        List<GradeTermDate> gtaList = gradeTerm.getGradeTermDates().stream()
                .filter(gtaObj -> (gtaObj.getDistrictContextOid().equals(context.getOid())
                        && gtaObj.getSchoolOid().equals(m_school.getOid())))
                .collect(Collectors.toList());
        // ONLY one value is expected in gtaList.
        for (GradeTermDate gta : gtaList) {
            gtaStartDate = gta.getStartDate();
            gtaEndDate = gta.getEndDate();
        }
        for (ScheduleTeacher secTeacher : sectionTeacherList) {
            String mtcStartDateString = (String) secTeacher.getFieldValueByAlias("all-mtc-StartDate");
            String mtcEndDateString = (String) secTeacher.getFieldValueByAlias("all-mtc-EndDate");
            // Date Range 1
            PlainDate startDate = DateUtils.getDate((String) secTeacher.getFieldValueByAlias("all-mtc-StartDate"));
            PlainDate endDate = DateUtils.getDate((String) secTeacher.getFieldValueByAlias("all-mtc-EndDate"));
            String role = StringUtils.isBlank(secTeacher.getRole()) ? "" : secTeacher.getRole();
            // TODO 6/17/2020 Date Range 2 comment was - These dates will be addressed after changes
            // to MTC are rolled out
            // Above change made on 7/7/2022. Role 2 is not a required field so is only looked at if
            // date range 2 is populated and the role 2 is populated as well.
            PlainDate startDate_2 = secTeacher.getFieldValueByAlias("all-mtc-StartDate2") != null
                    ? DateUtils.getDate((String) secTeacher.getFieldValueByAlias("all-mtc-StartDate2"))
                            : null;
            PlainDate endDate_2 = secTeacher.getFieldValueByAlias("all-mtc-EndDate2") != null
                    ? DateUtils.getDate((String) secTeacher.getFieldValueByAlias("all-mtc-EndDate2"))
                            : null;
            String role_2 = secTeacher.getFieldValueByAlias("all-mtc-Role2") != null
                    ? (String) secTeacher.getFieldValueByAlias("all-mtc-Role2")
                            : "";
            if (gtaStartDate != null && gtaEndDate != null && startDate != null) {
                boolean teacherNamesObtained = false;
                // check start date
                if (!StringUtils.isBlank((String) secTeacher.getFieldValueByAlias("all-mtc-StartDate"))
                        && (startDate.equals(gtaEndDate) || startDate.before(gtaEndDate)) &&
                        (role.equalsIgnoreCase("Primary") || role.equalsIgnoreCase("Co-Teach"))) {

                    if (!StringUtils.isBlank((String) secTeacher.getFieldValueByAlias("all-mtc-EndDate"))) {
                        if (endDate.equals(gtaEndDate) || endDate.after(gtaEndDate)) {
                            appendTeacherNames(teacherNames, secTeacher.getStaff().getNameView());
                            teacherNamesObtained = true;
                        }
                    } else {
                        appendTeacherNames(teacherNames, secTeacher.getStaff().getNameView());
                        teacherNamesObtained = true;
                    }
                }

                // check start date 2 if start date populated but does not meet above condition
                // so only checking start date 2 for second set of dates
                if (!teacherNamesObtained) {
                    if (!StringUtils.isBlank((String) secTeacher.getFieldValueByAlias("all-mtc-StartDate2"))
                            && (startDate_2.equals(gtaEndDate) || startDate_2.before(gtaEndDate)) &&
                            // check role 2 if populated otherwise check role
                            (((role_2 != null)
                                    && (role_2.equalsIgnoreCase("Primary") || role_2.equalsIgnoreCase("Co-Teach")))
                                    ||
                                    (role.equalsIgnoreCase("Primary") || role.equalsIgnoreCase("Co-Teach")))) {
                        if (!StringUtils.isBlank((String) secTeacher.getFieldValueByAlias("all-mtc-EndDate2"))) {
                            if (endDate_2.equals(gtaEndDate) || endDate_2.after(gtaEndDate)) {
                                appendTeacherNames(teacherNames, secTeacher.getStaff().getNameView());
                                teacherNamesObtained = true;
                            }
                        } else {
                            appendTeacherNames(teacherNames, secTeacher.getStaff().getNameView());
                            teacherNamesObtained = true;
                        }
                    }
                }

                // other cases
            } else if (StringUtils.isBlank(mtcStartDateString)
                    && StringUtils.isBlank(mtcEndDateString)
                    && (role.equalsIgnoreCase("Primary") || role.equalsIgnoreCase("Co-Teach"))) {
                appendTeacherNames(teacherNames, secTeacher.getStaff().getNameView());
            } else if (StringUtils.isBlank(mtcStartDateString)
                    && !StringUtils.isBlank(mtcEndDateString)
                    && (role.equalsIgnoreCase("Primary") || role.equalsIgnoreCase("Co-Teach"))) {
                if (endDate.equals(gtaEndDate) || endDate.after(gtaEndDate)) {
                    appendTeacherNames(teacherNames, secTeacher.getStaff().getNameView());
                }
            } else if (role.equalsIgnoreCase("Primary") || role.equalsIgnoreCase("Co-Teach")) {
                // We should not add staff without a proper date validation
            }
        }
        return teacherNames.toString();
    }

    /**
     * Append teacher names with a semicolon between teachers.
     *
     * @param teacherNames StringBuilder
     * @param name String
     */
    private void appendTeacherNames(StringBuilder teacherNames, String name) {
        if (teacherNames.length() > 1) {
            teacherNames.append("; " + name);
        } else {
            teacherNames.append(name);
        }
    }

    /**
     * Retrieve Reference Codes Map based on reference table oid.
     *
     * @param broker the broker
     * @param referenceTableOid the reference table oid
     * @return the reference codes
     */
    public Map<String, ReferenceCode> getReferenceCodes(X2Broker broker, String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = new HashMap<String, ReferenceCode>();

        if (referenceTableOid == null) {
            return codeMap;
        }

        ReferenceTable refTable = (ReferenceTable) broker.getBeanByOid(ReferenceTable.class, referenceTableOid);
        if (refTable != null) {
            codeMap = refTable.getCodeMap();
        }

        return codeMap;
    }

    /**
     * Retrieve Reference Codes Map based on reference table oid, an empty map is returned if no
     * table
     * is found for the referenceTableOid passed.
     *
     * @param broker the broker
     * @param referenceTableOid the reference table oid
     * @return Map&lt;rcdCode, ReferenceCode&gt;
     */
    public static Map<String, ReferenceCode> getReferenceCodesMap(X2Broker broker, String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = new HashMap<String, ReferenceCode>();

        if (referenceTableOid == null) {
            return codeMap;
        }

        ReferenceTable refTable = (ReferenceTable) broker.getBeanByOid(ReferenceTable.class, referenceTableOid);
        if (refTable != null) {
            codeMap = refTable.getCodeMap();
        }

        return codeMap;
    }

    /**
     * Get date if valid.
     *
     * @param dateIn - String
     * @param pattern - String
     * @return PlainDate
     */
    public static PlainDate getDateIfValid(String dateIn, String pattern) {
        // Set date format from input
        SimpleDateFormat sdfrmt = new SimpleDateFormat(pattern);
        sdfrmt.setLenient(false);

        // initialize output date
        PlainDate dateOut = null;

        // Create Date object, parse the string into date
        try {
            dateOut = new PlainDate(sdfrmt.parse(dateIn));
        }
        /* Date format is invalid */
        catch (ParseException e) {
            return dateOut;
        }

        return dateOut;
    }

    /**
     * Checks if board and school requires/allows signature printing and checks if staff has an valid signature file uploaded.
     *
     * @param broker X2Broker
     * @param school School
     *
     * @return boolean
     */
    public static boolean isSignatureRequired(X2Broker broker, School school) {
        try {
            Criteria dictionaryCriteria = new Criteria();
            dictionaryCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, OntarioAlias.EXT_ID_ON_PREFERENCES);

            ExtendedDataDictionary extendedDictionary = broker
                    .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, dictionaryCriteria));

            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(extendedDictionary, broker.getPersistenceKey());

            Criteria orgAttributesCriteria = new Criteria();
            orgAttributesCriteria.addEqualTo(
                    SisBeanPaths.ORGANIZATION_ATTRIBUTES.extendedDataDictionary().id().getPath(),
                    OntarioAlias.EXT_ID_ON_PREFERENCES);

            QueryByCriteria query = new QueryByCriteria(OrganizationAttributes.class, orgAttributesCriteria);
            OrganizationAttributes boardPreferences = broker.getBeanByQuery(query);

            boolean isBoardRequireSignature = StringUtils.isEqual("1",
                    (String) boardPreferences.getFieldValueByAliasExtended(
                            OntarioAlias.ALIAS_EXT_ORA_PREF_PUBLISH_ESIGNATURE, dataDictionary));

            if (!isBoardRequireSignature) {
                return false;
            }

            return StringUtils.isEqual("1",
                    (String) school.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_PUBLISH_WITH_ESIGNATURE));
        } catch (NullPointerException e) {
            //Just catch NullPointerException if aliases are not configured
            return false;
        }
    }

    /**
     * Gets staff signature Image.
     *
     * @param broker X2Broker
     * @param staffOid school admin oid
     *
     * @return Image
     *
     * @throws Exception throws Exception if e-signature configuration is missing, signature file is missing,
     * signature file is expired or throws IOException if any error occurred during reading signature file bytes.
     */
    public static Image getSignatureImage(X2Broker broker, String staffOid) throws Exception {
        Object signatureFile;
        String signatureEndDateString;
        try {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SisBeanPaths.USER_DEFINED_TABLE_C.staffOid().getPath(), staffOid);
            criteria.addEqualTo(SisBeanPaths.USER_DEFINED_TABLE_C.extendedDataDictionaryOid().getPath(), 
                    OntarioAlias.EXT_OID_STF_RPT_SIGN);

            QueryByCriteria query = new QueryByCriteria(UserDefinedTableC.class, criteria);
            query.addOrderByDescending(SisBeanPaths.USER_DEFINED_TABLE_C.lastModifiedTime().getPath());

            UserDefinedTableC signatureRecord = broker.getBeanByQuery(query);

            ExtendedDataDictionary extendedDataDictionary = 
                    broker.getBeanByOid(ExtendedDataDictionary.class, OntarioAlias.EXT_OID_STF_RPT_SIGN);
            DataDictionary dictionary = 
                    DataDictionary.getDistrictDictionary(extendedDataDictionary, broker.getPersistenceKey());

            signatureFile = 
                    signatureRecord.getFieldValueByAlias(OntarioAlias.ALIAS_EXT_UDC_STF_SIGNATURE_FILE, dictionary);
            signatureEndDateString = (String) signatureRecord
                    .getFieldValueByAlias(OntarioAlias.ALIAS_EXT_UDC_STF_SIGNATURE_END_DATE, dictionary);
        } catch (NullPointerException e) {
            throw new Exception("Signature file is missing/Le fichier de signature est manquant.");
        }

        byte[] signatureFileContent = (byte[]) signatureFile;
        byte[] delimiterBytes = FileDownloadAction.FIELDE_FILE_HEADER_DELIMITER.getBytes();
        int indexOfDelimiter = ByteArrayUtils.indexOf(signatureFileContent, delimiterBytes);

        if (signatureFile == null || indexOfDelimiter == -1) {
            throw new Exception("Signature file is missing/Le fichier de signature est manquant.");
        }

        PlainDate signatureEndDate;
        try {
            signatureEndDate = PlainDate.fromString(signatureEndDateString);
        } catch (NullPointerException e) {
            throw new Exception("Signature end date is missing/La date de fin de signature est manquante.");
        }

        PlainDate today = 
                new PlainDate(OrganizationManager.getTimeZone(OrganizationManager.getRootOrganization(broker)));
        if (today.after(signatureEndDate)) {
            throw new Exception("Signature File is expired/Le fichier de signature est expiré.");
        }

        Image signatureImage = null;
        if (indexOfDelimiter == 0) {
            byte[] fileContent = 
                    Arrays.copyOfRange(signatureFileContent, delimiterBytes.length, signatureFileContent.length);
            int fileNameEndIndex = ByteArrayUtils.indexOf(fileContent, delimiterBytes);
            byte[] signatureBytes = 
                    Arrays.copyOfRange(fileContent, fileNameEndIndex + delimiterBytes.length, fileContent.length);
            ImageIO.setUseCache(false);
            signatureImage = ImageIO.read(new ByteArrayInputStream(signatureBytes));
        }

        return signatureImage;
    }
}