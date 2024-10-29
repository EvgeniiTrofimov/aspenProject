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

package com.x2dev.reports.statereporting.on.register.original;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.procedures.statereporting.on.register.original.OntarioToolHelper;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Enrolment Register Helper class (will be extended by reports) for Ontario Add
 * as external data source to Enrolment Register reports for Ontario
 *
 * Treat as abstract, but not defined as abstract as needs to be added to Aspen
 *
 * @author Follett Software Company
 */
/**
 * @author Follett Software Company
 * @copyright 2021
 */
/**
 * @author ddesai
 *
 */
public class EnrRegRptHelperFte16 extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    // Input parameters
    protected static final String PARAM_REPORT_TYPE = "reportType";
    protected static final String PARAM_GRADE_RANGE = "gradeRange";
    protected static final String PARAM_EFFECTIVE_DATE = "effectiveDate";
    protected static final String PARAM_DISPLAY_DTL = "displayDtl";
    protected static final String PARAM_DISPLAY_SUM_STD_HC = "displaySumStdHc";
    protected static final String PARAM_DISPLAY_SUM_MONTH = "displaySumMonth";
    protected static final String PARAM_DISPLAY_SUM_STD_FTE = "displaySumStdFte";
    protected static final String PARAM_ADD_LOGS = "addLogs";

    // Input parameters - Subreport ids
    protected static final String SUBRPT_ID_DTL = "subrptIdDtl";
    protected static final String SUBRPT_ID_SUM_MONTH = "subrptIdSumMonth";
    protected static final String SUBRPT_ID_SUM_STD = "subrptIdSumStd";
    protected static final String SUBRPT_ID_SUM_STD_HC = "subrptIdSumStdHc";

    // Report parameters
    protected static final String REPORT_ONTARIO_ALIAS_FOR_RPT_MAP = "ontarioAliasForRptMap";
    protected static final String REPORT_PREFIX = "prefix";
    protected static final String REPORT_SCHOOL_LOCALE = "schoolLocale";
    protected static final String REPORT_SHORT_DATE_FMT_OUTPUT = "shortDateFormat";
    protected static final String REPORT_TIMESTAMP_FMT_OUTPUT = "timestampFormat";
    protected static final String REPORT_DATE_FMT_OUTPUT = "dateFmtOutput";
    protected static final String REPORT_INT_FMT_OUTPUT = "intFmtOutput";
    protected static final String REPORT_DEC_FMT_OUTPUT = "decFmtOutput";
    protected static final String REPORT_SUBRPT_FORMAT_DTL = "subrptFormatDtl";
    protected static final String REPORT_SUBRPT_FORMAT_SUM_MONTH = "subrptFormatSumMonth";
    protected static final String REPORT_SUBRPT_FORMAT_SUM_STD = "subrptFormatSumStd";
    protected static final String REPORT_SUBRPT_FORMAT_SUM_STD_HC = "subrptFormatSumStdHc";
    protected static final String REPORT_SUBRPT_DATA_DTL = "subrptDataDtl";
    protected static final String REPORT_SUBRPT_DATA_SUM_MONTH = "subrptDataSumMonth";
    protected static final String REPORT_SUBRPT_DATA_SUM_MONTH_ADULT = "subrptDataSumMonthAdult";
    protected static final String REPORT_SUBRPT_DATA_SUM_STD = "subrptDataSumStd";
    protected static final String REPORT_SUBRPT_DATA_SUM_STD_ADULT = "subrptDataSumStdAdult";
    protected static final String REPORT_SUBRPT_DATA_SUM_STD_HC_OCT = "subrptDataSumStdHcOct";
    protected static final String REPORT_SUBRPT_DATA_SUM_STD_HC_ADULT_OCT = "subrptDataSumStdHcAdultOct";
    protected static final String REPORT_SUBRPT_DATA_SUM_STD_HC_MAR = "subrptDataSumStdHcMar";
    protected static final String REPORT_SUBRPT_DATA_SUM_STD_HC_ADULT_MAR = "subrptDataSumStdHcAdultMar";
    protected static final String REPORT_SUBRPT_DATA_SUM_FTE_STD_OCT = "subrptDataSumFteStdOct";
    protected static final String REPORT_SUBRPT_DATA_SUM_FTE_STD_ADULT_OCT = "subrptDataSumFteStdAdultOct";
    protected static final String REPORT_SUBRPT_DATA_SUM_FTE_STD_MAR = "subrptDataSumFteStdMar";
    protected static final String REPORT_SUBRPT_DATA_SUM_FTE_STD_ADULT_MAR = "subrptDataSumFteStdAdultMar";
    protected static final String REPORT_ADD_MONTH_SIGNATURE_LINE = "addMonthSignatureLine";
    protected static final String REPORT_END_DATE_SKL_MONTH = "endDateSklMonth";
    protected static final String REPORT_CONST_SKL_MONTH_OCT = "sklMonthOct";
    protected static final String REPORT_CONST_SKL_MONTH_MAR = "sklMonthMar";
    protected static final String REPORT_ERR_EFFECTIVE_DATE = "errEffectiveDate";
    protected static final String REPORT_VERSION = "version";

    // Report parameters - Localized reference tables
    protected static final String REPORT_REF_LOC_GENDER = "refLocGender";
    protected static final String REPORT_REF_LOC_GRADE_LVL = "refLocGradeLvl";

    // Report parameters - Pupils count by month - Not Adult
    protected static final String REPORT_PB_CT_OCT = "pBCtOct";
    protected static final String REPORT_PB_CT_MAR = "pBCtMar";
    protected static final String REPORT_OP_CT_OCT = "oPCtOct";
    protected static final String REPORT_OP_CT_MAR = "oPCtMar";

    // Report parameters - Pupils count by month - Adult
    protected static final String REPORT_PB_CT_ADULT_OCT = "pBCtAdultOct";
    protected static final String REPORT_PB_CT_ADULT_MAR = "pBCtAdultMar";
    protected static final String REPORT_OP_CT_ADULT_OCT = "oPCtAdultOct";
    protected static final String REPORT_OP_CT_ADULT_MAR = "oPCtAdultMar";

    // Report parameters - Ftes sum by month - Not Adult
    protected static final String REPORT_PB_FTE_SUM_OCT = "pBFteSumOct";
    protected static final String REPORT_PB_FTE_SUM_MAR = "pBFteSumMar";
    protected static final String REPORT_OP_FTE_SUM_OCT = "oPFteSumOct";
    protected static final String REPORT_OP_FTE_SUM_MAR = "oPFteSumMar";

    // Report parameters - Ftes sum by month - Adult
    protected static final String REPORT_PB_FTE_SUM_ADULT_OCT = "pBFteSumAdultOct";
    protected static final String REPORT_PB_FTE_SUM_ADULT_MAR = "pBFteSumAdultMar";
    protected static final String REPORT_OP_FTE_SUM_ADULT_OCT = "oPFteSumAdultOct";
    protected static final String REPORT_OP_FTE_SUM_ADULT_MAR = "oPFteSumAdultMar";

    // Report parameters - Ftes sum by month (with high credit) - Not Adult
    protected static final String REPORT_PB_FTE_SUM_HC_OCT = "pBFteSumHcOct";
    protected static final String REPORT_PB_FTE_SUM_HC_MAR = "pBFteSumHcMar";
    protected static final String REPORT_OP_FTE_SUM_HC_OCT = "oPFteSumHcOct";
    protected static final String REPORT_OP_FTE_SUM_HC_MAR = "oPFteSumHcMar";

    // Report parameters - Ftes sum by month (with high credit) - Adult
    protected static final String REPORT_PB_FTE_SUM_HC_ADULT_OCT = "pBFteSumHcAdultOct";
    protected static final String REPORT_PB_FTE_SUM_HC_ADULT_MAR = "pBFteSumHcAdultMar";
    protected static final String REPORT_OP_FTE_SUM_HC_ADULT_OCT = "oPFteSumHcAdultOct";
    protected static final String REPORT_OP_FTE_SUM_HC_ADULT_MAR = "oPFteSumHcAdultMar";

    // Constant values
    protected static final List<String> CONST_ONTARIO_ALIAS_FOR_RPT_LIST = new ArrayList<String>(
            Arrays.asList("IMAGE_FILE_NAME_ONTARIO", "ALIAS_PSN_LEGAL_LAST_NAME", "ALIAS_PSN_LEGAL_FIRST_NAME",
                    "ENR_ELEM_CYCLE_MINS_TOT"));
    protected static final String CONST_EMPTY = "";
    protected static final Integer CONST_ZERO = Integer.valueOf(0);
    protected static final Double CONST_ZERO_DBL = Double.valueOf(0.0);
    protected static final String CONST_PATTERN_INT = "[0-9]+";
    protected static final String CONST_PATTERN_DOUBLE = "([0-9]*\\.*[0-9]*)+";

    // Constant values - Localization
    protected static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    protected static final String CONST_DATE_FMT_STR_ENG = "MM/dd/yyyy";
    protected static final String CONST_DATE_FMT_STR_FR = "dd/MM/yyyy";
    protected static final String CONST_DATE_FMT_STR_ENG_MMM_DD = "MMM dd";
    protected static final String CONST_DATE_FMT_STR_FR_DD_MMM = "dd MMM";
    protected static final String CONST_DATE_FMT_STR_ENG_WITH_TIME = "MM/dd/yyyy hh:mm aa";
    protected static final String CONST_DATE_FMT_STR_FR_WITH_TIME = "dd/MM/yyyy HH:mm";
    protected static final char CONST_NUM_FMT_FR_CHAR_DECIMAL = ',';
    protected static final String CONST_NUM_FMT_STR_INT = "#,##0";
    protected static final String CONST_NUM_FMT_STR_DEC = "#,##0.00";

    // Constant values - Report type
    protected static final String CONST_RPT_IN_ELEM_FT = "dayElemFt";
    protected static final String CONST_RPT_IN_ELEM_PT = "dayElemPt";
    protected static final String CONST_RPT_IN_SEC_FT = "daySecFt";
    protected static final String CONST_RPT_IN_SEC_PT = "daySecPt";
    protected static final String CONST_SKL_LVL_ELEM = "Elem";
    protected static final String CONST_SKL_LVL_SEC = "Sec";
    protected static final String CONST_RPT_TYPE_FT = "FT";
    protected static final String CONST_RPT_TYPE_PT = "PT";

    // Constant values - Grade ranges
    protected static final String CONST_GRADE_RANGE_ALL = "all";
    protected static final String CONST_GRADE_RANGE_JK = "jk";
    protected static final String CONST_GRADE_RANGE_K = "k";
    protected static final String CONST_GRADE_RANGE_1_TO_3 = "1-3";
    protected static final String CONST_GRADE_RANGE_4_TO_8 = "4-8";
    protected static final String CONST_GRADE_RANGE_9_TO_12 = "9-12";

    // Constant values - Grade levels (used in extending classes)
    protected static final Collection<String> CONST_GRADE_LVLS_JK = new ArrayList<String>(Arrays.asList("JK", "PK"));
    protected static final Collection<String> CONST_GRADE_LVLS_K = new ArrayList<String>(
            Arrays.asList("SK", "KF", "KP"));
    protected static final Collection<String> CONST_GRADE_LVLS_01_TO_03 = new ArrayList<String>(
            Arrays.asList("01", "02", "03"));
    protected static final Collection<String> CONST_GRADE_LVLS_04_TO_08 = new ArrayList<String>(
            Arrays.asList("04", "05", "06", "07", "08"));
    protected static final Collection<String> CONST_GRADE_LVLS_09_TO_12 = new ArrayList<String>(
            Arrays.asList("09", "10", "11", "12"));

    // Constant values - Student fte table register fields
    protected static final String CONST_STD_REG_REG_TYPE = "stdRegRegType";
    protected static final String CONST_STD_REG_CHG_DT = "stdRegChgDt";
    protected static final String CONST_STD_REG_CHG_SKL_MONTH = "stdRegChgDateSklMonth";

    // Constant values - Enrolment/Student Info
    protected static final String CONST_ENR_DATE = "enrDate";
    protected static final String CONST_ENR_REG_TYPE = "enrRegType";
    protected static final String CONST_ENR_CODE = "enrCode";
    protected static final String CONST_ENR_TYPE = "enrType";
    protected static final String CONST_ENR_SYS_CODE = "enrSysCode";
    protected static final String CONST_ENR_SKL_MONTH = "enrDateSklMonth";
    protected static final String CONST_ENR_STD_OID = "enrStdOid";
    protected static final String CONST_ENR_STD_OP_IND = "enrStdOpInd";
    protected static final String CONST_ENR_STD_PAYER = "enrStdPayer";
    protected static final String CONST_ENR_STD_GRADE_LVL = "enrStdGradeLvl";
    protected static final String CONST_ENR_STD_ADULT_IND = "enrStdAdultInd";
    protected static final String CONST_ENR_STD_ARRIVAL_IND = "enrStdArrivalInd";
    protected static final String CONST_ENR_MINS = "enrTotMins";
    protected static final String CONST_ENR_FTE = "enrFte";
    protected static final String CONST_ENR_FTE_HC = "enrFteHc";

    // entry fields
    protected static final String CONST_ENR_IN_CODE = "enrInCode";
    protected static final String CONST_ENR_IN_TYPE = "enrInType";
    protected static final String CONST_ENR_IN_DATE = "enrInDate";
    protected static final String CONST_ENR_IN_SYS_CODE = "enrInSysCode";
    protected static final String CONST_ENR_IN_CHG_TYPE = "enrInChgType";
    protected static final String CONST_ENR_IN_SKL_MONTH = "enrInDateSklMonth";
    // withdraw fields
    protected static final String CONST_ENR_OUT_CODE = "enrOutCode";
    protected static final String CONST_ENR_OUT_TYPE = "enrOutType";
    protected static final String CONST_ENR_OUT_DATE = "enrOutDate";
    protected static final String CONST_ENR_OUT_SYS_CODE = "enrOutSysCode";
    protected static final String CONST_ENR_OUT_CHG_TYPE = "enrOutChgType";
    protected static final String CONST_ENR_OUT_SKL_MONTH = "enrOutDateSklMonth";
    // constants with values
    protected static final String CONST_ENR_CHG_TYPE_VALUE_ENTRY_EXT = "typeEntryExt";
    protected static final String CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT = "typeEntryInt";
    protected static final String CONST_ENR_CHG_TYPE_VALUE_REG_TYPE = "chgRegType";
    protected static final String CONST_ENR_CHG_TYPE_VALUE_OP_IND = "chgOpInd";
    protected static final String CONST_ENR_CHG_TYPE_VALUE_YOG = "chgYog";
    protected static final String CONST_ENR_CHG_TYPE_VALUE_WITHDRAW = "typeWithdraw";
    protected static final List<String> CONST_ENR_CHG_TYPE_VALUES_ENTRY = new ArrayList<String>(
            Arrays.asList(CONST_ENR_CHG_TYPE_VALUE_ENTRY_EXT, CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT));
    protected static final List<String> CONST_ENR_CHG_TYPE_VALUES_CREATE_WITHDRAW = new ArrayList<String>(Arrays
            .asList(CONST_ENR_CHG_TYPE_VALUE_REG_TYPE, CONST_ENR_CHG_TYPE_VALUE_OP_IND, CONST_ENR_CHG_TYPE_VALUE_YOG));
    // change date fields
    protected static final String CONST_ENR_STD_CHG_DT_ADMIS_INT = "enrStdAdmisIntDt";
    protected static final String CONST_ENR_STD_CHG_DT_ADMIS_EXT = "enrStdAdmisExtDt";
    protected static final String CONST_ENR_STD_CHG_DT_TRANS_INT = "enrStdTransIntDt";
    protected static final String CONST_ENR_STD_CHG_DT_TRANS_EXT = "enrStdTransExtDt";
    protected static final String CONST_ENR_STD_CHG_DT_RETIRE = "enrStdRetireExtDt";

    // student fte Oct Mar counts
    protected static final String CONST_STD_FTE_STD_PUPIL_BOARD_IND_OCT = "enrStdPbIndOct";
    protected static final String CONST_STD_FTE_STD_PUPIL_BOARD_IND_MAR = "enrStdPbIndMar";
    protected static final String CONST_STD_FTE_STD_OTHER_PUPIL_IND_OCT = "enrStdOpIndOct";
    protected static final String CONST_STD_FTE_STD_OTHER_PUPIL_IND_MAR = "enrStdOpIndMar";

    // Constant values - Student monthly counts
    protected static final String CONST_STD_TYP_PB = "Pb";
    protected static final String CONST_STD_TYP_OP = "Op";

    // Constant values - Student Mins and Fte
    protected static final String CONST_STD_MINS_OCT = "stdMinsOct";
    protected static final String CONST_STD_MINS_MAR = "stdMinsMar";
    protected static final String CONST_STD_FTE_OCT = "stdFteOct";
    protected static final String CONST_STD_FTE_MAR = "stdFteMar";
    protected static final String CONST_STD_MINS_HC_OCT = "stdMinsHcOct";
    protected static final String CONST_STD_MINS_HC_MAR = "stdMinsHcMar";
    protected static final String CONST_STD_FTE_HC_OCT = "stdFteHcOct";
    protected static final String CONST_STD_FTE_HC_MAR = "stdFteHcMar";

    // Constant values - Others
    protected static final String CONST_ERR_EFF_DATE_START = "--startDt--";
    protected static final String CONST_ERR_EFF_DATE_END = "--endDt--";
    protected static final String CONST_ERR_EFF_DATE = "Effective date must be in range " + CONST_ERR_EFF_DATE_START
            + " to " + CONST_ERR_EFF_DATE_END + " for selected school year";

    // Grid fields - first rows, school
    protected static final String FIELD_DTL_LAST_ROW = "dtlLastRow";
    protected static final String FIELD_SUM_MONTH_LAST_ROW = "sumMonthLastRow";
    protected static final String FIELD_SUM_STD_LAST_ROW = "sumStdLastRow";
    protected static final String FIELD_SCHOOL = "school";

    // Grid fields - detail
    protected static final String FIELD_DTL_STUDENT = "dtlStudent";
    protected static final String FIELD_DTL_OTHER_PUPIL_IND = "dtlOtherPupilInd";
    protected static final String FIELD_DTL_PAYER = "dtlPayer";
    protected static final String FIELD_DTL_ADULT_IND = "dtlAdultInd";
    protected static final String FIELD_DTL_GRADE_LVL = "dtlGradeLvl";
    protected static final String FIELD_DTL_GRADE_LVL_PREV = "dtlGradeLvlPrev";
    protected static final String FIELD_DTL_ADMIS_INT_DT = "dtlAdmisIntDt";
    protected static final String FIELD_DTL_ADMIS_EXT_DT = "dtlAdmisExtDt";
    protected static final String FIELD_DTL_TRANS_INT_DT = "dtlTransIntDt";
    protected static final String FIELD_DTL_TRANS_EXT_DT = "dtlTransExtDt";
    protected static final String FIELD_DTL_RETIRE_DT = "dtlRetireDt";

    // Grid fields - summary - by month
    // - First row depends on when school year starts (used to be Sept, can be earlier now)
    protected static final String FIELD_SUM_MONTH_NUM = "sumSklMonth";
    protected static final String FIELD_SUM_ROW_ADULT_IND = "sumRowAdultInd";
    protected static final String FIELD_SUM_PREVIOUS_CT = "sumPrevCt";
    protected static final String FIELD_SUM_ADMIS_INT_CT = "sumAdmisIntCt";
    protected static final String FIELD_SUM_ADMIS_EXT_CT = "sumAdmisExtCt";
    protected static final String FIELD_SUM_TRANS_INT_CT = "sumTransIntCt";
    protected static final String FIELD_SUM_TRANS_EXT_CT = "sumTransExtCt";
    protected static final String FIELD_SUM_RETIRE_CT = "sumRetireCt";
    protected static final String FIELD_SUM_NET_CT = "sumNetCt";

    // Grid fields - summary - with student mins/fte
    // separate reports for October and March
    // will be fte report if that report is selected
    protected static final String FIELD_SUM_STUDENT = "sumStudent";
    protected static final String FIELD_SUM_OTHER_PUPIL_IND = "sumOtherPupilInd";
    protected static final String FIELD_SUM_ROW_ADULT_IND2 = "sumRowAdultInd";
    // mins/fte by October and March
    protected static final String FIELD_SUM_ROW_OCT_IND = "sumRowOctInd";
    protected static final String FIELD_SUM_ROW_MAR_IND = "sumRowMarInd";
    protected static final String FIELD_SUM_MINS_OCT = "sumMinsOct";
    protected static final String FIELD_SUM_FTE_OCT = "sumFteOct";
    protected static final String FIELD_SUM_MINS_MAR = "sumMinsMar";
    protected static final String FIELD_SUM_FTE_MAR = "sumFteMar";
    // - Additional high credit mins/fte
    protected static final String FIELD_SUM_MINS_HC_OCT = "sumMinsHcOct";
    protected static final String FIELD_SUM_FTE_HC_OCT = "sumFteHcOct";
    protected static final String FIELD_SUM_MINS_HC_MAR = "sumMinsHcMar";
    protected static final String FIELD_SUM_FTE_HC_MAR = "sumFteHcMar";

    // Variables - input
    protected SisSchool m_school;
    protected PlainDate m_startDate;
    protected PlainDate m_endDate;
    protected PlainDate m_endDateExceptExtTrRetire;
    protected PlainDate m_startDateSummaryFte;
    protected PlainDate m_endDateSummaryFte;
    protected PlainDate m_contextDateDec31;
    protected boolean m_addLogs;

    // Variables - other
    protected String m_sklLvl = CONST_EMPTY;
    protected String m_rptType = CONST_EMPTY;
    protected Integer m_startDateSklMonth = null;
    protected Integer m_endDateSklMonth = null;
    protected List<String> m_gradeLvls = null;
    protected String m_errEffectiveDate = CONST_EMPTY;
    protected MessageResources m_messageResources = null;
    protected OrganizationLocale m_locForLang = null;
    protected Map<String, ReferenceCode> m_refCodeEnrCodeMap;
    protected X2Criteria m_enrCriteria;
    protected ReportDataGrid m_gridDetail;
    protected ReportDataGrid m_gridSummaryMonth;
    protected ReportDataGrid m_gridSummaryMonthAdult;
    protected ReportDataGrid m_gridSummaryStd;
    protected ReportDataGrid m_gridSummaryStdAdult;
    protected ReportDataGrid m_gridSummaryStdHcOct;
    protected ReportDataGrid m_gridSummaryStdHcAdultOct;
    protected ReportDataGrid m_gridSummaryStdHcMar;
    protected ReportDataGrid m_gridSummaryStdHcAdultMar;
    protected ReportDataGrid m_gridSummaryFteStdOct;
    protected ReportDataGrid m_gridSummaryFteStdAdultOct;
    protected ReportDataGrid m_gridSummaryFteStdMar;
    protected ReportDataGrid m_gridSummaryFteStdAdultMar;
    protected DataDictionary m_dictionary;
    protected Calendar m_cal;

    // sub query of student oids based on UDD or UDC values - secondary only
    protected SubQuery m_udtSubQueryRptTypeSec;

    // Variables - field names
    protected String m_fieldPsnLegalLastName;
    protected String m_fieldPsnLegalFirstName;

    // Variables - report grids to write
    protected boolean m_writeGridDetail;
    protected boolean m_writeGridSummaryMonth;
    protected boolean m_writeGridSummaryMonthAdult;
    protected boolean m_writeGridSummaryStd;
    protected boolean m_writeGridSummaryStdAdult;
    protected boolean m_writeGridSummaryStdHc;
    protected boolean m_writeGridSummaryStdHcAdult;
    protected boolean m_writeGridSummaryFteStd;
    protected boolean m_writeGridSummaryFteStdAdult;

    // Variables - maps by student - from ENR by date, UDC by date/(or)UDD by month
    // depending on input
    protected List<String> m_stdOidsInOrder = null;
    protected Map<String, SisStudent> m_stdByStdOid = null;
    protected Map<String, Boolean> m_stdAdultIndByStdOid = null;
    protected Map<String, Map<Integer, Boolean>> m_stdPbIndByMthByStdOid = null;
    protected Map<String, Map<Integer, Boolean>> m_stdOpIndByMthByStdOid = null;
    protected Map<String, Map<String, Object>> m_minsAndFteFromDbInfoByStdOid = null;
    protected Map<String, Map<String, Object>> m_minsAndFteInfoByStdOid = null;
    protected Map<String, Map<String, Object>> m_minsAndFteAvgInfoByStdOid = null;
    protected Map<String, List<Map<String, Object>>> m_regChgInfoOnePerRegTypeChgByStdOid = null;
    protected Map<String, List<Map<String, Object>>> m_regChgInfoAllRegTypeChgByStdOid = null;
    protected Map<String, List<Map<String, Object>>> m_enrInfoListByStdOid = null;
    protected Map<String, List<Map<String, Object>>> m_enrInfoListStdFteByStdOid = null;

    // Variables - summary - month
    protected Map<Integer, Integer> m_sumAdmisIntCtPbBySklMonthMap;
    protected Map<Integer, Integer> m_sumAdmisIntCtOpBySklMonthMap;
    protected Map<Integer, Integer> m_sumAdmisExtCtPbBySklMonthMap;
    protected Map<Integer, Integer> m_sumAdmisExtCtOpBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransIntCtPbBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransIntCtOpBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransExtCtPbBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransExtCtOpBySklMonthMap;
    protected Map<Integer, Integer> m_sumRetireCtPbBySklMonthMap;
    protected Map<Integer, Integer> m_sumRetireCtOpBySklMonthMap;

    // Variables - summary - month (Adult)
    protected Map<Integer, Integer> m_sumAdmisIntCtPbAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumAdmisIntCtOpAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumAdmisExtCtPbAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumAdmisExtCtOpAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransIntCtPbAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransIntCtOpAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransExtCtPbAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumTransExtCtOpAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumRetireCtPbAdultBySklMonthMap;
    protected Map<Integer, Integer> m_sumRetireCtOpAdultBySklMonthMap;

    // Variables - summary for parameters
    // student counts for student fte report, monthly counts will be used otherwise
    protected int m_pBCtOct;
    protected int m_pBCtMar;
    protected int m_oPCtOct;
    protected int m_oPCtMar;
    protected int m_pBCtAdultOct;
    protected int m_pBCtAdultMar;
    protected int m_oPCtAdultOct;
    protected int m_oPCtAdultMar;
    // fte counts
    protected double m_pBFteSumOct;
    protected double m_oPFteSumOct;
    protected double m_pBFteSumMar;
    protected double m_oPFteSumMar;
    protected double m_pBFteSumAdultOct;
    protected double m_oPFteSumAdultOct;
    protected double m_pBFteSumAdultMar;
    protected double m_oPFteSumAdultMar;
    protected double m_pBFteSumHcOct;
    protected double m_oPFteSumHcOct;
    protected double m_pBFteSumHcMar;
    protected double m_oPFteSumHcMar;
    protected double m_pBFteSumHcAdultOct;
    protected double m_oPFteSumHcAdultOct;
    protected double m_pBFteSumHcAdultMar;
    protected double m_oPFteSumHcAdultMar;

    // Constants/Variables for logs
    protected static final String CONST_COMMA = ",";
    protected static final String CONST_QUOTE = "\"";
    protected static final String CONST_NEWLINE = "\r\n";
    protected static final String CONST_LOGS_HEADERS =
            "Log Type,Std Ct,Std Name,Enr Date,Pupil Board/Other,Adult/Under 21,"
                    + "m_sumFteOct,m_sumFteHcOct,m_sumFteMar,m_sumFteHcMar,"
                    + "m_pBFteSumOct, m_oPFteSumOct,m_pBFteSumMar,m_oPFteSumMar,"
                    + "m_pBFteSumAdultOct,m_oPFteSumAdultOct,m_pBFteSumAdultMar,m_oPFteSumAdultMar,"
                    + "m_pBFteSumHcOct,m_oPFteSumHcOct,m_pBFteSumHcMar,m_oPFteSumHcMar,"
                    + "m_pBFteSumHcAdultOct,m_oPFteSumHcAdultOct,m_pBFteSumHcAdultMar,m_oPFteSumHcAdultMar";
    protected static final String CONST_LOG_1 = "accumEnrMinsFte/pre summary upd method";
    protected static final String CONST_LOG_2 = "writeGridSummaryStd/post summary upd method";
    protected static final List<String> CONST_LOG_TYPES = new ArrayList<String>(
            Arrays.asList(CONST_LOG_1, CONST_LOG_2));
    protected Map<String, StringBuilder> m_logMessageMapToLogType = new HashMap<String, StringBuilder>();
    protected String m_log1StdOidPrev = OntarioAlias.CONST_EMPTY;
    protected int m_log1StdCt = CONST_ZERO.intValue();
    protected String m_log2StdOidPrev = OntarioAlias.CONST_EMPTY;
    protected int m_log2StdCt = CONST_ZERO.intValue();

    /**
     * Gather data.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        // initialize output logs based on input parameter
        initOutputLogs();

        // populate grid by student, do not gather data if effective date is out of
        // range
        if (StringUtils.isEmpty(m_errEffectiveDate)) {
            loadPreEnrolmentData();

            // load enrolment data
            if (m_writeGridSummaryFteStd) {
                loadEnrolmentDataForFteStdRpt();
            } else {
                loadEnrolmentDataNonFteRptsInclElem();
            }

            // populate grids
            populateGrids();
        } else {
            addParameter(REPORT_ERR_EFFECTIVE_DATE, m_errEffectiveDate);
        }

        // add parameters
        addRptParameters();

        // complete output grids/sub report
        completeSubreportsAndGrids();

        // set output logs based on input parameter
        setOutputLogs();

        // return one empty record (with school as used to group ireports)
        // - as output report is composed entirely of subreports
        ReportDataGrid gridReturn = new ReportDataGrid();
        gridReturn.append();
        gridReturn.set(FIELD_SCHOOL, m_school);
        gridReturn.beforeTop();
        return gridReturn;
    }

    /**
     * Initialize output logs
     */
    protected void initOutputLogs() {
        if (m_addLogs) {
            // log message initialize
            for (String logType : CONST_LOG_TYPES) {
                m_logMessageMapToLogType.put(logType, new StringBuilder().append(CONST_LOGS_HEADERS + CONST_NEWLINE));
            }
        }
    }

    /**
     * Load any pre-needed tables - these are needed for secondary registers at
     * present so method will have override this
     */
    protected void loadPreEnrolmentData() {
        // Override in extended class will load external (user defined or other) as
        // necessary

        // initialize variables (data maybe loaded with pre enrolment or enrolment data)
        m_stdOidsInOrder = new ArrayList<String>();
        m_stdByStdOid = new HashMap<String, SisStudent>();
        m_stdAdultIndByStdOid = new HashMap<String, Boolean>();
        m_stdPbIndByMthByStdOid = new HashMap<String, Map<Integer, Boolean>>();
        m_stdOpIndByMthByStdOid = new HashMap<String, Map<Integer, Boolean>>();
        m_minsAndFteInfoByStdOid = new HashMap<String, Map<String, Object>>();
        m_minsAndFteAvgInfoByStdOid = new HashMap<String, Map<String, Object>>();
    }

    /**
     * Load enrolment data - only for secondary FteStd report
     *
     * - only used for OP changes
     */
    protected void loadEnrolmentDataForFteStdRpt() {
        // execute query and loop through students
        createEnrolmentCriteriaForFteStdRpt();

        m_enrInfoListStdFteByStdOid = new HashMap<String, List<Map<String, Object>>>();
        List<Map<String, Object>> enrInfoListStdFteForStdOid = new ArrayList<Map<String, Object>>();
        String stdOidPrev = null;
        try (QueryIterator<X2BaseBean> iterator = getBroker().getIteratorByQuery(getEnrolmentQuery())) {
            while (iterator.hasNext()) {
                StudentEnrollment enr = (StudentEnrollment) iterator.next();
                String stdOid = enr.getStudentOid();
                SisStudent std = enr.getStudent();

                // get initially required enrolment fields
                String enrCode = enr.getEnrollmentCode();
                String enrType = enr.getEnrollmentType();
                String enrSysCode = CONST_EMPTY;

                // set system code
                ReferenceCode rcdEnrCode = m_refCodeEnrCodeMap.get(enrCode);
                if (rcdEnrCode != null) {
                    String rcdEnrCodeSysType = rcdEnrCode.getSystemCode();
                    if (!StringUtils.isEmpty(rcdEnrCodeSysType)) {
                        enrSysCode = rcdEnrCodeSysType;
                    }
                }

                // set system code to internal transfer as default if entry enrollment but no
                // system code found
                if (StudentEnrollment.YOG_CHANGE.equals(enrType)) {
                    enrSysCode = OntarioAlias.ENR_CODE_RCD_SYS_TRANS_INT;
                } else if ((StringUtils.isEmpty(enrSysCode)) && (StudentEnrollment.STATUS_CHANGE.equals(enrType))) {
                    enrSysCode = OntarioAlias.ENR_CODE_RCD_SYS_TRANS_INT;
                }

                // save for prev student
                if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOidPrev.equals(stdOid))) {
                    List<Map<String, Object>> enrInfoListStdFteForStdOidArrived =
                            getEnrInfoListArrivedOnly(enrInfoListStdFteForStdOid);

                    if (enrInfoListStdFteForStdOidArrived != null) {
                        m_enrInfoListStdFteByStdOid.put(stdOidPrev, enrInfoListStdFteForStdOid);
                    }

                    enrInfoListStdFteForStdOid = new ArrayList<Map<String, Object>>();
                }
                stdOidPrev = stdOid;

                // get other enrollment fields
                PlainDate enrDate = enr.getEnrollmentDate();

                // process student enrolment if not after effective end date or external
                // transfer or retirement
                if ((!enrDate.after(m_endDateExceptExtTrRetire))
                        || (enrSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_TRANS_EXT))
                        || (enrSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_RETIRE))) {

                    /*
                     * get student enrolment fields
                     */
                    m_cal.setTime(enrDate);
                    Integer enrCalMonth = Integer.valueOf(m_cal.get(Calendar.MONTH) + 1);
                    Integer enrSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(enrCalMonth);
                    Boolean enrOpInd = Boolean.FALSE;
                    String enrOpCode = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_OTHER_PUPIL);
                    if ((!StringUtils.isEmpty(enrOpCode)) && (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(enrOpCode))) {
                        enrOpInd = Boolean.TRUE;
                    }
                    Boolean stdAdultInd = m_stdAdultIndByStdOid.get(stdOid);
                    if (stdAdultInd == null) {
                        // get student adult indicator if not calculated already for student
                        if (m_sklLvl.equals(CONST_SKL_LVL_ELEM)) {
                            stdAdultInd = Boolean.FALSE;
                        } else {
                            stdAdultInd = isStdAdult(std);
                        }
                        m_stdAdultIndByStdOid.put(stdOid, stdAdultInd);
                    }
                    Boolean stdArrivalInd = Boolean.FALSE;
                    String enrRegType = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_REGISTER);
                    if (OntarioAlias.CONST_ENR_TYPE_ENTRY.contains(enrType)) {
                        // check student arrival.
                        // student may change back to not arrived after arriving so remove student
                        // from list if so.
                        if ((enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_ARRIVAL_STATUS) != null)
                                && (enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_ARRIVAL_STATUS)
                                        .equals(OntarioAlias.CONST_ARRIVAL_STATUS_ARRIVED))) {
                            stdArrivalInd = Boolean.TRUE;
                        }

                        // get the reg type on entry records (will be used for corresponding
                        // withdrawal)
                        enrRegType = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_REGISTER);

                        // in case of an null registration type, set it to FT (only needed in case
                        // of bad data)
                        if (enrRegType == null) {
                            enrRegType = OntarioAlias.ENR_REG_TYPE_FT;
                        }
                    }

                    // set previous enrolment fields map school month out
                    if (enrInfoListStdFteForStdOid.size() > 0) {
                        Map<String, Object> enrMapPrev = enrInfoListStdFteForStdOid
                                .get(enrInfoListStdFteForStdOid.size() - 1);
                        enrMapPrev.put(CONST_ENR_OUT_SKL_MONTH, enrSklMonth);
                    }

                    /*
                     * add student enrolment fields map to list for student
                     */
                    Map<String, Object> enrMap = new HashMap<String, Object>();
                    enrMap.put(CONST_ENR_DATE, enrDate);
                    enrMap.put(CONST_ENR_REG_TYPE, enrRegType);
                    enrMap.put(CONST_ENR_CODE, enrCode);
                    enrMap.put(CONST_ENR_TYPE, enrType);
                    enrMap.put(CONST_ENR_SYS_CODE, enrSysCode);
                    enrMap.put(CONST_ENR_IN_SKL_MONTH, enrSklMonth);
                    enrMap.put(CONST_ENR_STD_OID, stdOid);
                    enrMap.put(CONST_ENR_STD_OP_IND, enrOpInd);
                    enrMap.put(CONST_ENR_STD_ADULT_IND, stdAdultInd);
                    enrMap.put(CONST_ENR_STD_ARRIVAL_IND, stdArrivalInd);
                    enrInfoListStdFteForStdOid.add(enrMap);
                }
            }
            // save for last student
            if (!StringUtils.isEmpty(stdOidPrev)) {
                List<Map<String, Object>> enrInfoListStdFteForStdOidArrived =
                        getEnrInfoListArrivedOnly(enrInfoListStdFteForStdOid);

                if (enrInfoListStdFteForStdOidArrived != null) {
                    m_enrInfoListStdFteByStdOid.put(stdOidPrev, enrInfoListStdFteForStdOidArrived);
                }
            }
        }
    }

    /**
     * Builds the student enrolment criteria - only for secondary FteStd report
     *
     * - will look at both FT and PT register types as FteStd UDD table may have
     * different values
     */
    protected void createEnrolmentCriteriaForFteStdRpt() {
        m_enrCriteria = new X2Criteria();

        /**
         * add condition for school
         */
        m_enrCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, m_school.getOid());

        /**
         * Add condition for at least one entry of enrolment grade level
         */
        // create criteria with grade level condition
        String fieldEnrGradeLvl = getBeanPathFromAlias(OntarioAlias.ALIAS_ENR_GRADE_LEVEL, false, m_dictionary);
        if (m_gradeLvls != null) {
            X2Criteria enrCriteriaGradeLvl = m_enrCriteria.copy();

            // add condition for entry records
            enrCriteriaGradeLvl.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, OntarioAlias.CONST_ENR_TYPE_ENTRY);

            // add condition for grade level
            enrCriteriaGradeLvl.addIn(fieldEnrGradeLvl, m_gradeLvls);

            // add condition for context dates
            enrCriteriaGradeLvl.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
            // use end date, 1 has been subtracted if last day of school as that should not
            // be included
            enrCriteriaGradeLvl.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

            // create sub query of student oids with at least one enrolment of grade levels
            // in school year
            SubQuery enrSubQueryGradeLvl = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID,
                    enrCriteriaGradeLvl);

            // add to main criteria
            m_enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, enrSubQueryGradeLvl);
        } else {
            m_enrCriteria.addEqualTo(fieldEnrGradeLvl, CONST_EMPTY);
        }

        /**
         * add condition for context dates
         */
        m_enrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        // use end date, 1 has been subtracted if last day of school as that should not
        // be included
        m_enrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

        /**
         * Add condition for students with StdFte udd records
         */
        m_enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, m_udtSubQueryRptTypeSec);
    }

    /**
     * Load enrolment data - for all non fte reports
     */
    protected void loadEnrolmentDataNonFteRptsInclElem() {
        // execute query and loop through students
        createEnrolmentCriteria();

        List<Map<String, Object>> enrRecsForStd = new ArrayList<Map<String, Object>>();
        m_enrInfoListByStdOid = new HashMap<String, List<Map<String, Object>>>();
        String stdOidPrev = null;

        try (QueryIterator<X2BaseBean> iterator = getBroker().getIteratorByQuery(getEnrolmentQuery())) {
            while (iterator.hasNext()) {
                StudentEnrollment enr = (StudentEnrollment) iterator.next();
                String stdOid = enr.getStudentOid();
                SisStudent std = enr.getStudent();

                // get initially required enrolment fields
                String enrCode = enr.getEnrollmentCode();
                String enrType = enr.getEnrollmentType();
                String enrSysCode = CONST_EMPTY;

                // set system code
                ReferenceCode rcdEnrCode = m_refCodeEnrCodeMap.get(enrCode);
                if (rcdEnrCode != null) {
                    String rcdEnrCodeSysType = rcdEnrCode.getSystemCode();
                    if (!StringUtils.isEmpty(rcdEnrCodeSysType)) {
                        enrSysCode = rcdEnrCodeSysType;
                    }
                }

                // set system code to internal transfer as default if entry enrollment but no
                // system code found
                if (StudentEnrollment.YOG_CHANGE.equals(enrType)) {
                    enrSysCode = OntarioAlias.ENR_CODE_RCD_SYS_TRANS_INT;
                } else if ((StringUtils.isEmpty(enrSysCode)) && (StudentEnrollment.STATUS_CHANGE.equals(enrType))) {
                    enrSysCode = OntarioAlias.ENR_CODE_RCD_SYS_TRANS_INT;
                }

                // bypass change in fte (will only be secondary) records
                String enrReason = enr.getReasonCode();
                if ((StudentEnrollment.STATUS_CHANGE.equals(enrType))
                        && (enrSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_TRANS_INT))
                        && (!StringUtils.isBlank(enrReason))
                        && (enrReason.contains(OntarioAlias.ENR_REASON_SEC_FTE_CHG))) {
                    continue;
                }

                // save for prev student
                if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOidPrev.equals(stdOid))) {
                    processEnrRecsForStd(stdOidPrev, enrRecsForStd);
                    enrRecsForStd = new ArrayList<Map<String, Object>>();
                }
                stdOidPrev = stdOid;

                // get other enrollment fields
                PlainDate enrDate = enr.getEnrollmentDate();

                // process student enrolment if not after effective end date or external
                // transfer or retirement
                if ((!enrDate.after(m_endDateExceptExtTrRetire))
                        || (enrSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_TRANS_EXT))
                        || (enrSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_RETIRE))) {
                    /*
                     * get student enrolment fields
                     */
                    m_cal.setTime(enrDate);
                    Integer enrCalMonth = Integer.valueOf(m_cal.get(Calendar.MONTH) + 1);
                    Integer enrSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(enrCalMonth);
                    Boolean enrOpInd = Boolean.FALSE;
                    String enrOpCode = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_OTHER_PUPIL);
                    if ((!StringUtils.isEmpty(enrOpCode)) && (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(enrOpCode))) {
                        enrOpInd = Boolean.TRUE;
                    }
                    String enrOpPayer = CONST_EMPTY;
                    if (enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_FUNDING_PAYER) != null) {
                        enrOpPayer = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_FUNDING_PAYER);
                    }
                    String enrGradeLvl = CONST_EMPTY;
                    if (enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_GRADE_LEVEL) != null) {
                        enrGradeLvl = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_GRADE_LEVEL);
                    }
                    Boolean stdAdultInd = m_stdAdultIndByStdOid.get(stdOid);
                    if (stdAdultInd == null) {
                        // get student adult indicator if not calculated already for student
                        if (m_sklLvl.equals(CONST_SKL_LVL_ELEM)) {
                            stdAdultInd = Boolean.FALSE;
                        } else {
                            stdAdultInd = isStdAdult(std);
                        }
                        m_stdAdultIndByStdOid.put(stdOid, stdAdultInd);
                    }
                    Boolean stdArrivalInd = Boolean.FALSE;
                    String enrRegType = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_REGISTER);
                    if (OntarioAlias.CONST_ENR_TYPE_ENTRY.contains(enrType)) {
                        // check student arrival.
                        // student may change back to not arrived after arriving so remove student
                        // from list if so.
                        if ((enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_ARRIVAL_STATUS) != null)
                                && (enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_ARRIVAL_STATUS)
                                        .equals(OntarioAlias.CONST_ARRIVAL_STATUS_ARRIVED))) {
                            stdArrivalInd = Boolean.TRUE;

                            // add to stdOids as not for student fte report
                            if (!m_stdOidsInOrder.contains(stdOid)) {
                                m_stdOidsInOrder.add(stdOid);
                                m_stdByStdOid.put(stdOid, enr.getStudent());
                            }
                        } else {
                            if (m_stdOidsInOrder.contains(stdOid)) {
                                m_stdOidsInOrder.remove(stdOid);
                                m_stdByStdOid.remove(stdOid);
                            }
                        }

                        // get the reg type on entry records (will be used for corresponding
                        // withdrawal)
                        enrRegType = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_REGISTER);

                        // in case of an null registration type, set it to FT (only needed in case
                        // of bad data)
                        if (enrRegType == null) {
                            enrRegType = OntarioAlias.ENR_REG_TYPE_FT;
                        }
                    }

                    /*
                     * add student enrolment fields map to list for student
                     */
                    Map<String, Object> enrMap = new HashMap<String, Object>();
                    enrMap.put(CONST_ENR_DATE, enrDate);
                    enrMap.put(CONST_ENR_REG_TYPE, enrRegType);
                    enrMap.put(CONST_ENR_CODE, enrCode);
                    enrMap.put(CONST_ENR_TYPE, enrType);
                    enrMap.put(CONST_ENR_SYS_CODE, enrSysCode);
                    enrMap.put(CONST_ENR_SKL_MONTH, enrSklMonth);
                    enrMap.put(CONST_ENR_STD_OID, stdOid);
                    enrMap.put(CONST_ENR_STD_OP_IND, enrOpInd);
                    enrMap.put(CONST_ENR_STD_PAYER, enrOpPayer);
                    enrMap.put(CONST_ENR_STD_GRADE_LVL, enrGradeLvl);
                    enrMap.put(CONST_ENR_STD_ADULT_IND, stdAdultInd);
                    enrMap.put(CONST_ENR_STD_ARRIVAL_IND, stdArrivalInd);
                    // save minutes/fte, will only be used for elementary reports
                    String enrMinsDb = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_MINUTES);
                    enrMap.put(CONST_ENR_MINS, enrMinsDb);
                    String enrFteDb = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_FTE);
                    enrMap.put(CONST_ENR_FTE, enrFteDb);
                    String enrFteHcDb = (String) enr.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_FTE_HC);
                    enrMap.put(CONST_ENR_FTE_HC, enrFteHcDb);
                    enrRecsForStd.add(enrMap);
                }
            }
            // save for last student
            if (!StringUtils.isEmpty(stdOidPrev)) {
                processEnrRecsForStd(stdOidPrev, enrRecsForStd);
            }
        }
    }

    /**
     * Builds the student enrolment criteria
     */
    protected void createEnrolmentCriteria() {
        m_enrCriteria = new X2Criteria();

        /**
         * add condition for school
         */
        m_enrCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, m_school.getOid());

        /**
         * Add condition for at least one entry of enrolment grade level
         */
        // create criteria with grade level condition
        String fieldEnrGradeLvl = getBeanPathFromAlias(OntarioAlias.ALIAS_ENR_GRADE_LEVEL, false, m_dictionary);
        if (m_gradeLvls != null) {
            X2Criteria enrCriteriaGradeLvl = m_enrCriteria.copy();

            // add condition for entry records
            enrCriteriaGradeLvl.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, OntarioAlias.CONST_ENR_TYPE_ENTRY);

            // add condition for grade level
            enrCriteriaGradeLvl.addIn(fieldEnrGradeLvl, m_gradeLvls);

            // add condition for context dates
            enrCriteriaGradeLvl.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
            // use end date, 1 has been subtracted if last day of school as that should not
            // be included
            enrCriteriaGradeLvl.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

            // create sub query of student oids with at least one enrolment of grade levels
            // in school year
            SubQuery enrSubQueryGradeLvl = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID,
                    enrCriteriaGradeLvl);

            // add to main criteria
            m_enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, enrSubQueryGradeLvl);
        } else {
            m_enrCriteria.addEqualTo(fieldEnrGradeLvl, CONST_EMPTY);
        }

        /**
         * add condition for context dates
         */
        m_enrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        // use end date, 1 has been subtracted if last day of school as that should not
        // be included
        m_enrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

        /**
         * Add condition for students with at least one entry of report type
         */
        SubQuery enrSubQueryRptType = createRptTypeStdOidsSubQuery(m_enrCriteria);

        // add to main criteria, null condition added to avoid exception but will never
        // be needed
        if (enrSubQueryRptType != null) {
            m_enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, enrSubQueryRptType);
        }
    }

    /**
     * Create a sub query of student oids to include based on enrolment records or
     * udt table - Adds condition for at least one entry of report type. - Enrolment
     * table is checked in this helper. Udt will be checked in secondary override.
     *
     * @param enrCriteria
     * @return SubQuery - Sub query of student oids based on reg type
     */
    protected SubQuery createRptTypeStdOidsSubQuery(X2Criteria enrCriteria) {
        // create criteria with report type condition
        X2Criteria enrCriteriaRptType = enrCriteria.copy();
        String fieldEnrRegister = getBeanPathFromAlias(OntarioAlias.ALIAS_ENR_REGISTER, false, m_dictionary);

        // add condition for entry records
        enrCriteriaRptType.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, OntarioAlias.CONST_ENR_TYPE_ENTRY);

        // add condition based on input report type
        if (m_rptType.equals(CONST_RPT_TYPE_FT)) {
            enrCriteriaRptType.addEqualTo(fieldEnrRegister, OntarioAlias.ENR_REG_TYPE_FT);
        } else if (m_rptType.equals(CONST_RPT_TYPE_PT)) {
            enrCriteriaRptType.addEqualTo(fieldEnrRegister, OntarioAlias.ENR_REG_TYPE_PT);
        }

        // create sub query of student oids with at least one enrolment of report type
        // in school
        // year
        SubQuery enrSubQueryRptType = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID,
                enrCriteriaRptType);

        return enrSubQueryRptType;
    }

    /**
     * Builds the student enrolment query based on enrolment criteria. Fetching
     * entire enrolment record as more fields maybe needed (codes added later)
     *
     * @return QueryByCriteria
     */
    protected QueryByCriteria getEnrolmentQuery() {
        // create query
        QueryByCriteria enrQuery = new QueryByCriteria(StudentEnrollment.class, m_enrCriteria);

        // add sort by student name, state id (oen) if duplicates
        enrQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PERSON
                + PATH_DELIMITER + m_fieldPsnLegalLastName);
        enrQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PERSON
                + PATH_DELIMITER + m_fieldPsnLegalFirstName);

        enrQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);

        // add sort ascending by ate, timestamp
        enrQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrQuery.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        return enrQuery;
    }

    /**
     * Processes student enrolment records, combine with student fte udt data
     *
     * @param stdOid
     * @param enrRecsForStd
     */
    protected void processEnrRecsForStd(String stdOid, List<Map<String, Object>> enrRecsForStd) {
        /*
         * initialize variables
         */
        PlainDate minValue = new PlainDate(DateUtils.getDate("0000-01-01"));
        PlainDate maxValue = new PlainDate(DateUtils.getDate("9999-12-31"));
        PlainDate enrRecCurrDate = new PlainDate(minValue);
        PlainDate regChgCurrDate = new PlainDate(minValue);
        int enrRecMaxIndex = enrRecsForStd.size() - 1;
        int regChgMaxIndex = -1;
        List<Map<String, Object>> enrInfoListForStdOid = new ArrayList<Map<String, Object>>();

        // check if pre=enrolment data saved
        List<Map<String, Object>> regChgInfoOnePerRegTypeChgForStdOid = null;
        if ((m_regChgInfoOnePerRegTypeChgByStdOid != null)
                && (m_regChgInfoOnePerRegTypeChgByStdOid.get(stdOid) != null)) {
            regChgInfoOnePerRegTypeChgForStdOid = m_regChgInfoOnePerRegTypeChgByStdOid.get(stdOid);
            regChgMaxIndex = regChgInfoOnePerRegTypeChgForStdOid.size() - 1;
        } else {
            // if it is a secondary student and no pre-enrolment record for run reg type is found
            // bypass the student after writing an empty output map
            // that is because the student should not show in this case as the reg field on the
            // enrolment record is incorrect
            if (m_sklLvl.equals(CONST_SKL_LVL_SEC)) {
                m_enrInfoListByStdOid.put(stdOid, enrInfoListForStdOid);
                return;
            }

            // otherwise (elementary) set to max date
            regChgCurrDate = new PlainDate(maxValue);
        }
        List<Map<String, Object>> regChgInfoAllRegTypeChgForStdOid = null;
        if ((m_regChgInfoAllRegTypeChgByStdOid != null)
                && (m_regChgInfoAllRegTypeChgByStdOid.get(stdOid) != null)) {
            regChgInfoAllRegTypeChgForStdOid = m_regChgInfoAllRegTypeChgByStdOid.get(stdOid);
        }

        // previous enrolment variables
        Boolean enrOpIndPrev = null;
        String enrRegTypePrev = CONST_EMPTY;
        Boolean enrArrivalIndPrev = null;

        // set current indexes/indicators in lists
        PlainDate processCurrDate = new PlainDate(minValue);
        int enrRecCurrIndex = -1;
        int regChgCurrIndex = -1;
        Map<String, Object> enrRecCurr = null;
        Map<String, Object> regChgCurr = null;
        Map<String, Object> enrInfoCurr = null;
        String chgTypeInCurr = null;
        String chgTypeOutCurr = null;

        /*
         * Loop thru enrolment records saving info in entry/withdrawal pairs with udt
         * lookup
         */
        while (processCurrDate.before(maxValue)) {
            // set prev reg type
            if (enrInfoCurr != null) {
                enrRegTypePrev = (String) enrInfoCurr.get(CONST_ENR_REG_TYPE);
                enrOpIndPrev = (Boolean) enrInfoCurr.get(CONST_ENR_STD_OP_IND);
                enrArrivalIndPrev = (Boolean) enrInfoCurr.get(CONST_ENR_STD_ARRIVAL_IND);
            }

            if ((enrRecCurrIndex == -1) || (regChgCurrDate.after(enrRecCurrDate))) {
                if (enrRecCurrIndex < enrRecMaxIndex) {
                    enrRecCurrIndex++;
                    enrRecCurr = enrRecsForStd.get(enrRecCurrIndex);
                    enrRecCurrDate = (PlainDate) enrRecCurr.get(CONST_ENR_DATE);
                } else {
                    enrRecCurrDate = new PlainDate(maxValue);
                }
            } else {
                if (regChgCurrIndex < regChgMaxIndex) {
                    regChgCurrIndex++;
                    regChgCurr = regChgInfoOnePerRegTypeChgForStdOid.get(regChgCurrIndex);
                    regChgCurrDate = (PlainDate) regChgCurr.get(CONST_STD_REG_CHG_DT);

                    // check for change in register and go to next iteration if same
                    // -as looking for register changes from the udt here
                    String regChgRegType = (String) regChgCurr.get(CONST_STD_REG_REG_TYPE);
                    if (regChgRegType.equals(enrRegTypePrev)) {
                        continue;
                    }
                } else {
                    regChgCurrDate = new PlainDate(maxValue);
                }
            }

            processCurrDate = regChgCurrDate;
            if (enrRecCurrDate.before(processCurrDate)) {
                processCurrDate = enrRecCurrDate;
            }
            if (processCurrDate.equals(maxValue)) {
                continue;
            }

            if ((regChgCurr != null) && (regChgCurrDate.before(enrRecCurrDate)) && (!regChgCurrDate.equals(maxValue))) {
                // create if new or previous ended
                if (enrInfoCurr == null) {
                    enrInfoCurr = createNewEnrInfo(stdOid, enrInfoListForStdOid, regChgCurr, null,
                            null);
                }

                // determine change type
                chgTypeInCurr = CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT;
                chgTypeOutCurr = CONST_ENR_CHG_TYPE_VALUE_REG_TYPE;

                // update withdraw
                updateEnrInfoWithdraw(stdOid, enrInfoListForStdOid, enrInfoCurr, chgTypeOutCurr, regChgCurr, null);
                chgTypeOutCurr = null;

                // create new before entry
                enrInfoCurr = createNewEnrInfo(stdOid, enrInfoListForStdOid, regChgCurr, null, null);

                // update entry
                updateEnrInfoEntry(stdOid, enrInfoListForStdOid, enrInfoCurr, chgTypeInCurr, regChgCurr, null);
                chgTypeInCurr = null;
            }

            else {
                // create enrInfo if new or previous ended, also set enrRegType
                if (enrInfoCurr == null) {
                    enrInfoCurr = createNewEnrInfo(stdOid, enrInfoListForStdOid, null, enrRecCurr,
                            regChgInfoAllRegTypeChgForStdOid);
                }

                // determine change type
                String enrType = (String) enrRecCurr.get(CONST_ENR_TYPE);
                String enrRegType = (String) enrRecCurr.get(CONST_ENR_REG_TYPE);
                Boolean enrOpInd = (Boolean) enrRecCurr.get(CONST_ENR_STD_OP_IND);
                Boolean enrArrivalInd = (Boolean) enrRecCurr.get(CONST_ENR_STD_ARRIVAL_IND);

                // entry record
                String chgTypeInNew = null;
                String chgTypeOutNew = null;
                {
                    if (StudentEnrollment.ENTRY.equals(enrType)) {
                        chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_ENTRY_EXT;
                    } else
                    // entry record that changes not arrived (prior must exist) to arrived
                    if ((OntarioAlias.CONST_ENR_TYPE_ENTRY.contains(enrType))
                            && (enrArrivalInd.booleanValue())
                            && (enrArrivalIndPrev != null) && (!enrArrivalIndPrev.booleanValue())) {
                        chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_ENTRY_EXT;
                    } else
                    // yog change
                    if (StudentEnrollment.YOG_CHANGE.equals(enrType)) {
                        chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_YOG;
                        chgTypeOutNew = CONST_ENR_CHG_TYPE_VALUE_YOG;
                    } else
                    // other internal entry record
                    if (StudentEnrollment.STATUS_CHANGE.equals(enrType)) {
                        // initial student enrolment is S record
                        if (enrOpIndPrev == null) {
                            chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT;
                        }
                        // op change
                        else if (!enrOpInd.equals(enrOpIndPrev)) {
                            chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_OP_IND;
                            chgTypeOutNew = CONST_ENR_CHG_TYPE_VALUE_OP_IND;
                        }
                        // possible register change (anomaly) for secondary
                        else if ((regChgInfoOnePerRegTypeChgForStdOid != null)
                                && (!enrRegType.equals(enrRegTypePrev))) {
                            chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT;
                            chgTypeOutNew = CONST_ENR_CHG_TYPE_VALUE_REG_TYPE;
                        }
                        // default register change assumed - for elementary only here, bypass if
                        // secondary
                        else if (regChgInfoOnePerRegTypeChgForStdOid == null) {
                            chgTypeInNew = CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT;
                            chgTypeOutNew = CONST_ENR_CHG_TYPE_VALUE_REG_TYPE;
                        }
                    }
                    // withdrawal record
                    if (enrType.equals(StudentEnrollment.WITHDRAWAL)) {
                        chgTypeOutNew = CONST_ENR_CHG_TYPE_VALUE_WITHDRAW;
                    }
                    // reset chg type withdraw new value if a current entry record does not
                    // exist
                    // not needed for reg chg above as reg chg has withdraw/entry pair
                    if ((enrInfoCurr.get(CONST_ENR_IN_DATE) == null) && (!StringUtils.isEmpty(chgTypeOutNew))) {
                        chgTypeOutNew = null;
                    }

                    // set chg type curr values to the new ones
                    if (chgTypeInCurr == null) {
                        chgTypeInCurr = chgTypeInNew;
                    }
                    if (chgTypeOutCurr == null) {
                        chgTypeOutCurr = chgTypeOutNew;
                    }

                    // if reg type and date is same as previous enr info bypass withdraw and
                    // overlay
                    // entry when not a withdraw record
                    boolean sameRegTypeAndDate = false;
                    if (!((chgTypeOutNew != null) && ((chgTypeOutNew.equals(CONST_ENR_CHG_TYPE_VALUE_WITHDRAW))
                            || (chgTypeOutNew.equals(CONST_ENR_CHG_TYPE_VALUE_OP_IND))
                            || (chgTypeOutNew.equals(CONST_ENR_CHG_TYPE_VALUE_YOG))))) {
                        PlainDate enrInDate = (PlainDate) enrInfoCurr.get(CONST_ENR_IN_DATE);
                        if (!((enrRegType == null) || (enrRegTypePrev == null) || (enrInDate == null))
                                && (enrRegType.equals(enrRegTypePrev)) && (enrInDate.equals(enrRecCurrDate))) {
                            sameRegTypeAndDate = true;
                        }
                    }

                    // update withdraw
                    if ((!StringUtils.isEmpty(chgTypeOutCurr)) && (!sameRegTypeAndDate)) {
                        updateEnrInfoWithdraw(stdOid, enrInfoListForStdOid, enrInfoCurr, chgTypeOutCurr, null,
                                enrRecCurr);

                        // processing for mins/fte - bypass if enrInfoCurr has student not arrived
                        Boolean stdArrivalInd = (Boolean) enrInfoCurr.get(CONST_ENR_STD_ARRIVAL_IND);
                        if ((stdArrivalInd != null) && (stdArrivalInd.booleanValue())) {
                            accumEnrMinsFte(stdOid, enrInfoCurr, enrRecCurr,
                                    (String) enrRecCurr.get(CONST_ENR_REG_TYPE),
                                    (Integer) enrInfoCurr.get(CONST_ENR_OUT_SKL_MONTH), false, true,
                                    m_minsAndFteInfoByStdOid, m_minsAndFteFromDbInfoByStdOid);
                        }
                        chgTypeOutCurr = null;
                    }

                    // update entry
                    if (!StringUtils.isEmpty(chgTypeInCurr)) {
                        // if same reg type and date remove the last enr info entry from list as
                        // will be
                        // replaced in the
                        // update
                        if (sameRegTypeAndDate) {
                            enrInfoListForStdOid.remove(enrInfoListForStdOid.size() - 1);
                        }

                        // create new before entry if not to be overlaid
                        enrInfoCurr = createNewEnrInfo(stdOid, enrInfoListForStdOid, null, enrRecCurr,
                                regChgInfoAllRegTypeChgForStdOid);

                        updateEnrInfoEntry(stdOid, enrInfoListForStdOid, enrInfoCurr, chgTypeInCurr, null,
                                enrRecCurr);

                        // processing for mins/fte
                        Boolean stdArrivalInd = (Boolean) enrInfoCurr.get(CONST_ENR_STD_ARRIVAL_IND);
                        if ((stdArrivalInd != null) && (stdArrivalInd.booleanValue())) {
                            accumEnrMinsFte(stdOid, enrInfoCurr, enrRecCurr,
                                    (String) enrRecCurr.get(CONST_ENR_REG_TYPE),
                                    (Integer) enrInfoCurr.get(CONST_ENR_IN_SKL_MONTH), true, false,
                                    m_minsAndFteInfoByStdOid, m_minsAndFteFromDbInfoByStdOid);
                        }
                        chgTypeInCurr = null;
                    }
                }
            }
        }

        List<Map<String, Object>> enrInfoListForStdOidArrived =
                getEnrInfoListArrivedOnly(enrInfoListForStdOid);
        if (enrInfoListForStdOidArrived != null) {
            m_enrInfoListByStdOid.put(stdOid, enrInfoListForStdOidArrived);
        }
    }

    /**
     * Create new enrolment info paired record
     *
     * @param stdOid
     * @param enrInfoListForStdOid - updated with new one
     * @param regChg
     * @param enrRec
     * @param regChgInfoAllRegTypeChgForStdOid
     *
     * @return Map<String, Object> - new enrolment info
     */
    protected Map<String, Object> createNewEnrInfo(String stdOid,
                                                   List<Map<String, Object>> enrInfoListForStdOid,
                                                   Map<String, Object> regChg,
                                                   Map<String, Object> enrRec,
                                                   List<Map<String, Object>> regChgInfoAllRegTypeChgForStdOid) {
        String enrRegType = CONST_EMPTY;
        Boolean enrOpInd = Boolean.FALSE;
        String enrOpPayer = CONST_EMPTY;
        String enrGradeLvl = CONST_EMPTY;
        Boolean stdAdultInd = Boolean.FALSE;
        Boolean stdArrivalInd = Boolean.FALSE;

        // use student fte udt
        if (regChg != null) {
            enrRegType = (String) regChg.get(CONST_STD_REG_REG_TYPE);
            // the year/report would always start with a student enrolment record so fetch
            // these
            // from previous student enrolment or in case of data issue leaving blank
            Map<String, Object> enrInfoPrev = null;
            if (!enrInfoListForStdOid.isEmpty()) {
                int enrInfoPrevIndex = enrInfoListForStdOid.size() - 1;
                enrInfoPrev = enrInfoListForStdOid.get(enrInfoPrevIndex);
                enrOpInd = (Boolean) enrInfoPrev.get(CONST_ENR_STD_OP_IND);
                enrOpPayer = (String) enrInfoPrev.get(CONST_ENR_STD_PAYER);
                enrGradeLvl = (String) enrInfoPrev.get(CONST_ENR_STD_GRADE_LVL);
                stdAdultInd = (Boolean) enrInfoPrev.get(CONST_ENR_STD_ADULT_IND);
                stdArrivalInd = (Boolean) enrInfoPrev.get(CONST_ENR_STD_ARRIVAL_IND);
            }
        }
        // use student enrolment fields
        else if (enrRec != null) {
            enrRegType = (String) enrRec.get(CONST_ENR_REG_TYPE);
            enrOpInd = (Boolean) enrRec.get(CONST_ENR_STD_OP_IND);
            enrOpPayer = (String) enrRec.get(CONST_ENR_STD_PAYER);
            enrGradeLvl = (String) enrRec.get(CONST_ENR_STD_GRADE_LVL);
            stdAdultInd = (Boolean) enrRec.get(CONST_ENR_STD_ADULT_IND);
            stdArrivalInd = (Boolean) enrRec.get(CONST_ENR_STD_ARRIVAL_IND);

            // get regType from RegChg records if secondary school
            if (!m_sklLvl.equals(CONST_SKL_LVL_ELEM)) {
                PlainDate enrRecCurrDate = (PlainDate) enrRec.get(CONST_ENR_DATE);
                enrRegType =
                        getEnrRecRegTypeFromRegChgRecs(stdOid, enrRecCurrDate,
                                regChgInfoAllRegTypeChgForStdOid);
                enrRec.put(CONST_ENR_REG_TYPE, enrRegType);
            }
        }

        // save paired enrolment info
        Map<String, Object> enrInfoNew = new HashMap<String, Object>();
        enrInfoNew.put(CONST_ENR_STD_OID, stdOid);
        enrInfoNew.put(CONST_ENR_REG_TYPE, enrRegType);
        enrInfoNew.put(CONST_ENR_STD_OP_IND, enrOpInd);
        enrInfoNew.put(CONST_ENR_STD_PAYER, enrOpPayer);
        enrInfoNew.put(CONST_ENR_STD_GRADE_LVL, enrGradeLvl);
        enrInfoNew.put(CONST_ENR_STD_ADULT_IND, stdAdultInd);
        enrInfoNew.put(CONST_ENR_STD_ARRIVAL_IND, stdArrivalInd);

        return enrInfoNew;
    }

    /**
     * Get the reg type from the RegChg records with the date closest but not greater than the
     * enrolment record date
     *
     * @param stdOid
     * @param enrRecCurrDate
     * @param regChgInfoAllRegTypeChgForStdOid
     *
     * @return
     */
    private String getEnrRecRegTypeFromRegChgRecs(String stdOid,
                                                  PlainDate enrRecCurrDate,
                                                  List<Map<String, Object>> regChgInfoAllRegTypeChgForStdOid) {
        String enrRecRegTypeFromRegChg = CONST_EMPTY;
        for (Map<String, Object> regChgInfoOfDates : regChgInfoAllRegTypeChgForStdOid) {
            PlainDate regChgDate = (PlainDate) regChgInfoOfDates.get(CONST_STD_REG_CHG_DT);
            if (!regChgDate.after(enrRecCurrDate)) {
                enrRecRegTypeFromRegChg = (String) regChgInfoOfDates.get(CONST_STD_REG_REG_TYPE);
            }
        }

        return enrRecRegTypeFromRegChg;
    }

    /**
     * Update enrolment info paired record with entry fields
     *
     * @param stdOid
     * @param enrInfoListForStdOid
     * @param enrInfo
     * @param chgTypeIn
     * @param regChg
     * @param enrRec
     */
    private void updateEnrInfoEntry(String stdOid,
                                    List<Map<String, Object>> enrInfoListForStdOid,
                                    Map<String, Object> enrInfo,
                                    String chgTypeIn,
                                    Map<String, Object> regChg,
                                    Map<String, Object> enrRec) {
        String enrInCode = CONST_EMPTY;
        String enrInType = CONST_EMPTY;
        PlainDate enrInDate = null;
        String enrInSysCode = CONST_EMPTY;
        Integer enrInDateSklMonth = null;

        // use student fte udt
        if (regChg != null) {
            enrInDate = (PlainDate) regChg.get(CONST_STD_REG_CHG_DT);
            enrInDateSklMonth = (Integer) regChg.get(CONST_STD_REG_CHG_SKL_MONTH);
        }
        // use student enrolment fields
        else if (enrRec != null) {
            enrInCode = (String) enrRec.get(CONST_ENR_CODE);
            enrInType = (String) enrRec.get(CONST_ENR_TYPE);
            enrInDate = (PlainDate) enrRec.get(CONST_ENR_DATE);
            enrInSysCode = (String) enrRec.get(CONST_ENR_SYS_CODE);
            enrInDateSklMonth = (Integer) enrRec.get(CONST_ENR_SKL_MONTH);
        }

        // save paired enrolment info
        enrInfo.put(CONST_ENR_IN_CODE, enrInCode);
        enrInfo.put(CONST_ENR_IN_TYPE, enrInType);
        enrInfo.put(CONST_ENR_IN_DATE, enrInDate);
        enrInfo.put(CONST_ENR_IN_SYS_CODE, enrInSysCode);
        enrInfo.put(CONST_ENR_IN_CHG_TYPE, chgTypeIn);
        enrInfo.put(CONST_ENR_IN_SKL_MONTH, enrInDateSklMonth);

        enrInfoListForStdOid.add(enrInfo);
    }

    /**
     * Update enrolment info paired record with withdraw fields
     *
     * @param stdOid
     * @param enrInfoListForStdOid
     * @param enrInfo
     * @param chgTypeOut
     * @param regChg
     * @param enrRec
     */
    private void updateEnrInfoWithdraw(String stdOid,
                                       List<Map<String, Object>> enrInfoListForStdOid,
                                       Map<String, Object> enrInfo,
                                       String chgTypeOut,
                                       Map<String, Object> regChg,
                                       Map<String, Object> enrRec) {
        String enrOutCode = CONST_EMPTY;
        String enrOutType = CONST_EMPTY;
        PlainDate enrOutDate = null;
        String enrOutSysCode = CONST_EMPTY;
        Integer enrOutDateSklMonth = null;

        // use student fte udt
        if (regChg != null) {
            enrOutDate = (PlainDate) regChg.get(CONST_STD_REG_CHG_DT);
            enrOutDateSklMonth = (Integer) regChg.get(CONST_STD_REG_CHG_SKL_MONTH);
        }
        // use student enrolment fields
        else if (enrRec != null) {
            enrOutCode = (String) enrRec.get(CONST_ENR_CODE);
            enrOutType = (String) enrRec.get(CONST_ENR_TYPE);
            enrOutDate = (PlainDate) enrRec.get(CONST_ENR_DATE);
            enrOutSysCode = (String) enrRec.get(CONST_ENR_SYS_CODE);
            enrOutDateSklMonth = (Integer) enrRec.get(CONST_ENR_SKL_MONTH);
        }

        // save paired enrolment info
        enrInfo.put(CONST_ENR_OUT_CODE, enrOutCode);
        enrInfo.put(CONST_ENR_OUT_TYPE, enrOutType);
        enrInfo.put(CONST_ENR_OUT_DATE, enrOutDate);
        enrInfo.put(CONST_ENR_OUT_SYS_CODE, enrOutSysCode);
        enrInfo.put(CONST_ENR_OUT_CHG_TYPE, chgTypeOut);
        enrInfo.put(CONST_ENR_OUT_SKL_MONTH, enrOutDateSklMonth);
    }

    /**
     * Accumulate enrolment minutes
     *
     * From enrollment records for elementary schools - Resets to zero for
     * withdrawal records
     *
     * For secondary schools saved from user defined tables, resets to zero for
     * withdrawal records.
     *
     * @param stdOid
     * @param enrInfo - StudentEnrollment info
     * @param enrRec - enrolment record
     * @param enrRegType - record reg type - For Entry records, empty if
     *        Withdrawal
     * @param enrSklMonth - school month
     * @param saveEntry
     * @param saveWithdraw
     */
    protected void accumEnrMinsFte(String stdOid,
                                   Map<String, Object> enrInfo,
                                   Map<String, Object> enrRec,
                                   String enrRegType,
                                   Integer enrSklMonth,
                                   boolean saveEntry,
                                   boolean saveWithdraw,
                                   Map<String, Map<String, Object>> minsAndFteInfoByStdOid,
                                   Map<String, Map<String, Object>> minsAndFteFromDbInfoByStdOid) {
        // Override in extended class will save mins/fte
    }

    /**
     * Copy to create list of enrInfo maps for student which have arrived status
     * Return null if empty
     *
     * @param enrInfoListForStdOid
     *
     * @return List<Map<String, Object>>
     */
    private List<Map<String, Object>> getEnrInfoListArrivedOnly(List<Map<String, Object>> enrInfoListForStdOid) {
        List<Map<String, Object>> enrInfoListArrivedOnly = new ArrayList<Map<String, Object>>();

        for (Map<String, Object> enrInfo : enrInfoListForStdOid) {
            Boolean stdArrivalInd = (Boolean) enrInfo.get(CONST_ENR_STD_ARRIVAL_IND);
            if ((stdArrivalInd != null) && (stdArrivalInd.booleanValue())) {
                enrInfoListArrivedOnly.add(enrInfo);
            }
        }

        // return null if empty
        if (enrInfoListArrivedOnly.isEmpty()) {
            return null;
        }

        return enrInfoListArrivedOnly;
    }

    /**
     * Populates grid by student
     */
    protected void populateGrids() {
        // initialize output summary variables
        initVarSummaryGeneral();

        // initialize summary variables by month
        initVarSummaryMonth();

        // initialize summary variables by student
        if ((m_writeGridSummaryStd) || (m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
            initVarSummaryStd();
        }

        // loop through students
        for (String stdOid : m_stdOidsInOrder) {
            SisStudent std = m_stdByStdOid.get(stdOid);

            if (!m_writeGridSummaryFteStd) {
                List<Map<String, Object>> enrInfoListForStdOid = m_enrInfoListByStdOid.get(stdOid);

                int enrInfoCt = enrInfoListForStdOid.size();

                for (int enrInfoIndex = 0; enrInfoIndex < enrInfoCt; enrInfoIndex++) {
                    Map<String, Object> enrInfo = enrInfoListForStdOid.get(enrInfoIndex);

                    // only process when entry reg type matches report reg type
                    String enrRegType = (String) enrInfo.get(CONST_ENR_REG_TYPE);
                    if ((enrRegType == null) || (!enrRegType.equals(m_rptType))) {
                        continue;
                    }

                    // determine and process entry/withdrawal dates
                    addEnrInfoDates(std, enrInfo);

                    // write detail rows
                    if (m_writeGridDetail) {
                        Map<String, Object> enrInfoPrev = null;
                        if (enrInfoIndex > 0) {
                            enrInfoPrev = enrInfoListForStdOid.get(enrInfoIndex - 1);
                        }
                        // write detail - checks report/enrolment registration type
                        writeGridDetail(std, enrInfo, enrInfoPrev);
                    }
                }

                // write summary row by student
                if ((m_writeGridSummaryStd) || (m_writeGridSummaryStdHc)) {
                    writeGridSummaryStd(std, enrInfoListForStdOid);
                }
            }

            else {
                // write summary row by student for StdFte report
                List<Map<String, Object>> enrInfoListStdFteForStdOid = m_enrInfoListStdFteByStdOid.get(stdOid);

                writeGridSummaryStd(std, enrInfoListStdFteForStdOid);
            }
        }

        // accumulate monthly counts, will only be written if selected to write summary
        // grid by month
        if (!m_writeGridSummaryFteStd) {
            // initialize variables
            int prevCtPb = CONST_ZERO.intValue();
            int prevCtPbAdult = CONST_ZERO.intValue();
            int prevCtOp = CONST_ZERO.intValue();
            int prevCtOpAdult = CONST_ZERO.intValue();
            int netCtPb = CONST_ZERO.intValue();
            int netCtPbAdult = CONST_ZERO.intValue();
            int netCtOp = CONST_ZERO.intValue();
            int netCtOpAdult = CONST_ZERO.intValue();
            // write grid for Adult students separately if needed
            for (int sklMonthLoop = m_startDateSklMonth.intValue(); sklMonthLoop <= m_endDateSklMonth
                    .intValue(); sklMonthLoop++) {
                // set previous count to previous net count
                prevCtPb = netCtPb;
                prevCtOp = netCtOp;

                // get new net count
                Map<String, Integer> netCtByTypeMap = processGridSummaryMonth(m_gridSummaryMonth,
                        Integer.valueOf(sklMonthLoop), prevCtPb, prevCtOp, false);
                netCtPb = netCtByTypeMap.get(CONST_STD_TYP_PB);
                netCtOp = netCtByTypeMap.get(CONST_STD_TYP_OP);

                // write summary grid - adult maps
                // set previous count to previous net count
                prevCtPbAdult = netCtPbAdult;
                prevCtOpAdult = netCtOpAdult;

                // get new net count
                Map<String, Integer> netCtAdultByTypeMap = processGridSummaryMonth(m_gridSummaryMonthAdult,
                        Integer.valueOf(sklMonthLoop), prevCtPbAdult, prevCtOpAdult, true);
                netCtPbAdult = netCtAdultByTypeMap.get(CONST_STD_TYP_PB);
                netCtOpAdult = netCtAdultByTypeMap.get(CONST_STD_TYP_OP);

                // save student counts for Oct and Mar
                if (sklMonthLoop == OntarioAlias.CONST_SKL_MONTH_OCT) {
                    m_pBCtOct = netCtPb;
                    m_oPCtOct = netCtOp;
                    m_pBCtAdultOct = netCtPbAdult;
                    m_oPCtAdultOct = netCtOpAdult;
                } else if (sklMonthLoop == OntarioAlias.CONST_SKL_MONTH_MAR) {
                    m_pBCtMar = netCtPb;
                    m_oPCtMar = netCtOp;
                    m_pBCtAdultMar = netCtPbAdult;
                    m_oPCtAdultMar = netCtOpAdult;
                }
            }
        }
    }

    /**
     * Initialize variables - summary general
     */
    protected void initVarSummaryGeneral() {
        // count variable initialized for student fte report - other done with monthly
        // counts
        if (m_writeGridSummaryFteStd) {
            m_pBCtOct = CONST_ZERO.intValue();
            m_pBCtMar = CONST_ZERO.intValue();
            m_oPCtOct = CONST_ZERO.intValue();
            m_oPCtMar = CONST_ZERO.intValue();
            m_pBCtAdultOct = CONST_ZERO.intValue();
            m_pBCtAdultMar = CONST_ZERO.intValue();
            m_oPCtAdultOct = CONST_ZERO.intValue();
            m_oPCtAdultMar = CONST_ZERO.intValue();
        }
    }

    /**
     * Initialize variables - summary month
     */
    protected void initVarSummaryMonth() {
        m_sumAdmisIntCtPbBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisIntCtOpBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisExtCtPbBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisExtCtOpBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransIntCtPbBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransIntCtOpBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransExtCtPbBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransExtCtOpBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumRetireCtPbBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumRetireCtOpBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisIntCtPbAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisIntCtOpAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisExtCtPbAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumAdmisExtCtOpAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransIntCtPbAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransIntCtOpAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransExtCtPbAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumTransExtCtOpAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumRetireCtPbAdultBySklMonthMap = new HashMap<Integer, Integer>();
        m_sumRetireCtOpAdultBySklMonthMap = new HashMap<Integer, Integer>();
    }

    /**
     * Initialize variables - summary student (initially)
     */
    protected void initVarSummaryStd() {
        m_pBFteSumOct = CONST_ZERO_DBL.doubleValue();
        m_oPFteSumOct = CONST_ZERO_DBL.doubleValue();
        m_pBFteSumMar = CONST_ZERO_DBL.doubleValue();
        m_oPFteSumMar = CONST_ZERO_DBL.doubleValue();

        // additional high credit
        if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
            m_pBFteSumHcOct = CONST_ZERO_DBL.doubleValue();
            m_oPFteSumHcOct = CONST_ZERO_DBL.doubleValue();
            m_pBFteSumHcMar = CONST_ZERO_DBL.doubleValue();
            m_oPFteSumHcMar = CONST_ZERO_DBL.doubleValue();
        }
    }

    /**
     * Determine and add enrolment info dates
     *
     * @param std
     * @param enrInfo - updated
     */
    private void addEnrInfoDates(SisStudent std, Map<String, Object> enrInfo) {
        // get variables
        String stdOid = std.getOid();
        Boolean enrStdAdultInd = (Boolean) enrInfo.get(CONST_ENR_STD_ADULT_IND);

        String enrRegTyp = (String) enrInfo.get(CONST_ENR_REG_TYPE);
        String enrStdGradeLvl = (String) enrInfo.get(CONST_ENR_STD_GRADE_LVL);

        String enrInChgType = (String) enrInfo.get(CONST_ENR_IN_CHG_TYPE);
        PlainDate enrInDate = (PlainDate) enrInfo.get(CONST_ENR_IN_DATE);
        String enrInSysCode = (String) enrInfo.get(CONST_ENR_IN_SYS_CODE);
        Integer enrInSklMonth = (Integer) enrInfo.get(CONST_ENR_IN_SKL_MONTH);

        String enrOutChgType = (String) enrInfo.get(CONST_ENR_OUT_CHG_TYPE);
        PlainDate enrOutDate = (PlainDate) enrInfo.get(CONST_ENR_OUT_DATE);
        String enrOutSysCode = (String) enrInfo.get(CONST_ENR_OUT_SYS_CODE);
        Integer enrOutSklMonth = (Integer) enrInfo.get(CONST_ENR_OUT_SKL_MONTH);

        // get Pb, Op indicators
        Boolean stdPbIndSklMonth = null;
        Boolean stdOpIndSklMonth = null;
        if ((enrInfo.get(CONST_ENR_STD_OP_IND) != null) && ((Boolean) enrInfo.get(CONST_ENR_STD_OP_IND))) {
            stdPbIndSklMonth = Boolean.FALSE;
            stdOpIndSklMonth = Boolean.TRUE;
        } else {
            stdPbIndSklMonth = Boolean.TRUE;
            stdOpIndSklMonth = Boolean.FALSE;
        }

        // save Pb, Op indicators if register type is for run selection and month
        // Oct/Mar
        if ((((m_rptType.equals(CONST_RPT_TYPE_FT)) && (enrRegTyp.equals(OntarioAlias.ENR_REG_TYPE_FT)))
                || ((m_rptType.equals(CONST_RPT_TYPE_PT)) && (enrRegTyp.equals(OntarioAlias.ENR_REG_TYPE_PT))))
                && (m_gradeLvls.contains(enrStdGradeLvl))) {
            Map<Integer, Boolean> stdPbIndByMthForStdOid = m_stdPbIndByMthByStdOid.get(stdOid);
            Map<Integer, Boolean> stdOpIndByMthForStdOid = m_stdOpIndByMthByStdOid.get(stdOid);
            if (stdPbIndByMthForStdOid == null) {
                stdPbIndByMthForStdOid = new HashMap<Integer, Boolean>();
                stdOpIndByMthForStdOid = new HashMap<Integer, Boolean>();
            }
            if ((enrInSklMonth <= OntarioAlias.CONST_SKL_MONTH_OCT)
                    && ((enrOutSklMonth == null) || (enrOutSklMonth > OntarioAlias.CONST_SKL_MONTH_OCT))) {
                {
                    stdPbIndByMthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_OCT, stdPbIndSklMonth);
                    stdOpIndByMthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_OCT, stdOpIndSklMonth);
                }
            }
            if ((enrInSklMonth <= OntarioAlias.CONST_SKL_MONTH_MAR)
                    && ((enrOutSklMonth == null) || (enrOutSklMonth > OntarioAlias.CONST_SKL_MONTH_MAR))) {
                {
                    stdPbIndByMthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_MAR, stdPbIndSklMonth);
                    stdOpIndByMthForStdOid.put(OntarioAlias.CONST_SKL_MONTH_MAR, stdOpIndSklMonth);
                }
            }
            m_stdPbIndByMthByStdOid.put(stdOid, stdPbIndByMthForStdOid);
            m_stdOpIndByMthByStdOid.put(stdOid, stdOpIndByMthForStdOid);
        }

        // initialize variables.
        PlainDate enrAdmisIntDt = null;
        PlainDate enrAdmisExtDt = null;
        PlainDate enrTransIntDt = null;
        PlainDate enrTransExtDt = null;
        PlainDate enrRetireDt = null;

        // set output dates based on system code
        if (!StringUtils.isEmpty(enrOutSysCode)) {
            if (enrOutSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_RETIRE)) {
                enrRetireDt = enrOutDate;
                accumEnrTotalByMonth(stdOid, m_sumRetireCtPbBySklMonthMap, m_sumRetireCtOpBySklMonthMap,
                        m_sumRetireCtPbAdultBySklMonthMap, m_sumRetireCtOpAdultBySklMonthMap, enrOutSklMonth,
                        enrStdAdultInd, enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            } else if (enrOutSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_TRANS_EXT)) {
                enrTransExtDt = enrOutDate;
                accumEnrTotalByMonth(stdOid, m_sumTransExtCtPbBySklMonthMap, m_sumTransExtCtOpBySklMonthMap,
                        m_sumTransExtCtPbAdultBySklMonthMap, m_sumTransExtCtOpAdultBySklMonthMap, enrOutSklMonth,
                        enrStdAdultInd, enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            } else if (enrOutSysCode.equals(OntarioAlias.ENR_CODE_RCD_SYS_TRANS_INT)) {
                enrTransIntDt = enrOutDate;
                accumEnrTotalByMonth(stdOid, m_sumTransIntCtPbBySklMonthMap, m_sumTransIntCtOpBySklMonthMap,
                        m_sumTransIntCtPbBySklMonthMap, m_sumTransIntCtOpBySklMonthMap, enrOutSklMonth, enrStdAdultInd,
                        enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            }
        } else if (!StringUtils.isEmpty(enrOutChgType)) {
            if (CONST_ENR_CHG_TYPE_VALUES_CREATE_WITHDRAW.contains(enrOutChgType)) {
                enrTransIntDt = enrOutDate;
                accumEnrTotalByMonth(stdOid, m_sumTransIntCtPbBySklMonthMap, m_sumTransIntCtOpBySklMonthMap,
                        m_sumTransIntCtPbAdultBySklMonthMap, m_sumTransIntCtOpAdultBySklMonthMap, enrOutSklMonth,
                        enrStdAdultInd, enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            }
        }

        // set input dates based on change type and system code
        if (!StringUtils.isEmpty(enrInChgType)) {
            if (enrInChgType.equals(CONST_ENR_CHG_TYPE_VALUE_ENTRY_EXT)) {
                enrAdmisExtDt = enrInDate;
                accumEnrTotalByMonth(stdOid, m_sumAdmisExtCtPbBySklMonthMap, m_sumAdmisExtCtOpBySklMonthMap,
                        m_sumAdmisExtCtPbAdultBySklMonthMap, m_sumAdmisExtCtOpAdultBySklMonthMap, enrInSklMonth,
                        enrStdAdultInd, enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            } else if (enrInChgType.equals(CONST_ENR_CHG_TYPE_VALUE_ENTRY_INT)) {
                enrAdmisIntDt = enrInDate;
                accumEnrTotalByMonth(stdOid, m_sumAdmisIntCtPbBySklMonthMap, m_sumAdmisIntCtOpBySklMonthMap,
                        m_sumAdmisIntCtPbAdultBySklMonthMap, m_sumAdmisIntCtOpAdultBySklMonthMap, enrInSklMonth,
                        enrStdAdultInd, enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            }
            if (CONST_ENR_CHG_TYPE_VALUES_CREATE_WITHDRAW.contains(enrInChgType)) {
                enrAdmisIntDt = enrInDate;
                accumEnrTotalByMonth(stdOid, m_sumAdmisIntCtPbBySklMonthMap, m_sumAdmisIntCtOpBySklMonthMap,
                        m_sumAdmisIntCtPbAdultBySklMonthMap, m_sumAdmisIntCtOpAdultBySklMonthMap, enrInSklMonth,
                        enrStdAdultInd, enrRegTyp, enrStdGradeLvl, stdPbIndSklMonth, stdOpIndSklMonth);
            }
        }

        // add dates to enrolment info
        enrInfo.put(CONST_ENR_STD_CHG_DT_ADMIS_INT, enrAdmisIntDt);
        enrInfo.put(CONST_ENR_STD_CHG_DT_ADMIS_EXT, enrAdmisExtDt);
        enrInfo.put(CONST_ENR_STD_CHG_DT_TRANS_INT, enrTransIntDt);
        enrInfo.put(CONST_ENR_STD_CHG_DT_TRANS_EXT, enrTransExtDt);
        enrInfo.put(CONST_ENR_STD_CHG_DT_RETIRE, enrRetireDt);
    }

    /**
     * Accumulate enrolment totals by month
     *
     * @param stdOid
     * @param updatePbMap - Map (for Pupil Board) to be updated
     * @param updateOpMap - Map (for Other Pupil) to be updated
     * @param updatePbAdultMap - Map (for adult Pupil Board) to be updated
     * @param updateOpAdultMap - Map (for adult Other Pupil) to be updated
     * @param sklMonth - skl month
     * @param stdAdultInd - student adult ind
     * @param enrRegType - enrolment reg type
     * @param enrStdGradeLvl - enrolment grade level
     * @param stdPbIndSklMonth - Pupil Board on entry record
     * @param stdOpIndSklMonth - Other Pupil on entry record
     */
    protected void accumEnrTotalByMonth(String stdOid,
                                        Map<Integer, Integer> updatePbMap,
                                        Map<Integer, Integer> updateOpMap,
                                        Map<Integer, Integer> updatePbAdultMap,
                                        Map<Integer, Integer> updateOpAdultMap,
                                        Integer sklMonth,
                                        Boolean stdAdultInd,
                                        String enrRegTyp,
                                        String enrStdGradeLvl,
                                        Boolean stdPbIndSklMonth,
                                        Boolean stdOpIndSklMonth) {
        // check registration type
        if ((((m_rptType.equals(CONST_RPT_TYPE_FT)) && (enrRegTyp.equals(OntarioAlias.ENR_REG_TYPE_FT)))
                || ((m_rptType.equals(CONST_RPT_TYPE_PT)) && (enrRegTyp.equals(OntarioAlias.ENR_REG_TYPE_PT))))
                && (m_gradeLvls.contains(enrStdGradeLvl))) {

            // update counts
            int updateCtPb = CONST_ZERO.intValue();
            int updateCtOp = CONST_ZERO.intValue();
            // if adult counts are not separate or student is not adult update regular
            // counts
            if ((stdAdultInd == null) || (!stdAdultInd.booleanValue())) {
                if ((stdPbIndSklMonth != null) && (stdPbIndSklMonth)) {
                    if (updatePbMap.get(sklMonth) != null) {
                        updateCtPb = updatePbMap.get(sklMonth).intValue();
                    }
                    updateCtPb++;
                    updatePbMap.put(sklMonth, Integer.valueOf(updateCtPb));
                }
                if ((stdOpIndSklMonth != null) && (stdOpIndSklMonth)) {
                    if (updateOpMap.get(sklMonth) != null) {
                        updateCtOp = updateOpMap.get(sklMonth).intValue();
                    }
                    updateCtOp++;
                    updateOpMap.put(sklMonth, Integer.valueOf(updateCtOp));
                }
            } else
            // else update adult counts
            {
                if ((stdPbIndSklMonth != null) && (stdPbIndSklMonth)) {
                    if (updatePbAdultMap.get(sklMonth) != null) {
                        updateCtPb = updatePbAdultMap.get(sklMonth).intValue();
                    }
                    updateCtPb++;
                    updatePbAdultMap.put(sklMonth, Integer.valueOf(updateCtPb));
                }
                if ((stdOpIndSklMonth != null) && (stdOpIndSklMonth)) {
                    if (updateOpAdultMap.get(sklMonth) != null) {
                        updateCtOp = updateOpAdultMap.get(sklMonth).intValue();
                    }
                    updateCtOp++;
                    updateOpAdultMap.put(sklMonth, Integer.valueOf(updateCtOp));
                }
            }
        }
    }

    /**
     * Write detail grid
     *
     * @param std
     * @param enrInfo
     * @param enrInfoPrev
     */
    protected void writeGridDetail(SisStudent std, Map<String, Object> enrInfo, Map<String, Object> enrInfoPrev) {
        Boolean enrStdAdultInd = (Boolean) enrInfo.get(CONST_ENR_STD_ADULT_IND);
        String enrRegTyp = (String) enrInfo.get(CONST_ENR_REG_TYPE);
        Boolean enrStdOpInd = (Boolean) enrInfo.get(CONST_ENR_STD_OP_IND);
        String enrStdPayer = (String) enrInfo.get(CONST_ENR_STD_PAYER);
        String enrStdGradeLvl = (String) enrInfo.get(CONST_ENR_STD_GRADE_LVL);
        String enrStdGradeLvlPrev = enrStdGradeLvl;
        if (enrInfoPrev != null) {
            enrStdGradeLvlPrev = (String) enrInfoPrev.get(CONST_ENR_STD_GRADE_LVL);
        }
        PlainDate enrAdminIntDt = (PlainDate) enrInfo.get(CONST_ENR_STD_CHG_DT_ADMIS_INT);
        PlainDate enrAdminExtDt = (PlainDate) enrInfo.get(CONST_ENR_STD_CHG_DT_ADMIS_EXT);
        PlainDate enrTransIntDt = (PlainDate) enrInfo.get(CONST_ENR_STD_CHG_DT_TRANS_INT);
        PlainDate enrTransExtDt = (PlainDate) enrInfo.get(CONST_ENR_STD_CHG_DT_TRANS_EXT);
        PlainDate enrRetireDt = (PlainDate) enrInfo.get(CONST_ENR_STD_CHG_DT_RETIRE);

        // write detail record if report type matches registration type and grade level
        // is in selected values
        if ((((m_rptType.equals(CONST_RPT_TYPE_FT)) && (enrRegTyp.equals(OntarioAlias.ENR_REG_TYPE_FT)))
                || ((m_rptType.equals(CONST_RPT_TYPE_PT)) && (enrRegTyp.equals(OntarioAlias.ENR_REG_TYPE_PT))))
                && (m_gradeLvls.contains(enrStdGradeLvl))) {
            m_gridDetail.append();
            m_gridDetail.set(FIELD_DTL_STUDENT, std);
            m_gridDetail.set(FIELD_DTL_OTHER_PUPIL_IND, enrStdOpInd);
            m_gridDetail.set(FIELD_DTL_PAYER, enrStdPayer);
            m_gridDetail.set(FIELD_DTL_ADULT_IND, enrStdAdultInd);
            m_gridDetail.set(FIELD_DTL_GRADE_LVL, enrStdGradeLvl);
            m_gridDetail.set(FIELD_DTL_GRADE_LVL_PREV, enrStdGradeLvlPrev);
            m_gridDetail.set(FIELD_DTL_ADMIS_INT_DT, enrAdminIntDt);
            m_gridDetail.set(FIELD_DTL_ADMIS_EXT_DT, enrAdminExtDt);
            m_gridDetail.set(FIELD_DTL_TRANS_INT_DT, enrTransIntDt);
            m_gridDetail.set(FIELD_DTL_TRANS_EXT_DT, enrTransExtDt);
            m_gridDetail.set(FIELD_DTL_RETIRE_DT, enrRetireDt);
        }
    }

    /**
     * Write summary grid by Student
     *
     * @param std
     * @param enrInfoListForStdOid
     */
    protected void writeGridSummaryStd(SisStudent std, List<Map<String, Object>> enrInfoListForStdOid) {
        String stdOid = std.getOid();

        // get indicators for student
        Boolean stdAdultInd = m_stdAdultIndByStdOid.get(stdOid);
        if (stdAdultInd == null) {
            // get student adult indicator if not calculated already for student
            if (m_sklLvl.equals(CONST_SKL_LVL_ELEM)) {
                stdAdultInd = Boolean.FALSE;
            } else {
                stdAdultInd = isStdAdult(std);
            }
            m_stdAdultIndByStdOid.put(stdOid, stdAdultInd);
        }

        // get student indicators
        Boolean stdPbIndOct = null;
        Boolean stdOpIndOct = null;
        Boolean stdPbIndMar = null;
        Boolean stdOpIndMar = null;
        Map<Integer, Boolean> stdPbIndByMthForStdOid = m_stdPbIndByMthByStdOid.get(stdOid);
        if (stdPbIndByMthForStdOid != null) {
            stdPbIndOct = stdPbIndByMthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_OCT);
            stdPbIndMar = stdPbIndByMthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_MAR);
        }
        Map<Integer, Boolean> stdOpIndByMthForStdOid = m_stdOpIndByMthByStdOid.get(stdOid);
        if (stdOpIndByMthForStdOid != null) {
            stdOpIndOct = stdOpIndByMthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_OCT);
            stdOpIndMar = stdOpIndByMthForStdOid.get(OntarioAlias.CONST_SKL_MONTH_MAR);
        }

        // initialize student fte variables
        Integer stdMinsOct = null;
        Integer stdMinsMar = null;
        Integer stdMinsHcOct = null;
        Integer stdMinsHcMar = null;
        Double stdFteOct = null;
        Double stdFteMar = null;
        Double stdFteHcOct = null;
        Double stdFteHcMar = null;

        // get minutes and fte for student
        if (!m_writeGridSummaryFteStd) {
            // Daily/Monthly/Summary
            if (m_minsAndFteInfoByStdOid.get(stdOid) != null) {
                Map<String, Object> minsAndFteInfoForStd = m_minsAndFteInfoByStdOid.get(stdOid);
                stdMinsOct = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_OCT);
                stdMinsMar = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_MAR);
                stdMinsHcOct = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_HC_OCT);
                stdMinsHcMar = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_HC_MAR);
                stdFteOct = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_OCT);
                stdFteMar = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_MAR);
                stdFteHcOct = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_HC_OCT);
                stdFteHcMar = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_HC_MAR);
            }
        } else {
            // Student Fte
            if (m_minsAndFteAvgInfoByStdOid.get(stdOid) != null) {
                Map<String, Object> minsAndFteInfoForStd = m_minsAndFteAvgInfoByStdOid.get(stdOid);
                stdMinsOct = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_OCT);
                stdMinsMar = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_MAR);
                stdMinsHcOct = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_HC_OCT);
                stdMinsHcMar = (Integer) minsAndFteInfoForStd.get(CONST_STD_MINS_HC_MAR);
                stdFteOct = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_OCT);
                stdFteMar = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_MAR);
                stdFteHcOct = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_HC_OCT);
                stdFteHcMar = (Double) minsAndFteInfoForStd.get(CONST_STD_FTE_HC_MAR);
            }
        }

        // check if oct/mar reg and hc mins are null - will bypass if so
        boolean writeOutputOct = false;
        if ((m_endDateSklMonth.intValue() >= OntarioAlias.CONST_SKL_MONTH_OCT)
                && (((stdPbIndOct != null) && (stdPbIndOct)) || ((stdOpIndOct != null) && (stdOpIndOct)))
                && (!((stdMinsOct == null) && (stdMinsHcOct == null)))) {

            if ((!m_writeGridSummaryFteStd)
                    || ((m_writeGridSummaryFteStd) && (((stdMinsOct != null) && (stdMinsOct.intValue() > 0))
                            || ((stdMinsHcOct != null) && (stdMinsHcOct.intValue() > 0))))) {
                writeOutputOct = true;
            }
        }
        boolean writeOutputMar = false;
        if ((m_endDateSklMonth.intValue() >= OntarioAlias.CONST_SKL_MONTH_MAR)
                && (((stdPbIndMar != null) && (stdPbIndMar)) || ((stdOpIndMar != null) && (stdOpIndMar)))
                && (!((stdMinsMar == null) && (stdMinsHcMar == null)))) {

            if ((!m_writeGridSummaryFteStd)
                    || ((m_writeGridSummaryFteStd) && (((stdMinsMar != null) && (stdMinsMar.intValue() > 0))
                            || ((stdMinsHcMar != null) && (stdMinsHcMar.intValue() > 0))))) {
                writeOutputMar = true;
            }
        }

        // update student fte, also student counts for student fte report (as there are different
        // students written for this report)
        if (!stdAdultInd) {
            if ((writeOutputOct) && (stdPbIndOct != null) && (stdPbIndOct)) {
                m_pBFteSumOct += ((stdFteOct != null) ? stdFteOct.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_pBFteSumHcOct += ((stdFteHcOct != null) ? stdFteHcOct.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_pBCtOct++;
                }
            } else if ((writeOutputOct) && (stdOpIndOct != null) && (stdOpIndOct)) {
                m_oPFteSumOct += ((stdFteOct != null) ? stdFteOct.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_oPFteSumHcOct += ((stdFteHcOct != null) ? stdFteHcOct.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_oPCtOct++;
                }
            }
            if ((writeOutputMar) && (stdPbIndMar != null) && (stdPbIndMar)) {
                m_pBFteSumMar += ((stdFteMar != null) ? stdFteMar.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_pBFteSumHcMar += ((stdFteHcMar != null) ? stdFteHcMar.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_pBCtMar++;
                }
            } else if ((writeOutputMar) && (stdOpIndMar != null) && (stdOpIndMar)) {
                m_oPFteSumMar += ((stdFteMar != null) ? stdFteMar.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_oPFteSumHcMar += ((stdFteHcMar != null) ? stdFteHcMar.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_oPCtMar++;
                }
            }
        } else {
            if ((writeOutputOct) && (stdPbIndOct != null) && (stdPbIndOct)) {
                m_pBFteSumAdultOct += ((stdFteOct != null) ? stdFteOct.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_pBFteSumHcAdultOct += ((stdFteHcOct != null) ? stdFteHcOct.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_pBCtAdultOct++;
                }
            } else if ((writeOutputOct) && (stdOpIndOct != null) && (stdOpIndOct)) {
                m_oPFteSumAdultOct += ((stdFteOct != null) ? stdFteOct.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_oPFteSumHcAdultOct += ((stdFteHcOct != null) ? stdFteHcOct.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_oPCtAdultOct++;
                }
            }
            if ((writeOutputMar) && (stdPbIndMar != null) && (stdPbIndMar)) {
                m_pBFteSumAdultMar += ((stdFteMar != null) ? stdFteMar.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_pBFteSumHcAdultMar += ((stdFteHcMar != null) ? stdFteHcMar.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_pBCtAdultMar++;
                }
            } else if ((writeOutputMar) && (stdOpIndMar != null) && (stdOpIndMar)) {
                m_oPFteSumAdultMar += ((stdFteMar != null) ? stdFteMar.doubleValue() : CONST_ZERO_DBL.doubleValue());
                if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                    m_oPFteSumHcAdultMar += ((stdFteHcMar != null) ? stdFteHcMar.doubleValue()
                            : CONST_ZERO_DBL.doubleValue());
                }

                // student counts for student fte
                if (m_writeGridSummaryFteStd) {
                    m_oPCtAdultMar++;
                }
            }
        }

        // create grid to write new record
        ReportDataGrid gridStdCurrRow = new ReportDataGrid();

        // set output values
        gridStdCurrRow.append();
        gridStdCurrRow.set(FIELD_SUM_STUDENT, std);
        gridStdCurrRow.set(FIELD_SUM_MINS_OCT, stdMinsOct);
        gridStdCurrRow.set(FIELD_SUM_FTE_OCT, stdFteOct);
        gridStdCurrRow.set(FIELD_SUM_MINS_MAR, stdMinsMar);
        gridStdCurrRow.set(FIELD_SUM_FTE_MAR, stdFteMar);
        // write additional high credit
        if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
            gridStdCurrRow.set(FIELD_SUM_MINS_HC_OCT, stdMinsHcOct);
            gridStdCurrRow.set(FIELD_SUM_FTE_HC_OCT, stdFteHcOct);
            gridStdCurrRow.set(FIELD_SUM_MINS_HC_MAR, stdMinsHcMar);
            gridStdCurrRow.set(FIELD_SUM_FTE_HC_MAR, stdFteHcMar);
        }

        // set adult indicator and write output grids
        if (!stdAdultInd) {
            gridStdCurrRow.set(FIELD_SUM_ROW_ADULT_IND2, Boolean.FALSE);

            if (m_writeGridSummaryStd) {
                m_gridSummaryStd.append(gridStdCurrRow);
            }
            if (m_writeGridSummaryStdHc) {
                if (writeOutputOct) {
                    Map<String, Object> gridStdCurrRowClone1 = new HashMap<String, Object>();
                    gridStdCurrRowClone1.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryStdHcOct.append(gridStdCurrRowClone1);
                    m_gridSummaryStdHcOct.set(FIELD_SUM_OTHER_PUPIL_IND, stdOpIndOct);
                    m_gridSummaryStdHcOct.set(FIELD_SUM_ROW_OCT_IND, Boolean.TRUE);
                    m_gridSummaryStdHcOct.set(FIELD_SUM_ROW_MAR_IND, Boolean.FALSE);
                }
                if (writeOutputMar) {
                    Map<String, Object> gridStdCurrRowClone2 = new HashMap<String, Object>();
                    gridStdCurrRowClone2.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryStdHcMar.append(gridStdCurrRowClone2);
                    m_gridSummaryStdHcMar.set(FIELD_SUM_OTHER_PUPIL_IND, stdOpIndMar);
                    m_gridSummaryStdHcMar.set(FIELD_SUM_ROW_OCT_IND, Boolean.FALSE);
                    m_gridSummaryStdHcMar.set(FIELD_SUM_ROW_MAR_IND, Boolean.TRUE);
                }
            }
            if (m_writeGridSummaryFteStd) {
                if (writeOutputOct) {
                    Map<String, Object> gridStdCurrRowClone1 = new HashMap<String, Object>();
                    gridStdCurrRowClone1.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryFteStdOct.append(gridStdCurrRowClone1);
                    m_gridSummaryFteStdOct.set(FIELD_SUM_OTHER_PUPIL_IND, stdOpIndOct);
                    m_gridSummaryFteStdOct.set(FIELD_SUM_ROW_OCT_IND, Boolean.TRUE);
                    m_gridSummaryFteStdOct.set(FIELD_SUM_ROW_MAR_IND, Boolean.FALSE);
                }
                if (writeOutputMar) {
                    Map<String, Object> gridStdCurrRowClone2 = new HashMap<String, Object>();
                    gridStdCurrRowClone2.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryFteStdMar.append(gridStdCurrRowClone2);
                    m_gridSummaryFteStdMar.set(FIELD_SUM_OTHER_PUPIL_IND, stdOpIndMar);
                    m_gridSummaryFteStdMar.set(FIELD_SUM_ROW_OCT_IND, Boolean.FALSE);
                    m_gridSummaryFteStdMar.set(FIELD_SUM_ROW_MAR_IND, Boolean.TRUE);
                }
            }
        } else {
            gridStdCurrRow.set(FIELD_SUM_ROW_ADULT_IND2, Boolean.TRUE);

            if (m_writeGridSummaryStdAdult) {
                m_gridSummaryStdAdult.append(gridStdCurrRow);
            }
            if (m_writeGridSummaryStdHcAdult) {
                if (writeOutputOct) {
                    Map<String, Object> gridStdCurrRowClone1 = new HashMap<String, Object>();
                    gridStdCurrRowClone1.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryStdHcAdultOct.append(gridStdCurrRowClone1);
                    m_gridSummaryStdHcAdultOct.set(FIELD_SUM_ROW_OCT_IND, Boolean.TRUE);
                    m_gridSummaryStdHcAdultOct.set(FIELD_SUM_ROW_MAR_IND, Boolean.FALSE);
                }
                if (writeOutputMar) {
                    Map<String, Object> gridStdCurrRowClone2 = new HashMap<String, Object>();
                    gridStdCurrRowClone2.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryStdHcAdultMar.append(gridStdCurrRowClone2);
                    m_gridSummaryStdHcAdultMar.set(FIELD_SUM_ROW_OCT_IND, Boolean.FALSE);
                    m_gridSummaryStdHcAdultMar.set(FIELD_SUM_ROW_MAR_IND, Boolean.TRUE);
                }
            }
            if (m_writeGridSummaryFteStdAdult) {
                if (writeOutputOct) {
                    Map<String, Object> gridStdCurrRowClone1 = new HashMap<String, Object>();
                    gridStdCurrRowClone1.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryFteStdAdultOct.append(gridStdCurrRowClone1);
                    m_gridSummaryFteStdAdultOct.set(FIELD_SUM_ROW_OCT_IND, Boolean.TRUE);
                    m_gridSummaryFteStdAdultOct.set(FIELD_SUM_ROW_MAR_IND, Boolean.FALSE);
                }
                if (writeOutputMar) {
                    Map<String, Object> gridStdCurrRowClone2 = new HashMap<String, Object>();
                    gridStdCurrRowClone2.putAll(gridStdCurrRow.getCurrentRow());
                    m_gridSummaryFteStdAdultMar.append(gridStdCurrRowClone2);
                    m_gridSummaryFteStdAdultMar.set(FIELD_SUM_ROW_OCT_IND, Boolean.FALSE);
                    m_gridSummaryFteStdAdultMar.set(FIELD_SUM_ROW_MAR_IND, Boolean.TRUE);
                }
            }
        }

        if (m_addLogs) {
            Double sumFteOctLogs = (stdFteOct == null) ? CONST_ZERO_DBL : stdFteOct;
            Double sumFteHcOctLogs = (stdFteHcOct == null) ? CONST_ZERO_DBL : stdFteHcOct;
            Double sumFteMarLogs = (stdFteMar == null) ? CONST_ZERO_DBL : stdFteMar;
            Double sumFteHcMarLogs = (stdFteHcMar == null) ? CONST_ZERO_DBL : stdFteHcMar;
            if (!m_log2StdOidPrev.equals(stdOid)) {
                m_log2StdOidPrev = stdOid;
                m_log2StdCt++;
            }
            m_logMessageMapToLogType.get(CONST_LOG_2).append(CONST_LOG_2 + CONST_COMMA + m_log2StdCt + CONST_COMMA
                    + CONST_QUOTE + std.getNameView() + CONST_QUOTE + CONST_COMMA + CONST_COMMA
                    + (stdOpIndMar != null ? (stdOpIndMar ? "Other Pupil" : "Pupil Board") : "null") + CONST_COMMA
                    + (stdAdultInd != null ? (stdAdultInd ? "Adult" : "Under 21") : "null") + CONST_COMMA
                    + (sumFteOctLogs != null ? sumFteOctLogs.toString() : "null") + CONST_COMMA
                    + (sumFteHcOctLogs != null ? sumFteHcOctLogs.toString() : "null") + CONST_COMMA
                    + (sumFteMarLogs != null ? sumFteMarLogs.toString() : "null") + CONST_COMMA
                    + (sumFteHcMarLogs != null ? sumFteHcMarLogs.toString() : "null") + CONST_COMMA + m_pBFteSumOct
                    + CONST_COMMA + m_oPFteSumOct + CONST_COMMA + m_pBFteSumMar + CONST_COMMA + m_oPFteSumMar
                    + CONST_COMMA + m_pBFteSumAdultOct + CONST_COMMA + m_oPFteSumAdultOct + CONST_COMMA
                    + m_pBFteSumAdultMar + CONST_COMMA + m_oPFteSumAdultMar + CONST_COMMA + m_pBFteSumHcOct
                    + CONST_COMMA + m_oPFteSumHcOct + CONST_COMMA + m_pBFteSumHcMar + CONST_COMMA + m_oPFteSumHcMar
                    + CONST_COMMA + m_pBFteSumHcAdultOct + CONST_COMMA + m_oPFteSumHcAdultOct + CONST_COMMA
                    + m_pBFteSumHcAdultMar + CONST_COMMA + m_oPFteSumHcAdultMar + CONST_NEWLINE);
        }
    }

    /**
     * Process and write (if selected) summary grid by month
     *
     * @param gridSummary
     * @param sklMonth
     * @param prevCtPb
     * @param prevCtOp
     * @param adultMapInd
     *
     * @return Map<String, Integer> - netCt Map by Pb/Op Type
     */
    protected Map<String, Integer> processGridSummaryMonth(ReportDataGrid gridSummary,
                                                           Integer sklMonth,
                                                           int prevCtPb,
                                                           int prevCtOp,
                                                           boolean adultMapInd) {
        // get enrolment counts for school month
        Integer admisIntCtPb = getEnrTotalByMonth(m_sumAdmisIntCtPbBySklMonthMap, m_sumAdmisIntCtPbAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer admisIntCtOp = getEnrTotalByMonth(m_sumAdmisIntCtOpBySklMonthMap, m_sumAdmisIntCtOpAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer admisExtCtPb = getEnrTotalByMonth(m_sumAdmisExtCtPbBySklMonthMap, m_sumAdmisExtCtPbAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer admisExtCtOp = getEnrTotalByMonth(m_sumAdmisExtCtOpBySklMonthMap, m_sumAdmisExtCtOpAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer transIntCtPb = getEnrTotalByMonth(m_sumTransIntCtPbBySklMonthMap, m_sumTransIntCtPbAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer transIntCtOp = getEnrTotalByMonth(m_sumTransIntCtOpBySklMonthMap, m_sumTransIntCtOpAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer transExtCtPb = getEnrTotalByMonth(m_sumTransExtCtPbBySklMonthMap, m_sumTransExtCtPbAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer transExtCtOp = getEnrTotalByMonth(m_sumTransExtCtOpBySklMonthMap, m_sumTransExtCtOpAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer retireCtPb = getEnrTotalByMonth(m_sumRetireCtPbBySklMonthMap, m_sumRetireCtPbAdultBySklMonthMap,
                sklMonth, adultMapInd);
        Integer retireCtOp = getEnrTotalByMonth(m_sumRetireCtOpBySklMonthMap, m_sumRetireCtOpAdultBySklMonthMap,
                sklMonth, adultMapInd);

        // calculate net count
        int netCtPb = prevCtPb + admisIntCtPb.intValue() + admisExtCtPb.intValue() - transIntCtPb.intValue()
                - transExtCtPb.intValue() - retireCtPb.intValue();
        int netCtOp = prevCtOp + admisIntCtOp.intValue() + admisExtCtOp.intValue() - transIntCtOp.intValue()
                - transExtCtOp.intValue() - retireCtOp.intValue();

        // add to grid if selected to write
        if (m_writeGridSummaryMonth) {
            gridSummary.append();
            // if school year goes beyond June then subtract 12 to restart from July (declared as 1
            // in OntarioAlias)
            gridSummary.set(FIELD_SUM_MONTH_NUM, sklMonth > 12 ? sklMonth - 12 : sklMonth);
            if (!adultMapInd) {
                gridSummary.set(FIELD_SUM_ROW_ADULT_IND, Boolean.FALSE);
            } else {
                gridSummary.set(FIELD_SUM_ROW_ADULT_IND, Boolean.TRUE);
            }
            gridSummary.set(FIELD_SUM_PREVIOUS_CT, Integer.valueOf(prevCtPb + prevCtOp));
            gridSummary.set(FIELD_SUM_ADMIS_INT_CT, Integer.valueOf(admisIntCtPb.intValue() + admisIntCtOp.intValue()));
            gridSummary.set(FIELD_SUM_ADMIS_EXT_CT, Integer.valueOf(admisExtCtPb.intValue() + admisExtCtOp.intValue()));
            gridSummary.set(FIELD_SUM_TRANS_INT_CT, Integer.valueOf(transIntCtPb.intValue() + transIntCtOp.intValue()));
            gridSummary.set(FIELD_SUM_TRANS_EXT_CT, Integer.valueOf(transExtCtPb.intValue() + transExtCtOp.intValue()));
            gridSummary.set(FIELD_SUM_RETIRE_CT, Integer.valueOf(retireCtPb.intValue() + retireCtOp.intValue()));
            gridSummary.set(FIELD_SUM_NET_CT, Integer.valueOf(netCtPb + netCtOp));
        }

        // return net count
        Map<String, Integer> netCtByTypeMap = new HashMap<String, Integer>();
        netCtByTypeMap.put(CONST_STD_TYP_PB, Integer.valueOf(netCtPb));
        netCtByTypeMap.put(CONST_STD_TYP_OP, Integer.valueOf(netCtOp));

        return netCtByTypeMap;
    }

    /**
     * Get enrolment total by passed month
     *
     * @param updateMap - Map to be updated
     * @param updateAdultMap - Map (for adult students) to be updated
     * @param sklMonth - skl month
     * @param adultMapInd - adult map ind
     *
     * @return Integer
     */
    protected Integer getEnrTotalByMonth(Map<Integer, Integer> updateMap,
                                         Map<Integer, Integer> updateAdultMap,
                                         Integer sklMonth,
                                         boolean adultMapInd) {
        Integer count = CONST_ZERO;

        if ((!adultMapInd) && (updateMap.get(sklMonth) != null)) {
            count = updateMap.get(sklMonth);
        } else if ((adultMapInd) && (updateAdultMap.get(sklMonth) != null)) {
            count = updateAdultMap.get(sklMonth);
        }

        return count;
    }

    /**
     * Add report parameters
     */
    protected void addRptParameters() {
        // add parameters - student counts which are needed for most reports
        addParameter(REPORT_PB_CT_OCT, Integer.valueOf(m_pBCtOct));
        addParameter(REPORT_PB_CT_MAR, Integer.valueOf(m_pBCtMar));
        addParameter(REPORT_OP_CT_OCT, Integer.valueOf(m_oPCtOct));
        addParameter(REPORT_OP_CT_MAR, Integer.valueOf(m_oPCtMar));
        addParameter(REPORT_PB_CT_ADULT_OCT, Integer.valueOf(m_pBCtAdultOct));
        addParameter(REPORT_PB_CT_ADULT_MAR, Integer.valueOf(m_pBCtAdultMar));
        addParameter(REPORT_OP_CT_ADULT_OCT, Integer.valueOf(m_oPCtAdultOct));
        addParameter(REPORT_OP_CT_ADULT_MAR, Integer.valueOf(m_oPCtAdultMar));

        // add parameters - summary by student
        if ((m_writeGridSummaryStd) || (m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
            addParameter(REPORT_PB_FTE_SUM_OCT, Double.valueOf(m_pBFteSumOct));
            addParameter(REPORT_OP_FTE_SUM_OCT, Double.valueOf(m_oPFteSumOct));
            addParameter(REPORT_PB_FTE_SUM_MAR, Double.valueOf(m_pBFteSumMar));
            addParameter(REPORT_OP_FTE_SUM_MAR, Double.valueOf(m_oPFteSumMar));

            // additional high credit
            if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                addParameter(REPORT_PB_FTE_SUM_HC_OCT, Double.valueOf(m_pBFteSumHcOct));
                addParameter(REPORT_OP_FTE_SUM_HC_OCT, Double.valueOf(m_oPFteSumHcOct));
                addParameter(REPORT_PB_FTE_SUM_HC_MAR, Double.valueOf(m_pBFteSumHcMar));
                addParameter(REPORT_OP_FTE_SUM_HC_MAR, Double.valueOf(m_oPFteSumHcMar));
            }
        }

        // add parameters - summary by student for adult report
        if ((m_writeGridSummaryStd) || (m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
            addParameter(REPORT_PB_FTE_SUM_ADULT_OCT, Double.valueOf(m_pBFteSumAdultOct));
            addParameter(REPORT_OP_FTE_SUM_ADULT_OCT, Double.valueOf(m_oPFteSumAdultOct));
            addParameter(REPORT_PB_FTE_SUM_ADULT_MAR, Double.valueOf(m_pBFteSumAdultMar));
            addParameter(REPORT_OP_FTE_SUM_ADULT_MAR, Double.valueOf(m_oPFteSumAdultMar));

            // additional high credit
            if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryFteStd)) {
                addParameter(REPORT_PB_FTE_SUM_HC_ADULT_OCT, Double.valueOf(m_pBFteSumHcAdultOct));
                addParameter(REPORT_OP_FTE_SUM_HC_ADULT_OCT, Double.valueOf(m_oPFteSumHcAdultOct));
                addParameter(REPORT_PB_FTE_SUM_HC_ADULT_MAR, Double.valueOf(m_pBFteSumHcAdultMar));
                addParameter(REPORT_OP_FTE_SUM_HC_ADULT_MAR, Double.valueOf(m_oPFteSumHcAdultMar));
            }
        }

        // add extra report parameters
        Map<String, String> ontarioAliasForRptMap = OntarioAlias.buildStaticValuesMap(CONST_ONTARIO_ALIAS_FOR_RPT_LIST);
        addParameter(REPORT_ONTARIO_ALIAS_FOR_RPT_MAP, ontarioAliasForRptMap);
    }

    /**
     * Complete sub reports and their grids
     */
    protected void completeSubreportsAndGrids() {
        // set first/last row of each grid, append to return grid
        if (!m_gridDetail.isEmpty()) {
            m_gridDetail.bottom();
            m_gridDetail.set(FIELD_DTL_LAST_ROW, Boolean.TRUE);
            m_gridDetail.beforeTop();
            addSubreportParameter(SUBRPT_ID_DTL, REPORT_SUBRPT_FORMAT_DTL, REPORT_SUBRPT_DATA_DTL, m_gridDetail);
        } else {
            addParameter(REPORT_SUBRPT_DATA_DTL, null);
        }
        if (!m_gridSummaryMonth.isEmpty()) {
            m_gridSummaryMonth.bottom();
            m_gridSummaryMonth.set(FIELD_SUM_MONTH_LAST_ROW, Boolean.TRUE);
            m_gridSummaryMonth.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_MONTH, REPORT_SUBRPT_FORMAT_SUM_MONTH, REPORT_SUBRPT_DATA_SUM_MONTH,
                    m_gridSummaryMonth);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_MONTH, null);
        }
        if (!m_gridSummaryMonthAdult.isEmpty()) {
            m_gridSummaryMonthAdult.bottom();
            m_gridSummaryMonthAdult.set(FIELD_SUM_MONTH_LAST_ROW, Boolean.TRUE);
            m_gridSummaryMonthAdult.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_MONTH, REPORT_SUBRPT_FORMAT_SUM_MONTH,
                    REPORT_SUBRPT_DATA_SUM_MONTH_ADULT, m_gridSummaryMonthAdult);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_MONTH_ADULT, null);
        }
        if (!m_gridSummaryStd.isEmpty()) {
            m_gridSummaryStd.bottom();
            m_gridSummaryStd.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryStd.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD, REPORT_SUBRPT_FORMAT_SUM_STD, REPORT_SUBRPT_DATA_SUM_STD,
                    m_gridSummaryStd);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_STD, null);
        }
        if (!m_gridSummaryStdAdult.isEmpty()) {
            m_gridSummaryStdAdult.bottom();
            m_gridSummaryStdAdult.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryStdAdult.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD, REPORT_SUBRPT_FORMAT_SUM_STD, REPORT_SUBRPT_DATA_SUM_STD_ADULT,
                    m_gridSummaryStdAdult);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_STD_ADULT, null);
        }
        if (!m_gridSummaryStdHcOct.isEmpty()) {
            m_gridSummaryStdHcOct.bottom();
            m_gridSummaryStdHcOct.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryStdHcOct.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_STD_HC_OCT, m_gridSummaryStdHcOct);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_STD_HC_OCT, null);
        }
        if (!m_gridSummaryStdHcAdultOct.isEmpty()) {
            m_gridSummaryStdHcAdultOct.bottom();
            m_gridSummaryStdHcAdultOct.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryStdHcAdultOct.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_STD_HC_ADULT_OCT, m_gridSummaryStdHcAdultOct);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_STD_HC_ADULT_OCT, null);
        }
        if (!m_gridSummaryStdHcMar.isEmpty()) {
            m_gridSummaryStdHcMar.bottom();
            m_gridSummaryStdHcMar.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryStdHcMar.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_STD_HC_MAR, m_gridSummaryStdHcMar);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_STD_HC_MAR, null);
        }
        if (!m_gridSummaryStdHcAdultMar.isEmpty()) {
            m_gridSummaryStdHcAdultMar.bottom();
            m_gridSummaryStdHcAdultMar.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryStdHcAdultMar.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_STD_HC_ADULT_MAR, m_gridSummaryStdHcAdultMar);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_STD_HC_ADULT_MAR, null);
        }
        if (!m_gridSummaryFteStdOct.isEmpty()) {
            m_gridSummaryFteStdOct.bottom();
            m_gridSummaryFteStdOct.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryFteStdOct.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_FTE_STD_OCT, m_gridSummaryFteStdOct);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_FTE_STD_OCT, null);
        }
        if (!m_gridSummaryFteStdAdultOct.isEmpty()) {
            m_gridSummaryFteStdAdultOct.bottom();
            m_gridSummaryFteStdAdultOct.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryFteStdAdultOct.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_FTE_STD_ADULT_OCT, m_gridSummaryFteStdAdultOct);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_FTE_STD_ADULT_OCT, null);
        }
        if (!m_gridSummaryFteStdMar.isEmpty()) {
            m_gridSummaryFteStdMar.bottom();
            m_gridSummaryFteStdMar.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryFteStdMar.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_FTE_STD_MAR, m_gridSummaryFteStdMar);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_FTE_STD_MAR, null);
        }
        if (!m_gridSummaryFteStdAdultMar.isEmpty()) {
            m_gridSummaryFteStdAdultMar.bottom();
            m_gridSummaryFteStdAdultMar.set(FIELD_SUM_STD_LAST_ROW, Boolean.TRUE);
            m_gridSummaryFteStdAdultMar.beforeTop();
            addSubreportParameter(SUBRPT_ID_SUM_STD_HC, REPORT_SUBRPT_FORMAT_SUM_STD_HC,
                    REPORT_SUBRPT_DATA_SUM_FTE_STD_ADULT_MAR, m_gridSummaryFteStdAdultMar);
        } else {
            addParameter(REPORT_SUBRPT_DATA_SUM_FTE_STD_ADULT_MAR, null);
        }
    }

    /**
     * Add Subreport parameters - per sub report
     *
     * @param subrptId - Input
     * @param subrptFormat - Output
     * @param subrptData - Output
     * @param subrptGrid - Data
     */
    private void addSubreportParameter(String subrptId,
                                       String subrptFormat,
                                       String subrptData,
                                       ReportDataGrid subrptGrid) {
        Report subRpt = ReportUtils.getReport((String) getParameter(subrptId), getBroker());

        if (subRpt != null) {
            addParameter(subrptFormat, new ByteArrayInputStream(subRpt.getCompiledFormat()));
            addParameter(subrptData, subrptGrid);
        }
    }

    /**
     * Set output logs
     */
    protected void setOutputLogs() {
        if (m_addLogs) {
            // log message output
            for (String logType : CONST_LOG_TYPES) {
                logToolMessage(Level.INFO, m_logMessageMapToLogType.get(logType).toString(), false);
            }
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // initialize variables
        m_school = (SisSchool) getSchool();
        m_addLogs = getBooleanParameter(PARAM_ADD_LOGS);
        m_cal = Calendar.getInstance();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        // set report type variables, will be done in extending class override
        setReportType();

        // set grade levels, will be done in extending class override
        setGradeLevels();

        // set report grids to write indicators
        setReportGridWriteInds();

        // get school year dates (schedule) based on school and selected school year
        X2Criteria schCriteria = new X2Criteria();
        schCriteria.addEqualTo(Schedule.COL_SCHOOL_OID, m_school.getOid());
        schCriteria.addEqualTo(Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        QueryByCriteria schQuery = new QueryByCriteria(Schedule.class, schCriteria);
        Schedule sch = getBroker().getBeanByQuery(schQuery);
        m_contextDateDec31 = PlainDate
                .fromString(Integer.valueOf(sch.getDistrictContext().getSchoolYear() - 1) + "-12-31");
        PlainDate schEndDate = sch.getEndDate();

        // initialize start date from current schedule
        m_startDate = sch.getStartDate();

        // end date is set equal to effective date input parameter
        m_endDate = (PlainDate) getParameter(PARAM_EFFECTIVE_DATE);

        // validate that selected date is in current year, set error date if it is not
        if ((m_endDate.before(m_startDate)) || (m_endDate.after(schEndDate))) {
            m_errEffectiveDate = CONST_ERR_EFF_DATE;
            m_errEffectiveDate = m_errEffectiveDate.replace(CONST_ERR_EFF_DATE_START, m_startDate.toString())
                    .replace(CONST_ERR_EFF_DATE_END, schEndDate.toString());
        }

        // save these start/end date for the fte summary report
        if (m_writeGridSummaryFteStd) {
            m_startDateSummaryFte = m_startDate;
            m_endDateSummaryFte = m_endDate;
        }

        // if selected date is valid (checks it even if it is overlaid)
        // reset start and end dates based on first and last in session dates
        PlainDate casEndDate = null;

        X2Criteria casCriteria = new X2Criteria();
        casCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        casCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        casCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        // create query sorted by date, descending
        QueryByCriteria casQuery = new QueryByCriteria(SisSchoolCalendarDate.class, casCriteria);
        casQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        // The first bean in the query is the last InSession date in the calendar
        SisSchoolCalendarDate csdStartDateBean = (SisSchoolCalendarDate) getBroker().getBeanByQuery(casQuery);
        if (csdStartDateBean != null) {
            m_startDate = csdStartDateBean.getDate();
        }

        casCriteria = new X2Criteria();
        casCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        casCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        casCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        // create query sorted by date, descending
        casQuery = new QueryByCriteria(SisSchoolCalendarDate.class, casCriteria);
        casQuery.addOrderByDescending(SisSchoolCalendarDate.COL_DATE);

        // The first bean in the query is the last InSession date in the calendar
        SisSchoolCalendarDate csdEndDateBean = (SisSchoolCalendarDate) getBroker().getBeanByQuery(casQuery);
        if (csdEndDateBean != null) {
            casEndDate = csdEndDateBean.getDate();

            if (casEndDate.before(m_endDate)) {
                m_endDate = casEndDate;
            }
        }

        // save school month for effective date - start and end date
        // may not be from September to June, adjust to have -ve start in order to keep
        // 4 as October and 9 as March
        m_cal.setTime(m_startDate);
        Integer startDateCalMonth = Integer.valueOf(m_cal.get(Calendar.MONTH) + 1);
        m_startDateSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(startDateCalMonth);
        m_cal.setTime(m_endDate);
        Integer endDateCalMonth = Integer.valueOf(m_cal.get(Calendar.MONTH) + 1);
        Integer endDateCalYear = Integer.valueOf(m_cal.get(Calendar.YEAR));
        m_endDateSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(endDateCalMonth);
        // if endDateSklMonth is for school year (not year school year starts) and
        // if school year ends after June change school month to be greater than 12 (June)
        // condition is between June and October
        if ((endDateCalYear.intValue() == getCurrentContext().getSchoolYear())
                && (m_endDateSklMonth.intValue() < OntarioAlias.CONST_SKL_MONTH_OCT)) {
            int monthsAfterJune = m_endDateSklMonth.intValue();
            m_endDateSklMonth = Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_JUN + monthsAfterJune);
        }

        // save school month for effective date
        addParameter(REPORT_END_DATE_SKL_MONTH, m_endDateSklMonth);
        addParameter(REPORT_CONST_SKL_MONTH_OCT, Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_OCT));
        addParameter(REPORT_CONST_SKL_MONTH_MAR, Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_MAR));

        boolean addSignatureLine = false;
        if ((m_cal.get(Calendar.MONTH) + 1 == 10 && m_cal.get(Calendar.DATE) == 31) ||
                (m_cal.get(Calendar.MONTH) + 1 == 3 && m_cal.get(Calendar.DATE) == 31) ||
                (casEndDate != null && !casEndDate.after(m_endDate))) {
            addSignatureLine = true;
        }
        addParameter(REPORT_ADD_MONTH_SIGNATURE_LINE, Boolean.valueOf(addSignatureLine));

        // getting a date minus the last day of school for selected year
        m_endDateExceptExtTrRetire = m_endDate;
        if (m_endDateExceptExtTrRetire.equals(schEndDate)) {
            m_endDateExceptExtTrRetire = DateUtils.add(m_endDate, -1);
        }

        // initialize other variables
        m_gridDetail = new ReportDataGrid();
        m_gridSummaryMonth = new ReportDataGrid();
        m_gridSummaryMonthAdult = new ReportDataGrid();
        m_gridSummaryStd = new ReportDataGrid();
        m_gridSummaryStdAdult = new ReportDataGrid();
        m_gridSummaryStdHcOct = new ReportDataGrid();
        m_gridSummaryStdHcAdultOct = new ReportDataGrid();
        m_gridSummaryStdHcMar = new ReportDataGrid();
        m_gridSummaryStdHcAdultMar = new ReportDataGrid();
        m_gridSummaryFteStdOct = new ReportDataGrid();
        m_gridSummaryFteStdAdultOct = new ReportDataGrid();
        m_gridSummaryFteStdMar = new ReportDataGrid();
        m_gridSummaryFteStdAdultMar = new ReportDataGrid();
        m_fieldPsnLegalLastName = getBeanPathFromAlias(OntarioAlias.ALIAS_PSN_LEGAL_LAST_NAME, false, m_dictionary);
        m_fieldPsnLegalFirstName = getBeanPathFromAlias(OntarioAlias.ALIAS_PSN_LEGAL_FIRST_NAME, false, m_dictionary);

        // add tool version
        addParameter(REPORT_VERSION, getJob().getTool().getComment());

        // setting locale to English or French
        initializeLocalized();

        // setting localized parameters (after localization)
        if (m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
            SimpleDateFormat shortDateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG);
            addParameter(REPORT_SHORT_DATE_FMT_OUTPUT, shortDateFormatOut);
            SimpleDateFormat timestampFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG_WITH_TIME,
                    m_locForLang.getSystemLocale());
            addParameter(REPORT_TIMESTAMP_FMT_OUTPUT, timestampFormatOut);
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG_MMM_DD,
                    m_locForLang.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT, dateFormatOut);
            DecimalFormat intNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_INT);
            addParameter(REPORT_INT_FMT_OUTPUT, intNumFormatOut);
            DecimalFormat decNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC);
            addParameter(REPORT_DEC_FMT_OUTPUT, decNumFormatOut);
        } else {
            SimpleDateFormat shortDateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR);
            addParameter(REPORT_SHORT_DATE_FMT_OUTPUT, shortDateFormatOut);
            SimpleDateFormat timestampFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR_WITH_TIME,
                    m_locForLang.getSystemLocale());
            addParameter(REPORT_TIMESTAMP_FMT_OUTPUT, timestampFormatOut);
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR_DD_MMM,
                    m_locForLang.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT, dateFormatOut);
            DecimalFormat intNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_INT);
            intNumFormatOut.setGroupingUsed(Boolean.FALSE.booleanValue());
            addParameter(REPORT_INT_FMT_OUTPUT, intNumFormatOut);
            DecimalFormat decNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC);
            decNumFormatOut.setGroupingUsed(Boolean.FALSE.booleanValue());
            DecimalFormatSymbols decNumFormatOutDecimalFormatSymbols = decNumFormatOut.getDecimalFormatSymbols();
            decNumFormatOutDecimalFormatSymbols.setDecimalSeparator(CONST_NUM_FMT_FR_CHAR_DECIMAL);
            decNumFormatOut.setDecimalFormatSymbols(decNumFormatOutDecimalFormatSymbols);
            addParameter(REPORT_DEC_FMT_OUTPUT, decNumFormatOut);
        }

        // loads reference table for enrolment codes map
        m_refCodeEnrCodeMap = loadReferenceCodesMap(OntarioAlias.REF_OID_ENR_CODES);

        // loads and saves localized reference table codes
        if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
            Map<String, String> refCodeLocGender = loadReferenceCodeLocMap(Person.class, Person.COL_GENDER_CODE);
            addParameter(REPORT_REF_LOC_GENDER, refCodeLocGender);
            Map<String, String> refCodeLocGradeLvl =
                    loadReferenceCodeLocMap(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
            addParameter(REPORT_REF_LOC_GRADE_LVL, refCodeLocGradeLvl);
        }
    }

    /**
     * Initializes for localization Language is set for organization or school
     *
     * Adds the localization parameters
     */
    private void initializeLocalized() {
        Map<String, Object> locVarMap = new HashMap<String, Object>();
        m_messageResources = OntarioToolHelper.initializeLocalized(getOrganization(), null, locVarMap,
                getBroker());
        m_locForLang = (OrganizationLocale) locVarMap.get(OrganizationLocale.class.getName());

        addParameter(REPORT_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
        addParameter(REPORT_SCHOOL_LOCALE, m_messageResources);
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();
        logToolMessage(Level.INFO, "Sample variable size BEFORE clearing it, m_dataDictionary: " + m_dictionary + " "
                + com.x2dev.utils.MemoryUtils.formatMemoryUsed(getObjectSize(m_dictionary, false)), false);
        clearLocallyDeclaredVariables(this.getClass(), true);
        logToolMessage(Level.INFO, "Sample variable size AFTER clearing it, m_dataDictionary: " + m_dictionary + " "
                + com.x2dev.utils.MemoryUtils.formatMemoryUsed(getObjectSize(m_dictionary, false)), false);
    }

    /**
     * Method to sift through all your declared member variables and sets them to
     * null for proper garbage collection.
     *
     * @param classToClear Class
     * @param recordOnToolLog boolean
     */
    private void clearLocallyDeclaredVariables(Class classToClear, boolean recordOnToolLog) {
        if (recordOnToolLog) {
            logToolMessage(Level.INFO, "Memory used by tool: " + getTotalMemoryUsedByClass(classToClear), false);
        }
        for (java.lang.reflect.Field field : classToClear.getDeclaredFields()) {
            if (field.getModifiers() == java.lang.reflect.Modifier.PRIVATE && !field.getType().isPrimitive()) {
                field.setAccessible(true);
            }
            if (field.isAccessible()) {
                try {
                    field.set(this, null);
                } catch (IllegalArgumentException e) {
                    if (recordOnToolLog) {
                        logToolMessage(Level.INFO,
                                "Unable to clear " + field.getName() + " due to IllegalArgumentException" + "\n" + e,
                                false);
                    }
                } catch (IllegalAccessException e) {
                    if (recordOnToolLog) {
                        logToolMessage(Level.INFO,
                                "Unable to clear " + field.getName() + " due to IllegalAccessException" + "\n" + e,
                                false);
                    }
                }
            }
        }
        if (recordOnToolLog) {
            logToolMessage(Level.INFO, "Memory used after clearing: " + getTotalMemoryUsedByClass(classToClear), false);
        }
    }

    /**
     * Method that relies on reflection to gather the size of all locally declared
     * variables.
     *
     * @param classToMeasure
     * @return Easy to read memory size as a string
     */
    private String getTotalMemoryUsedByClass(Class classToMeasure) {
        long totalMemoryUsed = 0;
        for (java.lang.reflect.Field field : classToMeasure.getDeclaredFields()) {
            if (field.getModifiers() == java.lang.reflect.Modifier.PRIVATE) {
                field.setAccessible(true);
            }
            try {
                totalMemoryUsed = totalMemoryUsed + getObjectSize(field.get(this), false);
            } catch (IllegalArgumentException e) {
                // TODO You could log exception to learn more if needed.
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                // TODO You could log expection to learn more if needed.
                e.printStackTrace();
            }
        }
        return com.x2dev.utils.MemoryUtils.formatMemoryUsed(totalMemoryUsed);
    }

    /**
     * Returns approximate footprint of an object in memory.
     *
     * @param obj Object
     * @param recordOnToolLog boolean
     * @return long
     */
    private long getObjectSize(Object obj, boolean recordOnToolLog) {
        long memmoryUsed = 0;
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(obj);
            oos.close();
            if (recordOnToolLog) {
                logToolMessage(Level.INFO,
                        obj + " Object Data Size: " + com.x2dev.utils.MemoryUtils.formatMemoryUsed(baos.size()), false);
            }
            memmoryUsed += baos.size();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return memmoryUsed;
    }

    /**
     * Set report grid to write indicators depending on input options
     */
    private void setReportGridWriteInds() {
        // get input options
        boolean displayDtl = getBooleanParameter(PARAM_DISPLAY_DTL);
        boolean displaySumMonth = getBooleanParameter(PARAM_DISPLAY_SUM_MONTH);
        boolean displaySumStdHc = getBooleanParameter(PARAM_DISPLAY_SUM_STD_HC);
        boolean displaySumStdFte = getBooleanParameter(PARAM_DISPLAY_SUM_STD_FTE);

        // initialize indicators
        m_writeGridDetail = false;
        m_writeGridSummaryMonth = false;
        m_writeGridSummaryMonthAdult = false;
        m_writeGridSummaryStd = false;
        m_writeGridSummaryStdAdult = false;
        m_writeGridSummaryStdHc = false;
        m_writeGridSummaryStdHcAdult = false;
        m_writeGridSummaryFteStd = false;
        m_writeGridSummaryFteStdAdult = false;

        // set indicators based on input options
        // if fte do not check others, fte is only for secondary
        if ((displaySumStdFte) && (m_sklLvl.equals(CONST_SKL_LVL_SEC))) {
            m_writeGridSummaryFteStd = true;
            m_writeGridSummaryFteStdAdult = true;
        }
        // check and display other reports if not fte
        else {
            if (displayDtl) {
                m_writeGridDetail = true;
            }
            if ((displaySumMonth) && (m_sklLvl.equals(CONST_SKL_LVL_ELEM))) {
                m_writeGridSummaryMonth = true;
            } else if ((displaySumMonth) && (m_sklLvl.equals(CONST_SKL_LVL_SEC))) {
                m_writeGridSummaryMonth = true;
                m_writeGridSummaryMonthAdult = true;
            }
            if ((displaySumStdHc) && (m_sklLvl.equals(CONST_SKL_LVL_ELEM))) {
                m_writeGridSummaryStd = true;
                m_writeGridSummaryStdAdult = true;
            } else if ((displaySumStdHc) && (m_sklLvl.equals(CONST_SKL_LVL_SEC))) {
                m_writeGridSummaryStdHc = true;
                m_writeGridSummaryStdHcAdult = true;
            }
        }
    }

    /**
     * Loads reference codes by code for reference table
     *
     * @param rtbOid
     *
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadReferenceCodesMap(String rtbOid) {
        Map<String, ReferenceCode> rcdByCodeMap = new HashMap<String, ReferenceCode>();

        // create criteria for student contact
        X2Criteria rcdCriteria = new X2Criteria();

        // for students in student criteria
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);

        // select query
        QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, rcdCriteria);

        rcdByCodeMap = getBroker().getMapByQuery(rcdQuery, ReferenceCode.COL_CODE, 1024);

        // return map
        return rcdByCodeMap;
    }

    /**
     * Loads localized reference code by code for reference table
     *
     * @param refClass
     * @param columnName
     *
     * @return Map<String, String>
     */
    private Map<String, String> loadReferenceCodeLocMap(Class refClass, String columnName) {
        Map<String, String> rcdByCodeMap = new HashMap<String, String>();

        // initialize other variables
        DataDictionaryField field = m_dictionary.findDataDictionaryField(refClass.getName(), columnName);
        String rtbOid = field.getReferenceTableOid();
        ModelProperty codeModelProperty =
                new ModelProperty(ReferenceCode.class, ReferenceCode.COL_CODE, getBroker().getPersistenceKey());

        if (!StringUtils.isEmpty(rtbOid)) {
            // create criteria
            X2Criteria rcdCriteria = new X2Criteria();

            // add reference table condition
            rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);

            // select query
            QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, rcdCriteria);

            try (QueryIterator<X2BaseBean> iterator = getBroker().getIteratorByQuery(rcdQuery)) {
                while (iterator.hasNext()) {
                    ReferenceCode rcd = (ReferenceCode) iterator.next();
                    String rcdCode = rcd.getCode();
                    String rcdCodeLocOut = rcdCode;
                    String rcdCodeLoc =
                            LocalizationUtils.getRsmDataValue(m_messageResources, rcd, codeModelProperty, CONST_EMPTY);
                    // the rcdCodeLoc contains "???" if a translated value is not found (as per
                    // observation) so that is excluded
                    if ((!StringUtils.isEmpty(rcdCodeLoc)) && (!rcdCodeLoc.contains("???"))) {
                        rcdCodeLocOut = rcdCodeLoc;
                    }

                    rcdByCodeMap.put(rcdCode, rcdCodeLocOut);
                }
            }
        }

        // return map
        return rcdByCodeMap;
    }

    /**
     * Returns the value of a boolean parameter or false if the parameter does not
     * exist in the input definition.
     *
     * @param key String
     * @return boolean
     */
    protected boolean getBooleanParameter(String key) {
        return (getParameter(key) != null) ? ((Boolean) getParameter(key)).booleanValue() : false;
    }

    /**
     * Returns the value of a boolean is student adult indicator
     *
     * @param std SisStudent
     * @return Boolean
     */
    protected Boolean isStdAdult(SisStudent std) {
        Boolean isAdult = Boolean.FALSE;

        SisPerson psn = std.getPerson();
        if (psn.getAgeAsOfDate(m_contextDateDec31) >= OntarioAlias.CONST_ADULT_AGE.intValue()) {
            isAdult = Boolean.TRUE;
        }

        return isAdult;
    }

    /**
     * Set report type variables for report value of m_rptType, m_sklLvl must be set
     * in the override
     */
    protected void setReportType() {
        // Override in extended class will set grade levels
    }

    /**
     * Set grade levels for report value of m_gradeLvls must be set in the override
     */
    protected void setGradeLevels() {
        // Override in extended class will set grade levels
    }

    /**
     * Takes an alias of field and returns either the java name or the database name
     * that field depending on the status of returnBbName.
     *
     * @param alias - String,
     * @param returnJavaName - boolean
     *
     * @return String the java name or field name of the object that is aliased
     */
    protected String getBeanPathFromAlias(String alias, boolean returnJavaName, DataDictionary dictionary) {
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
}
