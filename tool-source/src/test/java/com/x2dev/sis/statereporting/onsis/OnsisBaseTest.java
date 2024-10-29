/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.statereporting.onsis;

import static com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.SubmissionType.ALIAS_SUBMISSION_PERIOD_CODE;
import static com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.SubmissionType.DDX_ID;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_CRS_COURSE_CODE_TYPE;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_CRS_COURSE_DELIVERY_TYPE;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_MST_LANGUAGE_OF_INSTRUCTION;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_SKL_BSID;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.ALIAS_STD_OEN;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.ALIAS_STF_MEN;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.COURSE_CODE_TYPE_HOMEROOM;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.DATE_FORMAT_YYYY_MM_DD_SLASHES;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.DEFAULT_ONSIS_CALENDAR_IDS;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.INPUT_PARAM_DEBUG_STAFF_OID;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.INPUT_PARAM_DEBUG_STUDENT_OID;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.INPUT_PARAM_XML_FIELDS;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.TIME_FORMAT_HHMMSS;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.appendTextElement;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_BATCH_FILE;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_DATA;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_HEADER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BeanPathException;
import com.follett.fsc.core.k12.business.InvalidDictionaryIdException;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.test.AspenIntegrationTestManager;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.on.EnrollmentSpanHelper;
import com.x2dev.procedures.statereporting.on.OnsisDmFteSchoolYearNew;
import com.x2dev.procedures.statereporting.on.OnsisExtractHelper;
import com.x2dev.procedures.statereporting.on.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.DictionaryExtractor;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.SubmissionType;
import com.x2dev.procedures.statereporting.on.OnsisPublishAll;
import com.x2dev.procedures.statereporting.on.OnsisSchoolSubmission;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData.DebugOutputter;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisExtractor;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisHelper;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisUserDataContainer;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisXmlBatchFile;
import com.x2dev.procedures.statereporting.on.OnsisValidations.OnsisValidator;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.GradeTermDate;
import com.x2dev.sis.model.beans.GradeTermDefinition;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.JDOMException;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class OnsisBaseTest extends X2BaseTest {

    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: CONSTANTS
     ********************************************************************************
     ********************************************************************************/
    public static final String SUBMISSION_TYPE_CODE_OCTELEM2 = "OCTELEM2";
    public static final String SUBMISSION_TYPE_CODE_MARELEM2 = "MARELEM2";
    public static final String SUBMISSION_TYPE_CODE_MARSEC1 = "MARSEC1";
    public static final String SUBMISSION_TYPE_CODE_OCTSEC1 = "OCTSEC1";
    public static final String SUBMISSION_TYPE_CODE_JUNELEM3 = "JUNELEM3";

    public static final String HDR_OCT_ELEM_SCHOOL = "October Elementary School";
    public static final String HDR_MAR_ELEM_SCHOOL = "March Elementary School";
    public static final String HDR_JUN_ELEM_SCHOOL = "June Elementary School";
    public static final String HDR_OCT_SECONDARY_SCHOOL = "October Secondary School";
    public static final String HDR_MAR_SECONDARY_SCHOOL = "March Secondary School";

    public static final String HDR_TOPIC_STUDENT_SCHOOL_ENROLMENT = "Student School Enrolment";
    public static final String HDR_TOPIC_REPORT_CARD = "Report Card";

    public static final String SKL_OID_HIGHVIEW_PUBLIC = "skl01000000018";
    public static final String SKL_BSID_HVW = "256307";
    public static final String SKL_NAME_HIGHVIEW = "Highview Public School";

    public static final String SKL_OID_FELLOWS_HS = "skl01000000014";
    public static final String SKL_BSID_FHS = "925365";
    public static final String SKL_NAME_FELLOWS_HS = "Fellowes High School";

    // Language of Instruction. OnsisCode=B
    public static final String CODE_LANG_OF_INSTR_BOTH = "Both";
    public static final String ONSIS_LANG_OF_INSTR_BOTH = "B";

    // Health Education OnsisCode=PP
    public static final String CODE_ELEM_SUBJECT_TYPE_HEALTH_EDUC = "003";
    public static final String ONSIS_ELEM_SUBJECT_TYPE_HEALTH_EDUC = "PP";

    // Health Education OnsisCode=PP
    public static final String CODE_ELEM_SUBJECT_TYPE_PHYSICAL_EDUC = "004";
    public static final String ONSIS_ELEM_SUBJECT_TYPE_PHYSICAL_EDUC = "PA";

    public static final String CODE_CLASS_TYPE_EXTERNAL_EDUCATOR = "NE";

    public static final String DDX_SUBMISSION_OID = "ddxOnSisSubFld";

    public static final String FTE_ENROLLMENT_CODE = "IT";
    public static final String FTE_ENROLLMENT_REASON = "Change in FTE";


    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: DEFAULT VALUES
     ********************************************************************************
     ********************************************************************************/
    public static final String SCHOOL_YEAR = "2021";
    public static final String DEFAULT_ACADEMIC_YEAR = "2020-2021";
    public static final String DEFAULT_ONSIS_CALENDAR_ID = "Regular";
    public static final String DEFAULT_SCHEDULE_NAME = "2020-2021 Schedule";
    public static final String DEFAULT_TRANSCRIPT_DEFN_NAME = "Grade 1-8 Transcript";
    public static final String DEFAULT_TEACHING_TYPE_CODE = "T"; // isTeacher: B or T
    public static final String DEFAULT_JOB_CODE = "Teacher";

    public static final String DEFAULT_CRS_GRADE_LEVEL = "05";
    public static final String DEFAULT_CRS_GRADE_LEVEL_ONSIS = "5";
    public static final String DEFAULT_SFP_POSITION_TYPE = "TEA";
    public static final String DEFAULT_TERM_NAME = "FY";
    public static final String DEFAULT_MST_LANG_OF_INSTRUCTION = CODE_LANG_OF_INSTR_BOTH;
    public static final String DEFAULT_MST_LANG_OF_INSTRUCTION_ONSIS = ONSIS_LANG_OF_INSTR_BOTH;

    public static final String DEFAULT_SKL_ELEM_SCHOOL_OID = SKL_OID_HIGHVIEW_PUBLIC;
    public static final String DEFAULT_SKL_ELEM_BSID = SKL_BSID_HVW;
    public static final String DEFAULT_SKL_ELEM_NAME = SKL_NAME_HIGHVIEW;

    public static final String DEFAULT_SKL_SECONDARY_SCHOOL_OID = SKL_OID_FELLOWS_HS;
    public static final String DEFAULT_SKL_SECONDARY_BSID = SKL_BSID_FHS;
    public static final String DEFAULT_SKL_SECONDARY_NAME = SKL_NAME_FELLOWS_HS;


    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: XML TAG SETS
     ********************************************************************************
     ********************************************************************************/
    /*
     * XML_TAG constants end in "_ROOT" if they are incomplete/ have children to include
     */
    public static final String XML_TAGS_SCHOOL_ROOT =
            "SCHOOL_SUBMISSION\n" +
                    "SCHOOL_SUBMISSION/ACADEMIC_YEAR\n" +
                    "SCHOOL_SUBMISSION/SUBMISSION_PERIOD_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_NUMBER\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLEAR_PENDING_AREA\n";

    public static final String XML_TAGS_SCHOOL_CLASS =
            "SCHOOL_SUBMISSION/SCHOOL/CLASS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/CLASS_CODE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/CLASS_START_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/CLASS_END_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/CLASS_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/LANGUAGE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/CLASSROOM_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/MINISTRY_DFND_CRS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/INSTITUTION_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/LOCAL_DEV_CRS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/LANGUAGE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/CREDIT_VALUE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/CLASS/SEGMENT/TOTAL_CLASS_COURSE\n";

    public static final String XML_TAGS_SCHOOL_EDUCATOR_ASSIGNMENT =
            "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/MEN\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/POSITION_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/EDUCATOR_LEAVE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/NEW_EDUCATOR_LEAVE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNMENT_WTHD_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/NEW_ASSIGNMENT_WTHD_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNMENT_START_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNMENT_END_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/FTE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/DEPARTMENT_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/BOARD_EMPLOYEE_NUMBER\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/TEMPORARY_LETTER_APPROVAL\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/LETTER_PERMISSION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/TEACHING_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/CORE_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/NTIP_STATUS_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/INSTRUCTIONAL_TM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/INSTRUCTIONAL_TM/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/INSTRUCTIONAL_TM/INSTRUCTIONAL_TM_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/CLASS_ASSIGNMENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/CLASS_ASSIGNMENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/CLASS_ASSIGNMENT/CLASS_CODE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/ELEMENTARY_SUBJECT_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/LANGUAGE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/GRADE_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/NUMBER_OF_CLASSES\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/ASSIGNED_GRADE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/ASSIGNED_GRADE/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/ASSIGNED_SUBJECT/ASSIGNED_GRADE/GRADE_TYPE\n";

    public static final String XML_TAGS_STUDENT_ROOT =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/OEN\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_LEGAL_GIVEN_NAME\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_LEGAL_SECOND_NAME\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_LEGAL_LAST_NAME\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_DOB\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/GENDER_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/LANGUAGE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_MIN_NUMBER\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/GRADE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/CLASS_CODE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/LANGUAGE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STU_BRD_RES_STAT_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/ATTENDANCE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/EDP_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/RESIDENCE_STATUS_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/CURRENT_RESIDENCE_COUNTRY\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/CURRENT_RESIDENCE_PROVINCE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/INDIGENOUS_SELF_IDENTIFICATION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_MOBILITY_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/COUNTRY_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PROVINCE_STATE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/LANGUAGE_TYPE_PREVIOUS_SCH\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_MOBILITY_TYPE_EXIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/COUNTRY_TYPE_EXIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PROVINCE_STATE_TYPE_EXIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/POSTAL_AREA_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/COUNTRY_TYPE_BIRTH\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PROVINCE_STATE_TYPE_BIRTH\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/COUNTRY_TYPE_PERM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PROVINCE_STATE_TYPE_PERM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/YEAR_OF_ENTRY_TO_CANADA\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/MAIN_SCHOOL_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SCHOOL_STUDENT_NUMBER\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/FRENCH_ADMISSION_APPROVAL_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/DAYS_ABSENT_YTD\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/TIMES_LATE_YTD\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/LOCAL_SCHOOL_PROGRAM_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/FTE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/ENROLMENT_START_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/ENROLMENT_END_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/MATURE_STUDENT_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REACH_AHEAD_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OYAP_FLAG\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_CLASS =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/CLASS_CODE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/MINISTRY_DFND_CRS\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/INSTITUTION_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/LOCAL_DEV_CRS\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/LANGUAGE_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/CREDIT_VALUE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_DELIVERY_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_SEM_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/OTHER_COURSE_INFO\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/OTHER_COURSE_INFO/ACTION\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/OTHER_COURSE_INFO/OTHER_COURSE_INFO_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/WITHDRAWAL_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/WITHDRAWAL_DATE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/IEP_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/PROGRAM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/CLASSES_MISSED\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/TIMES_LATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/TOTAL_CLASSES\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_START_DATE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_END_DATE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/EARNED_CREDIT_VALUE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/FINAL_MARK\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_SBST_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_COMPLETE_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_REPEAT_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COMPULSORY_COURSE_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_INCOMPLETE_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/COURSE_CONTINUED_FLAG\n";


    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_SPED =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/EXCEPTIONALITY_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/SPECIAL_EDU_PLMNT_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/NON_IDENTIFIED_STUDENT_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/MAIN_EXCEPTIONALITY_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/IPRC_REVIEW_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/IPRC_STUDENT_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPECIAL_EDUCATION/INDIVIDUAL_EDUCATION_PLAN_FLAG\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_SLP =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SECOND_LANGUAGE_PROGRAM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SECOND_LANGUAGE_PROGRAM/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SECOND_LANGUAGE_PROGRAM/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SECOND_LANGUAGE_PROGRAM/MINUTES_PER_DAY_OF_INSTRUCTION\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_PLAR_DIPL_OTH =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/MINISTRY_DFND_CRS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/STATUS_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/CREDIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/COMPULSORY_CREDIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/DATE_APPROVED\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/PLAR/NEW_DATE_APPROVED\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/DIPLOMA\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/DIPLOMA/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/DIPLOMA/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/DIPLOMA/CERTIFICATE_ISSUED\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/MINISTRY_DFND_CRS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/INSTITUTION_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/LOCAL_DEV_CRS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/COURSE_COMPLETE_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/NEW_COURSE_COMPLETE_DATE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/OTHER_COURSE_INFO\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/OTHER_COURSE_INFO/ACTION\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/OTHER_COURSE_INFO/OTHER_COURSE_INFO_TYPE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/NEW_CREDIT_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/EARNED_CREDIT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/OTHER_CREDIT/FINAL_MARK\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_REPORT_CARD =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/GRADE_IN_SEPTEMBER\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/TERM_CODE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/DAYS_ABSENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/TIMES_LATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/ALTERNATIVE_REPORT_CARD_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/ACTION\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/SUBJECT_STRAND_CODE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/SUBJECT_NAME\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/IEP_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/SUPPORT_PROGRAM_CODE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/SL_PROGRAM_CODE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/NA_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/FRENCH_FLAG\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND/MARK\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/LEARNING_SKILL\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/LEARNING_SKILL/ACTION\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/LEARNING_SKILL/LEARNING_SKILL_CODE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/LEARNING_SKILL/LEVEL\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_SALEP_SPCE =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/STATUS_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/EXIT_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/START_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/END_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/COMPONENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/COMPONENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP/COMPONENT/TYPE" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPCE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPCE/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SPCE/SPCE_TYPE\n";

    public static final String XML_TAGS_STUDENT_SCHOOL_ENROLMENT_SHSM_PROGRAM =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/ALL_REQUIREMENTS_MET_DATE\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/ACTION\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/DATE_COMPLETED\n"
                    +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/CERTIFICATION_HOURS\n";

    public static final String XML_TAGS_STUDENT_NON_ENROLMENT =
            "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/GRADE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/MAIN_SCHOOL_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/LITERACY_STATUS_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/COMMUNITY_INVOLMENT_ACCU_HOURS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/ONTARIO_SCHOLARSHIP_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/ONTARIO_SCHOLARSHIP_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SPECIAL_EDUCATION_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/DIPLOMA\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/DIPLOMA/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/DIPLOMA/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/DIPLOMA/CERTIFICATE_ISSUED\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SPCE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SPCE/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SPCE/SPCE_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/ALL_REQUIREMENTS_MET_DATE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION/DATE_COMPLETED\n";

    public static final String XML_TAGS_PLAR =
            "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_REPORT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_REPORT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_REPORT/GRADE_9_10_IND_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_REPORT/GRADE_11_12_EQU_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_REPORT/GRADE_11_12_CHA_FLAG\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COURSES\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COURSES/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COURSES/MINISTRY_DFND_CRS\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COUNT\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COUNT/ACTION\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COUNT/PLAR_MATURE_EQ_CNT_TYPE\n" +
                    "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COUNT/STUDENT_HEAD_COUNT";
    private SimpleDateFormat m_outputDateFormatter;


    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: XML and DOM
     ********************************************************************************
     ********************************************************************************/
    /**
     * @param xmlOutputStr
     * @return
     * @throws IOException
     */
    protected org.w3c.dom.Document xmlToDom(String xmlOutputStr) throws IOException {
        return OnsisStateReportData.convertStringToXMLDocument(xmlOutputStr);
    }

    /**
     * @param xmlOutputStr
     * @return
     * @throws IOException
     */
    protected Element xmlToElem(String xmlOutputStr) throws IOException {
        return xmlToDom(xmlOutputStr).getDocumentElement();
    }

    /**
     *
     * @param element
     * @return
     */
    protected String domToXml(Element element) {
        return getDebugOutputter().outputString(element);
    }

    /**
     *
     * @param document
     * @return
     * @throws IOException
     */
    protected String domToXml(org.w3c.dom.Document document) throws IOException {
        return getDebugOutputter().outputString(document);
    }

    /**
     *
     * @param root
     * @param xpathExpression
     * @return
     * @throws XPathExpressionException
     */
    protected Element selectSingleElement(Element root, String xpathExpression) throws XPathExpressionException {
        Element result = null;
        XPath xpath = XPathFactory.newInstance().newXPath();
        NodeList elements = (NodeList) xpath.compile(xpathExpression)
                .evaluate(root, XPathConstants.NODESET);
        if (elements.getLength() > 0) {
            result = (Element) elements.item(0);
        }
        return result;
    }

    /**
     *
     * @param root
     * @param xpathToElement
     * @param content
     * @return
     * @throws XPathExpressionException
     */
    protected Element assertContent(Element root, String xpathToElement, String content)
            throws XPathExpressionException {
        String escapedContent = StringEscapeUtils.escapeXml(content);
        String expression = xpathToElement + "[.='" + escapedContent + "']";
        Element element = selectSingleElement(root, expression);
        assertNotNull(element);
        return element;
    }

    /**
     *
     * @param document
     * @param xpath
     * @return
     * @throws XPathExpressionException
     * @throws JDOMException
     */
    protected Element assertElement(org.w3c.dom.Document document, String xpath) throws XPathExpressionException {
        return assertElement(document.getDocumentElement(), xpath);
    }

    /**
     *
     * @param root
     * @param xpath
     * @return
     * @throws XPathExpressionException
     */
    protected Element assertElement(Element root, String xpath) throws XPathExpressionException {
        Element element = selectSingleElement(root, xpath);
        if (element == null) {
            fail("Unable to locate " + xpath + " \nat " + pathFromRoot(root) + "\n" + debugStr(root));
        }
        return element;
    }

    /**
     *
     * @param root
     * @param xpath
     * @throws XPathExpressionException
     */
    protected void assertNoElement(Element root, String xpath) throws XPathExpressionException {
        Element element = selectSingleElement(root, xpath);
        if (element != null) {
            fail("Did not expect to find " + xpath + " \nat " + pathFromRoot(root) + "\nin: " + debugStr(root));
        }
    }

    /**
     *
     * @param exampleRoot
     * @param testRoot
     * @throws Exception
     */
    protected void assertSubset(String exampleRoot, Element testRoot) throws Exception {
        assertFalse(StringUtils.isBlank(exampleRoot));
        assertSubset(xmlToElem(exampleRoot), testRoot);
    }

    /**
     * Deep-test if exampleRoot is a subset of testRoot
     * exampleRoot: <A><B>100</B><A>
     * testRoot: <A><B>999</B><B>100></B></A>
     *
     * @param exampleRoot
     * @param testRoot
     */
    protected void assertSubset(Element exampleRoot, Element testRoot) {
        assertNotNull(exampleRoot);
        assertNotNull(testRoot);

        boolean failIfNotFound = true;
        isSubset(exampleRoot, testRoot, failIfNotFound);
    }

    /**
     *
     * @param exampleChild
     * @param testParentRoot
     * @throws Exception
     */
    protected void assertHasChildSubset(String exampleChild, Element testParentRoot) throws Exception {
        assertFalse(StringUtils.isBlank(exampleChild));
        assertHasChildSubset(xmlToElem(exampleChild), testParentRoot);
    }

    /**
     * Deep-test if exampleChild is a child subset of testParentRoot
     * exampleChild: <B><C>X</C></B>
     * testParentRoot: <A><B><C>X</C>></B></A>
     *
     * @param exampleChild
     * @param testParentRoot
     */
    protected void assertHasChildSubset(Element exampleChild, Element testParentRoot) {
        Element element = findMatchingChild(exampleChild, testParentRoot);
        if (element == null) {
            fail("Unable to locate child " + debugStr(exampleChild) + "\nin: " + debugStr(testParentRoot));
        }
    }

    /**
     * Deep-test if exampleRoot is a subset of testRoot
     * exampleRoot: <A><B>100</B><A>
     * testRoot: <A><B>999</B><B>100></B></A>
     *
     * @param exampleRoot
     * @param testRoot
     * @param failIfNotFound
     * @return
     */
    protected boolean isSubset(Element exampleRoot, Element testRoot, boolean failIfNotFound) {
        /*
         * Match fails if masterRoot is different from exampleRoot
         */
        if (!StringUtils.isEqual(exampleRoot.getNodeName(), testRoot.getNodeName())) {
            if (failIfNotFound) {
                assertEquals(exampleRoot.getNodeName(), testRoot.getNodeName());
            }
            return false;
        }
        if (!StringUtils.isEqual(getText(exampleRoot), getText(testRoot))) {
            if (failIfNotFound) {
                assertEquals("Text for " + pathFromRoot(exampleRoot), getText(exampleRoot),
                        getText(testRoot));
            }
            return false;
        }

        /*
         * Match fails if testRoot is missing any of exampleRoot's children
         */
        List<Element> exampleChildren = OnsisStateReportData.getChildElements("*", exampleRoot);
        for (Element exampleChild : exampleChildren) {
            Element matchingChild = findMatchingChild(exampleChild, testRoot);
            if (matchingChild == null) {
                if (failIfNotFound) {
                    fail("Unable to find matching child element " + pathFromRoot(exampleChild)
                            + " with value " + debugStr(exampleChild));
                }
                return false;
            }
        }

        /*
         * Success: All components of exampleRoot were found in testRoot
         */
        return true;
    }

    /**
     *
     * @param node
     * @return
     */
    protected String getText(Node node) {
        NodeList list = node.getChildNodes();
        StringBuilder textContent = new StringBuilder();
        for (int i = 0; i < list.getLength(); ++i) {
            Node child = list.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                textContent.append(child.getTextContent());
            }
        }
        return textContent.toString().trim();
    }

    /**
     * Return the child of testRoot that recursively matches exampleChild
     *
     * @param exampleChild
     * @param testRoot
     * @return
     */
    protected Element findMatchingChild(Element exampleChild, Element testRoot) {
        List<Element> testChildren = OnsisStateReportData.getChildElements("*", testRoot);
        for (Element testChild : testChildren) {
            if (isSubset(exampleChild, testChild, false)) {
                return testChild;
            }
        }

        return null;
    }

    /**
     *
     * @param element
     * @return
     */
    public String pathFromRoot(Element element) {
        String pathFromRoot = element.getNodeName();
        Node currentElement = element;
        while (currentElement.getParentNode() != null) {
            currentElement = currentElement.getParentNode();
            if (!(currentElement instanceof Element)) {
                continue;
            }

            if (!StringUtils.isBlank(pathFromRoot)) {
                pathFromRoot = "/" + pathFromRoot;
            }
            pathFromRoot = currentElement.getNodeName() + pathFromRoot;
        }
        return pathFromRoot;
    }

    /**
     * @param element
     * @return
     */
    public String debugStr(Element element) {
        String debugStr = domToXml(element);
        return debugStr.substring(0, Math.min(2048, debugStr.length()));
    }



    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: ASPEN DATA SETUP
     ********************************************************************************
     ********************************************************************************/

    public static class SetupInfo {
        public String schoolOid;
        public String schoolBSID;
        public SisStaff staff;
        public String staffMen;
        public StaffPosition staffPosition;

        public Course course;
        public SchoolCourse schoolCourse;
        public MasterSchedule section;
        public ScheduleTeacher scheduleTeacher;
        public SisStudent student;
        public StudentSchedule studentSchedule;
        public Schedule schedule;
        public ScheduleTerm term;
        public StudentEnrollment enrollment;

        public String dashOen;
        public String cleanOen;
    }

    /**
     *
     * @param school
     * @param staff
     * @param isPrimary
     * @param student
     * @return
     * @throws X2BaseException
     */
    public SetupInfo basicSetup(SisSchool school, SisStaff staff, boolean isPrimary, SisStudent student)
            throws X2BaseException {
        long startTime = System.currentTimeMillis();

        SetupInfo setupInfo = new SetupInfo();
        setupInfo.schoolOid = school.getOid();
        setupInfo.schoolBSID = (String) school.getFieldValueByAlias(ALIAS_SKL_BSID);

        /*
         * Staff
         */
        if (staff == null) {
            setupInfo.staff = createStaff("UtxSmith", "Melba", setupInfo.schoolOid);

        } else {
            setupInfo.staff = staff;
        }
        setupInfo.staffMen = (String) setupInfo.staff.getFieldValueByAlias(ALIAS_STF_MEN);

        /*
         * StaffPosition
         */
        setupInfo.staffPosition = findStaffPosition(setupInfo.staff.getOid(), setupInfo.staff.getSchoolOid(),
                getContext().getStartDate());
        if (setupInfo.staffPosition == null) {
            setupInfo.staffPosition = createStaffPosition(setupInfo.staff.getOid(),
                    setupInfo.staff.getSchoolOid(), DEFAULT_JOB_CODE, DEFAULT_TEACHING_TYPE_CODE, null, null);
        }

        /*
         * Course
         */
        String courseNumber = "" + (90000 + (int) Math.floor(Math.random() * 9999));
        String courseName = "UTX " + courseNumber;
        String sectionNumber = "404";

        setupInfo.course = createCourse(courseName, courseNumber);
        setupInfo.course.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, COURSE_CODE_TYPE_HOMEROOM);
        saveDirtyBean(setupInfo.course);

        /*
         * SchoolCourse
         */
        TranscriptDefinition transcriptDefinition = getTranscriptDefinition(DEFAULT_TRANSCRIPT_DEFN_NAME);
        setupInfo.schoolCourse =
                createSchoolCourse(setupInfo.schoolOid, setupInfo.course, transcriptDefinition);
        assertNotNull("setup", setupInfo.schoolCourse);

        /*
         * MasterSchedule (section)
         */
        setupInfo.schedule = getSchedule(DEFAULT_SCHEDULE_NAME, setupInfo.schoolOid, getContext().getOid());
        assertNotNull("setup", setupInfo.schedule);
        setupInfo.term = getTerm(DEFAULT_TERM_NAME, setupInfo.schedule.getOid());
        assertNotNull("setup", setupInfo.term);

        setupInfo.section = createSection(setupInfo.term, setupInfo.schoolCourse, sectionNumber);

        /*
         * ScheduleTeacher
         */
        setupInfo.scheduleTeacher = createScheduleTeacher(setupInfo.staff.getOid(), setupInfo.section, isPrimary);

        /*
         * Section must have at least one student enrolled in it for a <CLASS_ASSIGNMENT> to export.
         * Student must also be enrolled in the school
         */
        String lastName = "UtxStudent";
        String firstName = "Barry";
        setupInfo.student = (student == null)
                ? createStudent(lastName, firstName, setupInfo.schoolOid, "8")
                : student;
        setupInfo.dashOen = (String) setupInfo.student.getFieldValueByAlias(ALIAS_STD_OEN);
        setupInfo.cleanOen = OnsisStateReportData.normalizeOen(setupInfo.dashOen);

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate enrollmentDate = findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);
        String entryType = StudentEnrollment.ENTRY;
        boolean updateStudent = true;
        setupInfo.enrollment =
                createEnrollment(setupInfo.student, setupInfo.schoolOid, entryType, enrollmentDate, updateStudent);

        /*
         * StudentSchedule (section enrollment)
         */
        setupInfo.studentSchedule = scheduleStudent(setupInfo.student, setupInfo.section);

        System.out.println("basicSetup took [" + (System.currentTimeMillis() - startTime) + "ms]");
        return setupInfo;
    }

    /**
     * Creates the staff.
     *
     * @param lastName String
     * @param firstName String
     * @param schoolOid String
     * @return Staff
     */
    public SisStaff createStaff(String lastName, String firstName, String schoolOid) {
        Organization organization = OrganizationManager.getRootOrganization(getBroker());
        String activeCode = PreferenceManager.getPreferenceValue(organization,
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

        HashMap<String, Object> colValues = new HashMap<>();
        colValues.put(Person.COL_LAST_NAME, lastName);
        colValues.put(Person.COL_FIRST_NAME, firstName);
        Person person = getTestObject(Person.class, getBroker(), colValues);
        person.setDob(new PlainDate(-645161497000L)); // Fri, 22 Jul 1949 20:28:23 GMT
        person.setOrganization1Oid(organization.getOid());
        person.setStaffIndicator(true);
        saveDirtyBean(person);

        colValues.clear();
        colValues.put(Staff.COL_PERSON_OID, person.getOid());
        SisStaff staff = getTestObject(SisStaff.class, getBroker(), colValues);
        staff.setNameView(lastName + ", " + firstName);
        staff.setSchoolOid(schoolOid);
        staff.setCalendarId(DEFAULT_ONSIS_CALENDAR_ID);
        staff.setStateId(generateMEN());
        staff.setStatus(activeCode);
        staff.setOrganization1Oid(organization.getOid());
        saveDirtyBean(staff);

        return staff;
    }

    /**
     *
     * @param staffOid
     * @param schoolOid
     * @param jobCode
     * @param teachingType
     * @param startDate
     * @param endDate
     * @return StaffPosition
     */
    public StaffPosition createStaffPosition(String staffOid,
                                             String schoolOid,
                                             String jobCode,
                                             String teachingType,
                                             PlainDate startDate,
                                             PlainDate endDate) {
        HashMap<String, Object> colValues = new HashMap<>();

        colValues.put(StaffPosition.COL_STAFF_OID, staffOid);
        colValues.put(StaffPosition.COL_SCHOOL_OID, schoolOid);
        colValues.put(StaffPosition.COL_JOB_CODE, jobCode);
        // colValues.put(StaffPosition.COL_SUBJECT_CODE, subject);

        if (startDate == null) {
            startDate = getContext().getStartDate();
        }
        colValues.put(StaffPosition.COL_START_DATE, startDate);

        if (endDate != null) {
            colValues.put(StaffPosition.COL_END_DATE, endDate);
        }

        setByAlias(colValues, OnsisStateReportData.StaffHelper.ALIAS_SFP_TEACHING_TYPE, teachingType);

        // colValues.put(StaffPosition.COL_SITE_CODE, "School");
        StaffPosition staffPosition = getTestObject(StaffPosition.class, getBroker(), colValues);

        return staffPosition;
    }

    /**
     *
     * @param staffOid
     * @param section
     * @param isPrimary
     * @return
     */
    public ScheduleTeacher createScheduleTeacher(String staffOid, MasterSchedule section, boolean isPrimary) {
        ScheduleTeacher scheduleTeacher = null;

        if (isPrimary) {
            // Setting section.PrimaryStaffOid will create a ScheduleTeacher.
            section.setPrimaryStaffOid(staffOid);
            getBroker().saveBeanForced(section);

            for (ScheduleTeacher testScheduleTeacher : section.getTeacherSections()) {
                if (staffOid.equals(testScheduleTeacher.getStaffOid())) {
                    scheduleTeacher = testScheduleTeacher;
                    if (scheduleTeacher.isNew()) {
                        saveTemporaryBeanForced(getBroker(), scheduleTeacher);
                    }
                }
            }
        } else {
            HashMap<String, Object> colValues = new HashMap<>();
            colValues.put(ScheduleTeacher.COL_STAFF_OID, staffOid);
            colValues.put(ScheduleTeacher.COL_SECTION_OID, section.getOid());
            colValues.put(ScheduleTeacher.COL_SCHEDULE_TERM_OID, section.getScheduleTermOid());
            scheduleTeacher = getTestObject(ScheduleTeacher.class, getBroker(), colValues);

            scheduleTeacher.setScheduleTermOid(section.getScheduleTermOid());
        }

        saveDirtyBean(scheduleTeacher);
        return scheduleTeacher;
    }

    /**
     *
     * @param courseName
     * @param courseNumber
     * @return
     */
    public Course createCourse(String courseName,
                               String courseNumber) {
        HashMap<String, Object> colValues = new HashMap<>();
        colValues.put(Course.COL_DISTRICT_CONTEXT_OID, getContext().getOid());
        colValues.put(Course.COL_NUMBER, courseNumber);
        colValues.put(Course.COL_DESCRIPTION, "UTX " + courseName);
        colValues.put(Course.COL_DEPARTMENT_CODE, "BUS");
        colValues.put(Course.COL_GRADE_LEVEL, "05");
        setByAlias(colValues, ALIAS_CRS_COURSE_DELIVERY_TYPE, "Day");

        Course course = getTestObject(Course.class, getBroker(), colValues);
        return course;
    }

    /**
     *
     * @param schoolOid
     * @param course
     * @param transcriptDefinition
     * @return
     */
    public SchoolCourse createSchoolCourse(String schoolOid, Course course, TranscriptDefinition transcriptDefinition) {
        HashMap<String, Object> colValues = new HashMap<>();
        colValues.put(SchoolCourse.COL_SCHOOL_OID, schoolOid);
        colValues.put(SchoolCourse.COL_COURSE_OID, course.getOid());
        colValues.put(SchoolCourse.COL_TRANSCRIPT_DEFINITION_OID, transcriptDefinition.getOid());
        colValues.put(SchoolCourse.COL_NUMBER, course.getNumber());
        colValues.put(SchoolCourse.COL_DESCRIPTION, "UTX " + course.getNumber());
        colValues.put(SchoolCourse.COL_MASTER_TYPE, "Class");
        colValues.put(SchoolCourse.COL_TERM_GRADES_TERM_MAP, "1111");
        SchoolCourse schoolCourse = getTestObject(SchoolCourse.class, getBroker(), colValues);
        return schoolCourse;
    }

    /**
     * Creates the section.
     *
     * @param term ScheduleTerm
     * @param schoolCourse SchoolCourse
     * @param sectionNumber String
     * @return MasterSchedule
     */
    public MasterSchedule createSection(ScheduleTerm term, SchoolCourse schoolCourse, String sectionNumber) {
        HashMap<String, Object> colValues = new HashMap<>();
        colValues.put(MasterSchedule.COL_SCHOOL_COURSE_OID, schoolCourse.getOid());
        colValues.put(MasterSchedule.COL_SECTION_NUMBER, sectionNumber);
        colValues.put(MasterSchedule.COL_SCHEDULE_OID, term.getScheduleOid());
        colValues.put(MasterSchedule.COL_SCHEDULE_TERM_OID, term.getOid());
        setByAlias(colValues, ALIAS_MST_LANGUAGE_OF_INSTRUCTION, DEFAULT_MST_LANG_OF_INSTRUCTION);
        MasterSchedule section = getTestObject(MasterSchedule.class, getBroker(), colValues);
        return section;
    }

    /**
     *
     * @param name
     * @return
     */
    public TranscriptDefinition getTranscriptDefinition(String name) {
        return findBeanByValues(TranscriptDefinition.class,
                TranscriptDefinition.COL_TRANSCRIPT_DEFINITION_NAME, name);
    }

    /**
     *
     * @param termCode
     * @param scheduleOid
     * @return
     */
    public ScheduleTerm getTerm(String termCode, String scheduleOid) {
        return findBeanByValues(ScheduleTerm.class,
                ScheduleTerm.COL_CODE, termCode,
                ScheduleTerm.COL_SCHEDULE_OID, scheduleOid);
    }

    /**
     *
     * @param scheduleName
     * @param schoolOid
     * @param contextOid
     * @return
     */
    public Schedule getSchedule(String scheduleName, String schoolOid, String contextOid) {
        return findBeanByValues(Schedule.class,
                Schedule.COL_NAME, scheduleName,
                Schedule.COL_SCHOOL_OID, schoolOid,
                Schedule.COL_DISTRICT_CONTEXT_OID, contextOid);
    }

    /**
     * Schedule student.
     *
     * @param student SisStudent
     * @param section MasterSchedule
     * @return StudentSchedule
     * @throws X2BaseException exception
     */
    public StudentSchedule scheduleStudent(SisStudent student, MasterSchedule section)
            throws X2BaseException {
        StudentSchedule studentSchedule = null;
        HashMap<String, String> sectionOids = new HashMap();
        sectionOids.put(section.getOid(), section.getScheduleTerm().getCode());

        Collection<StudentSchedule> studentSchedules =
                getScheduleManager().createStudentSchedule(MasterSchedule.class, sectionOids, student.getOid(),
                        new HashMap<String, List<String>>());
        for (StudentSchedule testStudentSchedule : studentSchedules) {
            if (testStudentSchedule.isNew()) {
                studentSchedule = testStudentSchedule;
                saveTemporaryBeanForced(getBroker(), testStudentSchedule);
                break;
            }
        }

        return studentSchedule;
    }

    /**
     * Creates the student.
     *
     * @param lastName String
     * @param firstName String
     * @param schoolOid String
     * @param gradeLevel String
     * @return SisStudent
     */
    public SisStudent createStudent(String lastName, String firstName, String schoolOid, String gradeLevel) {
        return createStudentWithProperties(lastName, firstName, schoolOid, gradeLevel);
    }

    /**
     * Creates the student with properties.
     *
     * @param lastName String
     * @param firstName String
     * @param schoolOid String
     * @param gradeLevel String
     * @param properties Object[]
     * @return SisStudent
     */
    public SisStudent createStudentWithProperties(String lastName,
                                                  String firstName,
                                                  String schoolOid,
                                                  String gradeLevel,
                                                  Object... properties) {
        Organization organization = OrganizationManager.getRootOrganization(getBroker());

        HashMap<String, Object> colValues = new HashMap<>();
        colValues.put(Person.COL_LAST_NAME, lastName);
        colValues.put(Person.COL_FIRST_NAME, firstName);
        colValues.put(Person.COL_DOB, generateDob(null));

        Person person = (Person) buildTestBean(Person.class, getBroker(), colValues);

        person.setEmail01("sif@follett.com");
        person.setEmail02("soap@follett.com");
        person.setOrganization1Oid(organization.getOid());
        person.setGenderCode("M");
        // person.setPhone01("(587) 215-5555");
        // person.setPhone02("(587) 217-5556");
        // person.setPhone03("(508) 217-5557");

        setPropertiesOnBean(person, properties);

        saveTemporaryBeanForced(getBroker(), person);

        // String activeStatus =
        // PreferenceManager.getPreferenceValue(OrganizationManager.getRootOrganization(getBroker()),
        // SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        SisStudent student = (SisStudent) buildTestBean(SisStudent.class, getBroker());

        student.setNameView(lastName + ", " + firstName);
        student.setPersonOid(person.getOid());
        student.setSchoolOid(schoolOid);

        String activeStatus =
                PreferenceManager.getPreferenceValue(organization, SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        student.setEnrollmentStatus(activeStatus);

        // student.setLocalId(generateLocalId());
        student.setStateId(generateDashOEN());
        student.setOrganization1Oid(organization.getOid());
        student.setGradeLevel(gradeLevel);

        if (StringUtils.isNumeric(gradeLevel)) {
            int gradeLevelInt = Integer.valueOf(gradeLevel);
            int gradYearsFromNow = Math.max(0, 12 - gradeLevelInt);
            int thisSchoolYear = getContext().getSchoolYear();
            int yog = thisSchoolYear + gradYearsFromNow;
            student.setYog(yog);
        }

        setPropertiesOnBean(student, properties);

        saveTemporaryBeanForced(getBroker(), student);

        return student;
    }

    /**
     * Generate a valid State ID.
     *
     * @return String
     */
    public String generateDashOEN() {
        String rawStr = generateCleanOEN();
        return rawStr.substring(0, 3) + "-" + rawStr.substring(3, 6) + "-" + rawStr.substring(6);
    }

    /**
     * Generate a valid State ID.
     *
     * @return String
     */
    public String generateCleanOEN() {
        return StringUtils.padInt((int) Math.floor(Math.random() * 999999999), 9);
    }

    /**
     * Generate dob.
     *
     * @param yyyymmdd String
     * @return PlainDate
     */
    public PlainDate generateDob(String yyyymmdd) {
        PlainDate dob = new PlainDate();
        SimpleDateFormat fmt = new SimpleDateFormat("yyyy-MM-dd");
        try {
            String dateStr = "2000-02-02";
            if (!StringUtils.isEmpty(yyyymmdd)) {
                dateStr = yyyymmdd;
                if (dateStr.length() == 8) {
                    dateStr = dateStr.substring(0, 4) + "-" + dateStr.substring(4, 6) + "-" + dateStr.substring(6, 8);
                }
            }
            dob = new PlainDate(fmt.parse(dateStr));
        } catch (ParseException e) {
            e.printStackTrace();
        }

        return dob;
    }

    /**
     * Set the given properties or bean paths on a bean.
     *
     * @param bean X2BaseBean
     * @param properties Name and Value pairs, where name is a property name or bean path
     */
    public void setPropertiesOnBean(X2BaseBean bean, Object... properties) {
        for (int i = 0; i < properties.length; i += 2) {
            String name = (String) properties[i];
            Object fieldValue = properties[i + 1];

            try {
                ModelProperty identifyingProperty = new ModelProperty(name, getDataDictionary());
                bean.setFieldValueByProperty(identifyingProperty, fieldValue);
            } catch (InvalidDictionaryIdException exc) {
                try {
                    bean.setFieldValueByBeanPath(name, fieldValue);
                } catch (BeanPathException bpe) {
                    /*
                     * This will happen in createStudentWithProperties
                     * Because it attempts each property from a combined map on both Student and
                     * Person beans
                     */
                    // System.out.println("Unable to set invalid property [" + name + "] on bean ["
                    // + bean + "] to value ["
                    // + fieldValue + "] \n" + LoggerUtils.convertThrowableToString(bpe));

                    try {
                        bean.setFieldValueByAlias(name, fieldValue);
                    } catch (Exception ex) {
                        //
                    }
                }
            }
        }
    }

    public String getActiveStatusCode() {
        return PreferenceManager.getPreferenceValue(OrganizationManager.getRootOrganization(getBroker()),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
    }

    public String getPreRegStatusCode() {
        return PreferenceManager.getPreferenceValue(OrganizationManager.getRootOrganization(getBroker()),
                SystemPreferenceDefinition.STUDENT_PRE_REG);

    }

    /**
     * Creates the enrollment with properties.
     *
     * @param student SisStudent
     * @param schoolOid String
     * @param entryType String
     * @param enrollmentDate PlainDate
     * @param updateStudent boolean
     * @param properties Object[]
     * @return StudentEnrollment
     */
    public StudentEnrollment createEnrollment(SisStudent student,
                                              String schoolOid,
                                              String entryType,
                                              PlainDate enrollmentDate,
                                              boolean updateStudent,
                                              Object... properties) {
        String statusCode = (StudentEnrollment.WITHDRAWAL.equals(enrollmentDate))
                ? "Inactive"
                : getActiveStatusCode();
        return createEnrollment(student, schoolOid, entryType, enrollmentDate,
                "Internal Transfer", statusCode,
                updateStudent, properties);
    }

    /**
     *
     * @param student
     * @param schoolOid
     * @param entryType
     * @param enrollmentDate
     * @param enrollmentCode
     * @param statusCode
     * @param updateStudent
     * @param properties
     * @return
     */
    public StudentEnrollment createEnrollment(SisStudent student,
                                              String schoolOid,
                                              String entryType,
                                              PlainDate enrollmentDate,
                                              String enrollmentCode,
                                              String statusCode,
                                              boolean updateStudent,
                                              Object... properties) {
        HashMap<String, Object> colValues = new HashMap<>();
        colValues.put(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        colValues.put(StudentEnrollment.COL_SCHOOL_OID, schoolOid);
        colValues.put(StudentEnrollment.COL_ENROLLMENT_TYPE, entryType);
        colValues.put(StudentEnrollment.COL_REASON_CODE, "Reason");
        colValues.put(StudentEnrollment.COL_ENROLLMENT_CODE, enrollmentCode);
        colValues.put(StudentEnrollment.COL_STATUS_CODE, statusCode);

        Integer yog = Integer.valueOf(computeYOG(student));

        colValues.put(StudentEnrollment.COL_YOG, yog);
        colValues.put(StudentEnrollment.COL_ENROLLMENT_DATE, enrollmentDate);
        StudentEnrollment enrollment =
                (StudentEnrollment) buildTestBean(StudentEnrollment.class, getBroker(), colValues);

        // User overrides
        setPropertiesOnBean(enrollment, properties);

        saveTemporaryBeanForced(getBroker(), enrollment);

        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), student.getSchoolOid(), DEFAULT_ONSIS_CALENDAR_ID);
        if (schoolCalendar != null) {
            student.setCalendarCode(schoolCalendar.getCalendarId());
            getBroker().saveBeanForced(student);
        }

        if (updateStudent) {
            student.setEnrollmentStatus(statusCode);
            if (StudentEnrollment.ENTRY.equals(entryType)) {
                student.setSchoolOid(schoolOid);
            }
            getBroker().saveBeanForced(student);
        }

        return enrollment;
    }

    /**
     * Compute YOG based on current Student grade level
     *
     * @param student
     * @return
     */
    public int computeYOG(Student student) {
        int yog = 0;
        int currentSchoolYear = getContext().getSchoolYear();

        int currentGradeLevel = 0;
        String gradeLevelStr = student.getGradeLevel();
        if (StringUtils.isNumeric(gradeLevelStr)) {
            currentGradeLevel = Integer.parseInt(gradeLevelStr);
        }

        /*
         * yearsUntilGrad = (12 - currentGradeLevel)
         * YOG = currentYear + yearsUntilGrad
         */
        int yearsUntilGrad = Math.max(0, 12 - currentGradeLevel);
        yog = currentSchoolYear + yearsUntilGrad;

        return yog;
    }

    /**
     *
     * @param student
     * @param school
     * @param properties
     * @return
     */
    public StudentSchool createSecondaryEnrollment(SisStudent student,
                                                   SisSchool school,
                                                   Object... properties) {
        DistrictSchoolYearContext context = getContext();

        HashMap<String, Object> colValues = new HashMap<>();

        colValues.put(StudentSchool.COL_DISTRICT_CONTEXT_OID, context.getOid());
        colValues.put(StudentSchool.COL_TYPE, StudentSchool.SECONDARY);
        colValues.put(StudentSchool.COL_SCHOOL_OID, school.getOid());
        colValues.put(StudentSchool.COL_STUDENT_OID, student.getOid());
        colValues.put(StudentSchool.COL_START_DATE, context.getStartDate());
        colValues.put(StudentSchool.COL_END_DATE, context.getEndDate());

        StudentSchool ssk = (StudentSchool) buildTestBean(StudentSchool.class, getBroker(), colValues);

        // User overrides
        setPropertiesOnBean(ssk, properties);

        saveTemporaryBeanForced(getBroker(), ssk);

        return ssk;
    }

    /**
     * Create GradeTermDates
     *
     * @param term
     * @param gradeTermDefinition
     *
     * @param school
     */
    public void createGradeTermDates(ScheduleTerm term, GradeTermDefinition gradeTermDefinition) {
        Schedule schedule = term.getSchedule();
        SisSchool school = schedule.getSchool();
        SchoolCalendar calendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);

        // if (term.getBaseTermsPerYear() == 1)
        {
            Collection<GradeTerm> gradeTerms = getOrCreateGradeTerms(gradeTermDefinition);

            int iGradeTerm = 0;
            PlainDate lastEndDate = null;

            // optimalDaysPerTerm is (total days / # terms)
            long scheduleIntervalMillis = schedule.getEndDate().getTime() - schedule.getStartDate().getTime();
            long scheduleIntervalDays = scheduleIntervalMillis / (24 * 60 * 60 * 1000);
            int optimalDaysPerTerm = (int) (scheduleIntervalDays / gradeTerms.size());

            for (GradeTerm gradeTerm : gradeTerms) {

                // Determine the date range for the term
                PlainDate startDate;
                if (iGradeTerm == 0) {
                    startDate = schedule.getStartDate();
                } else {
                    startDate = findFirstInSessionDate(calendar, DateUtils.add(lastEndDate, 1), true);
                }

                PlainDate endDate;
                if (iGradeTerm == gradeTerms.size() - 1) {
                    endDate = schedule.getEndDate();
                } else {
                    // Find most recent going backward
                    endDate = findFirstInSessionDate(calendar, DateUtils.add(startDate, optimalDaysPerTerm), false);
                }

                // Build the GradeTermDate
                GradeTermDate termDate = X2BaseBean.newInstance(GradeTermDate.class, getBroker().getPersistenceKey());
                termDate.setDistrictContextOid(getContext().getOid());
                termDate.setSchoolOid(school.getOid());
                termDate.setGradeTermOid(gradeTerm.getOid());

                termDate.setStartDate(startDate);
                termDate.setEndDate(endDate);

                saveTemporaryBeanForced(getBroker(), termDate);

                lastEndDate = endDate;
                iGradeTerm++;
            }
        }
    }

    /**
     *
     * @param gradeTermDefinition
     * @return
     */
    public Collection<GradeTerm> getOrCreateGradeTerms(GradeTermDefinition gradeTermDefinition) {
        // Create GradeTerms
        if (gradeTermDefinition.getGradeTerms().size() == 0) {
            for (int iGradeTerm = 0; iGradeTerm < gradeTermDefinition.getGradeTermsPerYear(); iGradeTerm++) {
                GradeTerm gradeTerm = X2BaseBean.newInstance(GradeTerm.class, getBroker().getPersistenceKey());
                gradeTerm.setGradeTermId("T" + (iGradeTerm + 1));
                gradeTerm.setGradeTermNum(iGradeTerm + 1);
                gradeTerm.setGradeTermDefinitionOid(gradeTermDefinition.getOid());
                saveTemporaryBeanForced(getBroker(), gradeTerm);
            }
        }

        return gradeTermDefinition.getGradeTerms();
    }

    /**
     * @param colValues
     * @param alias
     * @param string
     */
    protected void setByAlias(HashMap<String, Object> colValues, String alias, String value) {
        DataDictionaryField field = getDataDictionary()
                .findDataDictionaryFieldByAlias(alias);
        assertNotNull(field);
        colValues.put(field.getJavaName(), value);
    }

    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: DATE/CALENDAR HELPERS
     ********************************************************************************
     ********************************************************************************/

    /**
     * Find school calendar.
     *
     * @param contextOid
     * @param schoolOid
     * @param calendarId
     * @return
     */
    public SchoolCalendar findSchoolCalendar(String contextOid, String schoolOid, String calendarId) {
        Criteria criteria = new Criteria();

        criteria.addEqualTo(SchoolCalendar.COL_CALENDAR_ID, calendarId);
        criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, contextOid);
        criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, schoolOid);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, criteria);

        return getBroker().getBeanByQuery(query);
    }

    /**
     * Find first not in-session date in either direction
     *
     * @param calendar SchoolCalendar
     * @param startingDate PlainDate
     * @param forward
     *
     * @return PlainDate
     */
    public PlainDate findFirstNotInSessionDate(SchoolCalendar calendar, PlainDate startingDate, boolean forward) {
        boolean inSession = false;

        return findFirstInSessionDate(calendar, startingDate, forward, inSession);
    }

    /**
     * Find first in-session date in either direction
     *
     * @param calendar SchoolCalendar
     * @param startingDate PlainDate
     * @param forward
     * @return PlainDate
     */
    public PlainDate findFirstInSessionDate(SchoolCalendar calendar, PlainDate startingDate, boolean forward) {
        boolean inSession = true;

        return findFirstInSessionDate(calendar, startingDate, forward, inSession);
    }

    /**
     * Find first in-session date in either direction
     *
     * @param calendar SchoolCalendar
     * @param startingDate PlainDate
     * @param forward
     * @param inSession
     *
     * @return PlainDate
     */
    public PlainDate findFirstInSessionDate(SchoolCalendar calendar,
                                            PlainDate startingDate,
                                            boolean forward,
                                            boolean inSession) {
        PlainDate foundDate = null;

        int direction = forward ? 1 : -1;
        PlainDate testDate = startingDate;
        while (true) {
            boolean isInSession = CalendarManager.isInSession(calendar.getSchoolOid(), calendar.getCalendarId(),
                    testDate, getBroker());
            if ((inSession && isInSession) || (!inSession && !isInSession)) {
                foundDate = testDate;
                break;
            }

            testDate = DateUtils.add(testDate, direction);
        }

        return foundDate;
    }

    /**
     * Find last in-session for this school calendar.
     *
     * @param schoolCalendar
     *
     * @return PlainDate
     */
    public PlainDate findLastInSessionDate(SchoolCalendar schoolCalendar) {
        SisSchoolCalendarDate lastInSessionDate =
                CalendarManager.getLastInSessionDate(schoolCalendar, getBroker());
        if (lastInSessionDate == null) {
            throw new RuntimeException("Unable to find last in session date for school calendar: " + schoolCalendar);
        }

        return lastInSessionDate.getDate();
    }

    /**
     * @param date
     * @return
     */
    public String formatOutputDate(PlainDate date) {
        return getOutputDateFormatter().format(date);
    }

    /**
     *
     * @return
     */
    public SimpleDateFormat getOutputDateFormatter() {
        if (m_outputDateFormatter == null) {
            m_outputDateFormatter = new SimpleDateFormat(DATE_FORMAT_YYYY_MM_DD_SLASHES);
        }
        return m_outputDateFormatter;
    }


    /********************************************************************************
     ********************************************************************************
     * HELPERS FOR UNIT TESTS: UTILITY HELPERS
     ********************************************************************************
     ********************************************************************************/
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;

    /**
     *
     * @param alias
     */
    protected void assertAlias(String alias) {
        assertNotNull("Unable to find alias [" + alias + "]",
                getDataDictionary().findDataDictionaryFieldByAlias(alias));
    }

    /**
     *
     * @param bean
     * @param alias
     * @return
     */
    public String lookupStateCodeByAlias(X2BaseBean bean, String alias) {
        String value = (String) bean.getFieldValueByAlias(alias);
        return lookupStateCodeByAlias(value, alias);
    }

    /**
     *
     * @param value
     * @param alias
     * @return
     */
    public String lookupStateCodeByAlias(String value, String alias) {
        DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        assertNotNull("Unable to find alias [" + alias + "]");
        Map<String, ReferenceCode> refCodes = getReferenceCodes(dictionaryField.getReferenceTableOid());

        ReferenceCode code = refCodes.get(value);
        return (code == null)
                ? null
                : code.getStateCode();
    }

    /**
     * Lookup a map of reference codes for a reference table oid. Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;

        if (m_refTableMap == null) {
            m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    referenceTableOid);
            if (refTable != null) {
                codeMap = refTable.getCodeMap(null, null, getBroker(), false);
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }


    /**
     *
     * @param ddxId
     * @param broker
     * @return
     */
    public DataDictionary getExtendedDataDictionary(String ddxId, X2Broker broker) {
        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) broker.getBeanByQuery(ddxQuery);
        return DataDictionary.getDistrictDictionary(ddx, broker.getPersistenceKey());
    }

    /**
     * Gets the submission type.
     *
     * @param submissionTypeCode
     * @return Submission type
     */
    public SubmissionType getSubmissionType(String submissionTypeCode) {
        DictionaryExtractor dictExtractor = new DictionaryExtractor(getBroker());
        DataDictionaryField submissionCodeField = dictExtractor.getDictionary(DDX_ID)
                .findDataDictionaryFieldByAlias(ALIAS_SUBMISSION_PERIOD_CODE);

        UserDefinedTableA submissionRecord = findBeanByValues(UserDefinedTableA.class,
                UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID, DDX_SUBMISSION_OID,
                submissionCodeField.getJavaName(), submissionTypeCode);
        assertNotNull("Unable to locate " + submissionTypeCode, submissionRecord);

        return SubmissionType.find(getBroker(), submissionRecord.getOid(), dictExtractor);
    }

    /**
     *
     * @param submissionTypeCode
     * @return
     */
    public boolean isElem(String submissionTypeCode) {
        return submissionTypeCode.contains("ELEM");
    }

    /**
     *
     * @param submissionTypeCode
     * @return
     */
    protected String getDefaultSchoolOid(String submissionTypeCode) {
        return (isElem(submissionTypeCode))
                ? DEFAULT_SKL_ELEM_SCHOOL_OID
                : DEFAULT_SKL_SECONDARY_SCHOOL_OID;
    }

    /**
     *
     * @param submissionTypeCode
     * @return
     */
    protected String getDefaultSchoolBsid(String submissionTypeCode) {
        return (isElem(submissionTypeCode))
                ? DEFAULT_SKL_ELEM_BSID
                : DEFAULT_SKL_SECONDARY_BSID;
    }

    /**
     *
     * @param submissionTypeCode
     * @return
     */
    protected String getDefaultSchoolName(String submissionTypeCode) {
        return (isElem(submissionTypeCode))
                ? DEFAULT_SKL_ELEM_NAME
                : DEFAULT_SKL_SECONDARY_NAME;
    }

    /**
     * Generate a valid MEN Staff ID for Ontario
     *
     * @param length
     * @return String
     */
    public String generateMEN() {
        int men = 900000000 + (int) Math.floor(Math.random() * 99999999);
        return "" + men;
    }

    /**
     * Check bean is dirty before save
     *
     * @param bean
     */
    public void saveDirtyBean(X2BaseBean bean) {
        if (bean.isDirty()) {
            getBroker().saveBeanForced(bean);
        }
    }

    /**
     * Find bean by multiple values.
     *
     * @param <B> the generic type
     * @param beanClass Class
     * @param colAndVals String[]
     * @return B
     */
    public <B extends X2BaseBean> B findBeanByValues(Class beanClass, String... colAndVals) {
        Criteria criteria = new Criteria();
        for (int i = 0; i < colAndVals.length; i += 2) {
            String col = colAndVals[i];
            String val = colAndVals[i + 1];
            criteria.addEqualTo(col, val);
        }
        QueryByCriteria query = new BeanQuery(beanClass, criteria);
        return (B) getBroker().getBeanByQuery(query);
    }

    /**
     *
     * @param schoolOid
     * @param staffOid
     * @param startDate
     * @return
     */
    public StaffPosition findStaffPosition(String staffOid, String schoolOid, PlainDate startDate) {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StaffPosition.COL_STAFF_OID, staffOid);
        criteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, schoolOid);
        criteria.addEqualTo(StaffPosition.COL_START_DATE, startDate);
        return getBroker().getBeanByQuery(new QueryByCriteria(StaffPosition.class, criteria));
    }


    /********************************************************************************
     ********************************************************************************
     * INTERNAL BASE TEST STUFF
     ********************************************************************************
     ********************************************************************************/

    protected UserDataContainer m_userData = null;
    protected DebugOutputter m_xmlOutputter = null;
    protected static OnsisExtractor s_onsisExtractor = null;
    protected DataDictionary m_dictionary = null;
    private ScheduleManager m_scheduleManager = null;
    private OnsisHelper m_onsisHelper = null;

    @BeforeClass
    public static void beforeClass() {
        try {
            Class.forName("org.jaxen.BaseXPath");
        } catch (Throwable t) {
            fail("OnsisBaseTest requires jaxen-x.x.x.jar in classpath");
        }
    }

    /**
     * Getter for the dictionary.
     *
     * @return DataDictionary
     */
    public DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }

        return m_dictionary;
    }

    /**
     *
     * @return
     */
    protected DebugOutputter getDebugOutputter() {
        if (m_xmlOutputter == null) {
            m_xmlOutputter = new DebugOutputter();
        }
        return m_xmlOutputter;
    }

    /**
     *
     * @return
     */
    protected OnsisHelper getOnsisHelper() {
        if (m_onsisHelper == null) {
            m_onsisHelper = new OnsisHelper();
        }
        return m_onsisHelper;
    }

    /**
     * Getter for the ScheduleManager.
     *
     * @return ScheduleManager
     */
    protected ScheduleManager getScheduleManager() {
        if (m_scheduleManager == null) {
            m_scheduleManager = new ScheduleManager(getBroker());
        }
        return m_scheduleManager;
    }

    /**
     * Default SpanHelper for Onsis unit tests:
     * queryAsOfDate = null
     * cutoffBeforeLastYEar = true
     *
     * @return
     */
    protected EnrollmentSpanHelper createOnsisSpanHelper() {
        PlainDate queryAsOfDate = null;
        boolean cutoffBeforeLastYear = true;
        return createOnsisSpanHelper(queryAsOfDate, cutoffBeforeLastYear);
    }

    /**
     *
     * @param queryAsOfDate
     * @param cutoffBeforeLastYear
     * @return
     */
    protected EnrollmentSpanHelper createOnsisSpanHelper(PlainDate queryAsOfDate, boolean cutoffBeforeLastYear) {
        EnrollmentSpanHelper.SpanConfiguration spanConfig = createOnsisSpanConfiguration(getOrganization());
        spanConfig.queryAsOfDate = queryAsOfDate;
        spanConfig.historicalCutoffDate = (cutoffBeforeLastYear)
                ? getContext(-1).getStartDate()
                : null;

        EnrollmentSpanHelper spanHelper = new EnrollmentSpanHelper(spanConfig);
        return spanHelper;
    }

    /**
     *
     * @param org
     * @return
     */
    protected EnrollmentSpanHelper.SpanConfiguration createOnsisSpanConfiguration(Organization org) {
        EnrollmentSpanHelper.SpanConfiguration spanConfig =
                new EnrollmentSpanHelper.SpanConfiguration(org);
        spanConfig.defaultCalendarIds = Arrays.asList(DEFAULT_ONSIS_CALENDAR_IDS);
        spanConfig.aliasEnrollmentCalendarCode = null; // TODO
        spanConfig.statusBreak = true;
        spanConfig.yogBreak = false;
        spanConfig.ignoreForSpansCallback = new Predicate<StudentEnrollment>() {
            @Override
            public boolean test(StudentEnrollment enr) {
                return OnsisDmFteSchoolYearNew.isFteRecord(enr);
            }
        };
        spanConfig.historicalCutoffDate = getContext(-1).getStartDate();
        return spanConfig;
    }

    /**
     *
     * @param submissionTypeCode
     * @param schoolOids
     * @param studentOid
     * @param staffOid
     * @param overrideTags
     * @param csvContents
     * @return
     * @throws Exception
     */
    public String performExport(String submissionTypeCode,
                                List<String> schoolOids,
                                String studentOid,
                                String staffOid,
                                String overrideTags,
                                String... csvContents)
            throws Exception {
        String xmlOutputStr = null;

        try (ByteArrayOutputStream resultOutputStream = new ByteArrayOutputStream();
                Writer writer = new BufferedWriter(
                        new OutputStreamWriter(resultOutputStream, OnsisXmlBatchFile.ENCODING_UTF_8))) {

            performExport(writer,
                    submissionTypeCode,
                    schoolOids,
                    studentOid,
                    staffOid,
                    overrideTags,
                    csvContents);
            writer.flush();
            writer.close();
            xmlOutputStr = resultOutputStream.toString();


        } catch (Exception e) {
            System.out.print(LoggerUtils.convertThrowableToString(e));
        }

        return xmlOutputStr;
    }

    /**
     *
     * @param writer
     * @param submissionTypeCode
     * @param schoolOids
     * @param studentOid
     * @param staffOid
     * @param overrideTags
     * @param csvContents
     * @throws Exception
     */
    protected void performExport(Writer writer,
                                 String submissionTypeCode,
                                 List<String> schoolOids,
                                 String studentOid,
                                 String staffOid,
                                 String overrideTags,
                                 String... csvContents)
            throws Exception {
        Map<String, Object> parameters = new HashMap<>();
        if (schoolOids != null && !schoolOids.isEmpty()) {
            parameters.put(OnsisPublishAll.PARAM_SCHOOL_OIDS,
                    StringUtils.convertCollectionToDelimitedString(schoolOids, ','));
        }
        if (!StringUtils.isBlank(studentOid)) {
            parameters.put(INPUT_PARAM_DEBUG_STUDENT_OID, studentOid);
        }
        if (!StringUtils.isBlank(staffOid)) {
            parameters.put(INPUT_PARAM_DEBUG_STAFF_OID, staffOid);
        }
        if (!StringUtils.isBlank(overrideTags)) {
            parameters.put(INPUT_PARAM_XML_FIELDS, overrideTags);
        }
        // parameters.put(INPUT_PARAM_DEBUG_DETAIL, Boolean.TRUE);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        Element batchFileElement = OnsisStateReportData.createTextElement(ONSIS_ELEM_BATCH_FILE, null, document);
        document.appendChild(batchFileElement);

        String batchType = "";
        appendHeader(batchFileElement, batchType);

        /*
         * <DATA>
         * <SCHOOL_SUBMISSION>
         * <ACADEMIC_YEAR>2017-2018</ACADEMIC_YEAR>
         * <SUBMISSION_PERIOD_TYPE>OCTSEC1</SUBMISSION_PERIOD_TYPE>
         */
        Element dataElement = OnsisStateReportData.appendTextElement(ONSIS_ELEM_DATA, null, batchFileElement);

        /*
         * CSV content
         */
        DictionaryExtractor dictExtractor = new DictionaryExtractor(getBroker());
        String submissionTypeOid = findSubmissionTypeForCode(submissionTypeCode, dictExtractor, getBroker());
        SubmissionType submissionType = SubmissionType.find(getBroker(), submissionTypeOid, dictExtractor);
        OnsisValidator onsisValidator = null;
        Set<String> missingTags = new HashSet<String>();
        Map<String, Set<String>> exceptions = new LinkedHashMap<>();

        OnsisExtractHelper extractHelper = new OnsisExtractHelper(getBroker());
        for (String csvContent : csvContents) {
            try (Reader reader = new StringReader(csvContent)) {
                addCsvMatcher(extractHelper, reader);
            }
        }

        if (s_onsisExtractor == null) {
            s_onsisExtractor = new OnsisExtractor();
        }

        s_onsisExtractor.setBroker(getBroker())
                .setContext(getContext())
                .setExtractHelper(extractHelper)
                .setExceptions(exceptions)
                .setOrganization(getOrganization())
                .setParameters(parameters)
                .setSubmissionType(submissionType)
                .setTopic(OnsisSchoolSubmission.ONSIS_TOPIC)
                .setMissingTags(missingTags)
                //TODO: Ken Bakke, check this code
                .setUserData(new OnsisUserDataContainer(m_userData.getPrivilegeSet(), m_userData.getUserOid()))
                .setValidator(onsisValidator);

        s_onsisExtractor.extractOnsis(dataElement);

        s_onsisExtractor.writeDocument(document, writer);

        for (String missingTag : missingTags) {
            System.out.println(missingTag);
        }

        if (!missingTags.isEmpty()) {
            StringBuilder errorMessage = new StringBuilder();
            errorMessage.append("Missing tags exception:");
            errorMessage.append("\r\n");
            for (String missingTag : missingTags) {
                errorMessage.append(missingTag);
                errorMessage.append("\r\n");
            }
            throw new RuntimeException(errorMessage.toString());
        }
    }


    /**
     *
     * @param parentElement
     * @param batchType
     * @return
     */
    private Element appendHeader(Element parentElement, String batchType) {
        Element headerElement =
                OnsisStateReportData.createTextElement(ONSIS_ELEM_HEADER, null, parentElement.getOwnerDocument());
        appendTextElement(OnsisXmlBatchFile.ONSIS_ELEM_VERSION, "1.0", headerElement);
        appendTextElement(OnsisXmlBatchFile.ONSIS_ELEM_BATCH_TYPE, batchType, headerElement);
        appendTextElement(OnsisXmlBatchFile.ONSIS_ELEM_BATCH_FILE_ID, "0001", headerElement);
        appendTextElement(OnsisXmlBatchFile.ONSIS_ELEM_DATE, "" + new PlainDate(), headerElement);
        appendTextElement(OnsisXmlBatchFile.ONSIS_ELEM_TIME, "" + new PlainTime(), headerElement);
        parentElement.appendChild(headerElement);
        return headerElement;
    }

    /**
     * @param broker
     * @return
     */
    private void addCsvMatcher(OnsisExtractHelper extractHelper, Reader reader) {
        OnsisExtractRecords matcherToAdd = extractHelper.getMatcherFromReader(reader, null);
        if (matcherToAdd != null) {
            extractHelper.setMatcher(matcherToAdd.getExtractType(), matcherToAdd);
        }
    }

    /**
     *
     * @param submissionTypeCode
     * @param dictExtractor
     * @param broker
     * @return
     */
    protected String findSubmissionTypeForCode(String submissionTypeCode,
                                               DictionaryExtractor dictExtractor,
                                               X2Broker broker) {
        UserDefinedTableA submissionTypeUda = findSubmissionTypeUdaForCode(submissionTypeCode, dictExtractor, broker);
        return submissionTypeUda.getOid();
    }

    /**
     *
     * @param submissionTypeCode
     * @param dictExtractor
     * @param broker
     * @return
     */
    protected UserDefinedTableA findSubmissionTypeUdaForCode(String submissionTypeCode,
                                                             DictionaryExtractor dictExtractor,
                                                             X2Broker broker) {
        String submissionTypeCodeColumn = dictExtractor.getDictionary(SubmissionType.DDX_ID)
                .findDataDictionaryFieldByAlias(SubmissionType.ALIAS_SUBMISSION_PERIOD_CODE).getJavaName();
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID, "ddxOnSisSubFld");
        criteria.addEqualTo(submissionTypeCodeColumn, submissionTypeCode);

        return broker.getBeanByQuery(new QueryByCriteria(UserDefinedTableA.class, criteria));
    }

    /**
     * @return
     */
    protected DistrictSchoolYearContext getContext() {
        return getOrganization().getCurrentContext();
    }

    /**
     *
     * @param relativeOrYearNum
     * @return
     */
    protected DistrictSchoolYearContext getContext(int relativeOrYearNum) {
        Map<Integer, DistrictSchoolYearContext> m_contextBySchoolYear = null;
        if (m_contextBySchoolYear == null) {
            m_contextBySchoolYear = new HashMap<>();
        }
        if (m_contextBySchoolYear.containsKey(relativeOrYearNum)) {
            return m_contextBySchoolYear.get(relativeOrYearNum);
        }

        int schoolYear = (relativeOrYearNum < 1900)
                ? getContext().getSchoolYear() + relativeOrYearNum
                : relativeOrYearNum;

        DistrictSchoolYearContext context = getDistrictContextRaw(schoolYear);
        m_contextBySchoolYear.put(Integer.valueOf(relativeOrYearNum), context);
        assertNotNull("Unable to locate DistrictSchoolYearContext [" + schoolYear + "]", context);
        return context;
    }

    /**
     *
     * @param schoolYear
     * @return
     */
    public DistrictSchoolYearContext getDistrictContextRaw(int schoolYear) {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(schoolYear));
        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);

        Assert.assertNotNull("Setup:", contexts);
        Assert.assertEquals("Setup:", 1, contexts.size());

        return contexts.iterator().next();
    }


    /**
     * @return
     */
    protected Organization getOrganization() {
        return OrganizationManager.getRootOrganization(getBroker());
    }

    /**
     * Sets the preference.
     *
     * @param key String
     * @param value String
     * @throws InvalidPreferenceException exception
     */
    public void setPreference(String key, String value) throws InvalidPreferenceException {
        String currentValue = PreferenceManager.getPreferenceValue(getOrganization(), key);
        if (!value.equals(currentValue)) {
            // m_previousPreferenceValues.put(key, currentValue);
            PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), key, value);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2BaseTest#setUp()
     */
    @Override
    public void setUp() throws Exception {
        super.setUp();

        setPreference(SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE, Boolean.TRUE.toString());
        setPreference(SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE, Boolean.FALSE.toString());
    }

    @Override
    public void tearDown() throws Exception {
        ModelBroker broker = new ModelBroker(getPrivilegeSet());
        Organization rootOrg = OrganizationManager.getRootOrganization(broker);
        if (rootOrg == null) {
            Assert.fail("Default org is null, we should never have this be null");
        }

        // Set context to 2004 to avoid X2BaseTest error message
        rootOrg.setCurrentContextOid(AspenIntegrationTestManager.ROOT_ORG_CONTEXT_OID);
        broker.saveBeanForced(rootOrg);

        super.tearDown();

        // Set context to current year
        resetSchoolYear(rootOrg, broker);
    }

    /**
     *
     * @param rootOrg
     * @param broker
     */
    protected void resetSchoolYear(Organization rootOrg, X2Broker broker) {
        if (rootOrg == null) {
            return;
        }

        String currentContextOid = null;
        DistrictSchoolYearContext currentContext = rootOrg.getCurrentContext();
        if (currentContext != null) {
            currentContextOid = currentContext.getOid();
        }

        if (StringUtils.isBlank(currentContextOid)
                || AspenIntegrationTestManager.ROOT_ORG_CONTEXT_OID.contentEquals(currentContextOid)) {
            X2Criteria currentYearCriteria = new X2Criteria();

            currentYearCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, SCHOOL_YEAR);

            QueryByCriteria currentYearQuery =
                    new QueryByCriteria(DistrictSchoolYearContext.class, currentYearCriteria);

            currentContext = broker.getBeanByQuery(currentYearQuery);

            if (currentContext == null) {
                Assert.fail("Unable to set the school year: " + SCHOOL_YEAR);
            }

            rootOrg.setCurrentContextOid(currentContext.getOid());
            broker.saveBeanForced(rootOrg);
        }
    }

    /********************************************************************************
     ********************************************************************************
     * SUPPORT METHODS FOR BASE TEST
     ********************************************************************************
     ********************************************************************************/

    /**
     * Write header.
     *
     * @param xmlWriter XMLStreamWriter
     * @throws XMLStreamException exception
     */
    protected void writeHeader(XMLStreamWriter xmlWriter, String batchType) throws XMLStreamException {
        xmlWriter.writeStartElement(OnsisXmlBatchFile.ONSIS_ELEM_HEADER);

        writeTag(xmlWriter, OnsisXmlBatchFile.ONSIS_ELEM_VERSION, "1.0");
        writeTag(xmlWriter, OnsisXmlBatchFile.ONSIS_ELEM_BATCH_TYPE, batchType);
        writeTag(xmlWriter, OnsisXmlBatchFile.ONSIS_ELEM_BATCH_FILE_ID, "0001");
        writeTag(xmlWriter, OnsisXmlBatchFile.ONSIS_ELEM_DATE, new PlainDate());
        writeTag(xmlWriter, OnsisXmlBatchFile.ONSIS_ELEM_TIME, new PlainTime());

        xmlWriter.writeEndElement();
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param tagName String
     * @param date PlainDate
     * @throws XMLStreamException exception
     */
    public void writeTag(XMLStreamWriter xmlWriter, String tagName, PlainDate date) throws XMLStreamException {
        String dateStr = formatOutputDate(date);
        writeTag(xmlWriter, tagName, dateStr);
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param tagName String
     * @param time PlainTime
     * @throws XMLStreamException exception
     */
    public void writeTag(XMLStreamWriter xmlWriter, String tagName, PlainTime time) throws XMLStreamException {
        SimpleDateFormat timeFormatter = new SimpleDateFormat(TIME_FORMAT_HHMMSS);
        String timeStr = timeFormatter.format(time);

        writeTag(xmlWriter, tagName, timeStr);
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param tagName String
     * @param text String
     * @throws XMLStreamException exception
     */
    public void writeTag(XMLStreamWriter xmlWriter, String tagName, String text) throws XMLStreamException {
        xmlWriter.writeStartElement(tagName);
        xmlWriter.writeCharacters(text);
        xmlWriter.writeEndElement();
    }

    /********************************************************************************
     ********************************************************************************
     * CSV BUILDERS
     ********************************************************************************
     ********************************************************************************/
    protected String headerForSubmission(String submissionType) {
        switch (submissionType) {
            case SUBMISSION_TYPE_CODE_OCTELEM2:
                return HDR_OCT_ELEM_SCHOOL;
            case SUBMISSION_TYPE_CODE_MARELEM2:
                return HDR_MAR_ELEM_SCHOOL;
            case SUBMISSION_TYPE_CODE_JUNELEM3:
                return HDR_JUN_ELEM_SCHOOL;
            case SUBMISSION_TYPE_CODE_OCTSEC1:
                return HDR_OCT_SECONDARY_SCHOOL;
            case SUBMISSION_TYPE_CODE_MARSEC1:
                return HDR_MAR_SECONDARY_SCHOOL;
        }
        fail("Need to define CSV header for submission type [" + submissionType + "]");
        return null;
    }

    /**
     *
     * @param submissionTypeCode
     * @return
     */
    protected String createDummyCsv(String submissionTypeCode) {
        String oen = "111111111";
        EnrolmentCsvBuilder enrBuilder = new EnrolmentCsvBuilder();
        enrBuilder.h1(DEFAULT_ACADEMIC_YEAR, headerForSubmission(submissionTypeCode));
        if (SUBMISSION_TYPE_CODE_OCTSEC1.equals(submissionTypeCode)
                || SUBMISSION_TYPE_CODE_MARSEC1.equals(submissionTypeCode)) {
            enrBuilder.h2(DEFAULT_SKL_SECONDARY_BSID, DEFAULT_SKL_SECONDARY_NAME);
        } else {
            enrBuilder.h2(DEFAULT_SKL_ELEM_BSID, DEFAULT_SKL_ELEM_NAME);
        }
        enrBuilder.dt(oen, new PlainDate());
        String enrolmentsCsv = enrBuilder.build();
        return enrolmentsCsv;
    }

    /**
     *
     * @param submissionTypeCode
     * @param academicYear
     * @param oen
     * @param bsid
     * @param schoolName
     * @return
     */
    protected String createEnrolmentCsv(String submissionTypeCode,
                                        String academicYear,
                                        String oen,
                                        PlainDate startDate,
                                        String bsid,
                                        String schoolName) {
        EnrolmentCsvBuilder enrBuilder = new EnrolmentCsvBuilder();
        enrBuilder.h1(academicYear, headerForSubmission(submissionTypeCode));
        enrBuilder.h2(bsid, schoolName);
        enrBuilder.dt(oen, startDate);
        String enrolmentsCsv = enrBuilder.build();
        return enrolmentsCsv;
    }

    /**
     *
     */
    public abstract static class CsvBuilder {
        protected StringBuilder builder = new StringBuilder();

        public CsvBuilder() {
            //
        }

        public abstract String getTopic();

        public CsvBuilder h1(String schoolYear, String submission) {
            builder.append("'H1','" + getTopic() + "','1','"
                    + schoolYear + "','" + submission + "','2019/11/13','02:27'\n");
            return this;
        }

        public CsvBuilder h2(String schoolId, String schoolName) {
            builder.append("'H2','" + schoolId + "','" + schoolName + "','02'\n");
            return this;
        }

        public String build() {
            return builder.toString().replaceAll("'", "\"");
        }

        protected String quoted(String value) {
            return ",'" + value + "'";
        }
    }

    /**
     *
     */
    public class EnrolmentCsvBuilder extends CsvBuilder {
        // public EnrolmentCsvBuilder() {
        // super();
        // }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisSchoolTest.CsvBuilder#getTopic()
         */
        @Override
        public String getTopic() {
            return HDR_TOPIC_STUDENT_SCHOOL_ENROLMENT;
        }

        @Override
        public EnrolmentCsvBuilder h1(String schoolYear, String submission) {
            return (EnrolmentCsvBuilder) super.h1(schoolYear, submission);
        }

        @Override
        public EnrolmentCsvBuilder h2(String schoolId, String schoolName) {
            return (EnrolmentCsvBuilder) super.h2(schoolId, schoolName);
        }

        /**
         *
         * @param oen
         * @param startDate
         * @return
         */
        public EnrolmentCsvBuilder dt(String oen, PlainDate startDate) {
            builder.append(
                    "'DT',"
                            + oen
                            + StringUtils.repeat(",''", 34)
                            + ",'" + formatOutputDate(startDate) + "'"
                            + "\n");
            return this;
        }
    }

    /**
     *
     */
    public class ReportCardCsvBuilder extends CsvBuilder {

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisSchoolTest.CsvBuilder#getTopic()
         */
        @Override
        public String getTopic() {
            return HDR_TOPIC_REPORT_CARD;
        }

        @Override
        public ReportCardCsvBuilder h1(String schoolYear, String submission) {
            return (ReportCardCsvBuilder) super.h1(schoolYear, submission);
        }

        @Override
        public ReportCardCsvBuilder h2(String schoolId, String schoolName) {
            return (ReportCardCsvBuilder) super.h2(schoolId, schoolName);
        }

        /**
         * Note, ReportCard has two Export Format Definitions for DT
         * where the only difference is whether TermCode is a CSV key or not
         *
         * @param oen
         * @return
         */
        public ReportCardCsvBuilder dt(String oen) {
            String termCode = "";
            String gradeInSep = "";
            String altReportCardFlag = "";
            String daysAbsent = "";
            String timesLate = "";

            builder.append("'DT'"
                    + quoted(oen)
                    + quoted(termCode)
                    + quoted(gradeInSep)
                    + quoted(altReportCardFlag)
                    + quoted(daysAbsent)
                    + quoted(timesLate)
                    + "\n");
            return this;
        }
    }
}
