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
package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GpaClassSizeLookup;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * The Class TNStandardFormatExport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class TNStandardFormatExport extends ToolJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String ALIAS_ADR_FAX_NUMBER = "cust-ADR-fax";
    private static final String ALIAS_ASD_XML_CFIG = "all-asd-StandardFormatConfiguration";
    private static final String ALIAS_SKL_CEEB_CODE = "all-skl-CEEBCode";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_STD_COUNSELOR = "Counselor";
    private static final String ALIAS_STD_DIPLOMA_TYPE = "DOE COMPLETION DOCUMENT TYPE";
    private static final String ALIAS_STD_GRADUATION_DATE = "DOE COMPLETION DOCUMENT DATE";
    private static final String ALIAS_STD_UNWEIGHTED_GPA = "all-std-TNTranscriptExchangeUnweightedGPA";
    private static final String ALIAS_STD_UNWEIGHTED_RANK = "all-std-TNTranscriptExchangeUnweightedRank";
    private static final String ALIAS_STD_WEIGHTED_GPA = "all-std-TNTranscriptExchangeWeightedGPA";
    private static final String ALIAS_STD_WEIGHTED_RANK = "all-std-TNTranscriptExchangeWeightedRank";
    private static final String ALIAS_TRN_SCHOOL_NAME = "Transcript School";
    private static final String ALIAS_TRN_SESSION_START_DATE = "all-trn-SessionStartDate";
    private static final String ALIAS_TRN_SESSION_END_DATE = "all-trn-SessionEndDate";

    private static final String CONST_DISABLE_BEAN_PATH = "-9999";
    private static final String CONST_NO_MATCH = "---no-match---";
    private static final String CONST_REGEX_MATCH_AREACODE = "(?<=\\()(.*?)(?=\\))";
    private static final String CONST_REGEX_MATCH_NON_DIGITS = "[^\\d]";

    private static final String DATE_DEFAULT = "1920-12-31";
    private static final String DATE_YYYY_MM = "yyyy-MM";
    private static final String DATE_YYYY_MM_DD = "yyyy-MM-dd";

    private static final String ELEM_ROOT_NMSPCE = "arb";
    private static final String ELEM_ROOT_NAME = "AcademicRecordBatch";
    private static final String ELEM_ROOT_URL = "urn:org:pesc:message:AcademicRecordBatch:v1.0.0";
    private static final String ELEM_ROOT_NS2_NMSPCE = "ns2";
    private static final String ELEM_ROOT_NS2_URL = "urn:org:pesc:message:HighSchoolTranscript:v1.3.0";
    private static final String ELEM_ROOT_NS3_NMSPCE = "ns3";
    private static final String ELEM_ROOT_NS3_URL = "urn:org:pesc:core:CoreMain:v1.3.0";
    private static final String ELEM_ROOT_NS4_NMSPCE = "ns4";
    private static final String ELEM_ROOT_NS4_URL = "urn:org:pesc:core:CoreMain:v1.12.0";
    private static final String ELEM_STD = "Student";
    private static final String ELEM_STD_ACA = "AcademicRecord";
    private static final String ELEM_STD_ACA_SLV = "StudentLevel";
    private static final String ELEM_STD_ACA_SLV_ELEM_CODE = "StudentLevelCode";
    private static final String ELEM_STD_ACA_ELEM_YOG = "CohortGraduationYear";
    private static final String ELEM_STD_ACA_AWD = "AcademicAward";
    private static final String ELEM_STD_ACA_AWD_ELEM_LVL = "AcademicAwardLevel";
    private static final String ELEM_STD_ACA_AWD_ELEM_DATE = "AcademicAwardDate";
    private static final String ELEM_STD_ACA_AWD_ELEM_TITL = "AcademicAwardTitle";
    private static final String ELEM_STD_ACA_AWD_ELEM_COMPDATE = "AcademicCompletionDate";
    private static final String ELEM_STD_ACA_SUM = "AcademicSummary";
    private static final String ELEM_STD_ACA_SUM_ELEM_TYP = "AcademicSummaryType";
    private static final String ELEM_STD_ACA_SUM_GPA = "GPA";
    private static final String ELEM_STD_ACA_SUM_GPA_ELEM_CRDHRS = "CreditHoursEarned";
    private static final String ELEM_STD_ACA_SUM_GPA_ELEM_GPA = "GradePointAverage";
    private static final String ELEM_STD_ACA_SUM_ELEM_CLRANK = "ClassRank";
    private static final String ELEM_STD_ACA_SUM_ELEM_CLSIZE = "ClassSize";
    private static final String ELEM_STD_ACA_SUM_ELEM_ENTDATE = "EntryDate";
    private static final String ELEM_STD_ACA_SES = "AcademicSession";
    private static final String ELEM_STD_ACA_SES_DTL = "AcademicSessionDetail";
    private static final String ELEM_STD_ACA_SES_DTL_ELEM_DES = "SessionDesignator";
    private static final String ELEM_STD_ACA_SES_DTL_ELEM_NAME = "SessionName";
    private static final String ELEM_STD_ACA_SES_DTL_ELEM_YEAR = "SessionSchoolYear";
    private static final String ELEM_STD_ACA_SES_DTL_ELEM_BEGDATE = "SessionBeginDate";
    private static final String ELEM_STD_ACA_SES_DTL_ELEM_ENDDATE = "SessionEndDate";
    private static final String ELEM_STD_ACA_SES_SCH = "School";
    private static final String ELEM_STD_ACA_SES_SCH_ELEM_NAME = "OrganizationName";
    private static final String ELEM_STD_ACA_SES_LVL = "StudentLevel";
    private static final String ELEM_STD_ACA_SES_LVL_ELEM_CODE = "StudentLevelCode";
    private static final String ELEM_STD_ACA_SES_CRS = "Course";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_CRDBASIS = "CourseCreditBasis";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_CRDVAL = "CourseCreditValue";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_CRDERND = "CourseCreditEarned";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_ACAGRD = "CourseAcademicGrade";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_LVL = "CourseLevel";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_ID = "OriginalCourseID";
    private static final String ELEM_STD_ACA_SES_CRS_ELEM_TITLE = "CourseTitle";
    private static final String ELEM_STD_HLT = "Health";
    private static final String ELEM_STD_HLT_IMM = "Immunizations";
    private static final String ELEM_STD_HLT_IMM_ELEM_DATE = "ImmunizationDate";
    private static final String ELEM_STD_HLT_IMM_ELEM_CODE = "ImmunizationCode";
    private static final String ELEM_STD_PSN = "Person";
    private static final String ELEM_STD_PSN_ELEM_ID = "SchoolAssignedPersonID";
    private static final String ELEM_STD_PSN_AGN = "AgencyIdentifier";
    private static final String ELEM_STD_PSN_AGN_ELEM_ID = "AgencyAssignedID";
    private static final String ELEM_STD_PSN_AGN_ELEM_CODE = "AgencyCode";
    private static final String ELEM_STD_PSN_ELEM_SSN = "SSN";
    private static final String ELEM_STD_PSN_BIR = "Birth";
    private static final String ELEM_STD_PSN_BIR_ELEM_DATE = "BirthDate";
    private static final String ELEM_STD_PSN_NM = "Name";
    private static final String ELEM_STD_PSN_NM_ELEM_FNM = "FirstName";
    private static final String ELEM_STD_PSN_NM_ELEM_MNM = "MiddleName";
    private static final String ELEM_STD_PSN_NM_ELEM_LNM = "LastName";
    private static final String ELEM_STD_PSN_CNT = "Contacts";
    private static final String ELEM_STD_PSN_CNT_ADR = "Address";
    private static final String ELEM_STD_PSN_CNT_ADR_ELEM_LINE1 = "AddressLine";
    private static final String ELEM_STD_PSN_CNT_ADR_ELEM_CITY = "City";
    private static final String ELEM_STD_PSN_CNT_ADR_ELEM_STATE = "StateProvinceCode";
    private static final String ELEM_STD_PSN_CNT_ADR_ELEM_ZIP = "PostalCode";
    private static final String ELEM_STD_PSN_CNT_ADR_ELEM_ATTN = "AttentionLine";
    private static final String ELEM_STD_PSN_CNT_PHN = "Phone";
    private static final String ELEM_STD_PSN_CNT_PHN_ELEM_AREACODE = "AreaCityCode";
    private static final String ELEM_STD_PSN_CNT_PHN_ELEM_NUMBER = "PhoneNumber";
    private static final String ELEM_STD_PSN_GEN = "Gender";
    private static final String ELEM_STD_PSN_GEN_ELEM_CODE = "GenderCode";
    private static final String ELEM_STD_PSN_RCE = "EthnicityRace";
    private static final String ELEM_STD_PSN_RCE_ELEM_ETH_CODE = "EthnicityCode";
    private static final String ELEM_STD_PSN_RCE_ELEM_RCE_CODE = "RaceCode";
    private static final String ELEM_STD_TST_SUB = "Subtest";
    private static final String ELEM_STD_TST_SUB_SCR = "TestScores";
    private static final String ELEM_STD_TST_SUB_SCR_ELEM_VAL = "ScoreValue";
    private static final String ELEM_TRAN = "TransmissionData";
    private static final String ELEM_TRAN_ELEM_DOC_ID = "DocumentID";
    private static final String ELEM_TRAN_ELEM_CREATE_DT = "CreatedDateTime";
    private static final String ELEM_TRAN_ELEM_DOC_TYPE = "DocumentTypeCode";
    private static final String ELEM_TRAN_ELEM_DOC_TYPE_VAL = "InstitutionRequest";
    private static final String ELEM_TRAN_ELEM_TYPE = "TransmissionType";
    private static final String ELEM_TRAN_ELEM_TYPE_VAL = "Original";
    private static final String ELEM_TRAN_SRC = "Source";
    private static final String ELEM_TRAN_DST = "Destination";
    private static final String ELEM_TRAN_SRC_DST_ORG = "Organization";
    private static final String ELEM_TRAN_SRC_DST_ORG_ELEM_CEEBACT = "CEEBACT";
    private static final String ELEM_TRAN_SRC_DST_ORG_LOC = "LocalOrganizationID";
    private static final String ELEM_TRAN_SRC_DST_ORG_LOC_ELEM_CODE = "LocalOrganizationIDCode";
    private static final String ELEM_TRAN_SRC_DST_ORG_LOC_ELEM_QUAL = "LocalOrganizationIDQualifier";
    private static final String ELEM_TRAN_SRC_DST_ORG_ELEM_NAME = "OrganizationName";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT = "Contacts";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_ADR = "Address";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_LINE1 = "AddressLine";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_CITY = "City";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_STATE = "StateProvinceCode";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_ZIP = "PostalCode";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_PHN = "Phone";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_PHN_ELEM_AREACODE = "AreaCityCode";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_PHN_ELEM_NUMBER = "PhoneNumber";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_FAXPHN = "FaxPhone";
    private static final String ELEM_TRAN_SRC_DST_ORG_CNT_FAXPHN_ELEM_NUMBER = "PhoneNumber";
    private static final String ELEM_TRN_NAME = "HighSchoolTranscript";

    // Input parameters
    private static final String PARM_QUERY_BY = "queryBy";
    private static final String PARM_QUERY_STRING = "queryString";
    private static final String PARM_STATUSES = "statuses";
    private static final String PARM_TEST_SCORES = "testScores";
    private static final String PARM_UNWEIGHTED_GPA = "gpaUnweightedOid";
    private static final String PARM_WEIGHTED_GPA = "gpaWeightedOid";

    private static final SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
    private static final String AGENCY_CODE = "State";
    private static final String ACAD_SUMMARY_TYPE_NONWEIGHTED = "NonWeighted";
    private static final String ACAD_SUMMARY_TYPE_WEIGHTED = "Weighted";

    private static final Map<String, String> COURSE_CRD_BASIS_MAP;
    static {
        COURSE_CRD_BASIS_MAP = new HashMap<String, String>();
        COURSE_CRD_BASIS_MAP.put("N", "Regular");// TN - Not AP or Honors
        COURSE_CRD_BASIS_MAP.put("STD", "Regular");// Knox - Standard
        COURSE_CRD_BASIS_MAP.put("COL", "Regular");// TN - College level courses
        COURSE_CRD_BASIS_MAP.put("COLL", "Regular");// Knox - College level courses
        COURSE_CRD_BASIS_MAP.put("NIC", "Regular");// TN & Knox - National Industry
                                                   // Certification/CTE National Industry
                                                   // Certification Course
        COURSE_CRD_BASIS_MAP.put("IB", "InternationalBaccalaureate");// TN & Knox - International
                                                                     // Baccalaureate
        COURSE_CRD_BASIS_MAP.put("HON", "Regular");// TN & Knox - Honors
        COURSE_CRD_BASIS_MAP.put("SDC", "HighSchoolDualCredit");// TN - Statewide Dual Credit
        COURSE_CRD_BASIS_MAP.put("DC", "HighSchoolDualCredit");// Knox - Dual Credit Course
        COURSE_CRD_BASIS_MAP.put("DE", "HighSchoolDualCredit");// Knox - Dual Enrollment Course
        COURSE_CRD_BASIS_MAP.put("AP", "AdvancedPlacement");// TN & Knox - Advanced Placement
    }
    private static final Map<String, String> COURSE_LVL_MAP;
    static {
        COURSE_LVL_MAP = new HashMap<String, String>();
        COURSE_LVL_MAP.put("N", "Basic");// TN - Not AP or Honors
        COURSE_LVL_MAP.put("STD", "Basic");// Knox - Standard
        COURSE_LVL_MAP.put("COL", "CollegeLevel");// TN - College level courses
        COURSE_LVL_MAP.put("COLL", "CollegeLevel");// Knox - College level courses
        COURSE_LVL_MAP.put("IB", "InternationalBaccalaureate");// TN & Knox - International
                                                               // Baccalaureate
        COURSE_LVL_MAP.put("HON", "Honors");// TN & Knox - Honors
        COURSE_LVL_MAP.put("AP", "AdvancedPlacement");// TN & Knox - Advanced Placement
    }
    private static final Map<String, String> GRADE_LVL_MAP;
    static {
        GRADE_LVL_MAP = new HashMap<String, String>();
        GRADE_LVL_MAP.put("01", "FirstGrade");
        GRADE_LVL_MAP.put("02", "SecondGrade");
        GRADE_LVL_MAP.put("03", "ThirdGrade");
        GRADE_LVL_MAP.put("04", "FourthGrade");
        GRADE_LVL_MAP.put("05", "FifthGrade");
        GRADE_LVL_MAP.put("06", "SixthGrade");
        GRADE_LVL_MAP.put("07", "SeventhGrade");
        GRADE_LVL_MAP.put("08", "EighthGrade");
        GRADE_LVL_MAP.put("09", "NinthGrade");
        GRADE_LVL_MAP.put("10", "TenthGrade");
        GRADE_LVL_MAP.put("11", "EleventhGrade");
        GRADE_LVL_MAP.put("12", "TwelfthGrade");
        GRADE_LVL_MAP.put("K", "KindergartenGrade");
    }
    private static final Map<String, String> RACE_CODE_MAP;
    static {
        RACE_CODE_MAP = new HashMap<String, String>();
        RACE_CODE_MAP.put("A", "Asian");
        RACE_CODE_MAP.put("B", "BlackAfricanAmerican");
        RACE_CODE_MAP.put("I", "AmericanIndianAlaskaNative");
        RACE_CODE_MAP.put("P", "NativeHawaiianPacificIslander");
        RACE_CODE_MAP.put("W", "White");
    }
    private static final String STATE_CODE = "TN";

    private static final String XML_ATTR1_NAME = "standalone";
    private static final String XML_ATTR1_VALUE = "yes";

    /**
     * Get phone area code, number.
     *
     * @param phoneNumber String
     * @return List<String>
     */
    private static List<String> getPhoneNumberParts(String phoneNumber) {
        List<String> phoneNumberParts = null;

        if (!StringUtils.isEmpty(phoneNumber)) {
            phoneNumberParts = new ArrayList<String>();
            int phoneNumberLength = phoneNumber.length();

            Pattern pattern = Pattern.compile(CONST_REGEX_MATCH_AREACODE);
            Matcher matcher = pattern.matcher(phoneNumber);

            String areacode = "";
            if (matcher.find()) {
                areacode = matcher.group(1);
            } else {
                if (phoneNumberLength > 3) {
                    areacode = phoneNumber.substring(0, 3);
                }
            }
            phoneNumberParts.add(areacode);

            String number = phoneNumber.replaceFirst(areacode, "").replaceAll(CONST_REGEX_MATCH_NON_DIGITS, "");
            phoneNumberParts.add(number);
        }

        return phoneNumberParts;
    }

    /**
     * The Class ExtendedElement.
     */
    class ExtendedElement extends Element {

        /**
         * Instantiates a new extended element.
         */
        public ExtendedElement() {
            super();
        }

        /**
         * Instantiates a new extended element.
         *
         * @param name String
         */
        public ExtendedElement(String name) {
            super(name);
        }

        /**
         * Adds the content conditional.
         *
         * @param childName String
         * @param value String
         * @param addEmpty boolean
         * @return ExtendedElement
         */
        public ExtendedElement addContentConditional(String childName, String value, boolean addEmpty) {
            if (addEmpty || !StringUtils.isEmpty(value)) {
                Element element = new Element(childName);
                element.addContent(StringUtils.unNullify(value));
                this.addContent(element);
            }
            return this;
        }

        /**
         * Adds the content extended.
         *
         * @param element Element
         * @return ExtendedElement
         */
        public ExtendedElement addContentExtended(Element element) {
            this.addContent(element);
            return this;
        }
    }

    /**
     * The Class SessionKey.
     */
    class SessionKey implements Comparable<SessionKey> {
        private String m_schoolName;
        private String m_gradeLevel;
        String m_sessionDesignator;
        String m_sessionBeginDateOut;
        String m_sessionEndDateOut;
        private String m_termCode;
        private Transcript m_transcript;

        /**
         * Instantiates a new session key.
         *
         * @param transcript Transcript
         */
        public SessionKey(Transcript transcript) {
            m_transcript = transcript;
            SisStudent std = transcript.getStudent();
            SisSchool skl = transcript.getSchool();
            String overrideSchoolName = getTranscriptOverrideSchoolName(transcript);
            if (StringUtils.isEmpty(overrideSchoolName)) {
                m_schoolName = skl != null ? skl.getName() : std.getSchool().getName();
            } else {
                m_schoolName = overrideSchoolName;
            }

            m_gradeLevel = std.getGradeLevel();
            if (!StringUtils.isEmpty(transcript.getGradeLevel())) {
                m_gradeLevel = transcript.getGradeLevel();
            }

            m_termCode = "";
            MasterSchedule mst = transcript.getMasterSchedule();
            if (mst != null) {
                ScheduleTerm trm = mst.getScheduleTerm();
                if (trm != null) {
                    m_termCode = trm.getCode();
                }
            }

            if (StringUtils.isEmpty(m_termCode)) {
                m_termCode = transcript.getTermCode();
            }
            if (StringUtils.isEmpty(m_termCode)) {
                m_termCode = "FY";
            }
        }

        /**
         * Compare to.
         *
         * @param o SessionKey
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(SessionKey o) {
            int value = m_schoolName.compareTo(o.m_schoolName);
            if (value == 0) {
                value = m_gradeLevel.compareTo(o.m_gradeLevel);
            }
            if (value == 0) {
                value = m_termCode.compareTo(o.m_termCode);
            }
            return value;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            SessionKey other = (SessionKey) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_gradeLevel == null) {
                if (other.m_gradeLevel != null) {
                    return false;
                }
            } else if (!m_gradeLevel.equals(other.m_gradeLevel)) {
                return false;
            }
            if (m_schoolName == null) {
                if (other.m_schoolName != null) {
                    return false;
                }
            } else if (!m_schoolName.equals(other.m_schoolName)) {
                return false;
            }
            if (m_termCode == null) {
                if (other.m_termCode != null) {
                    return false;
                }
            } else if (!m_termCode.equals(other.m_termCode)) {
                return false;
            }
            return true;
        }

        /**
         * Gets the schedule name.
         *
         * @return String
         */
        public String getScheduleName() {
            return m_termCode;
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        public String getSchoolName() {
            return m_schoolName;
        }

        /**
         * Gets the school year.
         *
         * @return String
         */
        public String getSchoolYear() {
            Integer schoolYearInt = null;
            if (m_transcript.getDistrictContext() != null) {
                schoolYearInt = Integer.valueOf(m_transcript.getDistrictContext().getSchoolYear());
            }
            if (schoolYearInt == null && m_transcript.getMasterSchedule() != null) {
                schoolYearInt = m_transcript.getMasterSchedule().getSchedule().getDistrictContext().getSchoolYear();
            }
            Integer schoolYearPrevInt = schoolYearInt == null ? null : Integer.valueOf(schoolYearInt.intValue() - 1);
            return schoolYearInt == null ? "" : schoolYearPrevInt.toString() + "-" + schoolYearInt.toString();
        }

        /**
         * Gets the session designator.
         *
         * @return the sessionDesignator
         */
        public String getSessionDesignator() {
            if (m_sessionDesignator == null) {
                initSessionDates();
            }
            return m_sessionDesignator;
        }

        /**
         * Gets the session begin date out.
         *
         * @return the sessionBeginDateOut
         */
        public String getSessionBeginDateOut() {
            if (m_sessionBeginDateOut == null) {
                initSessionDates();
            }
            return m_sessionBeginDateOut;
        }

        /**
         * Gets the session end date out.
         *
         * @return the sessionEndDateOut
         */
        public String getSessionEndDateOut() {
            if (m_sessionEndDateOut == null) {
                initSessionDates();
            }
            return m_sessionEndDateOut;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_gradeLevel == null) ? 0 : m_gradeLevel.hashCode());
            result = prime * result + ((m_schoolName == null) ? 0 : m_schoolName.hashCode());
            result = prime * result + ((m_termCode == null) ? 0 : m_termCode.hashCode());
            return result;
        }

        /**
         * Gets the outer type.
         *
         * @return TN standard format export
         */
        private TNStandardFormatExport getOuterType() {
            return TNStandardFormatExport.this;
        }

        /**
         * Inits the session dates.
         */
        private void initSessionDates() {
            m_sessionDesignator = getDateFormat(DATE_YYYY_MM).format(m_defaultSessionDate);
            m_sessionBeginDateOut = getDateFormat(DATE_YYYY_MM_DD).format(m_defaultSessionDate);
            m_sessionEndDateOut = getDateFormat(DATE_YYYY_MM_DD).format(m_defaultSessionDate);
            Date sessionBeginDate = null;
            Date sessionEndDate = null;
            // Set based on schedule if found;
            ScheduleTerm trm =
                    m_transcript.getMasterSchedule() != null ? m_transcript.getMasterSchedule().getScheduleTerm()
                            : null;
            Collection<ScheduleTermDate> tmdList = null;
            if (trm == null) {
                tmdList = getScheduleTermDates(m_transcript.getDistrictContextOid(), m_termCode);
            } else if (trm != null) {
                tmdList = getScheduleTermDates(trm.getOid());
            }
            if (tmdList != null && !tmdList.isEmpty()) {
                for (ScheduleTermDate tmd : tmdList) {
                    if (tmd.getStartDate() != null
                            && (sessionBeginDate == null || sessionBeginDate.after(tmd.getStartDate()))) {
                        sessionBeginDate = tmd.getStartDate();
                    }
                    if (tmd.getEndDate() != null
                            && (sessionEndDate == null || sessionEndDate.before(tmd.getEndDate()))) {
                        sessionEndDate = tmd.getEndDate();
                    }
                }
            }
            // If not found found look at input parameter fields
            if ((sessionBeginDate == null) || (sessionEndDate == null)) {
                try {
                    String sessionBeginDateStr =
                            (String) m_transcript.getFieldValueByAlias(ALIAS_TRN_SESSION_START_DATE);
                    if (!StringUtils.isEmpty(sessionBeginDateStr)) {
                        sessionBeginDate = getDateFormat(DATE_YYYY_MM_DD).parse(sessionBeginDateStr);
                    }
                    String sessionEndDateStr = (String) m_transcript.getFieldValueByAlias(ALIAS_TRN_SESSION_END_DATE);
                    if (!StringUtils.isEmpty(sessionEndDateStr)) {
                        sessionEndDate = getDateFormat(DATE_YYYY_MM_DD).parse(sessionEndDateStr);
                    }
                } catch (ParseException pe) {
                    pe.printStackTrace();
                }
            }
            // If not found based on schedule or input parameters, look at Grade Terms
            if ((trm != null) && ((sessionBeginDate == null) || (sessionEndDate == null))) {
                String gradeTermMapStr = trm.getGradeTermMap();
                if (null != gradeTermMapStr) {
                    // If not found based on schedule or input parameters, look at Grade Terms
                    char[] gradeTermMap;
                    gradeTermMap = gradeTermMapStr.toCharArray();
                    for (int i = 0; i < gradeTermMap.length; i++) {
                        if (gradeTermMap[i] == '1') {
                            GradeTermDate gta = getGradeTermDate(m_transcript, i + 1, gradeTermMap.length);
                            if (gta != null) {
                                Date gtaStartDate = gta.getStartDate();
                                if ((sessionBeginDate == null) ||
                                        (gtaStartDate.before(sessionBeginDate))) {
                                    sessionBeginDate = gtaStartDate;
                                }
                                Date gtaEndDate = gta.getEndDate();
                                if ((sessionEndDate == null) ||
                                        (gtaEndDate.after(sessionEndDate))) {
                                    sessionEndDate = gtaEndDate;
                                }
                            }
                        }
                    }
                }
            }
            // Move to output fields
            if (!((sessionBeginDate == null) || (sessionEndDate == null))) {
                m_sessionDesignator = getDateFormat(DATE_YYYY_MM).format(sessionEndDate);
                m_sessionBeginDateOut = getDateFormat(DATE_YYYY_MM_DD).format(sessionBeginDate);
                m_sessionEndDateOut = getDateFormat(DATE_YYYY_MM_DD).format(sessionEndDate);
            }
        }

    }

    /**
     * The Class TranscriptInfo.
     */
    class TranscriptInfo {
        private Transcript m_trn;

        /**
         * Instantiates a new transcript info.
         *
         * @param trn Transcript
         */
        public TranscriptInfo(Transcript trn) {
            this.m_trn = trn;
        }

        /**
         * Gets the course credit basis.
         *
         * @return String
         */
        public String getCourseCreditBasis() {
            SchoolCourse crs = m_trn.getSchoolCourse();
            String academicLevel = crs.getAcademicLevel();
            String courseCreditBasis = null;
            if (!StringUtils.isEmpty(academicLevel)) {
                courseCreditBasis = COURSE_CRD_BASIS_MAP.get(academicLevel);
            }
            if (StringUtils.isEmpty(courseCreditBasis)) {
                courseCreditBasis = "Regular";
            }
            return courseCreditBasis;
        }

        /**
         * Gets the course description.
         *
         * @return String
         */
        public String getCourseDescription() {
            SchoolCourse crs = m_trn.getSchoolCourse();
            String courseDescription = crs.getDescription();
            if (!StringUtils.isEmpty(m_trn.getCourseDescription())) {
                courseDescription = m_trn.getCourseDescription();
            }
            return courseDescription;
        }

        /**
         * Gets the course level.
         *
         * @return String
         */
        public String getCourseLevel() {
            SchoolCourse crs = m_trn.getSchoolCourse();
            String academicLevel = crs.getAcademicLevel();
            String courseLevel = null;
            if (!StringUtils.isEmpty(academicLevel)) {
                courseLevel = COURSE_LVL_MAP.get(academicLevel);
            }
            return courseLevel;
        }

        /**
         * Gets the course number.
         *
         * @return String
         */
        public String getCourseNumber() {
            return m_trn.getSchoolCourse().getNumber();
        }

        /**
         * Gets the credit earned.
         *
         * @return String
         */
        public String getCreditEarned() {
            BigDecimal credit = m_trn.getTotalCredit();
            return credit == null ? "" : credit.toString();
        }

        /**
         * Gets the credit total.
         *
         * @return String
         */
        public String getCreditTotal() {
            BigDecimal credit = m_trn.getSchoolCourse().getCredit();
            return credit == null ? "" : credit.toString();
        }

        /**
         * Gets the final grade.
         *
         * @return String
         */
        public String getFinalGrade() {
            String finalGrade = m_trn.getFinalGrade();
            if (!StringUtils.isEmpty(finalGrade)) {
                if (StringUtils.isNumeric(finalGrade)) {
                    finalGrade = Long.valueOf(Math.round(Double.parseDouble(finalGrade))).toString();
                }
            }
            return finalGrade;
        }

    }

    private Map<String, List<StudentAssessment>> m_assessmentsMap;
    private GpaClassSizeLookup m_classSizeLookup;
    private HashMap<DataDictionaryField, SystemStringConverter> m_converterMap;
    private String m_customFileName;
    private Map<String, SimpleDateFormat> m_dateFormatMap = new HashMap();
    private Date m_defaultSessionDate;
    private Map<String, DataDictionary> m_dictionaryMap = new HashMap();
    private Element m_elementTransmissionData;
    private Map<String, StudentEnrollment> m_enrollmentsMap;
    private String m_fieldOverrideSchool;
    private Map<Integer, String> m_gpaContextOidsMap;
    private Map<String, GradeTermDate> m_gradeTermDatesMap;
    private Map<String, List<HealthImmunizationDose>> m_immunizationDosesMap;
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = new HashMap();
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDatesByTrmMap;
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDatesByYearTermMap;
    private X2Criteria m_studentCriteria;
    private SubQuery m_studentOidSubQuery;
    private Map<String, Map<SessionKey, List<Transcript>>> m_studentSessionsMap;
    private Map<String, StudentEnrollment> m_withdrawalsMap;

    /**
     * Gets the custom file name.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        if (m_customFileName == null) {
            Random random = new Random();
            m_customFileName = "export" + random.nextInt(1000) + ".xml";
            getJob().getInput().setFormat(ToolInput.XML_FORMAT);
        }
        return m_customFileName;
    }

    /**
     * Create Xml document.
     *
     * @return Element
     * @throws Exception exception
     */
    protected Element createDocument() throws Exception {
        Namespace rootName = Namespace.getNamespace(ELEM_ROOT_NMSPCE, ELEM_ROOT_URL);
        Element root = new Element(ELEM_ROOT_NAME, rootName);
        Namespace rootNs2 = Namespace.getNamespace(ELEM_ROOT_NS2_NMSPCE, ELEM_ROOT_NS2_URL);
        Namespace rootNs3 = Namespace.getNamespace(ELEM_ROOT_NS3_NMSPCE, ELEM_ROOT_NS3_URL);
        Namespace rootNs4 = Namespace.getNamespace(ELEM_ROOT_NS4_NMSPCE, ELEM_ROOT_NS4_URL);
        root.addNamespaceDeclaration(rootNs2);
        root.addNamespaceDeclaration(rootNs3);
        root.addNamespaceDeclaration(rootNs4);

        BeanQuery query = new BeanQuery(SisStudent.class, getStudentCriteria());
        try (QueryIterator iter = getBroker().getIteratorByQuery(query)) {
            while (iter.hasNext()) {
                SisStudent student = (SisStudent) iter.next();

                root.addContent(new Element(ELEM_TRN_NAME, rootNs2)
                        .addContent(createTransmissionDataElement())
                        .addContent(createStudentElement(student)));
            }
        }

        return root;
    }

    /**
     * Gets the date format.
     *
     * @param format String
     * @return Simple date format
     */
    protected SimpleDateFormat getDateFormat(String format) {
        if (!m_dateFormatMap.containsKey(format)) {
            m_dateFormatMap.put(format, new SimpleDateFormat(format));
        }
        return m_dateFormatMap.get(format);
    }

    /**
     * Gets the grade term date.
     *
     * @param trn Transcript
     * @param iGradeTerm int
     * @param cGradeTerm int
     * @return Grade term date
     */
    protected GradeTermDate getGradeTermDate(Transcript trn, int iGradeTerm, int cGradeTerm) {
        if (m_gradeTermDatesMap == null) {
            m_gradeTermDatesMap = new HashMap();
            QueryByCriteria query = new QueryByCriteria(GradeTermDate.class);

            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    GradeTermDate gtd = (GradeTermDate) iterator.next();

                    if (gtd.getGradeTerm() != null) {
                        String gtdKey = gtd.getDistrictContextOid() +
                                gtd.getSchoolOid() +
                                Integer.toString(gtd.getGradeTerm().getGradeTermNum()).toString() +
                                Integer.toString(gtd.getGradeTerm().getGradeTermDefinition().getGradeTermsPerYear())
                                        .toString();
                        m_gradeTermDatesMap.put(gtdKey, gtd);
                    }
                }
            }
        }
        String gtdKey = trn.getDistrictContextOid() +
                trn.getSchoolOid() +
                Integer.toString(iGradeTerm) +
                Integer.toString(cGradeTerm);
        return m_gradeTermDatesMap.get(gtdKey);
    }

    /**
     * Gets the schedule term dates.
     *
     * @param trmOid String
     * @return Collection
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(String trmOid) {
        if (m_scheduleTermDatesByTrmMap == null) {
            initScheduleTermDatesMap();
        }
        return m_scheduleTermDatesByTrmMap.get(trmOid);
    }

    /**
     * Gets the schedule term dates.
     *
     * @param ctxOid String
     * @param termCode String
     * @return Collection
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(String ctxOid, String termCode) {
        if (m_scheduleTermDatesByYearTermMap == null) {
            initScheduleTermDatesMap();
        }
        String trmKey = ctxOid + "-" + termCode;
        return m_scheduleTermDatesByYearTermMap.get(trmKey);
    }

    /**
     * Gets the transcript override school name.
     *
     * @param transcript Transcript
     * @return String
     */
    protected String getTranscriptOverrideSchoolName(Transcript transcript) {
        String value = null;
        if (m_fieldOverrideSchool == null) {
            DataDictionaryField fieldOverrideSchool =
                    getDataDictionary(null).findDataDictionaryFieldByAlias(ALIAS_TRN_SCHOOL_NAME);
            if (fieldOverrideSchool != null
                    && fieldOverrideSchool.getDataTable().getDataClass().equals(Transcript.class)) {
                m_fieldOverrideSchool = fieldOverrideSchool.getJavaName();
            } else {
                m_fieldOverrideSchool = "";
            }
        }
        if (!StringUtils.isEmpty(m_fieldOverrideSchool)) {
            value = (String) transcript.getFieldValueByBeanPath(m_fieldOverrideSchool);
        }
        return value;
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
        try {
            m_defaultSessionDate = getDateFormat(DATE_YYYY_MM_DD).parse(DATE_DEFAULT);
        } catch (ParseException pe) {
            throw new X2BaseException(pe);
        }
        buildEnrollmentMaps();
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        Element doc = createDocument();
        writeXml(doc);
    }

    /**
     * Write Xml output from document.
     *
     * @param documentRoot Element
     * @throws Exception exception
     */
    protected void writeXml(Element documentRoot) throws Exception {
        Document document = new org.jdom.Document(documentRoot);

        document.setProperty(XML_ATTR1_NAME, XML_ATTR1_VALUE);

        XMLOutputter outputter = new XMLOutputter(Format.getPrettyFormat());
        try (OutputStream out = getResultHandler().getOutputStream()) {
            outputter.output(document, out);
        }
    }

    /**
     * Add Student Academic Session Data Elements.
     *
     * @param student SisStudent
     * @param stdAcadRec Element
     * @return BigDecimal - Credit Hours Earned for Student
     */
    private BigDecimal addStdAcademicSessions(SisStudent student, Element stdAcadRec) {
        // Browse through transcript map to get transcripts by schedule term
        BigDecimal creditHoursEarnedStudent = BigDecimal.valueOf(0);
        Map<SessionKey, List<Transcript>> sessionsMap = getSessionsMap(student);
        if (sessionsMap != null) {
            for (Entry<SessionKey, List<Transcript>> item : sessionsMap.entrySet()) {
                creditHoursEarnedStudent = creditHoursEarnedStudent.add(
                        addStdAcademicSessionForSchTerm(student, stdAcadRec, item.getKey(), item.getValue()));
            }
        }
        return creditHoursEarnedStudent;
    }

    /**
     * Adds the std academic session for sch term.
     *
     * @param student SisStudent
     * @param stdAcadRec Element
     * @param key SessionKey
     * @param transcripts List<Transcript>
     * @return BigDecimal
     */
    private BigDecimal addStdAcademicSessionForSchTerm(SisStudent student,
                                                       Element stdAcadRec,
                                                       SessionKey key,
                                                       List<Transcript> transcripts) {

        BigDecimal creditHoursEarnedSession = BigDecimal.valueOf(0);

        // Construct element
        Element acadSession = new Element(ELEM_STD_ACA_SES);

        acadSession.addContent(new Element(ELEM_STD_ACA_SES_DTL)
                .addContent(createElement(ELEM_STD_ACA_SES_DTL_ELEM_DES, key.getSessionDesignator()))
                .addContent(createElement(ELEM_STD_ACA_SES_DTL_ELEM_NAME, key.getScheduleName()))
                .addContent(createElement(ELEM_STD_ACA_SES_DTL_ELEM_YEAR, key.getSchoolYear()))
                .addContent(createElement(ELEM_STD_ACA_SES_DTL_ELEM_BEGDATE, key.getSessionBeginDateOut()))
                .addContent(createElement(ELEM_STD_ACA_SES_DTL_ELEM_ENDDATE, key.getSessionEndDateOut())));
        acadSession.addContent(new Element(ELEM_STD_ACA_SES_SCH)
                .addContent(createElement(ELEM_STD_ACA_SES_SCH_ELEM_NAME, key.getSchoolName())));

        String sessionLevel = GRADE_LVL_MAP.get(key.getGradeLevel());
        if (!StringUtils.isEmpty(sessionLevel)) {
            acadSession.addContent(new Element(ELEM_STD_ACA_SES_LVL)
                    .addContent(createElement(ELEM_STD_ACA_SES_LVL_ELEM_CODE, sessionLevel)));
        }

        // Add courses
        for (Transcript trn : transcripts) {
            if (trn.getTotalCredit() != null) {
                creditHoursEarnedSession = creditHoursEarnedSession.add(trn.getTotalCredit());
            }
            TranscriptInfo trnInfo = new TranscriptInfo(trn);

            acadSession.addContent(new ExtendedElement(ELEM_STD_ACA_SES_CRS)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_CRDBASIS, trnInfo.getCourseCreditBasis(), true)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_CRDVAL, trnInfo.getCreditTotal(), true)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_CRDERND, trnInfo.getCreditEarned(), false)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_ACAGRD, trnInfo.getFinalGrade(), false)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_LVL, trnInfo.getCourseLevel(), false)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_ID, trnInfo.getCourseNumber(), true)
                    .addContentConditional(ELEM_STD_ACA_SES_CRS_ELEM_TITLE, trnInfo.getCourseDescription(), true));
        }

        stdAcadRec.addContent(acadSession);

        return creditHoursEarnedSession;
    }

    /**
     * Create Student Tests Data Elements.
     *
     * @param student SisStudent
     * @param std Element
     * @throws X2BaseException exception
     */
    private void addStdTests(SisStudent student, Element std) throws X2BaseException {
        // Browse through assessments map to get assessments for the student
        List<StudentAssessment> stdAssessmentsMap = getStudentAssessments(student);

        if (stdAssessmentsMap != null) {
            for (StudentAssessment asm : stdAssessmentsMap) {
                // Exclude test if grade level is populated and not in input parameter range
                String xml = (String) asm.getAssessmentDefinition().getFieldValueByAlias(ALIAS_ASD_XML_CFIG);
                if (!StringUtils.isEmpty(xml)) {
                    try {
                        SAXBuilder builder = new SAXBuilder();
                        Document xmlDocument = builder.build(new ByteArrayInputStream(xml.getBytes()));
                        Element root = xmlDocument.getRootElement();
                        if (ELEM_STD.equals(root.getName())) {
                            Iterator<Element> children = root.getChildren().iterator();
                            while (children.hasNext()) {
                                Element element = children.next();
                                Element assessment = getAssessment(asm, element);
                                if (cleanupAssessment(assessment)) {
                                    std.addContent(assessment);
                                }
                            }
                        }
                    } catch (JDOMException | IOException e) {
                        String bundle = null;
                        throw new X2BaseException(bundle,
                                "Cannot parse XML configuration for " + asm.getAssessmentDefinition().getId());
                    }
                }
            }
        }
    }

    /**
     * Builds the enrollment and withdrawal maps, which contain the latest enrollment and
     * withdrawal records within each student's current school.
     */
    private void buildEnrollmentMaps() {
        m_enrollmentsMap = new HashMap<String, StudentEnrollment>();
        m_withdrawalsMap = new HashMap<String, StudentEnrollment>();

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, getStudentOidSubQuery());
        List types = new ArrayList(2);
        types.add(StudentEnrollment.ENTRY);
        types.add(StudentEnrollment.WITHDRAWAL);

        enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, types);

        QueryByCriteria enrollmentQuery =
                new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);

        QueryIterator enrollments = getBroker().getIteratorByQuery(enrollmentQuery);
        try {
            while (enrollments.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollments.next();
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    m_enrollmentsMap.put(enrollment.getStudentOid(), enrollment);

                    /*
                     * Remove any withdrawal entries; we don't want to display a withdrawal date if
                     * an "E" record exists chronologically after it
                     */
                    m_withdrawalsMap.remove(enrollment.getStudentOid());
                } else {
                    m_withdrawalsMap.put(enrollment.getStudentOid(), enrollment);
                }
            }
        } finally {
            enrollments.close();
        }
    }

    /**
     * Cleanup assessment by removing an Subtests with ScoreValue value <= 0
     *
     * @param assessment Element
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean cleanupAssessment(Element assessment) throws X2BaseException {
        List<Element> invalidSubtests = new ArrayList();
        boolean containsSubtest = false;
        for (Object item : assessment.getChildren()) {
            Element child = (Element) item;
            if (ELEM_STD_TST_SUB.equals(child.getName())) {
                boolean containsScore = false;
                for (Object item2 : child.getChildren()) {
                    Element child2 = (Element) item2;
                    if (ELEM_STD_TST_SUB_SCR.equals(child2.getName())) {
                        for (Object item3 : child2.getChildren()) {
                            Element child3 = (Element) item3;
                            if (ELEM_STD_TST_SUB_SCR_ELEM_VAL.equals(child3.getName())) {
                                String value = child3.getText();
                                if (StringUtils.isNumeric(value)) {
                                    BigDecimal numericValue = new BigDecimal(value);
                                    if (numericValue != null && numericValue.compareTo(BigDecimal.ZERO) > 0) {
                                        containsScore = true;
                                    }
                                } else if (!StringUtils.isEmpty(value)) {
                                    containsScore = true;
                                }
                            }
                        }
                    }
                }
                if (containsScore) {
                    containsSubtest = true;
                } else {
                    invalidSubtests.add(child);
                }
            }
        }
        if (containsSubtest && !invalidSubtests.isEmpty()) {
            for (Element item : invalidSubtests) {
                assessment.removeContent(item);
            }
        }
        return containsSubtest;

    }

    /**
     * @param statuses
     * @return
     */
    private Collection codesFromRcdOids(String statusesString) {
        List<String> list = new LinkedList();
        list.add(CONST_NO_MATCH);
        for (String oid : Arrays.asList(statusesString.split("\\s*,\\s*"))) {
            ReferenceCode rcd = getBroker().getBeanByOid(ReferenceCode.class, oid);
            if (rcd != null) {
                list.add(rcd.getCode());
            }
        }
        return list;
    }

    /**
     * Creates the element.
     *
     * @param name String
     * @param value String
     * @return Element
     */
    private Element createElement(String name, String value) {
        Element element = new Element(name);
        element.addContent(StringUtils.unNullify(value));
        return element;
    }

    /**
     * Create Student Academic Record Data Element.
     *
     * @param student SisStudent
     * @return Element
     */
    private Element createStdAcademicRecord(SisStudent student) {
        Element stdAcadRec = new Element(ELEM_STD_ACA);

        String stdGradeLvl = GRADE_LVL_MAP.get(student.getGradeLevel());
        if (!StringUtils.isEmpty(stdGradeLvl)) {
            stdAcadRec.addContent(new Element(ELEM_STD_ACA_SLV)
                    .addContent(createElement(ELEM_STD_ACA_SLV_ELEM_CODE, stdGradeLvl)));
        }

        stdAcadRec.addContent(createElement(ELEM_STD_ACA_ELEM_YOG, Integer.toString(student.getYog())));

        String stdGradDate = (String) student.getFieldValueByAlias(ALIAS_STD_GRADUATION_DATE);
        if (stdGradDate != null) {
            ReferenceCode rcdDiplomaType = lookupRefCodeByAlias(student, ALIAS_STD_DIPLOMA_TYPE);
            String stdAcadAwardElemLvl = rcdDiplomaType == null ? "" : rcdDiplomaType.getFederalCode();
            String stdAcadAwardElemTitl = rcdDiplomaType == null ? "" : rcdDiplomaType.getDescription();

            stdAcadRec.addContent(new ExtendedElement(ELEM_STD_ACA_AWD)
                    .addContentConditional(ELEM_STD_ACA_AWD_ELEM_LVL, stdAcadAwardElemLvl, false)
                    .addContentConditional(ELEM_STD_ACA_AWD_ELEM_DATE, stdGradDate, true)
                    .addContentConditional(ELEM_STD_ACA_AWD_ELEM_TITL, stdAcadAwardElemTitl, true)
                    .addContentConditional(ELEM_STD_ACA_AWD_ELEM_COMPDATE, stdGradDate, false));
        }

        GradePointAverageDefinition gpdUnweighted =
                getGradePointAverageDefinition((String) getParameter(PARM_UNWEIGHTED_GPA));
        if (gpdUnweighted != null) {
            stdAcadRec.addContent(new ExtendedElement(ELEM_STD_ACA_SUM)
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_TYP, ACAD_SUMMARY_TYPE_NONWEIGHTED, true)
                    .addContentExtended(new ExtendedElement(ELEM_STD_ACA_SUM_GPA)
                            .addContentConditional(ELEM_STD_ACA_SUM_GPA_ELEM_CRDHRS, "", true)
                            .addContentConditional(ELEM_STD_ACA_SUM_GPA_ELEM_GPA,
                                    getGpa(student, ALIAS_STD_UNWEIGHTED_GPA),
                                    false))
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_CLRANK, getRank(student, ALIAS_STD_UNWEIGHTED_RANK),
                            false)
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_CLSIZE, getClassSize(student, gpdUnweighted), false)
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_ENTDATE, getEntryDate(student), false));
        }

        GradePointAverageDefinition gpdWeighted =
                getGradePointAverageDefinition((String) getParameter(PARM_WEIGHTED_GPA));
        if (gpdWeighted != null) {
            stdAcadRec.addContent(new ExtendedElement(ELEM_STD_ACA_SUM)
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_TYP, ACAD_SUMMARY_TYPE_WEIGHTED, true)
                    .addContentExtended(new ExtendedElement(ELEM_STD_ACA_SUM_GPA)
                            .addContentConditional(ELEM_STD_ACA_SUM_GPA_ELEM_CRDHRS, "", true)
                            .addContentConditional(ELEM_STD_ACA_SUM_GPA_ELEM_GPA,
                                    getGpa(student, ALIAS_STD_WEIGHTED_GPA),
                                    false))
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_CLRANK, getRank(student, ALIAS_STD_WEIGHTED_RANK),
                            false)
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_CLSIZE, getClassSize(student, gpdWeighted), false)
                    .addContentConditional(ELEM_STD_ACA_SUM_ELEM_ENTDATE, getEntryDate(student), false));
        }

        BigDecimal creditHoursEarnedTotal = addStdAcademicSessions(student, stdAcadRec);

        for (Element acadSummaryElement : ((List<Element>) stdAcadRec.getChildren(ELEM_STD_ACA_SUM))) {
            Element gpaElement = acadSummaryElement.getChild(ELEM_STD_ACA_SUM_GPA);
            Element creditHoursElement = gpaElement.getChild(ELEM_STD_ACA_SUM_GPA_ELEM_CRDHRS);
            creditHoursElement.addContent(creditHoursEarnedTotal.toString());
        }

        return stdAcadRec;
    }

    /**
     * Create Student Health Data Element.
     *
     * @param student SisStudent
     * @return Element
     */
    private Element createStdHealth(SisStudent student) {
        Element stdHlt = null;
        List<HealthImmunizationDose> stdImmunizationsMap = getImmunizationDoses(student);

        if (stdImmunizationsMap != null && !stdImmunizationsMap.isEmpty()) {
            // Create Element if there are immunizations for the student
            stdHlt = new Element(ELEM_STD_HLT);

            for (HealthImmunizationDose hid : stdImmunizationsMap) {
                String immDate = (hid.getDate() != null) ? getDateFormat(DATE_YYYY_MM_DD).format(hid.getDate()) : "";
                String immCode =
                        StringUtils.substring(hid.getImmunizationSeries().getImmunizationDefinition().getName(), 20);

                stdHlt.addContent(new ExtendedElement(ELEM_STD_HLT_IMM)
                        .addContentConditional(ELEM_STD_HLT_IMM_ELEM_DATE, immDate, false)
                        .addContentConditional(ELEM_STD_HLT_IMM_ELEM_CODE, immCode, true));
            }
        }

        return stdHlt;
    }


    /**
     * Create Student Person Data Element.
     *
     * @param student SisStudent
     * @return Element
     */
    private Element createStdPerson(SisStudent student) {

        SisPerson psn = student.getPerson();
        SisAddress psnAdrBean = psn.getPhysicalAddress();

        Element stdPsn = new Element(ELEM_STD_PSN);
        if (!StringUtils.isEmpty(student.getLocalId())) {
            stdPsn.addContent(createElement(ELEM_STD_PSN_ELEM_ID, student.getLocalId()));
            stdPsn.addContent(new Element(ELEM_STD_PSN_AGN)
                    .addContent(createElement(ELEM_STD_PSN_AGN_ELEM_ID, student.getLocalId()))
                    .addContent(createElement(ELEM_STD_PSN_AGN_ELEM_CODE, AGENCY_CODE)));
        }

        String ssn = psn.getPersonId();
        if (!StringUtils.isEmpty(ssn)) {
            ssn = ssn.replace("-", "");
            stdPsn.addContent(createElement(ELEM_STD_PSN_ELEM_SSN, ssn));
        }

        stdPsn.addContent(new Element(ELEM_STD_PSN_BIR)
                .addContent(createElement(ELEM_STD_PSN_BIR_ELEM_DATE, psn.getDob().toString())));
        ExtendedElement psnName = new ExtendedElement(ELEM_STD_PSN_NM)
                .addContentConditional(ELEM_STD_PSN_NM_ELEM_FNM, psn.getFirstName(), true)
                .addContentConditional(ELEM_STD_PSN_NM_ELEM_MNM, psn.getMiddleName(), false)
                .addContentConditional(ELEM_STD_PSN_NM_ELEM_LNM, psn.getLastName(), true);
        stdPsn.addContent(psnName);

        Element psnCnt = new Element(ELEM_STD_PSN_CNT);
        if (psnAdrBean != null &&
                !((StringUtils.isEmpty(psnAdrBean.getAddressLine01()))
                        || (StringUtils.isEmpty(psnAdrBean.getCity()))
                        || (StringUtils.isEmpty(psnAdrBean.getState()))
                        || (StringUtils.isEmpty(psnAdrBean.getPostalCode())))) {
            ExtendedElement psnCntAdr = new ExtendedElement(ELEM_STD_PSN_CNT_ADR)
                    .addContentConditional(ELEM_STD_PSN_CNT_ADR_ELEM_LINE1, psnAdrBean.getAddressLine01(), true)
                    .addContentConditional(ELEM_STD_PSN_CNT_ADR_ELEM_CITY, psnAdrBean.getCity(), true)
                    .addContentConditional(ELEM_STD_PSN_CNT_ADR_ELEM_STATE, psnAdrBean.getState(), true)
                    .addContentConditional(ELEM_STD_PSN_CNT_ADR_ELEM_ZIP, psnAdrBean.getPostalCode(), true)
                    .addContentConditional(ELEM_STD_PSN_CNT_ADR_ELEM_ATTN,
                            (String) student.getFieldValueByAlias(ALIAS_STD_COUNSELOR), false);
            psnCnt.addContent(psnCntAdr);
        }

        List<String> stdPhoneParts = getPhoneNumberParts(psn.getPhone01());
        if (stdPhoneParts != null
                && (!StringUtils.isEmpty(stdPhoneParts.get(0)) || !StringUtils.isEmpty(stdPhoneParts.get(1)))) {
            ExtendedElement psnCntPhn = new ExtendedElement(ELEM_STD_PSN_CNT_PHN)
                    .addContentConditional(ELEM_STD_PSN_CNT_PHN_ELEM_AREACODE, stdPhoneParts.get(0), false)
                    .addContentConditional(ELEM_STD_PSN_CNT_PHN_ELEM_NUMBER, stdPhoneParts.get(1), false);
            psnCnt.addContent(psnCntPhn);
        }

        stdPsn.addContent(psnCnt);

        stdPsn.addContent(new Element(ELEM_STD_PSN_GEN)
                .addContent(createElement(ELEM_STD_PSN_GEN_ELEM_CODE,
                        lookupRefCodeDecription(psn, SisPerson.COL_GENDER_CODE))));

        ExtendedElement psnRace = new ExtendedElement(ELEM_STD_PSN_RCE)
                .addContentConditional(ELEM_STD_PSN_RCE_ELEM_ETH_CODE,
                        psn.getHispanicLatinoIndicator() ? "Hispanic" : "NonHispanic", true);
        for (Race race : psn.getRaces()) {
            String raceCode = race.getRaceCode();
            String raceCodeOut = RACE_CODE_MAP.get(raceCode);
            psnRace.addContentConditional(ELEM_STD_PSN_RCE_ELEM_RCE_CODE, raceCodeOut, false);
        }
        stdPsn.addContent(psnRace);

        return stdPsn;
    }

    /**
     * Creates the student element.
     *
     * @param student SisStudent
     * @return Element
     * @throws X2BaseException exception
     */
    private Element createStudentElement(SisStudent student) throws X2BaseException {

        Element std = new Element(ELEM_STD);
        std.addContent(createStdPerson(student));
        std.addContent(createStdAcademicRecord(student));
        Element stdHlt = createStdHealth(student);
        if (stdHlt != null) {
            std.addContent(stdHlt);
        }
        addStdTests(student, std);
        return std;
    }

    /**
     * Creates the transmission data element.
     *
     * @return Element
     */
    private Element createTransmissionDataElement() {
        if (m_elementTransmissionData == null) {
            String createTimeStampStr = TIMESTAMP_FORMAT.format(new PlainDate());
            m_elementTransmissionData = new Element(ELEM_TRAN)
                    .addContent(createElement(ELEM_TRAN_ELEM_DOC_ID, createTimeStampStr))
                    .addContent(createElement(ELEM_TRAN_ELEM_CREATE_DT, createTimeStampStr))
                    .addContent(createElement(ELEM_TRAN_ELEM_DOC_TYPE, ELEM_TRAN_ELEM_DOC_TYPE_VAL))
                    .addContent(createElement(ELEM_TRAN_ELEM_TYPE, ELEM_TRAN_ELEM_TYPE_VAL));

            // Construct Source Element
            Element tranSrc = new Element(ELEM_TRAN_SRC);

            Element tranSrcOrg = new Element(ELEM_TRAN_SRC_DST_ORG)
                    .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_ELEM_CEEBACT,
                            (String) getSchool().getFieldValueByAlias(ALIAS_SKL_CEEB_CODE)))
                    .addContent(new Element(ELEM_TRAN_SRC_DST_ORG_LOC)
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_LOC_ELEM_CODE,
                                    (String) getSchool().getFieldValueByAlias(ALIAS_SKL_STATE_ID)))
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_LOC_ELEM_QUAL, STATE_CODE)))
                    .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_ELEM_NAME, getSchool().getName()));


            Address schoolAddress = getSchool().getAddress();
            String phoneAreacode = "";
            String phoneNumber = "";
            List<String> phoneParts = schoolAddress == null ? null : getPhoneNumberParts(schoolAddress.getPhone01());
            if (phoneParts != null) {
                phoneAreacode = phoneParts.get(0);
                phoneNumber = phoneParts.get(1);
            }

            Element tranSrcOrgCnt = new Element(ELEM_TRAN_SRC_DST_ORG_CNT)
                    .addContent(new Element(ELEM_TRAN_SRC_DST_ORG_CNT_ADR)
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_LINE1,
                                    schoolAddress == null ? "" : schoolAddress.getAddressLine01()))
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_CITY,
                                    schoolAddress == null ? "" : schoolAddress.getCity()))
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_STATE,
                                    schoolAddress == null ? "" : schoolAddress.getState()))
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_ADR_ELEM_ZIP,
                                    schoolAddress == null ? "" : schoolAddress.getPostalCode())))
                    .addContent(new Element(ELEM_TRAN_SRC_DST_ORG_CNT_PHN)
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_PHN_ELEM_AREACODE, phoneAreacode))
                            .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_PHN_ELEM_NUMBER, phoneNumber)));


            String faxPhoneNumber = schoolAddress == null ? ""
                    : StringUtils.unNullify((String) schoolAddress.getFieldValueByAlias(ALIAS_ADR_FAX_NUMBER))
                            .replaceAll(CONST_REGEX_MATCH_NON_DIGITS, "");
            if (!StringUtils.isEmpty(faxPhoneNumber)) {
                tranSrcOrgCnt.addContent(new Element(ELEM_TRAN_SRC_DST_ORG_CNT_FAXPHN)
                        .addContent(createElement(ELEM_TRAN_SRC_DST_ORG_CNT_FAXPHN_ELEM_NUMBER, faxPhoneNumber)));
            }

            tranSrcOrg.addContent(tranSrcOrgCnt);
            tranSrc.addContent(tranSrcOrg);

            // Construct Destination Element which is copy of Source
            Element tranDst = new Element(ELEM_TRAN_DST).addContent((Element) tranSrcOrg.clone());

            m_elementTransmissionData.addContent(tranSrc);
            m_elementTransmissionData.addContent(tranDst);
        }
        return (Element) m_elementTransmissionData.clone();
    }

    /**
     * Gets the assessment element from the assessment and the XML pattern
     *
     * @param asm StudentAssessment
     * @param source Element
     * @return Element
     * @throws X2BaseException exception
     */
    private Element getAssessment(StudentAssessment asm, Element source) throws X2BaseException {
        Element element = new Element(source.getName());
        Attribute beanpath = source.getAttribute("beanpath");
        if (beanpath != null) {
            Object value = getPropertyAsJavaType(asm.getAssessmentDefinition(), asm, beanpath.getValue());
            if (value instanceof PlainDate) {
                Attribute format = source.getAttribute("format");
                if (format != null) {
                    value = getDateFormat(format.getValue()).format(value);
                } else {
                    value = value.toString();
                }
            } else if (value instanceof BigDecimal) {
                value = value.toString();
            } else if (value instanceof Long) {
                value = value.toString();
            } else if (value instanceof Integer) {
                value = value.toString();
            }
            element.addContent(StringUtils.unNullify((String) value));
        } else {
            Attribute constant = source.getAttribute("constant");
            if (constant != null) {
                element.addContent(constant.getValue());
            }
        }
        Iterator<Element> children = source.getChildren().iterator();
        while (children.hasNext()) {
            Element child = children.next();
            element.addContent(getAssessment(asm, child));
        }
        return element;
    }

    /**
     * Gets the class size.
     *
     * @param student SisStudent
     * @param gpd GradePointAverageDefinition
     * @return String
     */
    private String getClassSize(SisStudent student, GradePointAverageDefinition gpd) {
        String value = null;
        if (gpd != null) {
            if (m_classSizeLookup == null) {
                m_classSizeLookup = new GpaClassSizeLookup(getOrganization(), getBroker());
            }
            Integer classSize = m_classSizeLookup.getClassSize(getSchool().getOid(),
                    gpd.getGpaDefinitionName(), student.getYog(), getGpaContextOid(student.getYog()));
            value = classSize == null ? "" : classSize.toString();
        }
        return value;
    }

    /**
     * For a DataDictionaryField, find a SystemStringConverter appropriate for the field.
     * If no converter is appropriate, return null.
     * Use a map to maintain a cache of converters.
     *
     *
     * @param field DataDictionaryField
     * @return SystemStringConverter
     */
    private SystemStringConverter getConverter(DataDictionaryField field) {
        SystemStringConverter converter = null;
        if (m_converterMap == null) {
            m_converterMap = new HashMap<DataDictionaryField, SystemStringConverter>();
        }
        if (m_converterMap.keySet().contains(field)) {
            converter = m_converterMap.get(field);
        } else {
            if (field.isString()) {
                Converter baseConverter = ConverterFactory.getConverterForClass(
                        field.getEffectiveJavaType(),
                        LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                        field.isString());
                if (baseConverter instanceof SystemStringConverter) {
                    converter = ((SystemStringConverter) baseConverter);
                }
            }
            m_converterMap.put(field, converter);
        }

        return converter;
    }

    /**
     * Returns a local instance of a district data dictionary.
     *
     * @param extendedDictionary ExtendedDictionaryAttributes
     * @return DataDictionary.
     */
    private DataDictionary getDataDictionary(ExtendedDictionaryAttributes extendedDictionary) {
        String key = extendedDictionary == null ? "null" : extendedDictionary.getOid();
        if (!m_dictionaryMap.containsKey(key)) {
            m_dictionaryMap.put(key,
                    DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey()));
        }
        return m_dictionaryMap.get(key);
    }

    /**
     * Gets the entry date.
     *
     * @param student SisStudent
     * @return String
     */
    private String getEntryDate(SisStudent student) {
        return (m_enrollmentsMap.get(student.getOid()) != null &&
                m_enrollmentsMap.get(student.getOid()).getEnrollmentDate() != null)
                        ? getDateFormat(DATE_YYYY_MM_DD)
                                .format(m_enrollmentsMap.get(student.getOid()).getEnrollmentDate())
                        : "";
    }

    /**
     * Gets the gpa.
     *
     * @param student SisStudent
     * @param alias String
     * @return String
     */
    private String getGpa(SisStudent student, String alias) {
        String gpa = "";
        if (!StringUtils.isEmpty(alias)) {
            DataDictionaryField dictionaryField =
                    getDataDictionary(null).findDataDictionaryFieldByAlias(alias);
            if (dictionaryField != null && dictionaryField.getDataTable().getDataClass().equals(student.getClass())) {
                Object value = student.getFieldValueByBeanPath(dictionaryField.getJavaName());
                if (value instanceof String) {
                    // Get a SystemStringConverter for the field and convert the value from a
                    // string
                    // value to java type.
                    SystemStringConverter converter = getConverter(dictionaryField);
                    if (converter != null) {
                        value = converter.parseSystemString((String) value);
                    }
                }
                if (value instanceof String) {
                    gpa = (String) value;
                } else if (value instanceof BigDecimal) {
                    BigDecimal numValue = (BigDecimal) value;
                    gpa = numValue.toString();
                }
            }
        }
        return gpa;
    }

    /**
     * Gets the gpa context oid.
     *
     * @param yog int
     * @return String
     */
    private String getGpaContextOid(int yog) {
        if (m_gpaContextOidsMap == null) {
            m_gpaContextOidsMap = new HashMap<Integer, String>();

            QueryByCriteria query = new QueryByCriteria(GpaHistory.class);
            query.addOrderByDescending(GpaHistory.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                    SisDistrictSchoolYearContext.COL_SCHOOL_YEAR);

            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    GpaHistory gpaHistory = (GpaHistory) iterator.next();
                    Integer yogInteger = Integer.valueOf(gpaHistory.getYog());

                    if (!m_gpaContextOidsMap.containsKey(yogInteger)) {
                        m_gpaContextOidsMap.put(yogInteger, gpaHistory.getDistrictContextOid());
                    }
                }
            }
        }
        return m_gpaContextOidsMap.get(Integer.valueOf(yog));
    }

    /**
     * Gets the grade point average definition.
     *
     * @param oid String
     * @return Grade point average definition
     */
    private GradePointAverageDefinition getGradePointAverageDefinition(String oid) {
        GradePointAverageDefinition value = null;
        if (!StringUtils.isEmpty(oid)) {
            value = (GradePointAverageDefinition) getBroker().getBeanByOid(GradePointAverageDefinition.class, oid);
        }
        return value;
    }

    /**
     * Gets the immunization doses.
     *
     * @param student SisStudent
     * @return List
     */
    private List<HealthImmunizationDose> getImmunizationDoses(SisStudent student) {
        if (m_immunizationDosesMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, getStudentOidSubQuery());
            QueryByCriteria query = new QueryByCriteria(HealthImmunizationDose.class, criteria);
            query.addOrderByAscending(HealthImmunizationDose.COL_STUDENT_OID);
            query.addOrderByAscending(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                    HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + PATH_DELIMITER +
                    HealthImmunizationDefinition.COL_SERIES_ID);
            query.addOrderByAscending(HealthImmunizationDose.COL_DATE);

            String[] columnKeys = {HealthImmunizationDose.COL_STUDENT_OID};
            int[] mapSizes = {5000};

            m_immunizationDosesMap = getBroker().getGroupedCollectionByQuery(query, columnKeys, mapSizes);
        }
        return m_immunizationDosesMap.get(student.getOid());
    }

    /**
     * Gets the property as java type.
     *
     * @param extendedDictionary ExtendedDictionaryAttributes
     * @param bean X2BaseBean
     * @param beanPath String
     * @return Object
     * @throws X2BaseException exception
     */
    private Object getPropertyAsJavaType(ExtendedDictionaryAttributes extendedDictionary,
                                         X2BaseBean bean,
                                         String beanPath)
            throws X2BaseException {
        Object value = WebUtils.getProperty(bean, beanPath);

        if (value instanceof String) {
            // Get a SystemStringConverter for the field and convert the value from a string
            // value to java type.
            ModelProperty prop = new ModelProperty(bean.getClass(), beanPath, getBroker().getPersistenceKey());
            DataDictionaryField field =
                    getDataDictionary(extendedDictionary).findDataDictionaryField(prop.getFieldId());
            SystemStringConverter converter = getConverter(field);
            if (converter != null) {
                value = converter.parseSystemString((String) value);
            }
        }
        return value;
    }

    /**
     * Gets the rank.
     *
     * @param student SisStudent
     * @param alias String
     * @return String
     */
    private String getRank(SisStudent student, String alias) {
        String rank = "";
        if (!StringUtils.isEmpty(alias)) {
            DataDictionaryField dictionaryField =
                    getDataDictionary(null).findDataDictionaryFieldByAlias(alias);
            if (dictionaryField != null && dictionaryField.getDataTable().getDataClass().equals(student.getClass())) {
                Object value = student.getFieldValueByBeanPath(dictionaryField.getJavaName());

                if (value instanceof String) {
                    // Get a SystemStringConverter for the field and convert the value from a string
                    // value to java type.
                    SystemStringConverter converter = getConverter(dictionaryField);
                    if (converter != null) {
                        value = converter.parseSystemString((String) value);
                    }
                }
                if (value instanceof String) {
                    String numericString = ((String) value).replaceAll("[^0-9.]", "");
                    if (!StringUtils.isEmpty(numericString)) {
                        value = Integer.valueOf(numericString);
                    }
                }
                if (value instanceof Number) {
                    Number numValue = (Number) value;
                    if (numValue.intValue() > 0) {
                        rank = Integer.toString(numValue.intValue());
                    }
                }
            }
        }
        return rank;
    }

    /**
     * Gets the sessions map.
     *
     * @param student SisStudent
     * @return Map
     */
    private Map<SessionKey, List<Transcript>> getSessionsMap(SisStudent student) {
        if (m_studentSessionsMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, getStudentOidSubQuery());
            criteria.addNotEqualTo(Transcript.COL_TRANSCRIPT_HIDE_IND, Boolean.TRUE);
            criteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_HIDE_TRANSCRIPT_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_CREDIT, BigDecimal.ZERO);

            m_studentSessionsMap = new HashMap();
            BeanQuery queryTranscripts = new BeanQuery(Transcript.class, criteria);
            queryTranscripts.addOrderByAscending(Transcript.COL_STUDENT_OID);
            queryTranscripts.addOrderByAscending(Transcript.COL_COURSE_DESCRIPTION);
            try (QueryIterator transcriptIterator = getBroker().getIteratorByQuery(queryTranscripts)) {
                while (transcriptIterator.hasNext()) {
                    Transcript transcript = (Transcript) transcriptIterator.next();
                    SessionKey key = new SessionKey(transcript);

                    Map<SessionKey, List<Transcript>> mapSessions =
                            m_studentSessionsMap.get(transcript.getStudent().getOid());
                    if (mapSessions == null) {
                        mapSessions = new TreeMap();
                        m_studentSessionsMap.put(transcript.getStudent().getOid(), mapSessions);
                    }
                    List<Transcript> transcripts = mapSessions.get(key);
                    if (transcripts == null) {
                        transcripts = new LinkedList();
                        mapSessions.put(key, transcripts);
                    }
                    transcripts.add(transcript);
                }
            }
        }
        return m_studentSessionsMap.get(student.getOid());
    }

    /**
     * Gets the student assessments.
     *
     * @param student SisStudent
     * @return List
     */
    private List<StudentAssessment> getStudentAssessments(SisStudent student) {
        if (m_assessmentsMap == null) {
            String testScores = (String) getParameter(PARM_TEST_SCORES);
            if (StringUtils.isEmpty(testScores)) {
                testScores = CONST_DISABLE_BEAN_PATH;
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentAssessment.COL_STUDENT_OID, getStudentOidSubQuery());
            criteria.addIn(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID,
                    StringUtils.convertDelimitedStringToList(testScores, ',', true));

            QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, criteria);
            query.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);
            query.addOrderByAscending(StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER +
                    AssessmentDefinition.COL_NAME);
            query.addOrderByDescending(StudentAssessment.COL_DATE);

            m_assessmentsMap = getBroker().getGroupedCollectionByQuery(query, StudentAssessment.COL_STUDENT_OID, 5000);
        }
        return m_assessmentsMap.get(student.getOid());
    }

    /**
     * Builds criteria based on user input.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentCriteria() {
        if (m_studentCriteria == null) {
            m_studentCriteria = new X2Criteria();
            if (isSchoolContext()) {
                m_studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            }

            int queryBy = ((Integer) getParameter(PARM_QUERY_BY)).intValue();
            switch (queryBy) {
                case 0: // Current Selection
                    m_studentCriteria = getCurrentCriteria();
                    break;
                case 1:
                    String yogString = (String) getParameter(PARM_QUERY_STRING);
                    if (StringUtils.isNumeric(yogString)) {
                        // YOG
                        Integer yog = Integer.valueOf(yogString);
                        m_studentCriteria.addEqualTo(SisStudent.COL_YOG, yog);
                        String statuses = (String) getParameter(PARM_STATUSES);
                        if (!StringUtils.isEmpty(statuses)) {
                            m_studentCriteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, codesFromRcdOids(statuses));
                        }
                    } else {
                        m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, CONST_NO_MATCH);
                    }
                    break;
                default:
                    // No additional criteria (this is the case for "All")
                    break;
            }
        }
        return m_studentCriteria;
    }

    /**
     * Gets the student oid sub query.
     *
     * @return Sub query
     */
    private SubQuery getStudentOidSubQuery() {
        if (m_studentOidSubQuery == null) {
            m_studentOidSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
        }
        return m_studentOidSubQuery;
    }

    /**
     * Load Schedule Term Dates Map.
     */
    private void initScheduleTermDatesMap() {
        m_scheduleTermDatesByTrmMap = new HashMap();
        m_scheduleTermDatesByYearTermMap = new HashMap();
        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ScheduleTermDate tmd = (ScheduleTermDate) iterator.next();
                String trmKey = tmd.getScheduleTermOid();

                Collection<ScheduleTermDate> dates = m_scheduleTermDatesByTrmMap.get(trmKey);
                if (dates == null) {
                    dates = new LinkedList();
                    m_scheduleTermDatesByTrmMap.put(trmKey, dates);
                }
                dates.add(tmd);

                ScheduleTerm trm = tmd.getScheduleTerm();
                Schedule sch = (trm != null) ? trm.getSchedule() : null;
                if (sch != null) {
                    String ctxOid = sch.getDistrictContextOid();
                    if (ctxOid != null) {
                        trmKey = ctxOid + "-" + trm.getCode();
                        dates = m_scheduleTermDatesByYearTermMap.get(trmKey);
                        if (dates == null) {
                            dates = new LinkedList();
                            m_scheduleTermDatesByYearTermMap.put(trmKey, dates);
                        }
                        dates.add(tmd);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Lookup ref code by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return ReferenceCode
     */
    private ReferenceCode lookupRefCodeByAlias(X2BaseBean bean, String alias) {
        ReferenceCode value = null;
        DataDictionaryField dictionaryField = getDataDictionary(null).findDataDictionaryFieldByAlias(alias);
        if (dictionaryField != null && dictionaryField.getDataTable().getDataClass().equals(bean.getClass())
                && dictionaryField.hasReferenceTable()) {
            value = lookupReferenceCode(dictionaryField.getReferenceTableOid(),
                    (String) bean.getFieldValueByAlias(alias));
        }
        return value;
    }

    /**
     * Lookup ref code decription.
     *
     * @param bean X2BaseBean
     * @param beanPath String
     * @return String
     */
    private String lookupRefCodeDecription(X2BaseBean bean, String beanPath) {
        String value = null;
        ModelProperty prop = new ModelProperty(bean.getClass(), beanPath, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = getDataDictionary(null).findDataDictionaryField(prop.getFieldId());
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            ReferenceCode code = lookupReferenceCode(dictionaryField.getReferenceTableOid(),
                    (String) bean.getFieldValueByBeanPath(beanPath));
            value = code.getDescription();
        }

        return value;
    }

    /**
     * Lookup reference code.
     *
     * @param referenceTableOid String
     * @param value String
     * @return ReferenceCode
     */
    private ReferenceCode lookupReferenceCode(String referenceTableOid, String value) {
        Map<String, ReferenceCode> map = null;
        if (!m_refTableMap.containsKey(referenceTableOid)) {
            ReferenceTable refTable =
                    (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                map = refTable.getCodeMap();
            }
            m_refTableMap.put(referenceTableOid, map);
        } else {
            map = m_refTableMap.get(referenceTableOid);
        }
        return map == null ? null : map.get(value);
    }
}
