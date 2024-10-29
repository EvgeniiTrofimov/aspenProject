/*
 *
 */
package com.x2dev.reports.sys.sped.on;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OwnershipStateImpl;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever.ReferenceCodeCriteria;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.presentation.ReferenceFieldFormatter;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.DateConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import net.sf.jasperreports5.engine.JRDataSource;
import net.sf.jasperreports5.engine.JRException;
import net.sf.jasperreports5.engine.JRRewindableDataSource;
import net.sf.jasperreports5.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports5.engine.util.JRLoader;
import net.sf.jasperreports5.engine.util.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class IepFormData.
 */
public class IepFormData extends OnBaseFormReportJavaSource {

    /**
     * The Enum AccommodationType.
     */

    enum AccommodationType {
        ASSESSMENT("Assessment", "assessmentAccommodationCollection", "iep-accomm-assessment-addl"),
        //
        ENVIRONMENTAL("Environmental", "environmentalAccommodationCollection", "iep-accomm-environmental-addl"),
        //
        EQAO("EQAO", "provincialAccommodationCollection", null) {
            @Override
            public DataDictionaryField getDataDictionaryField(DataDictionary dictionary) {
                return dictionary.findDataDictionaryFieldByAlias(ALIAS_IAC_EQAO_NAME);
            }

            @Override
            public int getMinimumRows() {
                return 2;
            }
        },
        //
        INSTRUCTIONAL("Instructional", "instructionalAccommodationCollection", "iep-accomm-instructional-addl");

        private static final String ALIAS_IAC_EQAO_NAME = "all-iac-EqaoAccommodation";

        private String m_category;
        private String m_parameterName;
        private String m_additionalAlias;

        /**
         * Instantiates a new accommodation type.
         *
         * @param category String
         * @param paramenterName String
         */
        private AccommodationType(String category, String paramenterName, String additionalAlias) {
            m_category = category;
            m_parameterName = paramenterName;
            m_additionalAlias = additionalAlias;
        }

        /**
         * Gets the alias for the additional accommodations field..
         *
         * @return String
         */
        public String getAdditionalAlias() {
            return m_additionalAlias;
        }

        /**
         * Gets the category.
         *
         * @return String
         */
        public String getCategory() {
            return m_category;
        }

        /**
         * Gets the data dictionary field.
         *
         * @param dictionary DataDictionary
         * @return Data dictionary field
         */
        public DataDictionaryField getDataDictionaryField(DataDictionary dictionary) {
            return dictionary.findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_NAME);
        }

        /**
         * Gets the indicator parameter.
         *
         * @return String
         */
        public String getIndicatorParameter() {
            return m_parameterName.replace("Collection", "Indicator");
        }

        /**
         * Gets the category.
         *
         * @return String
         */
        public int getMinimumRows() {
            return 4;
        }

        /**
         * Gets the parameter.
         *
         * @return String
         */
        public String getParameter() {
            return m_parameterName;
        }
    }

    private static final long serialVersionUID = 1L;

    private static final String ALIAS_CRS_MINISTRY_CODE = "all-crs-MinistryCourseCode";
    private static final String ALIAS_IEP_EQAO_EXEMPTIONS = "iep-eqao-exemption-statement";
    private static final String ALIAS_IEP_EQUIP_ADDITIONAL = "iep-indiv-equip-addl";
    private static final String ALIAS_IEP_HEALTH_SUPPORT = "iep-health-support";
    private static final String ALIAS_IEP_HEALTH_SUPPORT_OTHER = "iep-health-support-oth";
    private static final String ALIAS_IEP_IPRC_NEEDS = "iep-iprc-std-needs";
    private static final String ALIAS_IEP_IPRC_NEEDS_OTHER = "iep-iprc-std-needs-oth";
    private static final String ALIAS_IEP_IPRC_STRENGTHS = "iep-iprc-std-strengths";
    private static final String ALIAS_IEP_IPRC_STRENGTHS_OTHER = "iep-iprc-std-strengths-oth";
    private static final String ALIAS_IEP_MEDICAL_CONDITIONS = "iep-medical-conditions";
    private static final String ALIAS_IEP_MEDICAL_CONDITIONS_OTHER = "iep-medical-conditions-oth";
    private static final String ALIAS_IEP_OSSLT_RATIONALE = "iep-osslt-rationale";
    private static final String ALIAS_IEP_OTHER_SOURCES = "iep-other-sources";
    private static final String ALIAS_IEP_PARENT_COMMENT = "iep-parent-comment";
    private static final String ALIAS_IEP_PROGRAM_EXEMPTIONS = "iep-program-exemptions";
    private static final String ALIAS_IEP_PLACEMENT_DATE_TYPE = "iep-program-placement-type";
    private static final String ALIAS_IEP_SOURCES_CONSULTED = "iep-sources-consulted";
    private static final String ALIAS_IEP_STUDENT_COMMENT = "iep-student-comment";
    private static final String ALIAS_IEP_IPRC_DECISION = "iep-iprc-decision";
    private static final String ALIAS_IGL_CRS_NO = "igl-course-num";
    private static final String ALIAS_IGL_CRS_DESC = "igl-course-desc";
    private static final String ALIAS_IGL_CRS_GRADE_LEVEL = "igl-course-grade-level";
    private static final String ALIAS_IGL_CRS_MARK = "igl-course-mark";
    private static final String ALIAS_IGL_CRS_PREREQUISITE = "igl-course-prerequisite";
    private static final String ALIAS_IGL_CRS_STAFF = "igl-course-staff";
    private static final String ALIAS_IGL_CRS_TERM_CODE = "igl-course-term";
    private static final String ALIAS_IGL_CRS_DELIVERY = "igl-course-delivery";
    private static final String ALIAS_IGO_TERM = "igo-term-semester";
    private static final String ALIAS_IGO_ASSESSMENT_METHOD = "igo-assessment-methods";
    private static final String ALIAS_IGO_TEACHING_STRATEGY = "igo-teaching-strategies";
    private static final String ALIAS_IGO_TRANSITION_ACTIONS_REQ = "igo-transition-actions-req";
    private static final String ALIAS_IGO_TRANSITION_RESP_PERSONS = "igo-transition-resp-persons";
    private static final String ALIAS_IGO_TRANSITION_TIMELINES = "igo-transition-timelines";
    private static final String ALIAS_IPA_ASSESSMENT_NAME = "ipa-assessment-name";
    private static final String ALIAS_IPA_PARTICIPATION_ACCOMMODATION = "ipa-participation-accomm";
    private static final String ALIAS_IPA_ACCOMMODATION = "ipa-accommodation";
    private static final String ALIAS_IPA_EXEMPTION = "ipa-exemption-statement";
    private static final String ALIAS_IPA_DEFERRAL = "ipa-deferral-rationale";
    private static final String ALIAS_IPL_DATE = "ipl-date";
    private static final String ALIAS_IPL_ASSESSMENT = "ipl-assessment";
    private static final String ALIAS_IPL_DESCRIPTION = "ipl-other-description";
    private static final String ALIAS_IPL_RESULTS_SUMMARY = "ipl-results-summary";
    private static final String ALIAS_ISV_FREQUENCY = "isv-frequency";
    private static final String ALIAS_ISV_FREQUENCY_OTHER = "isv-frequency-other";
    private static final String ALIAS_ITM_IEP_MEMBER = "itm-iep-member-indicator";
    private static final String ALIAS_GSR_END_DATE = "all-gsr-EndDate";
    private static final String ALIAS_JNL_ACTIVITY_DESCRIPTION = "all-jnl-ActivityDescription";
    private static final String ALIAS_JNL_SPED_INDICATOR = "all-jnl-SpecialEdIndicator";
    private static final String ALIAS_JNL_SPED_STAFF = "jnl-initiated-by";
    private static final String ALIAS_UDA_INV_DESCRIPTION = "uda-spedinv-description";
    private static final String ALIAS_UDB_INV_RETURN_DATE = "udb-spedinv-return-date";
    private static final String ALIAS_UDB_INV_HIDE = "udb-spedinv-hide";

    private static final String CONSTANT_AC = "AC";
    private static final String CONSTANT_ALT = "ALT";
    private static final String CONSTANT_MOD = "MOD";
    private static final String CONSTANT_IGL_FOCUS_SECTION = "Section";
    private static final String CONSTANT_IGL_FOCUS_ADDITIONAL = "Additional";

    private static final String FIELD_COLUMN_1 = "column1";
    private static final String FIELD_COLUMN_2 = "column2";
    private static final String FIELD_IGL_BASELINE = "baseline";
    private static final String FIELD_IGL_CRS_AC = "AC";
    private static final String FIELD_IGL_CRS_ALT = "ALT";
    private static final String FIELD_IGL_CRS_DESC = "crsDesc";
    private static final String FIELD_IGL_CRS_GRADE_LEVEL = "crsGradeLevel";
    private static final String FIELD_IGL_CRS_MARK = "crsMark";
    private static final String FIELD_IGL_CRS_MOD = "MOD";
    private static final String FIELD_IGL_CRS_PREREQUISITE = "crsPrerequisite";
    private static final String FIELD_IGL_CRS_TERM_CODE = "crsTermCode";
    private static final String FIELD_IGL_GOAL = "goal";
    private static final String FIELD_IGL_OID = "oid";
    private static final String FIELD_IGO_TERM = "term";
    private static final String FIELD_IGO_ASSESSMENT_METHOD = "assessmentMethod";
    private static final String FIELD_IGO_LEARNING_EXPECTATION = "learningExpectation";
    private static final String FIELD_IGO_TEACHING_STRATEGY = "teachingStrategy";
    private static final String FIELD_IGL_ACTION = "action";
    private static final String FIELD_IGL_PERSON = "person";
    private static final String FIELD_IGL_TIMELINE = "timeline";

    private static final String FIELD_ISV_DATE = "date";
    private static final String FIELD_ISV_FREQUENCY = "frequency";
    private static final String FIELD_ISV_LOCATION = "location";
    private static final String FIELD_ISV_TYPE = "type";
    private static final String FIELD_ISV_PROVIDER = "provider";

    private static final String FIELD_IPL_DATE = "date";
    private static final String FIELD_IPL_INFORMATION_SOURCE = "source";
    private static final String FIELD_IPL_RESULTS_SUMMARY = "summary";

    private static final String FIELD_JNL_DESCRIPTION = "description";
    private static final String FIELD_JNL_FEEDBACK = "feedback";
    private static final String FIELD_JNL_SOURCE = "source";
    private static final String FIELD_JNL_STAFF = "staff";
    private static final String FIELD_JNL_DATE = "date";
    private static final String FIELD_JNL_TYPE = "type";

    private static final String FOCUS_TRANSITION = "Transition";

    private static final String ID_DDX_INVENTORY = "ON-SPED-INVENTORY";

    private static final int MIN_ROWS_ACTIVITY_LOG_DATA = 4;
    private static final int MIN_ROWS_ASSESSMENT_DATA = 1;
    private static final int MIN_ROWS_COMMENT_DATA = 2;
    private static final int MIN_ROWS_COURSE_DATA = 2;
    private static final int MIN_ROWS_DEVELOPMENT_TEAM_DATA = 4;
    private static final int MIN_ROWS_EQAO_DATA = 2;
    private static final int MIN_ROWS_HEALTH_SUPPORT_DATA = 1;
    private static final int MIN_ROWS_HUMAN_RESOURCES_DATA = 4;
    private static final int MIN_ROWS_MEDICAL_CONDITONS_DATA = 1;
    private static final int MIN_ROWS_OSSLT_RATIONALE_DATA = 2;
    private static final int MIN_ROWS_OTHER_SOURCES_DATA = 1;
    private static final int MIN_ROWS_STRENGTHS_DATA = 1;
    private static final int MIN_ROWS_TRANSITION_ACTIONS_DATA = 1;
    private static final int MIN_ROWS_TRANSITION_GOALS_DATA = 1;

    private static final String NODE_ID_IEP_IEP_LIST_DETAIL = "iep.iep.list.detail";
    private static final String NODE_ID_STD_STD_LIST_IEP_DETAIL = "student.std.list.iep.detail";

    private static final List<String> OSSLC_CODES = Arrays.asList("OLC3O", "OLC4O");
    private static final String PARAM_COLLECTION_DATA_COMMENT = "commentCollection";
    private static final String PARAM_COLLECTION_DATA_COMMENT2 = "commentCollection2";
    private static final String PARAM_COLLECTION_DATA_EQAO = "eqaoCollection";
    private static final String PARAM_COLLECTION_DATA_OSSLT_RATIONALE = "ossltRationaleCollection";

    private static final String PARAM_COLLECTION_SOURCES_CONSULTED_DATA = "sourcesConsultedCollection";
    private static final String PARAM_PROVENCIAL_ACCOMMODATION_DATA = "provincialAccommodationCollection";
    private static final String PARAM_EXCEPTIONALITY = "exceptionality";
    private static final String PARAM_GRADUATION_PROGRAMS = "graduationPrograms";
    private static final String PARAM_INDIVIDUAL_EQUIPMENT = "individualEquipment";
    private static final String PARAM_INDIVIDUAL_EQUIPMENT_ADDITIONAL = "individualEquipmentAdditional";
    private static final String PARAM_OSSLC_INDICATOR = "osslcIndicator";
    private static final String PARAM_PROVINCE_ASSESSMENT_YEAR = "provinceAssessmentYear";
    private static final String PARAM_SCHOOL = "school";
    private static final String PARAM_SCHOOL_YEAR = "schoolYear";
    private static final String PARAM_REPORTING_DATE = "reportingDate";
    private static final String PARAM_SCHEDULE_TERM_CODE = "scheduleTermCode";

    private static final String PARAM_SUB_GRID_ACTIVITY_LOG_DATA = "activityLogDataGrid";
    private static final String PARAM_SUB_GRID_ACTIVITY_LOG2_DATA = "activityLogDataGrid2";
    private static final String PARAM_SUB_GRID_ASSESSMENT_DATA = "assessmentDataGrid";
    private static final String PARAM_SUB_GRID_COURSE_DATA = "courseDataGrid";
    private static final String PARAM_SUB_GRID_COURSE_DETAIL_DATA = "courseDetailDataGrid";
    private static final String PARAM_SUB_GRID_DEVELOPMENT_TEAM_DATA = "developmentTeamDataGrid";
    private static final String PARAM_SUB_GRID_HEALTH_SUPPORT_DATA = "healthSupportDataGrid";
    private static final String PARAM_SUB_GRID_HUMAN_RESOURCES_DATA = "humanResourcesDataGrid";
    private static final String PARAM_SUB_GRID_MEDICAL_CONDITION_DATA = "medicalConditionDataGrid";
    private static final String PARAM_SUB_GRID_OTHER_SOURCES_DATA = "otherSourcesDataGrid";
    private static final String PARAM_SUB_GRID_PROVINCE_ASSESSMENT_DATA = "provinceAssessmentDataGrid";
    private static final String PARAM_SUB_GRID_STRENGTHS_DATA = "strengthsDataGrid";
    private static final String PARAM_SUB_GRID_TRANSITION_ACTIONS_DATA = "transitionActionsDataGrid";
    private static final String PARAM_SUB_GRID_TRANSITION_ACTIONS_FORMAT = "transitionActionsDataFormat";
    private static final String PARAM_SUB_GRID_TRANSITION_GOALS = "transitionGoalsData";
    private static final String PARAM_SUB_GRID_TRANSITION_GOALS_DATA = "transitionGoalsDataGrid";
    // paramters duplicated from superclass OnBaseFormReportJavaSource, for copying.
    private static final String PARAM_STUDENT_NAME = "studentName";
    private static final String PARAM_STUDENT_AGE = "studentAge";

    private static final Pattern PATTERN_SPLIT_BY_COMMA = Pattern.compile("\\s*,\\s*");
    private static final Pattern PATTERN_SPLIT_BY_CR = Pattern.compile("\\r\\n");

    private static final String INPUT_PARAMETER_TERM = "scheduleTerm";

    protected static final List<KeyValuePair<String, String>> SUB_REPORT_FORMATS = Arrays.asList(
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB1", "assessmentDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB2", "twoColumnDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB2", "twoColumnDataFormat2"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB3", "courseDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB4", "courseDetailDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB5", "oneColumnDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB6", "transitionActionsDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB7", "humanResourcesDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB2", "twoColumnDataFormat3"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB2", "twoColumnDataFormat4"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB5", "oneColumnDataFormat2"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB8", "activityLogDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB8", "activityLogDataFormat2"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB9", "transitionGoalsDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SU10", "provinceAssessmentDataFormat"));

    private IepData m_currentIep;
    private String m_ownerStaffOid;
    private String m_termOnSis;
    private List<String> m_term1TermCodes;
    private List<String> m_secondaryGrades = Arrays.asList("09", "10", "11", "12");
    private Collection<ReferenceCode> m_ScheduleTermCodes;
    private Map<String, Object> m_fieldParameters;
    private boolean m_initialized;

    // Localization
    private Map<String, String> m_validLocales;
    private MessageResources m_default_message_resource;
    private String m_defaultLocale; // Usually English
    private Locale m_userLocale;
    private String m_userLocaleLanguage;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String PARAM_TWO_PARENT_PAGE = "parentPageExtra";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    /**
     * The Class RewindableReportDataGrid.
     */
    class RewindableJRBeanDataGrid extends JRBeanCollectionDataSource implements JRRewindableDataSource {
        /**
         * @param beanCollection
         */
        public RewindableJRBeanDataGrid(Collection<?> beanCollection) {
            super(beanCollection);
        }

        /**
         * @see net.sf.jasperreports5.engine.JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() {
            super.moveFirst();
        }
    }

    /**
     * The Class RewindableReportDataGrid, make all ReportDataGrid rewindable.
     */
    class RewindableReportDataGrid extends ReportDataGrid implements JRRewindableDataSource {
        /**
         * @see net.sf.jasperreports5.engine.JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() {
            super.beforeTop();
        }
    }

    public class SimpleParamDataSource extends SimpleFormDataSource {
        private static final String PREFIX_PARAM = "parameter.";

        private Map<String, Object> m_prameters = new HashMap<String, Object>();

        /**
         * Constructs a new SimpleFormDataSource.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param multiPageGrid ReportDataGrid
         * @param overflowFormat byte[]
         * @param overflowFormatFields Map<String,Object>
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public SimpleParamDataSource(X2BaseBean formStorage, X2BaseBean formOwner, ReportDataGrid multiPageGrid,
                byte[] overflowFormat, Map<String, Object> overflowFormatFields, DataDictionary dictionary,
                Locale locale) {
            super(formStorage, formOwner, multiPageGrid, overflowFormat, overflowFormatFields, dictionary, locale);
        }

        /**
         * Constructs a new SimpleFormDataSource.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public SimpleParamDataSource(X2BaseBean formStorage, X2BaseBean formOwner, DataDictionary dictionary,
                Locale locale) {
            super(formStorage, formOwner, dictionary, locale);
        }

        /**
         * Populate the parameters map.
         *
         * @param parameters
         */
        public void setFieldParameters(Map<String, Object> parameters) {
            m_prameters = parameters;
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            /*
             * If the field starts with the parameter prefix, retrieve the field value
             * from the parameters map.
             */
            Object fieldValue = null;
            if (fieldName.startsWith(PREFIX_PARAM)) {
                String parameterName = fieldName.substring(PREFIX_PARAM.length());
                fieldValue = m_prameters.get(parameterName);
            } else {
                fieldValue = super.getFieldValue(fieldName);
            }

            return fieldValue;
        }
    }

    /**
     * An override to allow manipulating wrapper class to gather data and return the data source.
     * The super will be null since the report has not been run.
     * This is a bypass to allow us to call gatherData multiple times with different results.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#getDataSource()
     */
    @Override
    public Object getDataSource() {
        Object dataSource = super.getDataSource();
        if (dataSource == null) {
            try {
                dataSource = gatherData();
            } catch (Exception e) {
                // report failed. return null;
            }
        }
        return dataSource;
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        super.gatherData();

        addFieldParameter(PARAM_EXCEPTIONALITY, getExceptionality());
        addFieldParameter(PARAM_GRADUATION_PROGRAMS, getGraduationPrograms());
        addFieldParameter(PARAM_SCHOOL, getImplementationSchool());
        addFieldParameter(PARAM_SCHOOL_YEAR, getSchoolYear());
        getAliasedIndicators();
        
        // Superclass OnBaseFormReportJavaSource sets parameters that we move to
        // FieldParameters.
        addFieldParameter(PARAM_STUDENT_NAME, getParameter(PARAM_STUDENT_NAME));
        addFieldParameter(INPUT_PARAMETER_TERM, getParameter(INPUT_PARAMETER_TERM));

        // feature to hide student signature for students under 16, to be temporarily disabled.
        // use getParameter(PARAM_STUDENT_AGE) to enable the feature.
        // addFieldParameter(PARAM_STUDENT_AGE, getParameter(PARAM_STUDENT_AGE));
        addFieldParameter(PARAM_STUDENT_AGE, Integer.valueOf(18));

        initScheduleTerms();
        gatherAccommodationsData();
        gatherActivityLog();
        gatherAssessmentData();
        gatherCommentData();
        // gatherDFieldData(ALIAS_IEP_EQAO_EXEMPTIONS, MIN_ROWS_EQAO_DATA,
        // PARAM_COLLECTION_DATA_EQAO);
        // gatherDFieldData(ALIAS_IEP_OSSLT_RATIONALE, MIN_ROWS_OSSLT_RATIONALE_DATA,
        // PARAM_COLLECTION_DATA_OSSLT_RATIONALE);
        gatherMedicalConditions();
        gatherHealthSupport();
        gatherStrengths();
        gatherCourseData();
        gatherProvinceAssessmentData();
        gatherOsslcIndicator();
        gatherTransitionData();
        gatherHumanRecourcesData();
        gatherDevelopmentTeamData();
        gatherSourcesConsultedData();
        gatherEquipment();
        
        addFieldParameter(ALIAS_IEP_PROGRAM_EXEMPTIONS, getStringTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, 
        		getIep(), ALIAS_IEP_PROGRAM_EXEMPTIONS , getDictionary(), m_default_message_resource));

        SimpleParamDataSource source =
                new SimpleParamDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        source.setFieldParameters(m_fieldParameters);
        return source;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_fieldParameters = new HashMap<String, Object>();
        if (getFormInstance() != null) { // Print from checklist or forms
            super.initialize();
        } else if (m_currentIep != null) { // Print from IEP detail
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        } else if (getFormDefinition() != null) { // Print from forms as blank
            super.initialize();
        }
        initSubReports();

        initializeLocalized();
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();

        if (m_userLocale != null) {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_userLocale);
        } else {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
        }

        if (StringUtils.isBlank(m_userLocaleLanguage)) {
            m_userLocaleLanguage = LocalizationCache.getCurrentLocale().getDisplayLanguage();
        }



        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {
                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        loc.getLocale());
                // save the messages for that language
                resources.put(loc.getLocale(), messages);

                // populate the map of valid locales
                m_validLocales.put(loc.getName(), loc.getLocale());
            }
        }

        addParameter(PARAM_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
        addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
        // Additional hooks for enhanced implementations
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        List<String> detailsNodes =
                new ArrayList<String>(Arrays.asList(NODE_ID_IEP_IEP_LIST_DETAIL, NODE_ID_STD_STD_LIST_IEP_DETAIL));
        if (detailsNodes.contains(userData.getCurrentNode().getId())) {
            m_currentIep = userData.getCurrentRecord(IepData.class);
        }

        m_ownerStaffOid = userData.getStaffOid();
        m_userLocale = userData.getLocale();
        m_userLocaleLanguage = userData.getLocale().getDisplayLanguage();
    }

    /**
     * Put field based parameters into a parameters map for the Parameters Data source.
     */
    protected void addFieldParameter(String key, Object value) {
        m_fieldParameters.put(key, value);
    }

    /**
     * Override a public method from a core class to allow the caller to
     * inject a replacement IEP and reset the report for the new IEP.
     *
     * A bit of a hack, to allow a wrapping report to reinitialize this report instance
     * with a different IEP, reinitialize it and run it again. This must happen through a
     * method that already exists in parent core classes.
     * This is the only public settable method from core classes.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#setOrganization(com.follett.fsc.core.k12.beans.Organization)
     */
    @Override
    public void setFormDefinition(FormDefinition formDefinition) {
        if (formDefinition != null) {
            if (formDefinition.getOid() != null) {
                super.setFormDefinition(formDefinition);
            } else {
                // The IEP OID is stored in the category field.
                // Retrieve that and set it as the new storage IepData,
                // reinitialize everything for the new IEP.
                String iepOid = formDefinition.getCategory();
                if (iepOid != null && iepOid.startsWith("IEP")) {
                    IepData iepData = getBroker().getBeanByOid(IepData.class, iepOid);
                    if (iepData != null) {
                        m_fieldParameters = new HashMap<String, Object>();
                        m_currentIep = iepData;
                        m_iep = iepData;
                        setFormOwner(m_currentIep);
                        setFormStorage(m_currentIep);
                        setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey()));
                        addFormParameters();
                        addFieldParameter(PARAM_SCHOOL_YEAR, getSchoolYear());
                    }
                }
                if (!m_initialized) {
                    initializeLocalized();
                    m_initialized = true;
                }
            }
        }
    }

    /**
     * Ensure minimum rows.
     *
     * @param dataGrid ReportDataGrid
     * @param minRows int
     */
    private void ensureMinimumRows(ReportDataGrid dataGrid, int minRows) {
        if (dataGrid.rowCount() < minRows) {
            dataGrid.bottom();
            for (int i = dataGrid.rowCount(); i < minRows; ++i) {
                dataGrid.append();
            }
        }
    }

    /**
     * Find the Schedule Terms for the selected Schedule Term parameter and student school.
     */
    private void initScheduleTerm() {
        String termOid = (String) getParameter(INPUT_PARAMETER_TERM);
        m_term1TermCodes = new ArrayList<String>();
        ReferenceCode code = null;
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
        ReferenceTable refTable = field.getReferenceTable();
        if (refTable != null) {
            m_ScheduleTermCodes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode rcd : m_ScheduleTermCodes) {
                if (rcd.getOid().equals(termOid)) {
                    code = rcd;
                    m_termOnSis = rcd.getStateCode();
                }
                if ("1".equals(rcd.getStateCode())) {
                    m_term1TermCodes.add(rcd.getCode());
                }
            }
            if (code != null) {
                addFieldParameter(PARAM_SCHEDULE_TERM_CODE, translateCode(field, code.getDescription()));
                if (getIep() != null && getIep().getStudent() != null) {
                    PlainDate latestDate = null;
                    PlainDate worstDate = null;
                    Schedule schedule = getIep().getStudent().getSchool().getActiveSchedule();
                    if (schedule != null) {
                        Collection<ScheduleTerm> terms = schedule.getScheduleTerms(getBroker());
                        for (ScheduleTerm term : terms) {
                            if (code.getCode().equals(term.getCode())) {
                                // for exact match of code, use this term end date.
                                PlainDate reportingDate = null;
                                for (ScheduleTermDate termDate : term.getScheduleTermDates(getBroker())) {
                                    if (reportingDate == null || reportingDate.before(termDate.getEndDate())) {
                                        reportingDate = termDate.getEndDate();
                                    }
                                }
                                latestDate = reportingDate;
                                break;
                            } else {
                                // for non matching codes, find the latest date with the same ref
                                // code state code.
                                for (ReferenceCode rcd : m_ScheduleTermCodes) {
                                    if (rcd.getCode().equals(term.getCode())) {
                                        if (rcd.getStateCode() != null
                                                && rcd.getStateCode().compareTo(m_termOnSis) <= 0) {
                                            for (ScheduleTermDate termDate : term.getScheduleTermDates(getBroker())) {
                                                if (latestDate == null || latestDate.before(termDate.getEndDate())) {
                                                    latestDate = termDate.getEndDate();
                                                }
                                            }
                                        } else {
                                            for (ScheduleTermDate termDate : term.getScheduleTermDates(getBroker())) {
                                                if (worstDate == null || worstDate.after(termDate.getEndDate())) {
                                                    worstDate = termDate.getEndDate();
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    addFieldParameter(PARAM_REPORTING_DATE, latestDate != null ? latestDate : worstDate);
                }
            }
        }
    }

    /**
     * Find the Schedule Terms for the selected Schedule Term parameter and student school.
     */
    private void initScheduleTerms() {
        String termOid = (String) getParameter(INPUT_PARAMETER_TERM);
        m_term1TermCodes = new ArrayList<String>();
        ReferenceCode code = null;
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
        ReferenceTable refTable = field.getReferenceTable();
        if (refTable != null) {
            m_ScheduleTermCodes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode rcd : m_ScheduleTermCodes) {
                if (rcd.getOid().equals(termOid)) {
                    code = rcd;
                    m_termOnSis = rcd.getStateCode();
                }
                if ("1".equals(rcd.getStateCode())) {
                    m_term1TermCodes.add(rcd.getCode());
                }
            }
            if (code != null) {
                addFieldParameter(PARAM_SCHEDULE_TERM_CODE, translateCode(field, code.getCode()));
                if (getIep() != null && getIep().getStudent() != null) {
                    Set<PlainDate> termDates = new TreeSet<PlainDate>();
                    PlainDate matchingTermDate = null;
                    Schedule schedule = getIep().getStudent().getSchool().getActiveSchedule();
                    if (schedule != null) {
                        Collection<ScheduleTerm> terms = schedule.getScheduleTerms(getBroker());
                        for (ScheduleTerm term : terms) {
                            if (code.getCode().equals(term.getCode())) {
                                // for exact match of code, use this term end date.
                                PlainDate reportingDate = null;
                                for (ScheduleTermDate termDate : term.getScheduleTermDates(getBroker())) {
                                    matchingTermDate = termDate.getEndDate();
                                    termDates.add(matchingTermDate);
                                }
                            } else {
                                // for non matching codes, find the latest date with the same ref
                                // code state code.
                                for (ReferenceCode rcd : m_ScheduleTermCodes) {
                                    if (rcd.getCode().equals(term.getCode())) {
                                        if (rcd.getStateCode() != null
                                                && rcd.getStateCode().compareTo(m_termOnSis) <= 0) {
                                            for (ScheduleTermDate termDate : term.getScheduleTermDates(getBroker())) {
                                                termDates.add(termDate.getEndDate());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    DateConverter converter = (DateConverter) ConverterFactory
                            .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), false);
                    StringBuilder termDatesStr = new StringBuilder();
                    for (PlainDate date : termDates) {
                        if (matchingTermDate == null || !matchingTermDate.before(date)) {
                            if (termDatesStr.length() > 0) {
                                termDatesStr.append("  ");
                            }
                            termDatesStr.append(converter.javaToString(date));
                        }
                    }
                    addFieldParameter(PARAM_REPORTING_DATE, termDatesStr.toString());
                }
            }
        }
    }

    /**
     * Inits the sub reports.
     */
    private void initSubReports() {
        for (KeyValuePair<String, String> item : SUB_REPORT_FORMATS) {
            try {
                byte[] compiledFormat = ReportUtils.getReport(item.getKey(), getBroker()).getCompiledFormat();
                Object loadedJRReport = JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
                addParameter(item.getValue(), loadedJRReport);
            } catch (JRException e) {
                String message = "ERROR: Loading subreport for '" + item.getValue() + "' from report " + item.getKey();
                message += "\n" + e.getMessage();
                this.addCustomErrorMessage(message);
            }
        }
    }

    /**
     * Gather accommodations data.
     */
    private void gatherAccommodationsData() {
        Map<String, Set<IepAccommodation>> map = new HashMap();
        for (IepAccommodation iac : getAccommodations()) {
            Set<IepAccommodation> set = map.get(iac.getCategory());
            if (set == null) {
                set = new HashSet<IepAccommodation>();
                map.put(iac.getCategory(), set);
            }
            set.add(iac);
        }

        for (AccommodationType type : AccommodationType.values()) {
            addFieldParameter(type.getIndicatorParameter(), Boolean.TRUE);
            Set<IepAccommodation> set = map.get(type.getCategory());
            List<String> data = new ArrayList<String>();
            if (set != null) {
                data = set.stream().map(temp -> {
                    String value = "";
                    DataDictionaryField ddfd = getDictionary().findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_DESCRIPTION);
                    DataDictionaryField ddfn = getDictionary().findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_NAME);
                    if (ddfd != null) {
                        value = (String) temp.getFieldValueByBeanPath(ddfd.getJavaName());
                        String view = getView(ddfd, value, m_ownerStaffOid, m_default_message_resource);
                        if (!StringUtils.isEmpty(view)) {
                            value = view;
                        }
                    }
                    if (StringUtils.isEmpty(value) && ddfn != null) {
                        value = (String) temp.getFieldValueByBeanPath(ddfn.getJavaName());
                        String view = getView(ddfn, value, m_ownerStaffOid, m_default_message_resource);
                        if (!StringUtils.isEmpty(view)) {
                            value = view;
                        }
                    }

                    return value;
                }).collect(Collectors.toList());
            } else {
                addFieldParameter(type.getIndicatorParameter(), Boolean.FALSE);
            }

            String alias = type.getAdditionalAlias();
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                String additionalAccommodations = (String) getIep().getFieldValueByBeanPath(field.getJavaName());
                if (!StringUtils.isEmpty(additionalAccommodations)) {
                    String[] additionalArray = additionalAccommodations.replace("\r", "").split("\n");
                    for (String accom : additionalArray) {
                        data.add(accom);
                    }
                }
            }

            while (data.contains(null)) {
                data.remove(null);
            }
            data.sort(String.CASE_INSENSITIVE_ORDER);
            while (data.size() < type.getMinimumRows()) {
                data.add("");
            }
            addFieldParameter(type.getParameter(), new RewindableJRBeanDataGrid(data));
        }
    }

    /**
     * Gather activity log.
     */
    private void gatherActivityLog() {
        ReportDataGrid dataGrid = new RewindableReportDataGrid();
        ReportDataGrid dataGrid2 = new RewindableReportDataGrid();
        if (getIep() != null) {
            DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_JNL_SPED_INDICATOR);
            if (ddf == null) {
                throw new IllegalStateException("The alias " + ALIAS_JNL_SPED_INDICATOR + " must be defined");
            }

            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentJournal.COL_STUDENT_OID, getIep().getStudentOid());
            criteria.addEqualTo(ddf.getJavaName(), BooleanAsStringConverter.TRUE);

            QueryByCriteria query = new QueryByCriteria(StudentJournal.class, criteria);
            for (StudentJournal jnl : (Collection<StudentJournal>) getBroker().getCollectionByQuery(query)) {
                String initiatedBy = (String) jnl.getFieldValueByAlias(ALIAS_JNL_SPED_STAFF);
                if (StringUtils.isEmpty(initiatedBy)) {
                    User user = jnl.getUser();
                    if (user != null) {
                        initiatedBy = user.getNameView();
                    }
                }

                dataGrid.append();
                dataGrid2.append();

                String value = jnl.getReasonCode();
                DataDictionaryField jddf = getDictionary().findDataDictionaryField(StudentJournal.class.getName(),
                        StudentJournal.COL_REASON_CODE);
                value = getView(jddf, value, m_ownerStaffOid, m_default_message_resource);
                dataGrid.set(FIELD_JNL_SOURCE, value != null ? value : "");
                dataGrid2.set(FIELD_JNL_SOURCE, value != null ? value : "");

                value = jnl.getType();
                jddf = getDictionary().findDataDictionaryField(StudentJournal.class.getName(), StudentJournal.COL_TYPE);
                value = getView(jddf, value, m_ownerStaffOid, m_default_message_resource);
                dataGrid.set(FIELD_JNL_TYPE, value != null ? value : "");
                dataGrid2.set(FIELD_JNL_TYPE, value != null ? value : "");

                dataGrid.set(FIELD_JNL_DATE, jnl.getDate());
                dataGrid2.set(FIELD_JNL_DATE, jnl.getDate());

                dataGrid.set(FIELD_JNL_DESCRIPTION, jnl.getFieldValueByAlias(ALIAS_JNL_ACTIVITY_DESCRIPTION));
                dataGrid2.set(FIELD_JNL_DESCRIPTION, jnl.getFieldValueByAlias(ALIAS_JNL_ACTIVITY_DESCRIPTION));

                dataGrid.set(FIELD_JNL_STAFF, initiatedBy);
                dataGrid2.set(FIELD_JNL_STAFF, initiatedBy);

                dataGrid.set(FIELD_JNL_FEEDBACK, jnl.getComment());
                dataGrid2.set(FIELD_JNL_FEEDBACK, jnl.getComment());
            }

            ensureMinimumRows(dataGrid, MIN_ROWS_ACTIVITY_LOG_DATA);
            ensureMinimumRows(dataGrid2, MIN_ROWS_ACTIVITY_LOG_DATA);
        }

        dataGrid.beforeTop();
        dataGrid2.beforeTop();
        addFieldParameter(PARAM_SUB_GRID_ACTIVITY_LOG_DATA, dataGrid);
        addFieldParameter(PARAM_SUB_GRID_ACTIVITY_LOG2_DATA, dataGrid2);
    }

    /**
     * Gather assessment data.
     *
     * @throws X2BaseException exception
     */
    private void gatherAssessmentData() throws X2BaseException {
        ReportDataGrid assessmentDataGrid = new RewindableReportDataGrid();

        for (IepPerformanceLevel ipl : getIepPerformanceLevel()) {
            assessmentDataGrid.append();
            String source = (String) ipl.getFieldValueByAlias(ALIAS_IPL_ASSESSMENT, getDictionary());
            if ("Other Assessment".equals(source)) {
                String description = (String) ipl.getFieldValueByAlias(ALIAS_IPL_DESCRIPTION, getDictionary());
                assessmentDataGrid.set(FIELD_IPL_INFORMATION_SOURCE, description);
            } else {
                DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPL_ASSESSMENT);
                source = getView(ddf, source, m_ownerStaffOid, m_default_message_resource);
                assessmentDataGrid.set(FIELD_IPL_INFORMATION_SOURCE, source);
            }
            assessmentDataGrid.set(FIELD_IPL_DATE, getAliasAsJavaType(ipl, ALIAS_IPL_DATE, getDictionary()));
            assessmentDataGrid.set(FIELD_IPL_RESULTS_SUMMARY,
                    ipl.getFieldValueByAlias(ALIAS_IPL_RESULTS_SUMMARY, getDictionary()));
        }

        // Order the data by date and name.
        List<String> fields = new ArrayList<String>(2);
        List<Boolean> orders = new ArrayList<Boolean>(2);
        fields.add(FIELD_IPL_DATE);
        fields.add(FIELD_IPL_INFORMATION_SOURCE);
        orders.add(Boolean.TRUE);
        orders.add(Boolean.TRUE);
        assessmentDataGrid.sort(fields, orders, false);
        
        if (assessmentDataGrid.rowCount() < MIN_ROWS_ASSESSMENT_DATA) {
            for (int i = assessmentDataGrid.rowCount(); i < MIN_ROWS_ASSESSMENT_DATA; ++i) {
                assessmentDataGrid.append();
            }
        }

        assessmentDataGrid.beforeTop();

        addFieldParameter(PARAM_SUB_GRID_ASSESSMENT_DATA, assessmentDataGrid);
    }

    /**
     * Gather comment data.
     */
    private void gatherCommentData() {
        List<String> data = getAliasAsList(ALIAS_IEP_PARENT_COMMENT);
        data.addAll(getAliasAsList(ALIAS_IEP_STUDENT_COMMENT));
        while (data.size() < MIN_ROWS_COMMENT_DATA) {
            data.add("");
        }
        addFieldParameter(PARAM_COLLECTION_DATA_COMMENT, new RewindableJRBeanDataGrid(data));
        addFieldParameter(PARAM_COLLECTION_DATA_COMMENT2, new RewindableJRBeanDataGrid(data));
    }

    /**
     * Gather course data.
     *
     * @throws X2BaseException exception
     */
    private void gatherCourseData() throws X2BaseException {
        ReportDataGrid courseDataGrid = new RewindableReportDataGrid();
        ReportDataGrid courseDetailGrid = new RewindableReportDataGrid();

        // Get teaching strategies reference codes for code-to-description translation.
        DataDictionaryField teachingStrategiesField =
                getDictionary().findDataDictionaryFieldByAlias(ALIAS_IGO_TEACHING_STRATEGY);
        ReferenceTable teachingStrategiesRefTable = teachingStrategiesField.getReferenceTable();
        Collection<ReferenceCode> teachingStrategiesRefCodes = null;
        if (teachingStrategiesRefTable != null) {
            teachingStrategiesRefCodes = teachingStrategiesRefTable.getReferenceCodes(getBroker());
        }

        String grade = getIep().getStudent().getGradeLevel();
        boolean secondary = m_secondaryGrades.contains(grade);

        Collection<IepGoal> goals = getIepGoals();
        DataDictionaryField field1 = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IGL_CRS_TERM_CODE);
        DataDictionaryField field2 = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IGL_CRS_NO);
        if (field1 != null && field2 != null) {
            String sortProps[] = new String[2];
            sortProps[0] = field1.getJavaName();
            sortProps[1] = field2.getJavaName();
            goals = CollectionUtils.sortBeans(goals, sortProps, false, true);
        }

        for (IepGoal igl : goals) {
            // If the user selected Term 1, only display term 1 courses.
            String goalTermCode = (String) igl.getFieldValueByAlias(ALIAS_IGL_CRS_TERM_CODE, getDictionary());
            if ("1".equals(m_termOnSis) && !m_term1TermCodes.contains(goalTermCode)) {
                continue;
            }
            if (CONSTANT_IGL_FOCUS_SECTION.equals(igl.getFocus()) ||
                    CONSTANT_IGL_FOCUS_ADDITIONAL.equals(igl.getFocus())) {
                String term = (String) igl.getFieldValueByAlias(ALIAS_IGL_CRS_TERM_CODE, getDictionary());
                for (ReferenceCode rcd : m_ScheduleTermCodes) {
                    if (rcd.getCode().equals(term)) {
                        term = rcd.getDescription();
                        break;
                    }
                }

                courseDataGrid.append();
                
                String courseDescr =
                        secondary ? (String) igl.getFieldValueByAlias(ALIAS_IGL_CRS_NO, getDictionary()) : "";
                String descr = (String) igl.getFieldValueByAlias(ALIAS_IGL_CRS_DESC, getDictionary());
                if (descr != null) {
                    courseDescr += "  " + descr;
                }
                String staff = (String) igl.getFieldValueByAlias(ALIAS_IGL_CRS_STAFF, getDictionary());
                if (staff != null) {
                    courseDescr += "  " + staff;
                }
                courseDataGrid.set(FIELD_IGL_CRS_DESC, courseDescr);
                courseDataGrid.set(FIELD_IGL_CRS_TERM_CODE, term);
                String crsType = (String) igl.getFieldValueByAlias(ALIAS_IGL_CRS_DELIVERY, getDictionary());
                courseDataGrid.set(FIELD_IGL_CRS_AC, Boolean.valueOf(CONSTANT_AC.equals(crsType)));
                courseDataGrid.set(FIELD_IGL_CRS_ALT, Boolean.valueOf(CONSTANT_ALT.equals(crsType)));
                courseDataGrid.set(FIELD_IGL_CRS_MOD, Boolean.valueOf(CONSTANT_MOD.equals(crsType)));

                if (CONSTANT_ALT.equals(crsType) || CONSTANT_MOD.equals(crsType)) {
                    Collection<IepGoalObjective> objectives = igl.getIepGoalObjectives();
                    DataDictionaryField fieldo = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IGO_TERM);
                    objectives = CollectionUtils.sortBeans(objectives, fieldo.getJavaName(), false, true);
                    Iterator<IepGoalObjective> objectiveIterator = objectives.iterator();
                    do {
                        courseDetailGrid.append();
                        courseDetailGrid.set(FIELD_IGL_OID, igl.getOid());
                        courseDetailGrid.set(FIELD_IGL_CRS_ALT, Boolean.valueOf(CONSTANT_ALT.equals(crsType)));
                        courseDetailGrid.set(FIELD_IGL_CRS_MOD, Boolean.valueOf(CONSTANT_MOD.equals(crsType)));
                        courseDetailGrid.set(FIELD_IGL_CRS_DESC, courseDescr);
                        courseDetailGrid.set(FIELD_IGL_CRS_TERM_CODE,
                                igl.getFieldValueByAlias(ALIAS_IGL_CRS_TERM_CODE, getDictionary()));
                        courseDetailGrid.set(FIELD_IGL_CRS_GRADE_LEVEL,
                                igl.getFieldValueByAlias(ALIAS_IGL_CRS_GRADE_LEVEL, getDictionary()));
                        courseDetailGrid.set(FIELD_IGL_CRS_MARK,
                                igl.getFieldValueByAlias(ALIAS_IGL_CRS_MARK, getDictionary()));
                        courseDetailGrid.set(FIELD_IGL_CRS_PREREQUISITE,
                                igl.getFieldValueByAlias(ALIAS_IGL_CRS_PREREQUISITE, getDictionary()));
                        courseDetailGrid.set(FIELD_IGL_BASELINE, igl.getBaseline());
                        courseDetailGrid.set(FIELD_IGL_GOAL, igl.getGoal());
                        if (objectiveIterator.hasNext()) {
                            IepGoalObjective igo = objectiveIterator.next();
                            courseDetailGrid.set(FIELD_IGO_TERM,
                                    igo.getFieldValueByAlias(ALIAS_IGO_TERM, getDictionary()));
                            courseDetailGrid.set(FIELD_IGO_LEARNING_EXPECTATION, igo.getObjective());
                            courseDetailGrid.set(FIELD_IGO_ASSESSMENT_METHOD,
                                    igo.getFieldValueByAlias(ALIAS_IGO_ASSESSMENT_METHOD, getDictionary()));

                            String teachingStrategies =
                                    (String) igo.getFieldValueByAlias(ALIAS_IGO_TEACHING_STRATEGY, getDictionary());
                            if (teachingStrategiesRefCodes != null && !StringUtils.isEmpty(teachingStrategies)) {
                                ArrayList<String> strategies =
                                        StringUtils.convertDelimitedStringToList(teachingStrategies, ",", true);
                                StringBuilder builder = new StringBuilder();
                                for (String strategy : strategies) {
                                    boolean found = false;
                                    for (ReferenceCode code : teachingStrategiesRefCodes) {
                                        if (strategy.equals(code.getCode())) {
                                            if (builder.length() > 0) {
                                                builder.append(", ");
                                            }
                                            builder.append(code.getDescription());
                                            found = true;
                                            break;
                                        }
                                    }
                                    if (!found) {
                                        if (builder.length() > 0) {
                                            builder.append(", ");
                                        }
                                        builder.append(strategy);
                                    }
                                }
                                teachingStrategies = builder.toString();
                            }
                            courseDetailGrid.set(FIELD_IGO_TEACHING_STRATEGY, teachingStrategies);
                        }
                    } while (objectiveIterator.hasNext());
                }
            }
        }

        if (courseDataGrid.rowCount() < MIN_ROWS_COURSE_DATA) {
            for (int i = courseDataGrid.rowCount(); i < MIN_ROWS_COURSE_DATA; ++i) {
                courseDataGrid.append();
            }
        }

        courseDataGrid.beforeTop();
        courseDetailGrid.beforeTop();

        addFieldParameter(PARAM_SUB_GRID_COURSE_DATA, courseDataGrid);
        addFieldParameter(PARAM_SUB_GRID_COURSE_DETAIL_DATA, courseDetailGrid);
    }

    /**
     * Gather development team data.
     */
    private void gatherDevelopmentTeamData() {
        ReportDataGrid dataGrid = new RewindableReportDataGrid();
        DataDictionaryField iepMemberField =
                getDictionary().findDataDictionaryFieldByAlias(ALIAS_IGO_TEACHING_STRATEGY);

        for (IepTeamMember itm : getTeamMembers()) {
            if (BooleanAsStringConverter.TRUE.equals(itm.getFieldValueByAlias(ALIAS_ITM_IEP_MEMBER, getDictionary()))) {
                dataGrid.append();
                dataGrid.set(FIELD_COLUMN_1, itm.getNameView());
    
                String value = itm.getMemberRoleCode();
                DataDictionaryField ddf = getDictionary().findDataDictionaryField(IepTeamMember.class.getName(),
                        IepTeamMember.COL_MEMBER_ROLE_CODE);
                value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
                dataGrid.set(FIELD_COLUMN_2, value);
            }
        }

        ensureMinimumRows(dataGrid, MIN_ROWS_DEVELOPMENT_TEAM_DATA);
        dataGrid.beforeTop();
        addFieldParameter(PARAM_SUB_GRID_DEVELOPMENT_TEAM_DATA, dataGrid);

        List<String> otherSourcesList =
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), ALIAS_IEP_OTHER_SOURCES, getDictionary(), m_default_message_resource);
        populateOneColumnGrid(PARAM_SUB_GRID_OTHER_SOURCES_DATA,
                otherSourcesList,
                MIN_ROWS_OTHER_SOURCES_DATA);
    }

    /**
     * Gather D field data.
     *
     * @param alias String
     * @param minRows int
     * @param parameterName String
     */
    private void gatherDFieldData(String alias, int minRows, String parameterName) {
        DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(alias);
        List<String> data =
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), alias, getDictionary(), m_default_message_resource)
                        .stream().map(temp -> {
                            String value = "";
                            if (ddf != null) {
                                value = temp;
                                String view = getView(ddf, temp, m_ownerStaffOid, m_default_message_resource);
                                if (!StringUtils.isEmpty(view)) {
                                    value = view;
                                }
                            }
                            return value;
                        }).collect(Collectors.toList());

        while (data.size() < minRows) {
            data.add("");
        }
        addFieldParameter(parameterName, new RewindableJRBeanDataGrid(data));
    }

    /**
     * Gather individualized equipment data.
     */
    private void gatherEquipment() {
        List<String> equip = new ArrayList<String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ID_DDX_INVENTORY);
        ExtendedDataDictionary ddx =
                getBroker().getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));
        if (ddx != null) {
            DataDictionary invDictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
            DataDictionaryField returnDateField =
                    invDictionary.findDataDictionaryFieldByAlias(ALIAS_UDB_INV_RETURN_DATE);
            DataDictionaryField hideField = invDictionary.findDataDictionaryFieldByAlias(ALIAS_UDB_INV_HIDE);
            DataDictionaryField descriptionField =
                    invDictionary.findDataDictionaryFieldByAlias(ALIAS_UDA_INV_DESCRIPTION);

            Collection<UserDefinedTableB> recordBs = getInventory(ddx);
            for (UserDefinedTableB b : recordBs) {
                String returnDate = (String) b.getFieldValueByBeanPath(returnDateField.getJavaName());
                String hide = (String) b.getFieldValueByBeanPath(hideField.getJavaName());
                if (StringUtils.isEmpty(returnDate) && !BooleanAsStringConverter.TRUE.equals(hide)) {
                    UserDefinedTableC tableC = b.getUserDefinedTableC();
                    if (tableC != null) {
                        UserDefinedTableA tableA = tableC.getUserDefinedTableA();
                        if (tableA != null) {
                            equip.add((String) tableA.getFieldValueByBeanPath(descriptionField.getJavaName()));
                        }
                    }
                }
            }
        }
        equip.sort(String.CASE_INSENSITIVE_ORDER);
        StringUtils.convertCollectionToDelimitedString(equip, ", ");
        addFieldParameter(PARAM_INDIVIDUAL_EQUIPMENT, StringUtils.convertCollectionToDelimitedString(equip, ", "));

        equip = new ArrayList<String>();
        equip.addAll(getAliasAsList(ALIAS_IEP_EQUIP_ADDITIONAL));
        equip.sort(String.CASE_INSENSITIVE_ORDER);
        addFieldParameter(PARAM_INDIVIDUAL_EQUIPMENT_ADDITIONAL,
                StringUtils.convertCollectionToDelimitedString(equip, ", "));

    }

    /**
     * Gather human recources data.
     */
    private void gatherHumanRecourcesData() {
        ReportDataGrid dataGrid = new RewindableReportDataGrid();

        // actions
        for (IepService isv : getIepServices()) {
            dataGrid.append();

            String value = isv.getServiceType();
            DataDictionaryField ddf =
                    getDictionary().findDataDictionaryField(IepService.class.getName(), IepService.COL_SERVICE_TYPE);
            value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
            dataGrid.set(FIELD_ISV_TYPE, value != null ? value : "");

            value = isv.getProviderCode();
            ddf = getDictionary().findDataDictionaryField(IepService.class.getName(), IepService.COL_PROVIDER_CODE);
            value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
            dataGrid.set(FIELD_ISV_PROVIDER, value != null ? value : "");

            dataGrid.set(FIELD_ISV_DATE, isv.getStartDate());

            value = (String) isv.getFieldValueByAlias(ALIAS_ISV_FREQUENCY, getDictionary());
            if ("Other".equals(value)) {
                String otherValue = (String) isv.getFieldValueByAlias(ALIAS_ISV_FREQUENCY_OTHER, getDictionary());
                if (!StringUtils.isEmpty(otherValue)) {
                    value = otherValue;
                } else {
                    ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ISV_FREQUENCY);
                    value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
                }
            } else {
                ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ISV_FREQUENCY);
                value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
            }
            dataGrid.set(FIELD_ISV_FREQUENCY, value != null ? value : "");

            value = isv.getSettingCode();
            ddf = getDictionary().findDataDictionaryField(IepService.class.getName(), IepService.COL_SETTING_CODE);
            value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
            dataGrid.set(FIELD_ISV_LOCATION, value != null ? value : "");
        }

        ensureMinimumRows(dataGrid, MIN_ROWS_HUMAN_RESOURCES_DATA);

        dataGrid.beforeTop();
        addFieldParameter(PARAM_SUB_GRID_HUMAN_RESOURCES_DATA, dataGrid);
    }

    /**
     * Gather medical conditions.
     */
    private void gatherMedicalConditions() {
        ReportDataGrid dataGrid = new RewindableReportDataGrid();

        List<String> conditions = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), 
        		ALIAS_IEP_MEDICAL_CONDITIONS, getDictionary(), m_default_message_resource);
        conditions.addAll(getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), 
        		ALIAS_IEP_MEDICAL_CONDITIONS_OTHER, getDictionary(), m_default_message_resource));
        Collections.sort(conditions);
        populateGrid(dataGrid, conditions, MIN_ROWS_MEDICAL_CONDITONS_DATA);
        dataGrid.beforeTop();

        addFieldParameter(PARAM_SUB_GRID_MEDICAL_CONDITION_DATA, dataGrid);
    }

    /**
     * Gather health support.
     */
    private void gatherHealthSupport() {
        ReportDataGrid dataGrid = new RewindableReportDataGrid();

        List<String> support = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), 
        		ALIAS_IEP_HEALTH_SUPPORT, getDictionary(), m_default_message_resource);
        support.addAll(getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), 
        		ALIAS_IEP_HEALTH_SUPPORT_OTHER, getDictionary(), m_default_message_resource));
        Collections.sort(support);
        populateGrid(dataGrid, support, MIN_ROWS_HEALTH_SUPPORT_DATA);
        dataGrid.beforeTop();

        addFieldParameter(PARAM_SUB_GRID_HEALTH_SUPPORT_DATA, dataGrid);
    }

    /**
     * Gather osslc indicator.
     */
    private void gatherOsslcIndicator() {
        Boolean value = Boolean.FALSE;
        if (getIep() != null) {
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_MINISTRY_CODE);
            if (field != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, getIep().getStudentOid());
                criteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(), OSSLC_CODES);

                // From active Schedule for the selected year.
                criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        getCurrentContext().getOid());
                StudentSchedule ssc = getBroker().getBeanByQuery(new QueryByCriteria(StudentSchedule.class, criteria));
                if (ssc != null) {
                    value = Boolean.TRUE;
                }
            }
        }
        addFieldParameter(PARAM_OSSLC_INDICATOR, value);
    }

    /**
     * Gather province assessment data.
     *
     * @throws X2BaseException exception
     */
    private void gatherProvinceAssessmentData() throws X2BaseException {
        List<IepPlacement> placements = new ArrayList<IepPlacement>(getIep().getPlacements(getBroker()));
        List<Map<String, Object>> collection = new ArrayList<Map<String, Object>>();
        Iterator<IepPlacement> itr = placements.iterator();
        while (itr.hasNext()) {
            IepPlacement placement = itr.next();
            if (placement.getStatusCode() == 1) {
                Map<String, Object> map = new HashMap<String, Object>();

                String value = (String) placement.getFieldValueByAlias(ALIAS_IPA_ASSESSMENT_NAME, getDictionary());
                DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPA_ASSESSMENT_NAME);
                value = getView(ddf, value, m_ownerStaffOid, m_default_message_resource);
                map.put(ALIAS_IPA_ASSESSMENT_NAME, value);

                value = (String) placement.getFieldValueByAlias(ALIAS_IPA_PARTICIPATION_ACCOMMODATION, getDictionary());
                map.put(ALIAS_IPA_PARTICIPATION_ACCOMMODATION, value);

                List<String> accommodations = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, placement,
                        ALIAS_IPA_ACCOMMODATION, getDictionary(), m_default_message_resource);
                map.put(PARAM_PROVENCIAL_ACCOMMODATION_DATA, new RewindableJRBeanDataGrid(accommodations));

                map.put(ALIAS_IPA_EXEMPTION, placement.getFieldValueByAlias(ALIAS_IPA_EXEMPTION, getDictionary()));
                map.put(ALIAS_IPA_DEFERRAL, placement.getFieldValueByAlias(ALIAS_IPA_DEFERRAL, getDictionary()));
                collection.add(map);
            }
        }
        JRBeanCollectionDataSource dataSource = new RewindableJRBeanDataGrid(collection);
        addFieldParameter(PARAM_SUB_GRID_PROVINCE_ASSESSMENT_DATA, dataSource);
        addFieldParameter(PARAM_PROVINCE_ASSESSMENT_YEAR, Boolean.valueOf(collection.size() > 0));
    }

    /**
     * Gather sources consulted data.
     *
     * @throws X2BaseException exception
     */
    private void gatherSourcesConsultedData() throws X2BaseException {
        List<Pair<String, Boolean>> collection = new ArrayList<>();
        DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_SOURCES_CONSULTED);
        if (ddf != null && ddf.hasReferenceTable()) {
            List<String> selected = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), 
            		ALIAS_IEP_SOURCES_CONSULTED, getDictionary(), m_default_message_resource);
            DataDictionaryField rcdCodeField = getDictionary()
                    .findDataDictionaryField(ReferenceCode.class.getCanonicalName(), ReferenceCode.COL_CODE);
            MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());
            ReferenceTable rtbl = ddf.getReferenceTable();
            Map<String, ReferenceCode> refCodes = rtbl.getCodeMap();

            for (ReferenceCode refCode : refCodes.values()) {
                String code = refCode.getCode();
                String resourceKey = LocalizationUtils.generateKey(refCode.getOid(), rcdCodeField.getId());
                String codeTranslation = resources.getMessage(getLocale(), resourceKey);
                Boolean includes = Boolean.valueOf(selected.contains(code) || selected.contains(codeTranslation));
                String label = code;
                if (codeTranslation != null && !codeTranslation.startsWith("???")) {
                    label = codeTranslation;
                }
                Pair<String, Boolean> item = new Pair<>(label, includes);
                collection.add(item);
            }
        }
        JRBeanCollectionDataSource dataSource = new RewindableJRBeanDataGrid(collection);
        addFieldParameter(PARAM_COLLECTION_SOURCES_CONSULTED_DATA, dataSource);
    }

    /**
     * Gather strengths.
     */
    private void gatherStrengths() {
        List<String> strengths = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), 
        		ALIAS_IEP_IPRC_STRENGTHS, getDictionary(), m_default_message_resource);
        strengths.addAll(getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), 
        		ALIAS_IEP_IPRC_STRENGTHS_OTHER, getDictionary(), m_default_message_resource));

        Collections.sort(strengths);
        List<String> needs = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), 
        		ALIAS_IEP_IPRC_NEEDS, getDictionary(), m_default_message_resource);
        needs.addAll(getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), 
        		ALIAS_IEP_IPRC_NEEDS_OTHER, getDictionary(), m_default_message_resource));
        Collections.sort(needs);
        populateTwoColumnGrid(PARAM_SUB_GRID_STRENGTHS_DATA, strengths, needs, MIN_ROWS_STRENGTHS_DATA);
    }

    /**
     * Gather transition data.
     */
    private void gatherTransitionData() {
        byte[] actionsFormat = ReportUtils.getReport("SYS-SPED-ON-IEP-SUB6", getBroker()).getCompiledFormat();

        DataDictionaryField actionField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IGO_TRANSITION_ACTIONS_REQ);
        
        // actions
        ReportDataGrid goalsDataGrid = new RewindableReportDataGrid();
        ReportDataGrid transitionActionsDataGrid = null;
        int rows = 0;
        for (IepGoal igl : getIepGoals()) {
            if (FOCUS_TRANSITION.equals(igl.getFocus())) {
                goalsDataGrid.append();
                goalsDataGrid.set(FIELD_IGL_GOAL, igl.getGoal());
                transitionActionsDataGrid = new RewindableReportDataGrid();
                Collection<IepGoalObjective> objectives = igl.getIepGoalObjectives();
                objectives = CollectionUtils.sortBeans(objectives, actionField.getJavaName(), false, true);
                for (IepGoalObjective igo : objectives) {
                    transitionActionsDataGrid.append();
                    transitionActionsDataGrid.set(FIELD_IGL_ACTION,
                            igo.getFieldValueByAlias(ALIAS_IGO_TRANSITION_ACTIONS_REQ, getDictionary()));
                    transitionActionsDataGrid.set(FIELD_IGL_PERSON,
                            igo.getFieldValueByAlias(ALIAS_IGO_TRANSITION_RESP_PERSONS, getDictionary()));
                    transitionActionsDataGrid.set(FIELD_IGL_TIMELINE,
                            igo.getFieldValueByAlias(ALIAS_IGO_TRANSITION_TIMELINES, getDictionary()));
                }
                ensureMinimumRows(transitionActionsDataGrid, MIN_ROWS_TRANSITION_ACTIONS_DATA);
                transitionActionsDataGrid.beforeTop();
                goalsDataGrid.set(PARAM_SUB_GRID_TRANSITION_ACTIONS_DATA, transitionActionsDataGrid);
                goalsDataGrid.set(PARAM_SUB_GRID_TRANSITION_ACTIONS_FORMAT, new ByteArrayInputStream(actionsFormat));
                rows++;
            }
        }
        // goals
        // ensureMinimumRows(goalsDataGrid, MIN_ROWS_TRANSITION_GOALS_DATA);
        goalsDataGrid.beforeTop();
        addFieldParameter(PARAM_SUB_GRID_TRANSITION_GOALS_DATA, goalsDataGrid);
        addFieldParameter(PARAM_SUB_GRID_TRANSITION_GOALS, Integer.valueOf(rows));
    }

    /**
     * Gets the alias as java type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return Object
     * @throws X2BaseException exception
     */
    private Object getAliasAsJavaType(X2BaseBean bean, String alias, DataDictionary dictionary) throws X2BaseException {
        Object value = null;
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field == null) {
            throw new X2RuntimeException(
                    new IllegalStateException("The alias [" + alias + "] could not be found."));
        }
        value = WebUtils.getProperty(bean, field.getJavaName());

        if (value instanceof String) {
            // Get a SystemStringConverter for the field and convert the value from a string
            // value to java type.
            SystemStringConverter converter = getStringConverter(field);
            if (converter != null) {
                value = converter.parseSystemString((String) value);
            }

        }
        return value;
    }

    /**
     * Gets the alias as list.
     *
     * @param alias String
     * @return List
     */
    private List<String> getAliasAsList(String alias) {
        List<String> data = new ArrayList<>();
        List<String> values = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), alias, getDictionary(), m_default_message_resource);
        for (String value : values) {
            if (!StringUtils.isEmpty(value)) {
                data.add(value);
            }
        }
        return data;
    }

    /**
     * Gets the alias as list.
     *
     * @param alias String
     * @return List
     */
    private String getAliasedIndicators() {
    	String value = null;
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_PLACEMENT_DATE_TYPE);
        IepData iep = getIep();
        if (field != null && iep != null) {
        	String fieldValue = (String) iep.getFieldValueByBeanPath(field.getJavaName());
        	ReferenceTable refTbl = field.getReferenceTable();
        	if (refTbl != null && fieldValue != null) {
        		Collection<ReferenceCode> codes = refTbl.getReferenceCodes(getBroker());
        		for (ReferenceCode code : codes) {
        			if (fieldValue.equals(code.getCode())) {
        				value = code.getStateCode();
        				break;
        			}
        		}
        	}
        }
        addFieldParameter(ALIAS_IEP_PLACEMENT_DATE_TYPE, value);

    	value = null;
        field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_IPRC_DECISION);
        if (field != null && iep != null) {
        	String fieldValue = (String) iep.getFieldValueByBeanPath(field.getJavaName());
        	ReferenceTable refTbl = field.getReferenceTable();
        	if (refTbl != null && fieldValue != null) {
        		Collection<ReferenceCode> codes = refTbl.getReferenceCodes(getBroker());
        		for (ReferenceCode code : codes) {
        			if (fieldValue.equals(code.getCode())) {
        				value = code.getStateCode();
        				break;
        			}
        		}
        	}
        }
        addFieldParameter(ALIAS_IEP_IPRC_DECISION, value);

        
        return value;
    }

    /**
     * Gets the exceptionality.
     *
     * @return String
     */
    private String getExceptionality() {
        Collection<IepDisability> disabilities = getIepDisability();
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                        IepDisability.COL_DISABILITY_CODE);
        Map<String, ReferenceCode> rcdMap = new HashMap();
        if (field != null && field.hasReferenceTable()) {
            for (ReferenceCode code : field.getReferenceTable().getReferenceCodes()) {
                rcdMap.put(code.getCode(), code);
            }
        }

        if (disabilities == null || disabilities.isEmpty()) {
            if (getLocale().getLanguage().equals("fr")) {
                return "Non identifié";
            }
            return "Non-Identified";
        }
        StringBuilder sb = new StringBuilder();
        for (IepDisability disability : disabilities) {
            if (disability.getPrimaryIndicator()) {
                if (!StringUtils.isEmpty(disability.getDisabilityCode())) {
                    String description = (String) translateCode(field, disability.getDisabilityCode());
                    if ("en_US".equals(getLocale().toString())) {
                        // Core bug, translations will not be found in promary locale and will return the Code
                        // as default rather than the ref table view fields (description).
                        String code = disability.getDisabilityCode();
                        ReferenceCode rcd = rcdMap.get(code);
                        if (rcd != null) {
                            description = rcd.getDescription();
                        }
                    }
                    if (sb.length() > 0) {
                        sb.append(", ");
                    }
                    sb.append(description);
                }
            }
        }
        for (IepDisability disability : disabilities) {
            if (!disability.getPrimaryIndicator()) {
                if (!StringUtils.isEmpty(disability.getDisabilityCode())) {
                    String description = (String) translateCode(field, disability.getDisabilityCode());
                    if ("en_US".equals(getLocale().toString())) {
                        // Core bug, translations will not be found in promary locale and will return the Code
                        // as default rather than the ref table view fields (description).
                        String code = disability.getDisabilityCode();
                        ReferenceCode rcd = rcdMap.get(code);
                        if (rcd != null) {
                            description = rcd.getDescription();
                        }
                    }
                    if (sb.length() > 0) {
                        sb.append(", ");
                    }
                    sb.append(description);
                }
            }
        }
        if (sb.length() == 0) {
            if (getLocale().getLanguage().equals("fr")) {
                return "Non identifi�";
            }
            sb.append("Non-Identified");
        }
        return sb.toString();
    }

    /**
     * Gets the graduation programs.
     *
     * @return Sets the
     */
    private Set<String> getGraduationPrograms() {
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_GSR_END_DATE);
        DataDictionaryField diplomaTypeField = getDictionary().findDataDictionaryField(GraduationProgram.class.getName(), GraduationProgram.COL_DIPLOMA_TYPE);
        ReferenceTable diplomaTypeRtb = diplomaTypeField.getReferenceTable();
        Collection<ReferenceCode> refCodes = diplomaTypeRtb.getReferenceCodes(getBroker());
        DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        PlainDate today = new PlainDate();
        Set<String> gprNames = new HashSet();
        for (GraduationStudentProgram gsr : getProgramStudies()) {
            boolean useit = true;
            if (field != null) {
                String endDateStr = (String) gsr.getFieldValueByBeanPath(field.getJavaName());
                if (!StringUtils.isEmpty(endDateStr)) {
                    PlainDate endDate = (PlainDate) converter.parseSystemString(endDateStr);
                    if (endDate != null && endDate.before(today)) {
                        useit = false;
                    }
                }
            }
            if (useit) {
                String diplomaType = gsr.getProgramStudies().getDiplomaType();
                for (ReferenceCode code : refCodes) {
                    if (code.getCode().contentEquals(diplomaType)) {
                        gprNames.add(code.getStateCode());
                        break;
                    }
                }
            }
        }
        return gprNames;
    }

    /**
     * Gets the string converter.
     *
     * @param field DataDictionaryField
     * @return System string converter
     */
    private SystemStringConverter getStringConverter(DataDictionaryField field) {
        SystemStringConverter stringConverter = null;
        if (field.isString()) {
            Converter converter = ConverterFactory.getConverterForClass(
                    field.getEffectiveJavaType(),
                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                    field.isString());
            if (converter instanceof SystemStringConverter) {
                stringConverter = ((SystemStringConverter) converter);
            }
        }
        return stringConverter;
    }

    /**
     * Gets the term id.
     *
     * @return String
     */
    private String getTermId() {
        GradeTerm gradeTerm = null;
        if (getIep() != null && getIep().getStudent() != null) {
            GradesManager gradesManager = new GradesManager(getBroker());
            gradeTerm = gradesManager.getCurrentGradeTerm(getIep().getStudent().getSchoolOid());
        }
        return gradeTerm == null ? "" : gradeTerm.getGradeTermId();
    }

    /**
     * Populate grid.
     *
     * @param dataGrid ReportDataGrid
     * @param items List<String>
     * @param minRows int
     */
    private void populateGrid(ReportDataGrid dataGrid, List<String> items, int minRows) {
        int numRows = Math.max(minRows, (items.size() + 1) / 2);
        Iterator<String> iterator = items.iterator();
        // Populate column 1
        for (int i = 0; i < numRows; ++i) {
            dataGrid.append();
            if (iterator.hasNext()) {
                dataGrid.set(FIELD_COLUMN_1, iterator.next());
            }
        }
        dataGrid.beforeTop();
        // Populate column 2
        for (int i = 0; i < numRows; ++i) {
            dataGrid.next();
            if (iterator.hasNext()) {
                dataGrid.set(FIELD_COLUMN_2, iterator.next());
            }
        }
    }

    /**
     * Populate one column grid.
     *
     * @param parameterName String
     * @param items List<String>
     * @param minRows int
     */
    private void populateOneColumnGrid(String parameterName, List<String> items, int minRows) {
        Collections.sort(items);
        ReportDataGrid dataGrid = new RewindableReportDataGrid();
        for (String item : items) {
            dataGrid.append();
            dataGrid.set(FIELD_COLUMN_1, item);
        }

        ensureMinimumRows(dataGrid, minRows);

        dataGrid.beforeTop();
        addFieldParameter(parameterName, dataGrid);
    }

    /**
     * Populate two column grid.
     *
     * @param parameterName String
     * @param itemsOne List<String>
     * @param itemsTwo List<String>
     * @param minRows int
     */
    private void populateTwoColumnGrid(String parameterName,
                                       List<String> itemsOne,
                                       List<String> itemsTwo,
                                       int minRows) {
        ReportDataGrid dataGrid = new RewindableReportDataGrid();
        for (String item : itemsOne) {
            dataGrid.append();
            dataGrid.set(FIELD_COLUMN_1, item);
        }
        dataGrid.beforeTop();

        for (String item : itemsTwo) {
            if (dataGrid.isBottom()) {
                dataGrid.append();
            } else {
                dataGrid.next();
            }
            dataGrid.set(FIELD_COLUMN_2, item);
        }

        ensureMinimumRows(dataGrid, minRows);

        dataGrid.beforeTop();
        addFieldParameter(parameterName, dataGrid);
    }

    /**
     * Translate code.
     *
     * @param field DataDictionaryField
     * @param code String
     * @return Object
     */
    private Object translateCode(DataDictionaryField field, String code) {
        String value = code;
        if (field.hasReferenceTable()) {
            String view = getView(field, code, m_ownerStaffOid, m_default_message_resource);
            if (!StringUtils.isEmpty(view)) {
                value = view;
            }
        }
        return value;
    }
}
