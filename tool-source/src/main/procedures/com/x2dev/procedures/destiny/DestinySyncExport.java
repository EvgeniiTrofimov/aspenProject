/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2015 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.destiny;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.AuditXmlManager;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.AuditTypes.TableAuditType;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.GuidManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager.X2LogicalFormat;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.crypto.EncryptionException;
import com.follett.fsc.crypto.EncryptionUtil;
import com.follett.fsc.destiny.datatransfer.DataObjectFactory;
import com.follett.fsc.destiny.datatransfer.data.*;
import com.follett.fsc.destiny.datatransfer.enums.Action;
import com.follett.fsc.destiny.datatransfer.enums.EnrollMembership;
import com.follett.fsc.destiny.datatransfer.enums.EnrollStatus;
import com.follett.fsc.destiny.datatransfer.enums.Gender;
import com.follett.fsc.destiny.datatransfer.enums.ObjectType;
import com.follett.fsc.destiny.datatransfer.enums.Status;
import com.follett.fsc.destiny.datatransfer.impl.DataTransferFactory;
import com.follett.fsc.destiny.datatransfer.interfaces.DataTransferInterface;
import com.follett.fsc.ws.client.WebServiceException;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.lang.reflect.Method;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Handles all of the communication with Destiny including pushing all changes as well
 * as processing the auditing of objects to be sent to Destiny.
 *
 * @author Follett Software Company
 */
public class DestinySyncExport extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String COURSE_PROCEDURE_ID = "SchoolCourseProcedure";
    private static final String SECTION_PROCEDURE_ID = "SchoolSectionProcedure";
    private static final String SECTION_ENROLLMENT_PROCEDURE_ID = "SectionEnrollmentProcedure";
    private static final String STAFF_PROCEDURE_ID = "StaffPatronProcedure";
    private static final String STUDENT_PROCEDURE_ID = "StudentPatronProcedure";

    private static final String AUDIT_ONLY = "auditonly";
    private static final String ENABLE_AUDIT = "enableaudit";
    private static final String SEND_CLASS_INFORMATION = "sendClasses";

    private static final String ORA_DESTINY_DDX = "DESTINY-INTEGRATION";
    private static final String ORA_DESTINY_TIMESTAMPS = "destiny-timestamps";
    private static final String ORA_DESTINY_ACCESSTOKEN = "destiny-access-token";
    private static final String ORA_DESTINY_SPLITCOURSES = "destiny-split-sections";

    private static final String STUDENT_PATRON = "StudentPatron";
    private static final String STAFF_PATRON = "StaffPatron";
    private static final String COURSE = "Course";
    private static final String SECTION = "Section";
    private static final String SECTION_ENROLLMENT = "SectionEnrollment";

    private static final String PERSON_TABLE = "tblPerson";
    private static final String STUDENT_TABLE = "tblStudent";
    private static final String STAFF_TABLE = "tblStaff";
    private static final String SCHOOL_COURSE_TABLE = "tblCrsSchool";
    private static final String MASTER_SCHEDULE_TABLE = "tblSchMaster";
    private static final String STUDENT_SCHEDULE_TABLE = "tblStdSchChg";

    private static final String SCHOOL_PARAMETER = "SCHOOL";
    private static final String BEANOID_PARAMETER = "BEANOID";
    private static final String EXPRESSION_FIELD_PARAMETER = "EXPRESSION_FIELD";

    private String m_accessToken = null;
    private Map<String, List<SisSchool>> m_aspenSchools = null;
    private boolean m_auditOnly = false;
    private X2Broker m_broker = null;
    private String m_context = null;
    private DataTransferInterface m_datatransfer = null;
    private boolean m_enableAudit = true;
    private DataObjectFactory m_factory = null;
    private Collection<StateReportValidationError> m_initErrors = null;
    private Map<String, Long> m_objectTimeStamps = new HashMap<String, Long>();
    private String m_organizationAttributesOid = null;
    private boolean m_register = false;
    private String m_registrationToken = null;
    private StateReportData m_reportData = null;
    private Map<String, Map<String, String>> m_schoolScheduleExpressions = new HashMap<String, Map<String, String>>();
    private Map<String, String> m_schoolSites = new HashMap<String, String>();
    private boolean m_sendClasses = true;
    private KeyPair m_sisKeyPair = null;
    private Map<String, Integer> m_splitSections = new HashMap<String, Integer>();
    private String m_splitSectionsField = null;
    private String m_timestampsField = null;
    private List<String> m_timestampObjects = new ArrayList<String>();
    private boolean m_updatesEnabled = false;
    private String m_scheduleExpressionFieldPath = "scheduleDisplay";
    private List<String> m_auditObjectsProcessed = new ArrayList<String>();

    /*
     * Map of objects to tables which they need to monitor.
     */
    private static Map<String, List<String>> m_objectTables = new LinkedHashMap<String, List<String>>();

    /*
     * Counts for logging purposes.
     */
    private static int m_publishedCount;
    private static int m_processedCount;
    private static int m_unrelatedCount;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (m_register) {
            if (registerWithDestiny(true)) {
                logMessage("Successfully registered with destiny.");
            } else {
                logMessage("Failed to register with destiny");
            }
        } else {
            if (StringUtils.isEmpty(m_accessToken)) {
                logMessage(
                        "Destiny integration is not registered. It must be configured from the District Preferences / Remote Services.");
            } else {
                if (m_auditOnly) {
                    if (m_updatesEnabled) {
                        checkTablesForAudit(m_enableAudit);
                        linkAspenSchoolsToDestinySchools(false);
                        processAudits();
                    } else {
                        logMessage("Destiny updates are not enabled.");
                    }
                } else {
                    // link schools
                    linkAspenSchoolsToDestinySchools(true);

                    // initialize timestamps
                    initializeTopicTimeStamps();

                    // clear split courses list and write back to database
                    m_splitSections.clear();
                    writeSplitSections();

                    // Send all data.
                    sendInitialData();
                }
            }
        }
    }

    /**
     * Initializes the necessary configuration information for syncing the information
     * with Destiny.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        String server = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.server");
        String port = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.port");
        String context = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.context");
        m_updatesEnabled =
                "true".equals(PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.updates"));
        m_registrationToken = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.regtoken");

        DataTransferFactory.configure(server, port, "Aspen", AppGlobals.getVersion(true), "English");
        m_datatransfer =
                DataTransferFactory.getInterface(server, port, "Aspen", AppGlobals.getVersion(true), "English");
        m_factory = new DataObjectFactory(getOrganization().getName());
        m_broker = getBroker();
        m_context = context;

        try {
            KeyPairGenerator keyGenerator = KeyPairGenerator.getInstance("RSA");
            SecureRandom sr = SecureRandom.getInstance("SHA1PRNG", "SUN");
            sr.setSeed(new Date().getTime()); // Choose a better random key for real-world use
            keyGenerator.initialize(1024, sr);
            m_sisKeyPair = keyGenerator.generateKeyPair();
        } catch (NoSuchAlgorithmException noSuchAlgoritmException) {
            throw new X2BaseException(noSuchAlgoritmException.getCause());
        } catch (NoSuchProviderException noSuchProviderException) {
            throw new X2BaseException(noSuchProviderException.getCause());
        }

        m_aspenSchools = getSchools();
        m_sendClasses = ((Boolean) getParameter(SEND_CLASS_INFORMATION)).booleanValue();

        m_auditOnly = ((Boolean) getParameter(AUDIT_ONLY)).booleanValue();
        if (getParameter(ENABLE_AUDIT) != null) {
            m_enableAudit = ((Boolean) getParameter(ENABLE_AUDIT)).booleanValue();
        }

        // Add the objects that we are tracking timestamps for
        m_timestampObjects.add(STUDENT_PATRON);
        m_timestampObjects.add(STAFF_PATRON);
        m_timestampObjects.add(COURSE);
        m_timestampObjects.add(SECTION);
        m_timestampObjects.add(SECTION_ENROLLMENT);

        /*
         * Populate the objects and the related tables.
         */
        if (m_timestampObjects.contains(STUDENT_PATRON)) {
            m_objectTables.put(STUDENT_PATRON, Arrays.asList(STUDENT_TABLE, PERSON_TABLE));
        }
        if (m_timestampObjects.contains(STAFF_PATRON)) {
            m_objectTables.put(STAFF_PATRON, Arrays.asList(STAFF_TABLE, PERSON_TABLE));
        }
        if (m_timestampObjects.contains(COURSE)) {
            m_objectTables.put(COURSE, Arrays.asList(SCHOOL_COURSE_TABLE));
        }
        if (m_timestampObjects.contains(SECTION)) {
            m_objectTables.put(SECTION, Arrays.asList(MASTER_SCHEDULE_TABLE));
        }
        if (m_timestampObjects.contains(SECTION_ENROLLMENT)) {
            m_objectTables.put(SECTION_ENROLLMENT, Arrays.asList(STUDENT_SCHEDULE_TABLE));
        }

        // Load the organization attributes and locate the fields for the access token and
        // timestamps.
        loadOrganizationAttributes();

        if (m_sendClasses) {
            // Load all the schedule expressions to determine if we can eliminate the processing on
            // each record.
            loadScheduleExpressions();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        // Intentionally runs on application server
        runOnApplicationServer();
    }

    /**
     * Checks the necessary tables for whether audits are enabled or not. Optionally
     * it will enable auditing on those tables that are not already enabled.
     *
     * @param enableAudit boolean
     */
    private void checkTablesForAudit(boolean enableAudit) {
        int updatedTables = 0;

        List<String> tables =
                Arrays.asList(PERSON_TABLE, STUDENT_TABLE, STAFF_TABLE, SCHOOL_COURSE_TABLE, MASTER_SCHEDULE_TABLE);

        if ("false".equals(PreferenceManager.getPreferenceValue(getOrganization(), "sys.general.auditEnabled"))) {
            logMessage("Audit is disabled in the general preferences");
        }

        for (String tablename : tables) {
            DataDictionaryTable table = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryTableById(tablename);

            if (!table.getObjectPrefix().equals(DataAudit.OBJECT_PREFIX) &&
                    !table.getObjectPrefix().equals(DataTransaction.OBJECT_PREFIX)) {
                DataTableConfig districtTable = table.getDataTableConfig();

                if (districtTable.getTableAuditType() != TableAuditType.ALL.ordinal() && enableAudit) {
                    districtTable.setTableAuditType(TableAuditType.ALL.ordinal());
                    logMessage("Updated " + tablename + " to turn audit to ALL.");

                    getBroker().saveBeanForced(districtTable);
                    updatedTables++;
                } else if (districtTable.getTableAuditType() != TableAuditType.ALL.ordinal()) {
                    logMessage(tablename + " is not enabled for Auditing on all fields.");
                }
            }
        }

        if (enableAudit) {
            logMessage("Updated " + updatedTables + " tables to turn audit to ALL.");
        }
    }

    /**
     * Find new entry enrollment records since the last upload and send those students as Adds since
     * they will otherwise be ignored for changes.
     *
     * @param lastStudentUpload String
     * @param broker X2Broker
     * @return int
     * @throws X2BaseException exception
     */
    private int findNewEnrollments(String lastStudentUpload, X2Broker broker) throws X2BaseException {
        int newEnrollments = 0;

        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterThan(StudentEnrollment.COL_TIMESTAMP, lastStudentUpload);
        criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);

        // Query and order by timestamp so that the records are processed in the correct order.
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        // Iterate through each record and processing them in case they need to be published.
        QueryIterator iterator = broker.getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentEnrollment studentEnrollment = (StudentEnrollment) iterator.next();

                if (sendStudentPatronForSchool(studentEnrollment.getSchool(), studentEnrollment.getStudentOid(),
                        Action.ADD)) {
                    newEnrollments++;
                    logMessage("Published StudentPatron for new enrollment " + studentEnrollment.getOid() + " for "
                            + studentEnrollment.getStudentOid() + " from " + studentEnrollment.getTimestamp() + ".");
                }
            }
        } finally {
            iterator.close();
        }

        return newEnrollments;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Map<String, List<SisSchool>>
     */
    private Map<String, List<SisSchool>> getSchools() {
        // All active schools (not archive).
        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        primaryCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        /*
         * Limit the schools to the current organization where the tool is run from.
         */
        int level = getOrganization().getOrganizationDefinition().getLevel();
        String column = OrganizationManager.getOrganizationColumnForLevel(level);
        primaryCriteria.addEqualTo(column, getOrganization().getOid());

        QueryByCriteria query = new QueryByCriteria(SisSchool.class, primaryCriteria);

        List<SisSchool> schools = (List<SisSchool>) getBroker().getCollectionByQuery(query);
        Map<String, List<SisSchool>> schoolMap = new HashMap<String, List<SisSchool>>();

        for (SisSchool school : schools) {
            String siteId = PreferenceManager.getPreferenceValue(school, "sys.destiny.sitenumber");
            if (!StringUtils.isEmpty(siteId)) {
                List<SisSchool> schoolList = schoolMap.get(siteId);
                if (schoolList == null) {
                    schoolList = new ArrayList<SisSchool>();
                }

                schoolList.add(school);
                schoolMap.put(siteId, schoolList);
            }
        }

        return schoolMap;
    }

    /**
     * Initializes the object timestamps with the current time if their is not one stored already.
     */
    private void initializeTopicTimeStamps() {
        long executionTime = System.currentTimeMillis();
        boolean update = false;

        /*
         * Make sure we have topics and possible refresh if we don't given
         * this might be run on the non-sif nodes.
         */
        if (m_timestampObjects != null) {
            /*
             * For those topics associated with the deployment that have no time stamp
             * we need to put a value in so that it can be updated after the next run
             * through the audit trail event forwarder.
             */
            for (String topicKey : m_timestampObjects) {
                if (!m_objectTimeStamps.containsKey(topicKey)) {
                    m_objectTimeStamps.put(topicKey, Long.valueOf(executionTime));
                    update = true;
                }
            }

            if (update) {
                writeTimestamps();
            }
        }
    }

    /**
     * The first step after registering is to get a list of schools belonging to
     * the context (district) and linking them with Aspen school refids.
     *
     * @param syncLink boolean
     */
    private void linkAspenSchoolsToDestinySchools(boolean syncLink) {
        OrganizationRecord district = m_datatransfer.getContext(m_context, m_accessToken);

        if (district.getResult().getStatus().equals(DataTransferStatus.SUCCESS)) {
            List<SchoolRecord> schools = district.getSchools();

            for (String siteId : m_aspenSchools.keySet()) {
                for (SisSchool school : m_aspenSchools.get(siteId)) {
                    for (SchoolRecord destinySchool : schools) {
                        if (destinySchool.getSiteId().equals(siteId)) {
                            m_schoolSites.put(school.getOid(), destinySchool.getSiteId());

                            if (syncLink) {
                                // This inserts a row in Destiny's SifRefIds table
                                DataTransferResult result = m_datatransfer.linkRefIdRow(
                                        m_context,
                                        m_accessToken,
                                        ObjectType.SCHOOLINFO, // Set data type
                                        GuidManager.oidToGuid(school.getOid()), // Aspen school GUID
                                        destinySchool.getSiteId(), null, // Primary/secondary key
                                        destinySchool.getSiteId()); // Destiny school identifier
                                                                    // (same as Primary key)

                                if (result.getStatus().equals(DataTransferStatus.ERROR)) {
                                    // Get used to disappointment...
                                    logMessage("Error sending school sync " + school.getName() + " to destiny school "
                                            + destinySchool.getSchoolName());
                                } else {
                                    logMessage("Linked school " + school.getName() + " to Destiny school "
                                            + destinySchool.getSchoolName());
                                }
                            }

                            break;
                        }

                    }
                }
            }
        } else {
            // Do something to respond to error.
            for (String message : district.getResult().getMessages()) {
                logMessage(message);
            }
        }
    }

    /**
     * Loads the organization attributes bean for the Destiny extended dictionary.
     * If it doesn't exist it will be created and we will look up what java field
     * name that the access token and timestamp field are listed under.
     */
    private void loadOrganizationAttributes() {
        /*
         * Retrieve the root organization.
         */
        Organization org = getOrganization();
        if (org != null) {
            OrganizationAttributes orgAttrib = null;

            /*
             * Find the OrganizationAttributes record with reporting information matching
             * the SIF extended data dictionary.
             */
            for (OrganizationAttributes attrs : org.getOrganizationAttributes(m_broker)) {
                if (attrs.getExtendedDataDictionary() != null &&
                        ORA_DESTINY_DDX.equals(attrs.getExtendedDataDictionary().getId().trim())) {
                    /*
                     * Save the bean oid for quick retrieval later.
                     */
                    orgAttrib = attrs;
                    m_organizationAttributesOid = attrs.getOid();
                    break;
                }
            }

            /*
             * Find the extended data dictionary for the OrganizationAttributes "DESTINY-GEN".
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DESTINY_DDX);
            QueryByCriteria query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);

            ExtendedDataDictionary ddx = (ExtendedDataDictionary) m_broker.getBeanByQuery(query);
            if (ddx != null) {
                /*
                 * If there is no Organization Attibutes bean with this ddxOid then we
                 * need to create one and add the oid to the map.
                 */
                if (orgAttrib == null) {
                    orgAttrib = X2BaseBean.newInstance(OrganizationAttributes.class, m_broker.getPersistenceKey());
                    orgAttrib.setExtendedDataDictionaryOid(ddx.getOid());
                    orgAttrib.setOrganizationOid(org.getOid());
                    m_broker.saveBeanForced(orgAttrib);
                    m_organizationAttributesOid = orgAttrib.getOid();
                }

                /*
                 * Lookup the timestamp field by alias and save the bean path for that fields so we
                 * don't need
                 * to do this lookup each time we update the timestamp. Do the same for the access
                 * token.
                 */
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, m_broker.getPersistenceKey());
                if (dictionary != null) {
                    DataDictionaryField timestampField =
                            dictionary.findDataDictionaryFieldByAlias(ORA_DESTINY_TIMESTAMPS);
                    if (timestampField != null) {
                        /*
                         * Get the blob field data that contains the list of timestamps.
                         */
                        m_timestampsField = timestampField.getJavaName();
                        String timestampList = (String) orgAttrib.getFieldValueByBeanPath(m_timestampsField);

                        /*
                         * Iterate through the list of SIF topics for this deployment and for each
                         * topic do the
                         * following:
                         * 1. Get the current timestamp from the bean for the particular field.
                         * 2. If the timestamp is set then put the value in the map.
                         * 3. If the timestamp is null then we just assume now is the last event
                         * timestamp.
                         * This is because the first time we enable and run this procedure we don't
                         * want to process
                         * historical audit trail records.
                         */
                        if (timestampList != null) {
                            /*
                             * Convert the delimited list into a list of pairs.
                             */
                            List<String> pairList = StringUtils.convertDelimitedStringToList(timestampList, ',', true);
                            String topic = "";
                            String timestamp = "";

                            /*
                             * Iterate through each of the pairs doing the following:
                             * 1. Split the pair string into topic and timestamp
                             * 2. Add the topic with the timestamp to the returned map.
                             */
                            for (String pair : pairList) {
                                List<String> keyvalue = StringUtils.convertDelimitedStringToList(pair, '=', true);

                                if (keyvalue.size() == 2) {
                                    topic = keyvalue.get(0);
                                    timestamp = keyvalue.get(1);

                                    m_objectTimeStamps.put(topic, Long.valueOf(timestamp));
                                }
                            }
                        }
                    }

                    DataDictionaryField accessTokenField =
                            dictionary.findDataDictionaryFieldByAlias(ORA_DESTINY_ACCESSTOKEN);
                    if (accessTokenField != null) {
                        m_accessToken = (String) orgAttrib.getFieldValueByBeanPath(accessTokenField.getJavaName());
                    }

                    DataDictionaryField splitCoursesField =
                            dictionary.findDataDictionaryFieldByAlias(ORA_DESTINY_SPLITCOURSES);
                    if (splitCoursesField != null) {
                        /*
                         * Get the blob field data that contains the list of timestamps.
                         */
                        m_splitSectionsField = splitCoursesField.getJavaName();
                        String splitCoursesList = (String) orgAttrib.getFieldValueByBeanPath(m_splitSectionsField);

                        if (splitCoursesList != null) {
                            /*
                             * Convert the delimited list into a list of pairs.
                             */
                            List<String> pairList =
                                    StringUtils.convertDelimitedStringToList(splitCoursesList, ',', true);

                            /*
                             * Iterate through each of the pairs doing the following:
                             * 1. Split the pair string into course OID and count
                             * 2. Add the course with the count to the returned map.
                             */
                            for (String pair : pairList) {
                                List<String> keyvalue = StringUtils.convertDelimitedStringToList(pair, '=', true);

                                if (keyvalue.size() == 2) {
                                    m_splitSections.put(keyvalue.get(0), Integer.valueOf(keyvalue.get(1)));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Loads all the schedule expressions for all the schools that are configured for Destiny
     * syncing.
     */
    private void loadScheduleExpressions() {
        // Find export format definition for the Destiny Sections.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, "SYS-DST-MST");
        QueryByCriteria query = new QueryByCriteria(ExportFormatDefinition.class, criteria);
        ExportFormatDefinition efd = (ExportFormatDefinition) m_broker.getBeanByQuery(query);

        /*
         * Look for the field that the user chose for the schedule display to determine what
         * is to be used to parse for Destiny.
         */
        for (ExportFormatField eff : efd.getFields()) {
            if ("Meets1".equals(eff.getName())) {
                if (!StringUtils.isEmpty(eff.getFieldPath())) {
                    m_scheduleExpressionFieldPath = eff.getFieldPath();
                    break;
                }
            }
        }

        /*
         * Iterate through the schools grouping all the schedule expressions together for faster
         * lookup when
         * sending the schedule data to Destiny.
         */
        for (String siteId : m_aspenSchools.keySet()) {
            for (SisSchool school : m_aspenSchools.get(siteId)) {
                if (!StringUtils.isEmpty(siteId)) {
                    // Find all the schedule display results grouped together and put them into a
                    // map.
                    X2Criteria masterCriteria = new X2Criteria();
                    masterCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, school.getActiveScheduleOid());
                    masterCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                            SchoolCourse.MASTER_TYPE_CLASS);
                    masterCriteria.addNotNull(m_scheduleExpressionFieldPath);

                    ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(MasterSchedule.class,
                            new String[] {MasterSchedule.COL_SCHEDULE_OID, m_scheduleExpressionFieldPath},
                            masterCriteria);
                    reportQuery.addGroupBy(MasterSchedule.COL_SCHEDULE_OID);
                    reportQuery.addGroupBy(m_scheduleExpressionFieldPath);

                    // Return the list of expressions keyed off the schedule oid.
                    Map<String, List<String>> expressionMap =
                            m_broker.getGroupedColumnCollectionByQuery(reportQuery, 100);

                    /*
                     * This is the expression to period map which is keyed off the expression so we
                     * can look it up
                     * quickly as we process section records.
                     */
                    Map<String, String> expressionToPeriod = new HashMap<String, String>();

                    if (expressionMap.get(school.getActiveScheduleOid()) != null) {
                        /*
                         * If we find expressions for the current school's schedule then we iterate
                         * through the
                         * expression list and find the period number and populate another map.
                         */
                        for (String expression : expressionMap.get(school.getActiveScheduleOid())) {
                            boolean populated = false;
                            for (SchedulePeriod period : (Collection<SchedulePeriod>) school.getActiveSchedule()
                                    .getSchedulePeriods()) {
                                /*
                                 * If the active schedule is schedule expression period first and
                                 * the expression is not
                                 * empty. Then we determine which period is set and populate the
                                 * expression to period map.
                                 */
                                if (school.getActiveSchedule().scheduleExpressionPeriodFirst()
                                        && !StringUtils.isEmpty(expression)) {
                                    if (expression.startsWith(period.getId())) {
                                        expressionToPeriod.put(expression, Integer.valueOf(period.getNumber()).toString());
                                        populated = true;
                                        break;
                                    }
                                } else if (!StringUtils.isEmpty(expression)) {
                                    /*
                                     * If the active schedule is schedule expression day first and
                                     * the expression is not
                                     * empty. Then we determine which period is set and populate the
                                     * expression to period map.
                                     */
                                    int index = expression.indexOf('(');
                                    if (expression.startsWith(period.getId(), index + 1)) {
                                        expressionToPeriod.put(expression, Integer.valueOf(period.getNumber()).toString());
                                        populated = true;
                                        break;
                                    }
                                }
                            }

                            /*
                             * If we didn't find anything for this expression and there are schedule
                             * periods then we use the first one.
                             * Otherwise this will hard code it to be 1.
                             */
                            if (!populated) {
                                expressionToPeriod.put(expression, "1");
                            }
                        }
                    }

                    // Add the expression -> period map to the school -> expressiontoPeriod outer
                    // map.
                    m_schoolScheduleExpressions.put(school.getOid(), expressionToPeriod);
                }
            }
        }
    }

    /**
     * Process all the audits related to the interesting tables for Destiny.
     *
     * @throws X2BaseException exception
     */
    private void processAudits() throws X2BaseException {
        X2Broker broker = getBroker();
        long executionTime = System.currentTimeMillis();

        String lastStudentUpload = m_objectTimeStamps.get(STUDENT_PATRON).toString();

        // Iterate through the list of topics.
        for (String objectType : m_objectTables.keySet()) {
            if (!m_sendClasses) {
                if (objectType.equals(COURSE) || objectType.equals(SECTION) || objectType.equals(SECTION_ENROLLMENT)) {
                    continue;
                }
            }

            /*
             * Build criteria which includes changes in any of the tables specified for this topic
             * which have been modified since the last time stamp saved.
             */
            X2Criteria criteria = new X2Criteria();
            QueryByCriteria query = null;

            /*
             * Student schedule changes are handled separately because they are coming from the
             * Schedule Change History table.
             */
            if (objectType.equals(SECTION_ENROLLMENT)) {
                /*
                 * If we didn't have a stored timestamp before then set it to
                 * now so it will be updated on next successful update.
                 * TODO : Remove as this shouldn't be necessary.
                 */
                if (m_objectTimeStamps.get(objectType) == null) {
                    m_objectTimeStamps.put(objectType, Long.valueOf(executionTime));
                }
                criteria.addGreaterThan(StudentScheduleChange.COL_TIMESTAMP,
                        m_objectTimeStamps.get(objectType).toString());

                // Query and order by timestamp so that the records are processed in the correct
                // order.
                query = new QueryByCriteria(StudentScheduleChange.class, criteria);
                query.addOrderByAscending(StudentScheduleChange.COL_TIMESTAMP);
            } else {
                for (String table : m_objectTables.get(objectType)) {
                    criteria.addOrEqualTo(DataAudit.COL_TABLE_OID, table);
                }

                /*
                 * If we didn't have a stored timestamp before then set it to
                 * now so it will be updated on next successful update.
                 */
                if (m_objectTimeStamps.get(objectType) == null) {
                    m_objectTimeStamps.put(objectType, Long.valueOf(executionTime));
                }
                criteria.addGreaterThan(DataAudit.COL_TIMESTAMP, m_objectTimeStamps.get(objectType).toString());

                // Query and order by timestamp so that the records are processed in the correct
                // order.
                query = new QueryByCriteria(DataAudit.class, criteria);
                query.addOrderByAscending(DataAudit.COL_TIMESTAMP);
            }

            // Iterate through each record and processing them in case they need to be published.
            QueryIterator iterator = broker.getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object audit = iterator.next();

                    /*
                     * If the timestamp was updated properly then we continue processing audit
                     * records. Otherwise we break and will try to publish again at the next
                     * interval.
                     */
                    if (processAuditRecord(objectType, audit, broker)) {
                        m_processedCount++;
                    } else {
                        logMessage("Failed to publish audit record : " + ((X2BaseBean) audit).getOid() + " : "
                                + objectType);
                    }
                }
            } finally {
                iterator.close();
            }
        }

        int newEnrollments = findNewEnrollments(lastStudentUpload, broker);

        logMessage("Processed total " + m_processedCount + " audit records related to Destiny objects.");
        logMessage("Published total " + m_publishedCount + " Destiny objects.");
        logMessage("Skipped total " + m_unrelatedCount + " unrelated audit records.");
        logMessage("Published total " + newEnrollments + "  new enrollment records.");
    }

    /**
     * Processes the audit record for the various Destiny objects.
     *
     * @param object String
     * @param dataAudit Object
     * @param broker X2Broker
     * @return boolean
     * @throws X2BaseException exception
     */
    private boolean processAuditRecord(String object, Object dataAudit, X2Broker broker) throws X2BaseException {
        boolean result = false;

        if (object.equals(STUDENT_PATRON)) {
            result = processStudentPatronAuditRecord(object, (DataAudit) dataAudit, broker);
        } else if (object.equals(STAFF_PATRON)) {
            result = processStaffPatronAuditRecord(object, (DataAudit) dataAudit, broker);
        } else if (object.equals(COURSE)) {
            result = processCourseAuditRecord(object, (DataAudit) dataAudit, broker);
        } else if (object.equals(SECTION)) {
            result = processSectionAuditRecord(object, (DataAudit) dataAudit, broker);
        } else if (object.equals(SECTION_ENROLLMENT)) {
            result = processSectionEnrollmentAuditRecord(object, (StudentScheduleChange) dataAudit);
        }

        return result;
    }

    /**
     * Processes school course audit records.
     *
     * @param object String
     * @param dataAudit DataAudit
     * @param broker X2Broker
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean processCourseAuditRecord(String object, DataAudit dataAudit, X2Broker broker)
            throws X2BaseException {
        boolean result = true;

        String beanOid = dataAudit.getObjectOid();

        if (dataAudit.getTableOid().equals(m_objectTables.get(object).get(0))
                && !m_auditObjectsProcessed.contains(beanOid)) {
            SchoolCourse schoolCourse = (SchoolCourse) broker.getBeanByOid(SchoolCourse.class, beanOid);

            if (schoolCourse != null && dataAudit.getChangeType() == DataAudit.ChangeType.CREATE.ordinal()) {
                result = sendCoursesForSchool(schoolCourse.getSchool(), beanOid, Action.ADD);
            } else if (schoolCourse != null && dataAudit.getChangeType() == DataAudit.ChangeType.MODIFY.ordinal()) {
                result = sendCoursesForSchool(schoolCourse.getSchool(), beanOid, Action.CHANGE);
            } else if (dataAudit.getChangeType() == DataAudit.ChangeType.DELETE.ordinal()) {
                com.follett.fsc.destiny.datatransfer.data.Course course = m_factory.createCourse();
                course.setSchoolCourseInfoRefId(GuidManager.oidToGuid(beanOid));
                course.setAction(Action.DELETE);
                DataTransferResult transferresult = sendData(course);

                if (transferresult.getStatus().equals(DataTransferStatus.SUCCESS)) {
                    result = true;
                }
            }

            if (result) {
                m_objectTimeStamps.put(object, Long.valueOf(dataAudit.getTimestamp()));
                writeTimestamps();

                logMessage("Published CourseSchool audit record " + dataAudit.getObjectOid() + " from "
                        + dataAudit.getTimestamp() + ".");
                m_auditObjectsProcessed.add(dataAudit.getObjectOid());
                m_publishedCount++;
            }
        }

        return result;
    }

    /**
     * Processes schedule master audit records.
     *
     * @param object String
     * @param dataAudit DataAudit
     * @param broker X2Broker
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean processSectionAuditRecord(String object, DataAudit dataAudit, X2Broker broker)
            throws X2BaseException {
        boolean result = true;

        String beanOid = dataAudit.getObjectOid();

        if (dataAudit.getTableOid().equals(m_objectTables.get(object).get(0))
                && !m_auditObjectsProcessed.contains(beanOid)) {
            MasterSchedule masterSchedule = (MasterSchedule) broker.getBeanByOid(MasterSchedule.class, beanOid);

            /*
             * Adds or changes only need to be done for the current year schedule.
             */
            if (masterSchedule != null &&
                    masterSchedule.getSchedule().getDistrictContextOid().equals(getCurrentContext().getOid()) &&
                    masterSchedule.getScheduleOid()
                            .equals(masterSchedule.getSchedule().getSchool().getActiveScheduleOid())) {

                if (dataAudit.getChangeType() == DataAudit.ChangeType.CREATE.ordinal()) {
                    result = sendSectionsForSchool(masterSchedule.getSchedule().getSchool(), beanOid, Action.ADD);
                } else if (dataAudit.getChangeType() == DataAudit.ChangeType.MODIFY.ordinal()) {
                    /*
                     * Quickly parse the data audit changes to determine whether we need to delete
                     * add, and refresh student schedules. This is only necessary if the schedule
                     * display changes
                     */
                    String changeDefinition = dataAudit.getChangeDefinition();
                    boolean resendSections = false;

                    if (m_splitSections.get(beanOid) != null && changeDefinition.contains("mstSchdDisplay")) {
                        HashMap<String, String> previousValues = new HashMap<String, String>();
                        HashMap<String, String> changedValues = new HashMap<String, String>();
                        AuditXmlManager.parseAuditChangeDefinition(dataAudit, previousValues, changedValues);

                        if (!previousValues.get("mstSchdDisplay").equals(changedValues.get("mstSchdDisplay"))) {
                            resendSections = true;
                        }
                    }

                    // if we find this is a split section then it is safer to delete/add then send a
                    // change
                    if (m_splitSections.get(beanOid) != null && resendSections) {
                        int count = m_splitSections.get(beanOid).intValue();
                        while (count - 1 >= 0) {
                            count--;
                            Section section = m_factory.createSection();
                            section.setAction(Action.DELETE);
                            section.setSectionInfoRefId(GuidManager.oidToGuid(beanOid.concat(String.valueOf(count))));
                            DataTransferResult transferresult = sendData(section);
                            if (transferresult.getStatus().equals(DataTransferStatus.SUCCESS)) {
                                result = true;
                            }
                        }

                        m_splitSections.remove(beanOid);
                        result = sendSectionsForSchool(masterSchedule.getSchedule().getSchool(), beanOid, Action.ADD);
                        for (StudentSchedule ssc : masterSchedule.getStudentSections()) {
                            sendSectionEnrollmentRecord(ssc.getSectionOid(), ssc.getStudentOid(), Action.ADD);
                        }
                    } else {
                        result = sendSectionsForSchool(masterSchedule.getSchedule().getSchool(), beanOid,
                                Action.CHANGE);
                    }
                }
            }

            /*
             * Deletes could be sent for any section but realistically it will only be for the
             * current year sections.
             */
            if (dataAudit.getChangeType() == DataAudit.ChangeType.DELETE.ordinal()) {
                if (m_splitSections.get(beanOid) != null) {
                    int count = m_splitSections.get(beanOid).intValue();
                    while (count - 1 >= 0) {
                        count--;
                        Section section = m_factory.createSection();
                        section.setAction(Action.DELETE);
                        section.setSectionInfoRefId(GuidManager.oidToGuid(beanOid.concat(String.valueOf(count))));
                        DataTransferResult transferresult = sendData(section);
                        if (transferresult.getStatus().equals(DataTransferStatus.SUCCESS)) {
                            result = true;
                        }
                    }

                    m_splitSections.remove(beanOid);
                } else {
                    Section section = m_factory.createSection();
                    section.setAction(Action.DELETE);
                    section.setSectionInfoRefId(GuidManager.oidToGuid(beanOid));
                    DataTransferResult transferresult = sendData(section);

                    if (transferresult.getStatus().equals(DataTransferStatus.SUCCESS)) {
                        result = true;
                    }
                }
            }

            if (result) {
                m_objectTimeStamps.put(object, Long.valueOf(dataAudit.getTimestamp()));
                writeTimestamps();
                writeSplitSections();

                logMessage("Published MasterSchedule audit record " + dataAudit.getObjectOid() + " from "
                        + dataAudit.getTimestamp() + ".");
                m_auditObjectsProcessed.add(beanOid);
                m_publishedCount++;
            }
        }

        return result;
    }

    /**
     * Process section enrollment audit records. These are built directly here from the records
     * in the StudentScheduleChange table without using the export formats.
     *
     * @param object String
     * @param changeRecord StudentScheduleChange
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean processSectionEnrollmentAuditRecord(String object, StudentScheduleChange changeRecord)
            throws X2BaseException {
        boolean result = true;

        if (!StringUtils.isEmpty(m_schoolSites.get(changeRecord.getSchedule().getSchool().getOid()))) {
            MasterSchedule master = changeRecord.getMasterSchedule();

            result = sendSectionEnrollmentRecord(master.getOid(), changeRecord.getStudentOid(),
                    changeRecord.getChangeTypeCode().equals(StudentScheduleChange.CODE_ADD) ? Action.ADD
                            : Action.DELETE);

        }

        if (result) {
            m_objectTimeStamps.put(object, Long.valueOf(changeRecord.getTimestamp()));
            writeTimestamps();

            logMessage("Published StudentSchedulechange audit record " + changeRecord.getOid() + " from "
                    + changeRecord.getTimestamp() + ".");
            m_publishedCount++;
        }

        return result;
    }

    /**
     * Processes staff audit records.
     *
     * @param object String
     * @param dataAudit DataAudit
     * @param broker X2Broker
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean processStaffPatronAuditRecord(String object, DataAudit dataAudit, X2Broker broker)
            throws X2BaseException {
        boolean result = true;

        String beanOid = dataAudit.getObjectOid();

        if (dataAudit.getTableOid().equals(m_objectTables.get(object).get(0))
                && !m_auditObjectsProcessed.contains(beanOid)) {

            Staff staff = (Staff) broker.getBeanByOid(Staff.class, beanOid);

            if (staff != null && dataAudit.getChangeType() == DataAudit.ChangeType.CREATE.ordinal()) {
                result = sendStaffPatronForSchool(staff.getSchool(), beanOid, Action.ADD);
            } else if (staff != null && dataAudit.getChangeType() == DataAudit.ChangeType.MODIFY.ordinal()) {
                result = sendStaffPatronForSchool(staff.getSchool(), beanOid, Action.CHANGE);
            } else if (dataAudit.getChangeType() == DataAudit.ChangeType.DELETE.ordinal()) {
                Patron patron = m_factory.createTeacher();
                patron.setAction(Action.DELETE);
                patron.setRefId(GuidManager.oidToGuid(beanOid));
                DataTransferResult transferresult = sendData(patron);

                if (transferresult.getStatus().equals(DataTransferStatus.SUCCESS)) {
                    result = true;
                }
            }

            if (result) {
                m_objectTimeStamps.put(object, Long.valueOf(dataAudit.getTimestamp()));
                writeTimestamps();
                logMessage("Published StaffPatron audit record " + dataAudit.getObjectOid() + " from "
                        + dataAudit.getTimestamp() + ".");
                m_auditObjectsProcessed.add(beanOid);
                m_publishedCount++;
            }
        } else {
            /*
             * Query based on finding the Staff matching the Person record.
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(Staff.COL_PERSON_OID, dataAudit.getObjectOid());
            QueryByCriteria query = new QueryByCriteria(Staff.class, criteria);

            Staff staff = (Staff) broker.getBeanByQuery(query);

            if (staff != null && !m_auditObjectsProcessed.contains(staff.getOid())) {
                if (dataAudit.getChangeType() == DataAudit.ChangeType.CREATE.ordinal()) {
                    result = sendStaffPatronForSchool(staff.getSchool(), staff.getOid(), Action.ADD);
                } else if (dataAudit.getChangeType() == DataAudit.ChangeType.MODIFY.ordinal()) {
                    result = sendStaffPatronForSchool(staff.getSchool(), staff.getOid(), Action.CHANGE);
                }

                if (result) {
                    m_objectTimeStamps.put(object, Long.valueOf(dataAudit.getTimestamp()));
                    writeTimestamps();
                    logMessage("Published StaffPatron audit record " + dataAudit.getObjectOid() + " from "
                            + dataAudit.getTimestamp() + ".");
                    m_auditObjectsProcessed.add(staff.getOid());
                    m_publishedCount++;
                }
            } else {
                m_unrelatedCount++;
            }
        }

        return result;
    }

    /**
     * Processes student .
     *
     * @param object String
     * @param dataAudit DataAudit
     * @param broker X2Broker
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean processStudentPatronAuditRecord(String object, DataAudit dataAudit, X2Broker broker)
            throws X2BaseException {
        boolean result = true;

        String beanOid = dataAudit.getObjectOid();

        // If a student create the publish directly.
        if (dataAudit.getTableOid().equals(m_objectTables.get(object).get(0))
                && !m_auditObjectsProcessed.contains(beanOid)) {

            Student student = (Student) broker.getBeanByOid(Student.class, beanOid);

            if (student != null && dataAudit.getChangeType() == DataAudit.ChangeType.CREATE.ordinal()) {
                result = sendStudentPatronForSchool(student.getSchool(), beanOid, Action.ADD);
            } else if (student != null && dataAudit.getChangeType() == DataAudit.ChangeType.MODIFY.ordinal()) {
                result = sendStudentPatronForSchool(student.getSchool(), beanOid, Action.CHANGE);
            } else if (dataAudit.getChangeType() == DataAudit.ChangeType.DELETE.ordinal()) {
                Patron patron = m_factory.createStudent();
                patron.setAction(Action.DELETE);
                patron.setRefId(GuidManager.oidToGuid(beanOid));
                DataTransferResult transferresult = sendData(patron);

                if (transferresult.getStatus().equals(DataTransferStatus.SUCCESS)) {
                    result = true;
                }
            }

            if (result) {
                m_objectTimeStamps.put(object, Long.valueOf(dataAudit.getTimestamp()));
                writeTimestamps();
                logMessage("Published StudentPatron audit record " + dataAudit.getObjectOid() + " from "
                        + dataAudit.getTimestamp() + ".");
                m_auditObjectsProcessed.add(beanOid);
                m_publishedCount++;
            }
        } else {
            /*
             * Query based on finding the Student matching the Person record.
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(Student.COL_PERSON_OID, dataAudit.getObjectOid());
            QueryByCriteria query = new QueryByCriteria(Student.class, criteria);

            Student student = (Student) broker.getBeanByQuery(query);

            if (student != null && !m_auditObjectsProcessed.contains(student.getOid())) {
                if (dataAudit.getChangeType() == DataAudit.ChangeType.CREATE.ordinal()) {
                    result = sendStudentPatronForSchool(student.getSchool(), student.getOid(), Action.ADD);
                } else if (dataAudit.getChangeType() == DataAudit.ChangeType.MODIFY.ordinal()) {
                    result = sendStudentPatronForSchool(student.getSchool(), student.getOid(), Action.CHANGE);
                }

                if (result) {
                    m_objectTimeStamps.put(object, Long.valueOf(dataAudit.getTimestamp()));
                    writeTimestamps();
                    logMessage("Published StudentPatron  audit record " + dataAudit.getObjectOid() + " from "
                            + dataAudit.getTimestamp() + ".");
                    m_auditObjectsProcessed.add(student.getOid());
                    m_publishedCount++;
                }
            } else {
                m_unrelatedCount++;
            }
        }

        return result;
    }

    /**
     * Register with destiny. Store the access token that comes back from Destiny in the response.
     *
     * @param encryptToken boolean
     * @return true, if successful
     * @throws EncryptionException exception
     */
    private boolean registerWithDestiny(boolean encryptToken) throws EncryptionException {
        boolean register = false;
        List<String> messages = new ArrayList<String>();

        // This text identifies the organization to Destiny (will be displayed on Destiny's setup
        // form)
        String aspenName = getOrganization().getName();
        String server = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.server");
        String port = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.port");

        // Base64 representation of public key
        String encodedPublicKey = EncryptionUtil.encodeRSAKeyToString(m_sisKeyPair.getPublic());

        DataTransferFactory.configure(server, port, "Aspen", AppGlobals.getVersion(true), "English");
        m_datatransfer =
                DataTransferFactory.getInterface(server, port, "Aspen", AppGlobals.getVersion(true), "English");

        DataTransferResult result = m_datatransfer.register(m_context, m_registrationToken, aspenName,
                encryptToken ? encodedPublicKey : null);
        if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
            // Access token to be used in the future will be in message[0] -- store as encrypted
            // Base64
            m_accessToken = encryptToken
                    ? EncryptionUtil.encryptDataWithRSA(result.getMessages().get(0), m_sisKeyPair.getPrivate())
                    : result.getMessages().get(0);
            setAccessToken();
            register = true;
        }
        messages.addAll(result.getMessages());

        for (String message : messages) {
            logMessage(message);
        }

        return register;
    }

    /**
     * Send course(s) for a particular school or send an update on a course.
     * Initialized the export format for that school and send those school courses over.
     *
     * @param school School
     * @param courseOid String
     * @param updateAction Action
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean sendCoursesForSchool(School school, String courseOid, Action updateAction) throws X2BaseException {
        boolean success = false;

        String procedureId = (String) getParameter(COURSE_PROCEDURE_ID);

        if (!StringUtils.isEmpty(m_schoolSites.get(school.getOid()))) {
            // Lookup State report source data procedure
            m_initErrors = new ArrayList<StateReportValidationError>();
            m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
            if (m_reportData != null && m_initErrors.size() == 0) {
                try {
                    Map<String, Object> parameters = new HashMap<String, Object>();
                    parameters.put(SCHOOL_PARAMETER, school.getOid());
                    parameters.put(BEANOID_PARAMETER, courseOid);

                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setCurrentContext(getCurrentContext());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(parameters);
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();

                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    logMessage(init_msg + " " + init_msg + " " + init_msg + " " + x2be.getMessage());
                }

                for (StateReportValidationError error : m_reportData.getSetupErrors()) {
                    logMessage(error.getErrorMessage());
                }

                if (m_initErrors.size() == 0) {
                    if (m_reportData.open()) {
                        try {
                            StateReportEntity entity = null;
                            int count = 0;

                            while ((entity = m_reportData.next()) != null) {
                                // Construct the object
                                com.follett.fsc.destiny.datatransfer.data.Course course = m_factory.createCourse();

                                StateReportValidationError err = entity.filterEntity();
                                if (err == null) {
                                    entity.preProcess();

                                    SchoolCourse schoolCourse = (SchoolCourse) entity.getBean();

                                    Class courseClass = course.getClass();
                                    Method courseMethods[] = courseClass.getDeclaredMethods();

                                    if (updateAction != null) {
                                        course.setAction(updateAction);
                                    } else {
                                        course.setAction(Action.ADD);
                                    }
                                    course.setSchoolCourseInfoRefId(GuidManager.oidToGuid(schoolCourse.getOid()));
                                    course.setSchoolNum(m_schoolSites.get(school.getOid()));

                                    List<Method> courseMethodList = Arrays.asList(courseMethods);

                                    /*
                                     * Add all fields
                                     */
                                    for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                        FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                        String fieldValue = entity.getFieldValue(pos);

                                        /*
                                         * If the value requires padding, pad it and trim it to
                                         * field max length.
                                         */
                                        fieldValue = ExportFormatManager.doPadding(fieldValue,
                                                (field.getResizeMode() == null
                                                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                        : field.getResizeMode().ordinal()),
                                                field.getPaddingChar(),
                                                field.getExportLength());

                                        // Set the final value.
                                        for (Method method : courseMethodList) {
                                            if (method.getName().equals("set".concat(field.getFieldId()))) {
                                                try {
                                                    method.invoke(course, fieldValue);
                                                    break;
                                                } catch (Throwable e) {
                                                    logMessage(e.getMessage());
                                                }
                                            }
                                        }
                                    }
                                    entity.postProcess();
                                } else {
                                    m_initErrors.add(err);
                                }
                                DataTransferResult result = sendData(course);
                                if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                                    count++;
                                    success = true;
                                }
                            }
                            if (StringUtils.isEmpty(courseOid)) {
                                logMessage("Course count: " + count);
                            }
                        } finally {
                            m_reportData.close();
                        }
                    }
                }
            }
        }

        return success;
    }

    /**
     * Sends the DataObject to Destiny. If a WebServiceException is caught, an additional retry will
     * occur.
     * before aborting the current record.
     *
     * @param dataObject DataObject
     * @return DataTransferResult
     */
    private DataTransferResult sendData(DataObject dataObject) {
        DataTransferResult result = null;
        int attempts = 0;

        while (result == null && attempts < 2) {
            try {
                result = m_datatransfer.sendData(m_context, m_accessToken, dataObject);
            } catch (WebServiceException webServiceException) {
                attempts++;
                logMessage("\r\n" + webServiceException.getMessage());

                try {
                    // Sleep before attempting to process again.
                    Thread.sleep(10000);
                } catch (InterruptedException interruptedException) {
                    logMessage("\r\n" + interruptedException.getMessage());
                }
            }
        }

        if (result == null) {
            String message = "Failed to communicate with server to send data. \r\n" +
                    "Client Name: " + dataObject.getClientName() + "\r\n" +
                    "Object Data: " + dataObject.getObjectData() + "\r\n" +
                    "Ref ID: " + dataObject.getRefId() + "\r\n" +
                    "School Number: " + dataObject.getSchoolNum() + "\r\n" +
                    "Object Type: " + dataObject.getObjectType();

            logMessage(message);

            result = new DataTransferResult(DataTransferStatus.ERROR);
            result.addMessage(message);
        }

        return result;
    }

    /**
     * Sends all initial data for Patrons and Classes.
     *
     * @throws X2BaseException exception
     */
    private void sendInitialData() throws X2BaseException {
        // send patrons.
        for (String siteId : m_aspenSchools.keySet()) {
            // start sync
            PatronSyncStart syncStart = m_factory.createPatronSyncStart(siteId);
            DataTransferResult result = sendData(syncStart);

            if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                for (SisSchool school : m_aspenSchools.get(siteId)) {
                    // If we have matched the school site ids.
                    if (!StringUtils.isEmpty(m_schoolSites.get(school.getOid()))) {
                        if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                            // send students
                            sendStudentPatronForSchool(school, null, null);

                            // send staff
                            sendStaffPatronForSchool(school, null, null);
                        }
                    }
                }

                /*
                 * complete sync
                 * After all patron data has been transferred, tell Destiny we are finished
                 */
                PatronSyncComplete syncComplete = m_factory.createPatronSyncComplete(syncStart);
                result = sendData(syncComplete);

                if (result.getStatus().equals(DataTransferStatus.SUCCESS) && m_sendClasses) {
                    // start class sync
                    ClassSyncStart classSyncStart = m_factory.createClassSyncStart(siteId);
                    result = sendData(classSyncStart);

                    for (SisSchool school : m_aspenSchools.get(siteId)) {
                        if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                            // send courses
                            sendCoursesForSchool(school, null, null);

                            // send sections
                            sendSectionsForSchool(school, null, null);

                            // write out the split sections
                            writeSplitSections();

                            // send section enrollments
                            sendSectionEnrollmentsForSchool(school);
                        } else {
                            logMessage("Failed to start synchronization for classes.");
                            for (String message : result.getMessages()) {
                                logMessage("    " + message);
                            }
                        }
                    }

                    // After all class data has been transferred, tell Destiny we are finished
                    ClassSyncComplete classSyncComplete = m_factory.createClassSyncComplete(classSyncStart);
                    result = sendData(classSyncComplete);

                    if (result.getStatus().equals(DataTransferStatus.ERROR)) {
                        logMessage("Failed to complete synchronization for classes.");
                        for (String message : result.getMessages()) {
                            logMessage("    " + message);
                        }
                    }
                } else if (m_sendClasses) {
                    logMessage("Failed to complete synchronization for Patrons.");
                    for (String message : result.getMessages()) {
                        logMessage("    " + message);
                    }
                }
            } else {
                logMessage("Failed to start synchronization for Patrons on site " + siteId + " .");
                for (String message : result.getMessages()) {
                    logMessage("    " + message);
                }
            }
        }
    }

    /**
     * Sends the section enrollment records related to a master and student. Handles
     * the split of multi-term sections if necessary.
     *
     * @param masterOid String
     * @param studentOid String
     * @param action Action
     * @return boolean
     * @throws X2BaseException exception
     */
    private boolean sendSectionEnrollmentRecord(String masterOid, String studentOid, Action action)
            throws X2BaseException {
        boolean result = true;

        // if we find this is a split section then it is safer to delete/add then send a change
        if (m_splitSections.get(masterOid) != null) {
            int count = m_splitSections.get(masterOid).intValue();
            while (count - 1 >= 0) {
                count--;
                SectionEnrollment enrollment = m_factory.createSectionEnrollment();
                enrollment.setAction(action);
                enrollment.setSectionInfoRefId(GuidManager.oidToGuid(masterOid.concat(String.valueOf(count))));
                enrollment.setStudentPersonalRefId(GuidManager.oidToGuid(studentOid));
                DataTransferResult status = sendData(enrollment);

                if (status.getStatus().equals(DataTransferStatus.SUCCESS)) {
                    result = true;
                }
            }
        } else {
            SectionEnrollment enrollment = m_factory.createSectionEnrollment();
            enrollment.setAction(action);
            enrollment.setSectionInfoRefId(GuidManager.oidToGuid(masterOid));
            enrollment.setStudentPersonalRefId(GuidManager.oidToGuid(studentOid));
            DataTransferResult status = sendData(enrollment);

            if (status.getStatus().equals(DataTransferStatus.SUCCESS)) {
                result = true;
            }
        }

        return result;
    }

    /**
     * Send section(s) for a particular school or send an update on a section.
     * Initialized the export format for that school and send those sections over.
     *
     * @param school School
     * @param sectionOid String
     * @param updateAction Action
     * @return boolean
     * @throws X2BaseException exception
     */
    private boolean sendSectionsForSchool(School school, String sectionOid, Action updateAction)
            throws X2BaseException {
        boolean success = false;

        String procedureId = (String) getParameter(SECTION_PROCEDURE_ID);

        if (school != null && !StringUtils.isEmpty(m_schoolSites.get(school.getOid()))) {
            // Lookup State report source data procedure
            Map<String, String> scheduleExpressions = m_schoolScheduleExpressions.get(school.getOid());

            m_initErrors = new ArrayList<StateReportValidationError>();
            m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

            if (m_reportData != null && m_initErrors.size() == 0) {
                try {
                    Map<String, Object> parameters = new HashMap<String, Object>();
                    parameters.put(SCHOOL_PARAMETER, school.getOid());
                    parameters.put(BEANOID_PARAMETER, sectionOid);
                    parameters.put(EXPRESSION_FIELD_PARAMETER, m_scheduleExpressionFieldPath);

                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setCurrentContext(getCurrentContext());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(parameters);
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();

                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    logMessage(init_msg + " " + init_msg + " " + init_msg + " " + x2be.getMessage());
                }

                for (StateReportValidationError error : m_reportData.getSetupErrors()) {
                    logMessage(error.getErrorMessage());
                }

                if (m_initErrors.size() == 0) {
                    if (m_reportData.open()) {
                        try {
                            StateReportEntity entity = null;
                            int count = 0;

                            while ((entity = m_reportData.next()) != null) {
                                // Construct the object
                                Section section = m_factory.createSection();

                                StateReportValidationError err = entity.filterEntity();
                                if (err == null) {
                                    entity.preProcess();

                                    MasterSchedule master = (MasterSchedule) entity.getBean();

                                    if (entity.getRowCount() > 1) {
                                        m_splitSections.put(master.getOid(), Integer.valueOf(entity.getRowCount()));
                                    }

                                    Class sectionClass = section.getClass();
                                    Method sectionMethods[] = sectionClass.getDeclaredMethods();

                                    if (updateAction != null) {
                                        section.setAction(updateAction);
                                    } else {
                                        section.setAction(Action.ADD);
                                    }

                                    section.setSchoolCourseInfoRefId(
                                            GuidManager.oidToGuid(master.getSchoolCourse().getOid()));
                                    if (master.getPrimaryStaffOid() == null) {
                                        logMessage("No primary staff so not sending master record :" + master.getOid());
                                        continue;
                                    }

                                    section.setTeacherRefId1(GuidManager.oidToGuid(master.getPrimaryStaffOid()));
                                    section.setSchoolNum(m_schoolSites.get(school.getOid()));

                                    List<Method> sectionMethodList = Arrays.asList(sectionMethods);

                                    // Add all fields
                                    for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                        FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                        String fieldValue = entity.getFieldValue(pos);

                                        // If the value requires padding, pad it and trim it to
                                        // field max length.
                                        fieldValue = ExportFormatManager.doPadding(fieldValue,
                                                (field.getResizeMode() == null
                                                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                        : field.getResizeMode().ordinal()),
                                                field.getPaddingChar(),
                                                field.getExportLength());

                                        if (field.getFieldId().equals("Meets1")
                                                && scheduleExpressions.get(fieldValue) != null) {
                                            fieldValue = scheduleExpressions.get(fieldValue);
                                        }

                                        // Set the final value.
                                        for (Method method : sectionMethodList) {
                                            if (method.getName().equals("set".concat(field.getFieldId()))) {
                                                try {
                                                    method.invoke(section, fieldValue);
                                                    break;
                                                } catch (Throwable e) {
                                                    logMessage(e.getMessage());
                                                }
                                            }
                                        }
                                    }
                                    entity.postProcess();
                                } else {
                                    m_initErrors.add(err);
                                }

                                DataTransferResult result = sendData(section);
                                if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                                    count++;
                                    success = true;
                                }
                            }

                            if (StringUtils.isEmpty(sectionOid)) {
                                logMessage("Section count: " + count);
                            }
                        } finally {
                            m_reportData.close();
                        }
                    }
                }
            }
        }

        return success;
    }

    /**
     * Send section enrollments for a particular school.
     * Initialized the export format for that school and send those enrollments over.
     *
     * @param school School
     * @return boolean
     * @throws X2BaseException exception
     */
    private boolean sendSectionEnrollmentsForSchool(School school) throws X2BaseException {
        boolean success = false;

        String procedureId = (String) getParameter(SECTION_ENROLLMENT_PROCEDURE_ID);

        if (school != null && !StringUtils.isEmpty(m_schoolSites.get(school.getOid()))) {
            // Lookup State report source data procedure
            m_initErrors = new ArrayList<StateReportValidationError>();
            m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

            if (m_reportData != null && m_initErrors.size() == 0) {
                try {
                    Map<String, Object> parameters = new HashMap<String, Object>();
                    parameters.put(SCHOOL_PARAMETER, school.getOid());

                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setCurrentContext(getCurrentContext());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(parameters);
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();

                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    logMessage(init_msg + " " + init_msg + " " + init_msg + " " + x2be.getMessage());
                }

                for (StateReportValidationError error : m_reportData.getSetupErrors()) {
                    logMessage(error.getErrorMessage());
                }

                if (m_initErrors.size() == 0) {
                    if (m_reportData.open()) {
                        try {
                            StateReportEntity entity = null;
                            int count = 0;

                            while ((entity = m_reportData.next()) != null) {
                                // Construct the object
                                SectionEnrollment enrollment = m_factory.createSectionEnrollment();

                                StateReportValidationError err = entity.filterEntity();
                                if (err == null) {
                                    entity.preProcess();

                                    StudentSchedule studentSchedule = (StudentSchedule) entity.getBean();

                                    Class sectionEnrollmentClass = enrollment.getClass();
                                    Method sectionEnrollmentMethods[] = sectionEnrollmentClass.getDeclaredMethods();

                                    enrollment.setAction(Action.ADD);

                                    enrollment.setStudentPersonalRefId(
                                            GuidManager.oidToGuid(studentSchedule.getStudentOid()));
                                    enrollment.setSchoolNum(m_schoolSites.get(school.getOid()));

                                    List<Method> sectionMethodList = Arrays.asList(sectionEnrollmentMethods);

                                    // Add all fields
                                    for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                        FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                        String fieldValue = entity.getFieldValue(pos);

                                        // If the value requires padding, pad it and trim it to
                                        // field max length.
                                        fieldValue = ExportFormatManager.doPadding(fieldValue,
                                                (field.getResizeMode() == null
                                                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                        : field.getResizeMode().ordinal()),
                                                field.getPaddingChar(),
                                                field.getExportLength());

                                        // Set the final value.
                                        for (Method method : sectionMethodList) {
                                            if (method.getName().equals("set".concat(field.getFieldId()))) {
                                                try {
                                                    method.invoke(enrollment, fieldValue);
                                                    break;
                                                } catch (Throwable e) {
                                                    logMessage(e.getMessage());
                                                }
                                            }
                                        }
                                    }
                                    entity.postProcess();
                                } else {
                                    m_initErrors.add(err);
                                }

                                DataTransferResult result = sendData(enrollment);
                                if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                                    count++;
                                    success = true;
                                }
                            }

                            logMessage("Section Enrollment count: " + count);
                        } finally {
                            m_reportData.close();
                        }
                    }
                }
            }
        }

        return success;
    }

    /**
     * Send student(s) for a particular school or send an update on a student.
     * Initialized the export format for that school and send those students over.
     *
     * @param school School
     * @param studentOid String
     * @param updateAction Action
     * @return boolean
     * @throws X2BaseException exception
     */
    private boolean sendStudentPatronForSchool(School school, String studentOid, Action updateAction)
            throws X2BaseException {
        boolean success = false;

        String procedureId = (String) getParameter(STUDENT_PROCEDURE_ID);

        if (school != null && !StringUtils.isEmpty(m_schoolSites.get(school.getOid()))) {
            // Lookup State report source data procedure
            m_initErrors = new ArrayList<StateReportValidationError>();
            m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

            if (m_reportData != null && m_initErrors.size() == 0) {
                try {
                    Map<String, Object> parameters = new HashMap<String, Object>();
                    parameters.put(SCHOOL_PARAMETER, school.getOid());
                    parameters.put(BEANOID_PARAMETER, studentOid);

                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setCurrentContext(getCurrentContext());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(parameters);
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();

                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    logMessage(init_msg + " " + init_msg + " " + init_msg + " " + x2be.getMessage());
                }

                for (StateReportValidationError error : m_reportData.getSetupErrors()) {
                    logMessage(error.getErrorMessage());
                }

                if (m_initErrors.size() == 0) {
                    if (m_reportData.open()) {
                        try {
                            StateReportEntity entity = null;
                            int count = 0;

                            while ((entity = m_reportData.next()) != null) {
                                // Construct the object
                                Patron patron = m_factory.createStudent();
                                boolean skipPatron = false;

                                StateReportValidationError err = entity.filterEntity();
                                if (err == null) {
                                    entity.preProcess();

                                    SisStudent student = (SisStudent) entity.getBean();

                                    Class patronClass = patron.getClass();
                                    Method patronMethods[] = patronClass.getDeclaredMethods();
                                    patron.setRefId(GuidManager.oidToGuid(student.getOid())); // Aspen
                                                                                              // assigned
                                                                                              // GUID

                                    if (updateAction != null) {
                                        patron.setAction(updateAction);
                                    } else {
                                        patron.setAction(Action.ADD);
                                    }

                                    patron.setEnrollMembership(EnrollMembership.HOME); // Concurrent
                                                                                       // not
                                                                                       // currently
                                                                                       // supported
                                    patron.setEnrollStatus(EnrollStatus.ENROLLED); // Teaching at
                                                                                   // this school
                                    patron.setStatus(Status.ACTIVE);
                                    patron.setSchoolNum(m_schoolSites.get(school.getOid()));

                                    List<Method> patronMethodList = Arrays.asList(patronMethods);

                                    // Add all fields
                                    for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                        FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                        String fieldValue = entity.getFieldValue(pos);

                                        // If the value requires padding, pad it and trim it to
                                        // field max length.
                                        fieldValue = ExportFormatManager.doPadding(fieldValue,
                                                (field.getResizeMode() == null
                                                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                        : field.getResizeMode().ordinal()),
                                                field.getPaddingChar(),
                                                field.getExportLength());

                                        if (field.getFieldId().equals("Gender")) {
                                            patron.setGender("M".equals(fieldValue) ? Gender.MALE : Gender.FEMALE);
                                            continue;
                                        } else if (field.getFieldId().equals("GraduationYear")) {
                                            patron.setGraduationYear(Integer.valueOf(student.getYog()));
                                            continue;
                                        }

                                        // We don't want to send password or accesslevel fields
                                        // during updates.
                                        if (updateAction != null &&
                                                (field.getFieldId().equals("Password")
                                                        || field.getFieldId().equals("AccessLevel"))) {
                                            continue;
                                        }

                                        // Set the final value.
                                        for (Method method : patronMethodList) {
                                            if (method.getName().equals("set".concat(field.getFieldId()))) {
                                                try {
                                                    if (field.getFormatter() != null) {
                                                        Object object = field.getFormatter().parseObject(fieldValue);
                                                        method.invoke(patron, object);
                                                    } else {
                                                        method.invoke(patron, fieldValue);
                                                    }
                                                    break;
                                                } catch (Throwable e) {
                                                    logMessage(field.getFieldId() + " " + e.getMessage());
                                                }
                                            }
                                        }
                                    }
                                    entity.postProcess();
                                } else {
                                    m_initErrors.add(err);
                                }

                                if (!skipPatron) {
                                    DataTransferResult result = sendData(patron);
                                    if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                                        success = true;
                                        count++;
                                    }
                                }
                            }

                            if (StringUtils.isEmpty(studentOid)) {
                                logMessage("Student count: " + count);
                            }
                        } finally {
                            m_reportData.close();
                        }
                    }
                }
            }
        }

        return success;
    }

    /**
     * Send staff for a particular school or send an update on a staff member.
     * Initialized the export format for that school and send those staff over.
     *
     * @param school School
     * @param staffOid String
     * @param updateAction Action
     * @return boolean
     * @throws X2BaseException exception
     */
    private boolean sendStaffPatronForSchool(School school, String staffOid, Action updateAction)
            throws X2BaseException {
        boolean success = false;
        String procedureId = (String) getParameter(STAFF_PROCEDURE_ID);

        if (school != null && !StringUtils.isEmpty(m_schoolSites.get(school.getOid()))) {
            // Lookup State report source data procedure
            m_initErrors = new ArrayList<StateReportValidationError>();
            m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

            if (m_reportData != null && m_initErrors.size() == 0) {
                try {
                    Map<String, Object> parameters = new HashMap<String, Object>();
                    parameters.put(SCHOOL_PARAMETER, school.getOid());
                    parameters.put(BEANOID_PARAMETER, staffOid);

                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setCurrentContext(getCurrentContext());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(parameters);
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();

                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    logMessage(init_msg + " " + init_msg + " " + init_msg + " " + x2be.getMessage());
                }

                for (StateReportValidationError error : m_reportData.getSetupErrors()) {
                    logMessage(error.getErrorMessage());
                }

                if (m_initErrors.size() == 0) {
                    if (m_reportData.open()) {
                        try {
                            StateReportEntity entity = null;
                            int count = 0;

                            while ((entity = m_reportData.next()) != null) {
                                // Construct the object
                                Patron patron = m_factory.createTeacher();
                                boolean skipPatron = false;
                                StateReportValidationError err = entity.filterEntity();

                                if (err == null) {
                                    entity.preProcess();

                                    SisStaff staff = (SisStaff) entity.getBean();

                                    Class patronClass = patron.getClass();
                                    Method patronMethods[] = patronClass.getDeclaredMethods();
                                    patron.setRefId(GuidManager.oidToGuid(staff.getOid())); // Aspen
                                                                                            // assigned
                                                                                            // GUID

                                    if (updateAction != null) {
                                        patron.setAction(updateAction);
                                    } else {
                                        patron.setAction(Action.ADD);
                                    }

                                    patron.setEnrollMembership(EnrollMembership.HOME); // Concurrent
                                                                                       // not
                                                                                       // currently
                                                                                       // supported
                                    patron.setEnrollStatus(EnrollStatus.ENROLLED); // Teaching at
                                                                                   // this school
                                    patron.setStatus(Status.ACTIVE);
                                    patron.setSchoolNum(m_schoolSites.get(school.getOid()));
                                    List<Method> patronMethodList = Arrays.asList(patronMethods);

                                    // Add all fields
                                    for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                        FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                        String fieldValue = entity.getFieldValue(pos);

                                        // If the value requires padding, pad it and trim it to
                                        // field max length.
                                        fieldValue = ExportFormatManager.doPadding(fieldValue,
                                                (field.getResizeMode() == null
                                                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                        : field.getResizeMode().ordinal()),
                                                field.getPaddingChar(),
                                                field.getExportLength());

                                        if (field.getFieldId().equals("DistrictId")
                                                && StringUtils.isEmpty(fieldValue)) {
                                            logMessage("Skipping " + staff.getNameView()
                                                    + " as empty DistrictId attribute.");
                                            skipPatron = true;
                                        } else if (field.getFieldId().equals("Gender")) {
                                            patron.setGender("M".equals(fieldValue) ? Gender.MALE : Gender.FEMALE);
                                            continue;
                                        }

                                        // We don't want to send password or accesslevel fields
                                        // during updates.
                                        if (updateAction != null &&
                                                (field.getFieldId().equals("Password")
                                                        || field.getFieldId().equals("AccessLevel"))) {
                                            continue;
                                        }

                                        // Set the final value.
                                        for (Method method : patronMethodList) {
                                            if (method.getName().equals("set".concat(field.getFieldId()))) {
                                                try {
                                                    if (field.getFormatter() != null) {
                                                        Object object = null;
                                                        if (field.getFormatter() instanceof X2LogicalFormat) {
                                                            object = field.getFormatter().format(fieldValue);
                                                            object = Boolean.valueOf((String) object);
                                                        } else {
                                                            object = field.getFormatter().parseObject(fieldValue);
                                                        }
                                                        method.invoke(patron, object);
                                                    } else {
                                                        method.invoke(patron, fieldValue);
                                                    }
                                                    break;
                                                } catch (Throwable e) {
                                                    logMessage(e.getMessage());
                                                }
                                            }
                                        }
                                    }
                                    entity.postProcess();
                                } else {
                                    m_initErrors.add(err);
                                }

                                if (!skipPatron) {
                                    DataTransferResult result = sendData(patron);
                                    if (result.getStatus().equals(DataTransferStatus.SUCCESS)) {
                                        success = true;
                                        count++;
                                    }
                                }
                            }

                            if (StringUtils.isEmpty(staffOid)) {
                                logMessage("Staff count: " + count);
                            }
                        } finally {
                            m_reportData.close();
                        }
                    }
                }
            }
        }

        return success;
    }

    /**
     * Writes the access token out to the OrganizationAttributes bean.
     */
    private void setAccessToken() {
        Organization org = getOrganization();

        if (org != null) {
            OrganizationAttributes orgAttrib = null;

            /*
             * Find the OrganizationAttributes record with reporting information matching
             * the SIF extended data dictionary.
             */
            for (OrganizationAttributes attrs : org.getOrganizationAttributes(m_broker)) {
                if (attrs.getExtendedDataDictionary() != null &&
                        ORA_DESTINY_DDX.equals(attrs.getExtendedDataDictionary().getId().trim())) {
                    // Save the bean oid for quick retrieval later.
                    orgAttrib = attrs;
                    break;
                }
            }

            // Find the extended data dictionary for the OrganizationAttributes "ORA-DESTINY-GEN".
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DESTINY_DDX);
            QueryByCriteria query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);

            ExtendedDataDictionary ddx = (ExtendedDataDictionary) m_broker.getBeanByQuery(query);
            if (ddx != null) {
                /*
                 * If there is no Organization Attibutes bean with this ddxOid then we
                 * need to create one and add the oid to the map.
                 */
                if (orgAttrib == null) {
                    orgAttrib = X2BaseBean.newInstance(OrganizationAttributes.class, m_broker.getPersistenceKey());
                    orgAttrib.setExtendedDataDictionaryOid(ddx.getOid());
                    orgAttrib.setOrganizationOid(org.getOid());
                    m_broker.saveBeanForced(orgAttrib);
                }

                /*
                 * Lookup the timestamp field by alias and save the bean path for that fields so we
                 * don't need
                 * to do this lookup each time we update the timestamp.
                 */
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, m_broker.getPersistenceKey());

                if (dictionary != null && orgAttrib != null) {
                    DataDictionaryField accessTokenField =
                            dictionary.findDataDictionaryFieldByAlias(ORA_DESTINY_ACCESSTOKEN);
                    if (accessTokenField != null) {
                        orgAttrib.setFieldValueByBeanPath(accessTokenField.getJavaName(), m_accessToken);
                        m_broker.saveBeanForced(orgAttrib);
                    }
                }
            }
        }
    }

    /**
     * Write the list of sections which needed to be split before sending to Destiny.
     */
    private void writeSplitSections() {
        // Get the organization attributes bean to lookup the event time stamps.
        OrganizationAttributes orgAttrib = (OrganizationAttributes) m_broker.getBeanByOid(OrganizationAttributes.class,
                m_organizationAttributesOid);

        if (orgAttrib != null) {
            List<String> delimitedList = new ArrayList<String>();

            // Iterate through the map that was build appending the pairs.
            for (String key : m_splitSections.keySet()) {
                delimitedList.add(key + "=" + m_splitSections.get(key).toString());
            }

            // Convert the list of pairs into a single string
            String listtoblob = StringUtils.convertCollectionToDelimitedString(delimitedList, ",", null, true);

            // Set the bean field and save the bean
            orgAttrib.setFieldValueByBeanPath(m_splitSectionsField, listtoblob);
            m_broker.saveBeanForced(orgAttrib);
        }
    }

    /**
     * Write the time stamps for the Organization Attributes to the database.
     */
    private void writeTimestamps() {
        // Get the organization attributes bean to lookup the event time stamps.
        OrganizationAttributes orgAttrib = (OrganizationAttributes) m_broker.getBeanByOid(OrganizationAttributes.class,
                m_organizationAttributesOid);

        if (orgAttrib != null) {
            List<String> delimitedList = new ArrayList<String>();

            // Iterate through the map that was build appending the pairs.
            for (String key : m_objectTimeStamps.keySet()) {
                delimitedList.add(key + "=" + m_objectTimeStamps.get(key).toString());
            }

            // Convert the list of pairs into a single string
            String listtoblob = StringUtils.convertCollectionToDelimitedString(delimitedList, ",", null, true);

            // Set the bean field and save the bean
            orgAttrib.setFieldValueByBeanPath(m_timestampsField, listtoblob);
            m_broker.saveBeanForced(orgAttrib);
        }
    }
}
