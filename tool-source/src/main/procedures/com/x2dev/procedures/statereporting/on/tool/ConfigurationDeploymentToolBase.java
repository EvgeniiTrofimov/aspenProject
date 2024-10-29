package com.x2dev.procedures.statereporting.on.tool;
/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2021 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.NotSerializableException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.IterableQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.CalculatedField;
import com.follett.fsc.core.k12.beans.CalculatedFieldMember;
import com.follett.fsc.core.k12.beans.CommentBankTable;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DataRelationship;
import com.follett.fsc.core.k12.beans.DataTable;
import com.follett.fsc.core.k12.beans.DataTableConfig;
import com.follett.fsc.core.k12.beans.DataValidationRule;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ExtendedDataField;
import com.follett.fsc.core.k12.beans.ExtendedDataTable;
import com.follett.fsc.core.k12.beans.FieldSet;
import com.follett.fsc.core.k12.beans.FieldSetMember;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.MessageResource;
import com.follett.fsc.core.k12.beans.OrganizationAttributes;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.SavedFilter;
import com.follett.fsc.core.k12.beans.SavedQuery;
import com.follett.fsc.core.k12.beans.SystemPreference;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.ToolNav;
import com.follett.fsc.core.k12.beans.ToolRole;
import com.follett.fsc.core.k12.beans.ToolSchoolVisibility;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.UserDefinedNavResource;
import com.follett.fsc.core.k12.beans.UserDefinedNavigation;
import com.follett.fsc.core.k12.beans.ViewTemplate;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.AuditTypes.FieldAuditType;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryUtils;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.portability.ObjectImporter;
import com.follett.fsc.core.k12.business.portability.Portable;
import com.follett.fsc.core.k12.business.portability.PortableHelper;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.ToolObjectCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ContextResourceManager;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.nav.NavConfigXMLHandler;
import com.follett.fsc.core.k12.web.template.TemplateConstants;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.beans.UserDefinedTableD;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;

/**
 * Base procedure to extend, included as part of a bundle, for making a variety
 * of configuration changes
 *
 * @author Follett Software Company
 * @copyright 2020
 */
public class ConfigurationDeploymentToolBase extends ProcedureJavaSource {

	private static final long serialVersionUID = -6483516265568569976L;

	public static final String PARAM_DEPLOYMENT_PROCEDURE = "deploymentProcedure";
	public static final String PARAM_DEPLOYMENT_METHOD = "deploymentMethod";
	public static final String PARAM_DEVOPS_TASK = "devOpsTask";
	private static final String DEPLOYMENT_OBC_CHANGE_LOG = "obcChangeLog";

	public static final String ALIAS_ORGANIZATION_LANGUAGE = "all-org-BoardLanguage";
	public static final String ORGANIZATION_LANGUAGE_FRENCH_CODE = "F";

	public static final String PORTABLE_DOCUMENT = "PortableDocument";

	private static final String UPDATE_PROCEDURE_NAME = "_DevOps";

	private static final String CALCULATED_FIELD = "calculated-field";
	private static final String CALCULATED_FIELD_NAME = "name";
	private static final String CALCULATED_FIELD_DESCRIPTION = "description";
	private static final String CALCULATED_FIELD_CALCULATED_EXPRESSION = "calculated-expression";
	private static final String CALCULATED_FIELD_PROCEDURE_ID = "procedure-id";
	private static final String CALCULATED_FIELD_FIELD_OID = "field-oid";
	private static final String CALCULATED_FIELD_FIELD_ALIAS = "field-alias";

	public static final String DATA_TABLE_CONFIG = "data-table-config";
	public static final String DATA_FIELD_CONFIG = "data-field-config";
	public static final String DATA_FIELD_CONFIG_DATA_TABLE_ATTR = "data-table";
	public static final String DATA_FIELD_CONFIG_CALC_FIELD_NAME_ATTR = "calculated-field-name";
	public static final String DATA_FIELD_CONFIG_CHANGE_REF_TABLE_NAME_ATTR = "change-reference-table-name";

	private static final String DATA_VALIDATION_RULE = "data-validation-rule";

	public static final String EXTENDED_DATA_DICTIONARY_ID_ATTR = "extended-data-dictionary-id";

	private static final String FIELD_AUDIT = "field-audit";
	private static final String FIELD_AUDIT_TYPE = "type";
	private static final String FIELD_AUDIT_FIELD_OID = "field-oid";

	public static final String FIELD_SET = "field-set";
	public static final String FIELD_SET_MEMBER = "field-set-member";

	public static final String EXTENDED_DATA_DICTIONARY = "extended-data-dictionary";
	public static final String EXTENDED_DATA_TABLE = "table";
	public static final String EXTENDED_DATA_TABLE_ID_ATTR = "id";
	public static final String EXTENDED_DATA_TABLE_NAME_ATTR = "name";
	public static final String EXTENDED_DATA_FIELD = "field";
	public static final String EXTENDED_DATA_FIELD_AUDIT_TYPE_ATTR = "field-audit-type";
	public static final String EXTENDED_DATA_FIELD_CALC_FIELD_NAME_ATTR = "calculated-field-name";
	public static final String EXTENDED_DATA_FIELD_CONFIG_OID_ATTR = "data-field-config-oid";
	public static final String EXTENDED_DATA_FIELD_LONG_NAME_ATTR = "long-name";
	public static final String EXTENDED_DATA_FIELD_SHORT_NAME_ATTR = "short-name";
	public static final String EXTENDED_DATA_FIELD_DEFAULT_VALUE_ATTR = "default-value";
	public static final String EXTENDED_DATA_FIELD_DEPENDENCY_ATTR = "dependency";
	public static final String EXTENDED_DATA_FIELD_DETAIL_CONTROL_ATTR = "detail-control";
	public static final String EXTENDED_DATA_FIELD_DETAIL_WIDTH_ATTR = "detail-width";
	public static final String EXTENDED_DATA_FIELD_ENABLED_ATTR = "enabled";
	public static final String EXTENDED_DATA_FIELD_LIST_EDIT_ATTR = "list-edit";
	public static final String EXTENDED_DATA_FIELD_LIST_WIDTH_ATTR = "list-width";
	public static final String EXTENDED_DATA_FIELD_LOCALIZED_ATTR = "localized";
	public static final String EXTENDED_DATA_FIELD_READ_ONLY_ATTR = "read-only";
	public static final String EXTENDED_DATA_FIELD_REQUIRED_ATTR = "required";
	public static final String EXTENDED_DATA_FIELD_SEQUENCE_NUMBER_ATTR = "sequence-number";
	public static final String EXTENDED_DATA_FIELD_PICKLIST_FIELDS_ATTR = "picklist-fields";
	public static final String EXTENDED_DATA_FIELD_RECORD_LEVEL_SECURITY_ATTR = "record-level-security";
	public static final String EXTENDED_DATA_FIELD_REF_TABLE_OID_ATTR = "reference-table-oid";
	public static final String EXTENDED_DATA_FIELD_SPELL_CHECK_ATTR = "spell-check";
	public static final String EXTENDED_DATA_FIELD_USER_DECIMAL_ATTR = "decimal";
	public static final String EXTENDED_DATA_FIELD_USER_LENGTH_ATTR = "length";
	public static final String EXTENDED_DATA_FIELD_USER_TYPE_ATTR = "type";
	public static final String EXTENDED_DATA_FIELD_UPDATE_ATTR = "update";
	public static final String EXTENDED_DATA_FIELD_VALID_REFERENCE_ONLY_INDICATOR_ATTR = "valid-reference-only";

	public static final String MESSAGE_RESOURCE = "resource";

	private static final String REFERENCE_TABLE = "reference-table";
	private static final String REFERENCE_TABLE_NAME_ATTR = "user-name";
	private static final String REFERENCE_TABLE_CODE_LENGTH_ATTR = "code-length";
	private static final String REFERENCE_TABLE_SEQUENCE_ORDER_ATTR = "sequence-order";
	private static final String REFERENCE_TABLE_EXTENDED_DICTIONARY_ID_ATTR = "ext-dictionary-id";
	private static final String REFERENCE_CODE = "reference-code";
	private static final String REFERENCE_CODE_SEQUENCE_NUMBER_ATTR = "sequence-number";
	private static final String REFERENCE_CODE_DEPENDENCY_ATTR = "dependency-code";
	private static final String REFERENCE_CODE_DISABLED_ATTR = "disabled";
	private static final String REFERENCE_CODE_LOCAL_CODE_ATTR = "local-code";
	private static final String REFERENCE_CODE_STATE_CODE_ATTR = "state-code";
	private static final String REFERENCE_CODE_FEDERAL_CODE_ATTR = "federal-code";
	private static final String REFERENCE_CODE_SYSTEM_CODE_ATTR = "system-code";
	private static final String REFERENCE_CODE_EDFI_CODE_ATTR = "edfi-code";
	private static final String REFERENCE_CODE_SIF_CODE_ATTR = "sif-code";
	private static final String REFERENCE_CODE_EXTENDED_DICTIONARY_ID_ATTR = "ext-dictionary-id";
	private static final String REFERENCE_CODE_TEMPLATE_CONTEXT_ATTR = "template-context";
	private static final String REFERENCE_CODE_CUSTOM_FIELD_ALIAS_ATTR = "field-alias";

	public static final String SAVED_FILTER_QUERY = "saved-filter-query";
	public static final String SAVED_FILTER = "saved-filter";
	public static final String SAVED_QUERY = "saved-query";

	public static final String SQL_STATEMENT = "sql";
	public static final String SQL_SELECT = "select";
	public static final String SQL_UPDATE = "update";
	public static final String SQL_STORED_PROCEDURE = "stored-procedure";
	public static final String SQL_CLEAR_CACHE = "clear-cache";
	public static final String SQL_UPDATE_MESSAGE_ATTR = "message";

	public static final String TOOL_BYTE_CODE_IMPORT_EXPORT_DEFINITION = "import-export-definition";
	public static final String TOOL_BYTE_CODE_PROCEDURE = "procedure";
	public static final String TOOL_BYTE_CODE_REPORT = "report";
	public static final String TOOL_BYTE_CODE_RUN_TOOL = "runTool";
	public static final String TOOL_BYTE_CODE_DELETE_AFTER_RUN = "deleteAfterRun";

	public static final String VIEW_TEMPLATE = "view-template";
	public static final String VIEW_TEMPLATE_OVERRIDE_CUSTOM_ATTR = "override-custom";
	public static final String VIEW_TEMPLATE_DEFAULT_TEMPLATE_ATTR = "default-template";
	public static final String VIEW_TEMPLATE_PREVIOUS_NAME_ATTR = "previous-name";
	public static final String VIEW_TEMPLATE_TEMPLATE_DESCRIPTION = "template-description";

	public static final String XML_ENCODING_STRING = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";

	private static final String USER_DEFINED_NAVIGATION = "user-defined-navigation";
	private static final String USER_DEFINED_NAV_DEFINITION = "navdefinition";
	private static final String USER_DEFINED_NAV_RESOURCE = "user-defined-nav-resource";

	private static final String WORK_FLOW_PHASE = "workflow-phase";
	private static final String WORK_FLOW_PHASE_UPDATE = "update-phase";
	private static final String WORK_FLOW_PHASE_ACTION = "action";

	// Duplicate of Ontario Alias in order to not risk deploying that procedure with
	// each bundle
	private static final String EXT_ID_ONTARIO_PREFERENCES = "ON-PREFERENCES";
	private static final String OID_ONTARIO_PREFERENCES = "oraOntarioPref";
	private static final String ALIAS_BOARD_LANGUAGE_ONTARIO_PREF = "ora-pref-board-language";

	// Used when importing DataFieldConfig and ExtendedDataField configuration
	private enum DataFieldType {
		CHAR, INT, BOOLEAN
	}

	public static List<String> SKIP_ATTRIBUTES_LIST = Arrays.asList(X2BaseBean.COL_OID,
			X2BaseBean.COL_LAST_MODIFIED_TIME, DataFieldConfig.COL_SEQUENCE_NUMBER,
			DataFieldConfig.COL_ORGANIZATION1_OID, DataFieldConfig.COL_ORGANIZATION2_OID,
			DataFieldConfig.COL_ORGANIZATION3_OID, DataFieldConfig.COL_ORGANIZATION4_OID,
			DataFieldConfig.COL_ORGANIZATION5_OID);

	private LinkedHashMap<String, DataFieldConfig> m_existingConfigs;

	public SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

	// Constants for use in subclasses
	public static final String FILTER_CONTEXT_SCHOOL = ".school";
	public static final String FILTER_CONTEXT_ORG1 = ".root";

	public boolean m_deleteAfterRun = true;
	public DataDictionary m_dictionary;
	public boolean m_errors = false;
	private boolean m_isBoardLanguageFrench = false;
	private StringBuilder m_messages = new StringBuilder();
	public boolean m_reloadDataDictionary = false;
	public boolean m_reloadLocalizationCache = false;
	public boolean m_reloadNavigation = false;
	public boolean m_reloadProcedureCache = false;
	public boolean m_reloadObjectCache = false;
	public UserDataContainer m_userData;
	private ObcChangeLog m_obcChangeLog;

	/**
	 * Class to create an OBC Change Log record, either in OBC (to be deployed) on
	 * for a customer site (what was deployed)
	 */
	public class ObcChangeLog {
		// OBC Configuration Change Log
		public static final String EXTENDED_DICTIONARY_OID = "ddxOnCfgChgLog";
		public static final String ALIAS_BUNDLE = "cfg-udd-bundle";
		public static final String ALIAS_BUNDLE_HASH = "cfg-udd-bundle-hash";
		public static final String ALIAS_CREATE_DATE = "cfg-udd-create-date";
		public static final String ALIAS_DESCRIPTION = "cfg-udd-description";
		public static final String ALIAS_DEPLOYED_DATE = "cfg-udd-deployed-date";
		public static final String ALIAS_DEVOPS_TASK = "cfg-udd-devops-task";
		public static final String ALIAS_LOG = "cfg-udd-log";
		public static final String ALIAS_STATUS = "cfg-udd-status";
		public static final String ALIAS_SUMMARY = "cfg-udd-summary";
		public static final String ALIAS_SYNC_START = "cfg-udd-sync-start";
		public static final String ALIAS_SYNC_END = "cfg-udd-sync-end";
		public static final String ALIAS_SYNC_DURATION = "cfg-udd-duration";
		public static final String ALIAS_SYNC_USER = "cfg-udd-sync-user";
		public static final String ALIAS_TICKETS_RESOLVED = "cfg-udd-tickets-resolved";

		private static final String STATUS_FAILED = "Failed";
		public static final String STATUS_SUCCEEDED = "Succeeded";
		private static final String STATUS_COMPLETED_WITH_ERRORS = "Completed with errors";

		private static final String RESERVED_OID = "CL";

		private Converter dateConverter;
		private SimpleDateFormat datetimeConverter;
		private UserDefinedTableD changeLog;
		private DataDictionary dictionary = null;
		private StringBuilder log;
		private boolean errors = false;
		private boolean newRecord = false;
		private long startTime;
		private X2Broker broker;

		public ObcChangeLog(X2Broker broker) {
			this.broker = broker;
			ExtendedDataDictionary extendedDataDictionary = this.broker.getBeanByOid(ExtendedDataDictionary.class,
					EXTENDED_DICTIONARY_OID);
			if (extendedDataDictionary != null) {
				dictionary = DataDictionary.getDistrictDictionary(extendedDataDictionary,
						this.broker.getPersistenceKey());
			}
			dateConverter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), "yyyy-MM-dd");
			datetimeConverter = new SimpleDateFormat("dd-MMM-yyyy hh:mm:ss aa");
			log = new StringBuilder();
			changeLog = null;
		}

		/**
		 * Final save of change log and set bean to null
		 */
		public void closeForBundle() {
			if (changeLog != null) {
				broker.saveBeanForced(changeLog);
				// Set change log to null to prevent any further operations
				changeLog = null;
			}
		}

		/**
		 * Store results for the import, including end time, duration, status, and log
		 * Set bean to null to prevent further updates
		 */
		public void closeForImport() {
			if (changeLog != null) {
				if (dictionary == null) {
					AppGlobals.getLog().info("OBC Change Log dictionary is null");
				}
				changeLog.setFieldValueByAlias(ALIAS_LOG, log.toString(), dictionary);
				changeLog.setFieldValueByAlias(ALIAS_SYNC_END, datetimeConverter.format(new java.util.Date()),
						dictionary);
				changeLog.setFieldValueByAlias(ALIAS_SYNC_DURATION,
						String.valueOf(System.currentTimeMillis() / 1000 - startTime),
						dictionary);
				changeLog.setFieldValueByAlias(ALIAS_STATUS, STATUS_SUCCEEDED, dictionary);
				if (!hasErrors()) {
					changeLog.setFieldValueByAlias(ALIAS_STATUS, STATUS_SUCCEEDED, dictionary);
				} else {
					changeLog.setFieldValueByAlias(ALIAS_STATUS, STATUS_COMPLETED_WITH_ERRORS, dictionary);
				}
				broker.saveBeanForced(changeLog);
				// Set change log to null to prevent any further operations
				changeLog = null;
			}
		}

		/**
		 * Returns whether this record is new
		 *
		 * @return
		 */
		public boolean isNew() {
			return newRecord;
		}

		/**
		 * Save the log as we go so if the import fails catastrophically the last thing
		 * to succeed will be recorded
		 *
		 * @param logMessage
		 */
		public void logMessage(String logMessage) {
			if (changeLog != null) {
				log.append(logMessage);
				changeLog.setFieldValueByAlias(ALIAS_LOG, log.toString());
				broker.saveBeanForced(changeLog);
			}
		}

		/**
		 * Opens a new change log entry for creating a deployment bundle
		 *
		 * @param devOpsTask
		 * @param broker
		 * @return
		 */
		public boolean openForBundle(String devOpsTask) {
			boolean opened = false;
			if (dictionary != null && !StringUtils.isBlank(devOpsTask)) {
				devOpsTask = devOpsTask.toUpperCase().replace("_DEVOPS", "");
				// Look for existing record by OID and by devops task
				if (!StringUtils.isBlank(devOpsTask)) {
					changeLog = getChangeLog(null, devOpsTask);

					if (changeLog == null) {
						// This bundle hasn't been created, so create a new one
						String oid = broker.generateOid(UserDefinedTableD.class);
						// Modify to use the reserved oid so it is unique across environments
						oid = oid.substring(0, 3) + RESERVED_OID + oid.substring(5, oid.length());
						changeLog = createChangeLog(oid);
						changeLog.setOrganization1Oid("*dst");
						changeLog.setExtendedDataDictionaryOid(EXTENDED_DICTIONARY_OID);
						changeLog.setFieldValueByAlias(ALIAS_DEVOPS_TASK, devOpsTask, dictionary);
						changeLog.setFieldValueByAlias(ALIAS_CREATE_DATE, (new PlainDate()).toString(), dictionary);
					}
				}
				if (changeLog != null) {
					changeLog.setFieldValueByAlias(ALIAS_DEVOPS_TASK, devOpsTask, dictionary);
					String createDateString = (String) changeLog.getFieldValueByAlias(ALIAS_CREATE_DATE,
							dictionary);
					if (StringUtils.isBlank(createDateString)) {
						changeLog.setFieldValueByAlias(ALIAS_CREATE_DATE, (new PlainDate()).toString(), dictionary);
					}
					// Set status to failed in case of catastrophic failure
					broker.saveBeanForced(changeLog);
					opened = true;
				}
			}
			return opened;
		}

		/**
		 * Opens a new change log entry for logging an import/deployment (in target DB)
		 *
		 * @param oid             - required
		 * @param devOpsTask      - required
		 * @param summary
		 * @param description
		 * @param ticketsResolved
		 * @param username
		 * @param localChangeLog
		 * @param broker
		 * @return
		 */
		public boolean openForImport(String oid, String devOpsTask, String summary, String description,
				String ticketsResolved, String bundleHash, String username, UserDefinedTableD localChangeLog) {
			boolean opened = false;
			if (dictionary != null && !StringUtils.isBlank(devOpsTask)) {
				devOpsTask = devOpsTask.toUpperCase().replace("_DEVOPS", "");

				// Change log provided by ASAPI client
				changeLog = localChangeLog;

				// Look for existing record by OID and by devops task
				if (changeLog == null) {
					changeLog = getChangeLog(oid, devOpsTask);
				}

				if (changeLog == null) {
					// This task hasn't been imported, so create a new change log
					changeLog = createChangeLog(oid);
					changeLog.setOrganization1Oid("*dst");
					changeLog.setExtendedDataDictionaryOid(EXTENDED_DICTIONARY_OID);
					changeLog.setFieldValueByAlias(ALIAS_DEVOPS_TASK, devOpsTask, dictionary);
				}

				changeLog.setFieldValueByAlias(ALIAS_DEVOPS_TASK, devOpsTask, dictionary);
				if (!StringUtils.isBlank(summary)) {
					summary = summary.replaceAll("(?i)" + Pattern.quote("_devops"), "").trim();
				}
				changeLog.setFieldValueByAlias(ALIAS_SUMMARY, summary, dictionary);
				changeLog.setFieldValueByAlias(ALIAS_DESCRIPTION, description, dictionary);
				changeLog.setFieldValueByAlias(ALIAS_TICKETS_RESOLVED, ticketsResolved, dictionary);
				changeLog.setFieldValueByAlias(ALIAS_DEPLOYED_DATE, dateConverter.javaToString(new PlainDate()),
						dictionary);
				changeLog.setFieldValueByAlias(ALIAS_SYNC_USER, username, dictionary);
				// Set status to failed in case of catastrophic failure
				changeLog.setFieldValueByAlias(ALIAS_STATUS, STATUS_FAILED, dictionary);
				setErrors(false);
				changeLog.setFieldValueByAlias(ALIAS_BUNDLE_HASH, bundleHash, dictionary);
				changeLog.setFieldValueByAlias(ALIAS_SYNC_START, datetimeConverter.format(new java.util.Date()),
						dictionary);
				startTime = System.currentTimeMillis() / 1000;
				// If there's an existing log for this record, add a separator
				String existingLog = (String) changeLog.getFieldValueByAlias(ALIAS_LOG, dictionary);
				if (!StringUtils.isBlank(existingLog)) {
					log.append(existingLog);
				}
				log.append("\n===================================================================\n");
				log.append("Deployment log started at " + datetimeConverter.format(new java.util.Date()));
				log.append("\n===================================================================\n");
				changeLog.setFieldValueByAlias(ALIAS_LOG, log.toString(), dictionary);
				broker.saveBeanForced(changeLog);
				opened = true;
			}
			return opened;
		}

		/**
		 * Use SQL to create the change log so we can use the provided oid
		 *
		 * @param oid
		 * @return
		 */
		private UserDefinedTableD createChangeLog(String oid) {
			String sql = "INSERT INTO USER_DEFINED_TABLE_D (UDD_OID) VALUES ('" + oid + "')";
			Connection connection = null;
			try {
				connection = broker.borrowConnection();
				PreparedStatement statement = connection.prepareStatement(sql);
				statement.executeUpdate();
				newRecord = true;
			} catch (SQLException sqle) {
				logMessage(sqle.toString());
				setErrors(true);
			} finally {
				if (connection != null) {
					broker.returnConnection();
				}
			}
			return broker.getBeanByOid(UserDefinedTableD.class, oid);
		}

		/**
		 * Returns a change log bean, searching first by oid and then by devops task
		 *
		 * @param oid
		 * @param devOpsTask
		 * @return
		 */
		private UserDefinedTableD getChangeLog(String oid, String devOpsTask) {
			if (oid != null) {
				changeLog = broker.getBeanByOid(UserDefinedTableD.class, oid);
			}
			if (changeLog == null) {
				DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DEVOPS_TASK);
				if (field != null) {
					X2Criteria criteria = new X2Criteria();
					criteria.addEqualTo(UserDefinedTableD.COL_EXTENDED_DATA_DICTIONARY_OID, EXTENDED_DICTIONARY_OID);
					criteria.addEqualTo(field.getJavaName(), devOpsTask);
					BeanQuery query = new BeanQuery(UserDefinedTableD.class, criteria);
					query.addOrderByDescending(X2BaseBean.COL_OID);
					changeLog = broker.getBeanByQuery(query);
				}
			}

			return changeLog;
		}

		private boolean hasErrors() {
			return errors;
		}

		private void setBundle(String newContent, boolean addToTop) {
			if (changeLog != null) {
				String bundle = (String) changeLog.getFieldValueByAlias(ALIAS_BUNDLE, dictionary);
				if (StringUtils.isBlank(bundle)) {
					bundle = deploymentProcedureInputStart + deploymentProcedureInputEnd;
				}
				if (addToTop) {
					// Add new content first (e.g., Reference Tables)
					bundle = bundle.replace("<" + PORTABLE_DOCUMENT + ">",
							"<" + PORTABLE_DOCUMENT + ">\n\n" + newContent + "\n");
				} else {
					// Add new content at the bottom
					bundle = bundle.replace("</" + PORTABLE_DOCUMENT + ">",
							"\n" + newContent + "\n</" + PORTABLE_DOCUMENT + ">");
				}
				changeLog.setFieldValueByAlias(ALIAS_BUNDLE, bundle, dictionary);
			}
		}

		private void setErrors(boolean error) {
			if (changeLog != null) {
				errors = error;
			}
		}
	}

	/**
	 * The SqlImport class facilitates importing with SQL rather than Java beans.
	 * This method bypasses all triggers including beforeSave, afterSave, and
	 * calculated field procedures, so it must be used with care.
	 */
	private class SqlImport {
		private Map<String, String> sqlFields = new LinkedHashMap<String, String>();
		private DataDictionary dictionary;
		private String objectPrefix;
		private Class beanClass;
		private String databaseTableName;
		private String oid;
		private byte[] bytes;
		private boolean insert = false;

		/**
		 * Instantiates a new tool byte code import object
		 *
		 * @param objectPrefix
		 * @param className
		 * @param broker
		 */
		public SqlImport(String objectPrefix, Class beanClass, DataDictionary dictionary, X2Broker broker) {
			super();
			this.dictionary = dictionary;
			this.objectPrefix = objectPrefix;
			this.beanClass = beanClass;
			this.databaseTableName = dictionary.findDataDictionaryTableByPrefix(objectPrefix).getDatabaseName();
		}

		/**
		 * Adds a field / boolean value to be imported
		 *
		 * @param beanPath
		 * @param value
		 */
		public void addField(String beanPath, boolean value) {
			addFormattedField(beanPath, value ? "1" : "0");
		}

		/**
		 * Adds a field / int value to be imported
		 *
		 * @param beanPath
		 * @param value
		 */
		public void addField(String beanPath, int value) {
			addFormattedField(beanPath, String.valueOf(value));
		}

		/**
		 * Adds a field / long value to be imported
		 *
		 * @param beanPath
		 * @param value
		 */
		public void addField(String beanPath, long value) {
			addFormattedField(beanPath, String.valueOf(value));
		}

		/**
		 * Adds a field / varbinary value to be imported
		 *
		 * @param beanPath
		 * @param bytes
		 */
		public void addField(String beanPath, byte[] bytes) {
			this.bytes = bytes;
			addFormattedField(beanPath, "?");
		}

		/**
		 * Adds a field / string value to be imported
		 *
		 * @param beanPath
		 * @param value
		 */
		public void addField(String beanPath, String value) {
			if (!StringUtils.isBlank(value)) {
				value = "'" + value.replace("'", "''") + "'";
			} else {
				value = null;
			}
			addFormattedField(beanPath, value);
		}

		/**
		 * Sets all attributes on the bean by iterating over its fields
		 *
		 * @param element
		 * @param bean
		 * @param beanClass
		 * @param beanPathsToSkip
		 */
		public void addFields(Element element, Class beanClass, boolean includeOid,
				List<String> beanPathsToSkip) {
			Set<String> pathsToSkip;
			if (beanPathsToSkip == null) {
				pathsToSkip = new HashSet<String>();
			} else {
				pathsToSkip = new HashSet<String>(beanPathsToSkip);
			}
			/*
			 * Unless specified, don't include the OID
			 */
			if (!includeOid) {
				pathsToSkip.add(X2BaseBean.COL_OID);
			}
			for (DataDictionaryField field : m_dictionary.getFieldsForContext(beanClass.getCanonicalName())) {
				/*
				 * Loop through all fields for the class unless specifically excluded
				 */
				if (!pathsToSkip.contains(field.getJavaName())) {
					String value = element.getAttributeValue(field.getJavaName());
					if (value != null) {
						if ("L".equals(field.getDatabaseType())) {
							addField(field.getJavaName(), Boolean.parseBoolean(value));
						} else if ("N".equals(field.getDatabaseType())) {
							addField(field.getJavaName(), Integer.parseInt(value));
						} else if (Report.COL_FORMAT.equals(field.getJavaName())
								|| ToolSourceCode.COL_SOURCE_CODE.equals(field.getJavaName())
								|| ToolSourceCode.COL_INPUT_DEFINITION.equals(field.getJavaName())) {
							// For reports and tool source, decode the source (format, Java, input defn)
							addField(field.getJavaName(), new String(Base64.getDecoder().decode(value)));
						} else if (Report.COL_COMPILED_FORMAT.equals(field.getJavaName())
								|| ToolSourceCode.COL_COMPILED_CODE.equals(field.getJavaName())) {
							// For reports and tool source, decode the compiled source (format, Java)
							addField(field.getJavaName(), Base64.getDecoder().decode(value));
						} else {
							addField(field.getJavaName(), value);
						}
					}
				}
			}
		}

		/**
		 * Adds a field / value to be imported
		 *
		 * @param beanPath
		 * @param value
		 */
		public void addFormattedField(String beanPath, String value) {
			String databaseName = null;
			if (X2BaseBean.COL_OID.equals(beanPath)) {
				databaseName = objectPrefix + "_OID";
			} else {
				DataDictionaryField dataDictionaryField = dictionary.findDataDictionaryField(beanClass.getName(),
						beanPath);
				if (dataDictionaryField != null) {
					databaseName = dataDictionaryField.getDatabaseName();
				}
			}

			if (databaseName != null) {
				sqlFields.put(databaseName, String.valueOf(value));
			}
		}

		/**
		 * Returns the bytes
		 */
		public byte[] getBytes() {
			return bytes;
		}

		/**
		 * Returns the oid
		 */
		public String getOid() {
			return oid;
		}

		/**
		 * Generate SQL INSERT or UPDATE for all the fields/values in this object
		 *
		 * @return
		 */
		public String getSqlStatement() {
			// Add the Last modified to the list of fields
			addField(X2BaseBean.COL_LAST_MODIFIED_TIME, System.currentTimeMillis());
			// Generate SQL insert or update
			StringBuilder sqlStatement = new StringBuilder();
			if (insert) {
				sqlStatement.append("INSERT INTO " + databaseTableName + " (");
				sqlStatement.append(String.join(",", sqlFields.keySet()));
				sqlStatement.append(") VALUES (");
				sqlStatement.append(String.join(",", sqlFields.values()));
				sqlStatement.append(");");
			} else {
				sqlStatement.append("UPDATE " + databaseTableName + " SET ");
				int count = 0;
				for (Map.Entry<String, String> entry : sqlFields.entrySet()) {
					if (count > 0) {
						sqlStatement.append(", ");
					}
					sqlStatement.append(entry.getKey() + " = " + entry.getValue());
					count++;
				}
				sqlStatement.append(" WHERE " + objectPrefix + "_OID = '" + oid + "'");
			}
			return sqlStatement.toString();
		}

		/**
		 *
		 * @param beanPath
		 * @param oid
		 */
		public void setInsert(String beanPath, String oid) {
			insert = true;
			this.oid = oid;
			addField(beanPath, oid);
		}

		/**
		 * Sets the
		 *
		 * @param oid
		 */
		public void setUpdate(String oid) {
			insert = false;
			this.oid = oid;
		}
	}

	@Override
	protected void afterExecute() {
		// If set in this class or subclasses, execute reloads
		if (m_reloadObjectCache) {
			reloadObjectCache();
		}
		if (m_reloadDataDictionary) {
			reloadDataDictionary();
		}
		if (m_reloadLocalizationCache) {
			reloadLocalizationCache();
		}
		if (m_reloadNavigation) {
			reloadNavigation();
		}
		if (m_reloadProcedureCache) {
			reloadProcedureCache();
		}

		super.logMessage(m_messages.toString());

		if (m_deleteAfterRun) {
			// Remove this procedure
			getBroker().deleteBeanByOid(Procedure.class, this.getJob().getTool().getOid());
			logMessage("\nCompleted successfully. This procedure has been removed");
		}
	}

	/**
	 * @see com.follett.fsc.core.k12.tools.ToolJavaSource#beforeExecute()
	 */
	@Override
	protected void beforeExecute() {
		m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
		m_obcChangeLog = new ObcChangeLog(getBroker());
		m_isBoardLanguageFrench = ORGANIZATION_LANGUAGE_FRENCH_CODE.equals(getBoardLanguage());
	}

	@Override
	protected void execute() throws Exception {
		/*
		 * Implemented in subclasses
		 */
	}

	/**
	 * Gets the document from the parent
	 *
	 * @param parent   Element
	 * @param rootName String
	 * @return Element
	 * @throws JDOMException exception
	 * @throws IOException   Signals that an I/O exception has occurred.
	 */
	protected Element getParentDocument(Element parent, String rootName) throws JDOMException, IOException {
		Element document = null;
		if (parent.getChildren() != null && parent.getChildren().size() > 0) {
			Iterator<Element> children = parent.getChildren().iterator();
			while (children.hasNext()) {
				Element element = children.next();
				String name = element.getName();
				if (rootName.equals(name)) {
					document = element;
					break;
				}
			}
		}
		return document;
	}

	/**
	 * Imports portables as defined in the input definition XML (see end of this
	 * class for example config)
	 *
	 * @throws Excpetion
	 */
	protected void importPortables() throws Exception {
		Procedure procedure = (Procedure) this.getJob().getTool();
		if (procedure != null) {
			String inputDefinition = procedure.getFullInputDefinition();
			importPortables(inputDefinition);
		} else {
			logErrorMessage("Not able to find XML root in input definition");
		}
	}

	/**
	 * Imports portables as defined in the portablesDefinition
	 *
	 * @param bundleDefinition
	 * @param oid
	 * @param devOpsTask
	 * @param summary
	 * @param description
	 * @param ticketsResolved
	 * @param username
	 * @param localChangeLog
	 * @throws Exception
	 */
	protected void importPortables(String bundleDefinition, String oid, String devOpsTask, String summary,
			String description, String ticketsResolved, String bundleHash,
			String username, UserDefinedTableD localChangeLog, boolean doNotLog) throws Exception {
		if (!doNotLog) {
			m_obcChangeLog.openForImport(oid, devOpsTask, summary, description, ticketsResolved, bundleHash, username,
					localChangeLog);
		}
		importPortables(bundleDefinition);
		m_obcChangeLog.closeForImport();
	}

	/**
	 * Imports portables as defined in the portables definition string
	 *
	 * @param portablesDefinition
	 * @throws JDOMException
	 * @throws IOException
	 */
	protected void importPortables(String portablesDefinition) throws JDOMException, IOException {
		Element root = getXmlRoot(portablesDefinition);
		if (root != null) {
			/*
			 * Create list of tools that need to be executed after import is done
			 */
			Map<Tool, Boolean> toolsToRun = new LinkedHashMap<Tool, Boolean>();

			// Initialize objects needed for different imports
			Map<String, String> nameToIdMap = new HashMap();
			nameToIdMap.put("assessment-definition", "tblAssessDef");

			Element parent = getParentDocument(root, PORTABLE_DOCUMENT);
			if (parent != null) {
				Iterator<Element> children = parent.getChildren().iterator();
				while (children.hasNext()) {
					Element element = children.next();
					String name = element.getName();
					String tableId = nameToIdMap.get(name);
					DataDictionaryTable table = m_dictionary.findDataDictionaryTableById(tableId);
					if (table != null) {
						importTable(element, name, tableId, table);
					} else if (CALCULATED_FIELD.equals(name)) {
						importCalculatedField(element, m_dictionary);
					} else if (DATA_VALIDATION_RULE.equals(name)) {
						importDataValidationRule(element);
					} else if (DATA_TABLE_CONFIG.equals(name)) {
						importDataTableConfig(element);
					} else if (EXTENDED_DATA_DICTIONARY.equals(name)) {
						importExtendedDataDictionary(element);
					} else if (FIELD_AUDIT.equals(name)) {
						importDataFieldAudit(element);
					} else if (FIELD_SET.equals(name)) {
						importFieldSet(element);
					} else if (MESSAGE_RESOURCE.equals(name)) {
						importMessageResource(element);
					} else if (REFERENCE_TABLE.equals(name)) {
						importReferenceTable(element);
					} else if (SAVED_FILTER_QUERY.equals(name)) {
						importSavedFilterQuery(element);
					} else if (SQL_STATEMENT.equalsIgnoreCase(name)) {
						importSqlAndExecute(element);
					} else if (TOOL_BYTE_CODE_IMPORT_EXPORT_DEFINITION.equals(name)) {
						importToolByteCode(ImportExportDefinition.class, ImportExportDefinition.OBJECT_PREFIX,
								element, toolsToRun);
					} else if (TOOL_BYTE_CODE_PROCEDURE.equals(name)) {
						importToolByteCode(Procedure.class, Procedure.OBJECT_PREFIX, element, toolsToRun);
					} else if (TOOL_BYTE_CODE_REPORT.equals(name)) {
						importToolByteCode(Report.class, Report.OBJECT_PREFIX, element, toolsToRun);
					} else if (VIEW_TEMPLATE.equals(name)) {
						importViewTemplate(element);
					} else if (USER_DEFINED_NAVIGATION.equals(name)) {
						importUserDefinedNavigation(element);
					} else if (WORK_FLOW_PHASE.equals(name)) {
						updateWorkflowPhase(element);
					}
				}

				// Reload data dictionary
				if (m_reloadDataDictionary) {
					/*
					 * Call these immediately in case other code needs to lookup new fields by alias
					 */
					reloadObjectCache();
					reloadDataDictionary();
					m_reloadDataDictionary = false;
				}
			} else {
				logErrorMessage("Unable to import Portables..." + PORTABLE_DOCUMENT + " element not found");
			}

			/*
			 * Run any tools
			 */
			for (Map.Entry<Tool, Boolean> toolEntry : toolsToRun.entrySet()) {
				runTool(toolEntry.getKey(), toolEntry.getValue());
			}
		} else {
			logErrorMessage("Unable to import Portables...no root element");
		}
	}

	/**
	 * Release resources.
	 *
	 * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#releaseResources()
	 */
	@Override
	protected void releaseResources() {
		super.releaseResources();
		clearLocallyDeclaredVariables(this.getClass());
	}

	/**
	 * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState()
	 */
	@Override
	protected void saveState(UserDataContainer userData) {
		m_userData = userData;
	}

	/**
	 * Adds a field to a field set
	 *
	 * @param context
	 * @param name
	 * @param fieldOid
	 * @param fieldType
	 * @param relation
	 * @param sequence
	 * @param width
	 */
	public void addFieldToFieldSet(String context, String name, String fieldOid, int fieldType, String relation,
			int sequence, int width) {
		FieldSet fieldSet = getFieldSet(context, name, null);
		FieldSetMember fieldSetMember = getFieldSetMember(fieldSet.getOid(), fieldOid);
		if (fieldSetMember == null) {
			fieldSetMember = X2BaseBean.newInstance(FieldSetMember.class, getBroker().getPersistenceKey());
			fieldSetMember.setFieldSetOid(fieldSet.getOid());
			fieldSetMember.setObjectOid(fieldOid);
		}
		fieldSetMember.setRelation(relation);
		fieldSetMember.setFieldType(fieldType);
		fieldSetMember.setSequenceNumber(sequence);
		fieldSetMember.setWidth(width);
		if (fieldSetMember.isDirty()) {
			getBroker().saveBeanForced(fieldSetMember);
		}

		logMessage("Added " + fieldOid + " to " + name + " for context '" + context + "'");
	}

	/**
	 * Creates/Updates a calculated field
	 *
	 * @param calculatedFieldName        - Calculated field name
	 * @param calculatedFieldDescription - Calculated field description (optional)
	 * @param calculatedExpression       - Calculated field expression (optional)
	 * @param procedureId                - ID of calculated field procedure
	 *                                   (optional)
	 * @param triggerFields              - Comma-separated list of field OIDs that
	 *                                   will trigger the calculated field
	 * @param fieldsToUpdate             - Comma-separated list of field OIDs that
	 *                                   will be updated by the calculated field
	 *                                   (optional)
	 */
	public void createCalculatedField(String calculatedFieldName, String calculatedFieldDescription,
			String calculatedExpression, String procedureId, String triggerFields, String fieldsToUpdate) {
		boolean newBean = false;
		String procedureOid = getProcedureById(procedureId).getOid();

		// Create/update the calculated field.
		CalculatedField calculatedField = getCalculatedField(calculatedFieldName);
		if (calculatedField == null) {
			calculatedField = X2BaseBean.newInstance(CalculatedField.class, getBroker().getPersistenceKey());
			newBean = true;
		}
		calculatedField.setName(calculatedFieldName);
		calculatedField.setDescription(calculatedFieldDescription);
		calculatedField.setOrganization1Oid(getOrganization().getOid());
		calculatedField.setCalculatedExpression(calculatedExpression);
		calculatedField.setProcedureOid(procedureOid);

		// Bypass afterSave to ignore error on saving a new CLC bean without an
		// expression
		if (calculatedField.isDirty()) {
			getBroker().saveBeanForced(calculatedField, true, true);
		}

		logMessage("Calculated field: " + calculatedFieldName + (newBean ? " inserted." : " updated."));

		// Insert calculated field members.
		List<String> triggerFieldsList = StringUtils.convertDelimitedStringToList(triggerFields, ',', true);

		for (String field : triggerFieldsList) {
			String triggerFieldName;
			DataField fld = getDataFieldByAlias(field);
			if (fld == null) {
				fld = getBroker().getBeanByOid(DataField.class, field);
			}
			if (fld != null) {
				CalculatedFieldMember fieldMember = getCalculatedFieldMember(calculatedField.getOid(), fld.getOid());
				if (fieldMember == null) {
					fieldMember = X2BaseBean.newInstance(CalculatedFieldMember.class,
							getOrganization().getPersistenceKey());
					fieldMember.setCalculatedFieldOid(calculatedField.getOid());
					fieldMember.setDataFieldOid(fld.getOid());
					getBroker().saveBeanForced(fieldMember);
				}

				triggerFieldName = fieldMember.getDataField().getJavaName() + " (Warning - config field not found!)";
				DataFieldConfig fdd = fieldMember.getDataField().getDataFieldConfigs().stream().findFirst().get();
				if (fdd != null) {
					triggerFieldName = fdd.getUserLongName();
				}
			} else {
				triggerFieldName = "ERROR - " + field + " not found!";
				m_errors = true;
			}
			logMessage("   Trigger field: " + triggerFieldName);
		}

		// Set field to be calculated/updated
		List<String> fieldsToUpdateList = StringUtils.convertDelimitedStringToList(fieldsToUpdate, ',', true);
		for (String field : fieldsToUpdateList) {
			String updateFieldName;
			DataField dataField = getDataFieldByAlias(field);
			if (dataField == null) {
				dataField = getBroker().getBeanByOid(DataField.class, field);
			}
			if (dataField != null) {
				updateFieldName = dataField.getJavaName() + " (Warning - config field not found!)";
				DataFieldConfig dataFieldConfig = dataField.getDataFieldConfigs().stream().findFirst().get();
				if (dataFieldConfig != null) {
					dataFieldConfig.setCalculatedFieldOid(calculatedField.getOid());
					if (dataFieldConfig.isDirty()) {
						getBroker().saveBeanForced(dataFieldConfig);
						m_reloadDataDictionary = true;
					}
					updateFieldName = dataFieldConfig.getUserLongName();
				}
			} else {
				updateFieldName = "ERROR - " + field + " not found!";
				m_errors = true;
			}
			logMessage("   Field to be calculated: " + updateFieldName);
		}
	}

	/**
	 * Creates/Updates a data validation rule by dataTableOid
	 *
	 * @param dataTableOid
	 * @param id
	 * @param errorMessage
	 * @param disabled
	 * @param condition
	 * @param expression
	 */
	public void createDataValidationRule(String dataTableOid, String id, String errorMessage, boolean disabled,
			String condition, String expression) {
		createDataValidationRule(dataTableOid, id, errorMessage, disabled, condition, expression, null);
	}

	/**
	 * Creates/Updates a data validation rule by dataTableOid
	 *
	 * @param dataTableOid
	 * @param id
	 * @param errorMessage
	 * @param disabled
	 * @param condition
	 * @param expression
	 * @param comments
	 */
	public void createDataValidationRule(String dataTableOid, String id, String errorMessage, boolean disabled,
			String condition, String expression, String comments) {
		DataTable dataTable = getBroker().getBeanByOid(DataTable.class, dataTableOid);
		if (dataTable != null) {
			String dataTableConfigOid = dataTable.getDataTableConfigs().stream().findFirst().get().getOid();
			createDataValidationRule(dataTableConfigOid, null, id, errorMessage, disabled, condition, expression,
					comments);
		} else {
			logErrorMessage("Can't find System Table for OID " + dataTableOid);
		}
	}

	/**
	 * Creates/Updates a data validation rule
	 *
	 * @param dataTableConfigOid
	 * @param extendedDataDictionaryId
	 * @param id
	 * @param errorMessage
	 * @param disabled
	 * @param condition
	 * @param expression
	 * @param comments
	 */
	public void createDataValidationRule(String dataTableConfigOid, String extendedDataDictionaryId, String id,
			String errorMessage, boolean disabled, String condition, String expression, String comments) {
		try {
			DataValidationRule dataValidationRule = getDataValidationRule(dataTableConfigOid, extendedDataDictionaryId,
					id);
			if (dataValidationRule == null) {
				dataValidationRule = X2BaseBean.newInstance(DataValidationRule.class, getBroker().getPersistenceKey());
				dataValidationRule.setDataTableConfigOid(dataTableConfigOid);
				if (!StringUtils.isBlank(extendedDataDictionaryId)) {

					DataTableConfig dataTableConfig = getBroker().getBeanByOid(DataTableConfig.class,
							dataTableConfigOid);
					if (dataTableConfig != null) {
						ExtendedDataTable extendedDataTable = getExtendedDataTable(extendedDataDictionaryId,
								dataTableConfig.getDataTableOid());
						if (extendedDataTable != null) {
							dataValidationRule.setExtendedDataTableOid(extendedDataTable.getOid());
						} else {
							logErrorMessage("Can't find extended data table for Data Validation Rule: ID = " + id
									+ ", dataTableConfig=" + dataTableConfigOid + ", extendedDictionaryOid="
									+ extendedDataDictionaryId);
						}
					} else {
						logErrorMessage("Can't find data table config for Data Validation Rule: ID = " + id
								+ ", dataTableConfig=" + dataTableConfigOid);
					}
				}
			}
			dataValidationRule.setId(id);
			dataValidationRule.setErrorMessage(errorMessage);
			dataValidationRule.setDisabled(disabled);
			dataValidationRule.setCondition(condition);
			dataValidationRule.setExpression(expression);
			if (!StringUtils.isBlank(comments)) {
				dataValidationRule.setComments(comments);
			}
			if (dataValidationRule.isDirty()) {
				getBroker().saveBeanForced(dataValidationRule);
			}
		} catch (Exception e) {
			logErrorMessage(
					"Exception error occurred creating validation rule:\n" + ExceptionUtils.getFullStackTrace(e));
		}
	}

	/**
	 * Creates a new deployment procedure
	 *
	 * @param devOpsTask
	 * @param newContent
	 * @param addToTop
	 * @throws IOException
	 * @throws X2BaseException
	 * @throws JDOMException
	 */
	public void createDeploymentProcedure(String devOpsTask, String newContent, boolean addToTop)
			throws X2BaseException, IOException, JDOMException {
		// Standard naming convention
		String name = UPDATE_PROCEDURE_NAME + " " + devOpsTask + " Import and Run";
		String id = UPDATE_PROCEDURE_NAME.trim() + devOpsTask.trim();

		// Search for an existing procedure to update
		X2Criteria criteria = new X2Criteria();
		criteria.addContainsIgnoreCase(Procedure.COL_NAME, devOpsTask + " ");
		criteria.addContainsIgnoreCase(Procedure.COL_NAME, UPDATE_PROCEDURE_NAME.replaceAll("_", "").trim());
		BeanQuery query = new BeanQuery(Procedure.class, criteria);

		Procedure procedure = getBroker().getBeanByQuery(query);

		if (procedure == null) {
			// Create a new deployment procedure
			ToolSourceCode toolSourceCode = X2BaseBean.newInstance(ToolSourceCode.class,
					getBroker().getPersistenceKey());
			toolSourceCode.setSourceCode(deploymentProcedureSource);
			toolSourceCode.setExternalSources(deploymentProcedureExternalSource);
			getBroker().saveBeanForced(toolSourceCode);

			// Create new deployment procedure
			procedure = X2BaseBean.newInstance(Procedure.class, getBroker().getPersistenceKey());
			procedure.setName(name);
			procedure.setId(id);
			procedure.setOrganization1Oid(getOrganization().getRootOrganization().getOid());
			procedure.setSourceCodeOid(toolSourceCode.getOid());
			getBroker().saveBeanForced(procedure, true, true);
		}

		// Add newContent to the input definition of this deployment procedure
		ToolSourceCode toolSourceCode = procedure.getSourceCode();
		String content = toolSourceCode.getInputDefinition();
		content = addContentToDeployment(newContent, addToTop, content);

		// Update input definition
		toolSourceCode.setInputDefinition(content);
		if (toolSourceCode.isDirty()) {
			getBroker().saveBeanForced(toolSourceCode, true, true);
			logMessage("Content added to procedure " + procedure.getName());
		}
	}

	/**
	 * Create a new field set for the context, name, members list and extended
	 * dictionary
	 *
	 * @param context
	 * @param fieldSetName
	 * @param fieldSetMembers
	 * @param extendedDictionary
	 */
	public void createFieldSet(String context, String fieldSetName, String fieldSetMembers,
			ExtendedDataDictionary extendedDictionary) {

		FieldSet fieldSet = getFieldSet(context, fieldSetName, extendedDictionary);

		if (fieldSet == null) {
			fieldSet = X2BaseBean.newInstance(FieldSet.class, getBroker().getPersistenceKey());
			fieldSet.setContext(context);
			fieldSet.setName(fieldSetName);
			fieldSet.setOwnerOid(getOrganization().getRootOrganization().getOid());
			fieldSet.setOwnerType(Ownable.OWNER_TYPE_ORG1);
		}
		if (extendedDictionary != null) {
			fieldSet.setExtendedDataDictionaryOid(extendedDictionary.getOid());
		}
		getBroker().saveBeanForced(fieldSet);

		Collection<String> fieldSetMembersList = Arrays.asList(fieldSetMembers.split("\\s*,\\s*"));

		// Use extended data dictionary, if not null, otherwise use the district
		// dictionary
		DataDictionary dictionary = null;
		if (extendedDictionary != null) {
			dictionary = getDistrictDictionaryByDdx(extendedDictionary.getId());
		} else {
			dictionary = m_dictionary;
		}

		// Create all field set members
		int sequence = 0;
		for (String member : fieldSetMembersList) {
			try {
				createFieldSetMember(fieldSet, member, sequence, dictionary);
				sequence++;
			} catch (Exception e) {
				logMessage(
						"Exception error occurred in method createFieldSet():\n" + ExceptionUtils.getFullStackTrace(e));
				m_errors = true;
			}
		}
	}

	/**
	 * Create a field set member
	 *
	 * @param fieldSet
	 * @param member
	 * @param sequence
	 * @param dictionary
	 */
	public void createFieldSetMember(FieldSet fieldSet, String member, int sequence, DataDictionary dictionary) {
		StringBuilder relation = new StringBuilder(8);

		member = splitFieldAndRelation(member, relation);

		DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(member);
		if (field == null) {
			// Look for field by oid
			field = dictionary.findDataDictionaryField(member);
		}

		if (field != null) {
			FieldSetMember fieldSetMember = getFieldSetMember(fieldSet.getOid(), field.getId());
			if (fieldSetMember == null) {
				fieldSetMember = X2BaseBean.newInstance(FieldSetMember.class, getBroker().getPersistenceKey());
				fieldSetMember.setFieldSetOid(fieldSet.getOid());
				fieldSetMember.setObjectOid(field.getId());
				fieldSetMember.setRelation(relation.toString());
				fieldSetMember.setFieldType(0);
				fieldSetMember.setSequenceNumber(sequence);
				getBroker().saveBeanForced(fieldSetMember);
			} else {
				logMessage(" Field " + member + " already exists in field set for " + fieldSet.getContext());
			}
		} else {
			logMessage(" Error: Field not found: " + member + " when creating field set for " + fieldSet.getContext());
			m_errors = true;
		}
	}

	/**
	 * Creates/Updates a saved filter for a given queryOid and context
	 *
	 * @param name      - Name for the filter
	 * @param ownerType - Owner type
	 * @param ownerOid  - Owner oid
	 * @param queryOid  - Query oid
	 * @param context   - Context for the filter
	 */
	public void createFilter(String name, int ownerType, String ownerOid, String queryOid, String context) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(SavedFilter.COL_NAME, name);
		criteria.addEqualTo(SavedFilter.COL_OWNER_TYPE, ownerType);
		BeanQuery query = new BeanQuery(SavedFilter.class, criteria);
		SavedFilter savedFilter = getBroker().getBeanByQuery(query);

		// Create filter for query
		if (savedFilter == null) {
			savedFilter = X2BaseBean.newInstance(SavedFilter.class, getBroker().getPersistenceKey());
		}
		savedFilter.setName(name);
		savedFilter.setContext(context);
		savedFilter.setDefinitionOid(queryOid);
		savedFilter.setDefinitionType(SavedFilter.DEFINITION_TYPE_QUERY);
		savedFilter.setOwnerOid(ownerOid);
		savedFilter.setOwnerType(ownerType);

		if (savedFilter.isDirty()) {
			getBroker().saveBeanForced(savedFilter);
		}
	}

	/**
	 * Creates/Updates a saved query
	 *
	 * @param tableOid        - Table for the query
	 * @param name            - Query name
	 * @param ownerType       - Owner type
	 * @param ownerOid        - Owner oid
	 * @param queryDefinition - Query definition
	 * @return
	 */
	public String createQuery(String tableOid, String name, int ownerType, String ownerOid, String queryDefinition) {
		// Query for saved query to avoid creating duplicates
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(SavedQuery.COL_DATA_TABLE_OID, tableOid);
		criteria.addEqualTo(SavedQuery.COL_NAME, name);
		criteria.addEqualTo(SavedQuery.COL_OWNER_TYPE, ownerType);
		BeanQuery query = new BeanQuery(SavedQuery.class, criteria);
		SavedQuery savedQuery = getBroker().getBeanByQuery(query);

		// Create the query if it doesn't exist
		if (savedQuery == null) {
			savedQuery = X2BaseBean.newInstance(SavedQuery.class, getBroker().getPersistenceKey());
		}
		savedQuery.setDataTableOid(tableOid);
		savedQuery.setName(name);
		savedQuery.setQueryDefinition(queryDefinition);
		savedQuery.setOwnerOid(ownerOid);
		savedQuery.setOwnerType(ownerType);

		if (savedQuery.isDirty()) {
			getBroker().saveBeanForced(savedQuery);
		}

		return savedQuery.getOid();
	}

	/**
	 * Creates/Updates a message resource.
	 *
	 * @param locale - Locale for new message resource
	 * @param key    - Key for new message resource
	 * @param value  - Value for new message resource
	 */
	public void createMessageResource(String locale, String key, String value) {
		createMessageResource(locale, key, value, null);
	}

	/**
	 * Creates/Updates a message resource.
	 *
	 * @param locale    - Locale for new message resource
	 * @param key       - Key for new message resource
	 * @param value     - Value for new message resource
	 * @param objectOid - Oid of related object
	 */
	public void createMessageResource(String locale, String key, String value, String objectOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(MessageResource.COL_LOCALE, locale);
		criteria.addEqualTo(MessageResource.COL_KEY, key);
		BeanQuery query = new BeanQuery(MessageResource.class, criteria);
		MessageResource messageResource = getBroker().getBeanByQuery(query);
		if (messageResource == null) {
			messageResource = X2BaseBean.newInstance(MessageResource.class, getBroker().getPersistenceKey());
		}
		messageResource.setLocale(locale);
		messageResource.setKey(key);
		messageResource.setValue(value);
		if (!StringUtils.isBlank(objectOid)) {
			messageResource.setObjectOid(objectOid);
		}

		if (messageResource.isDirty()) {
			getBroker().saveBeanForced(messageResource);
			m_reloadLocalizationCache = true;
		}
	}

	/**
	 * Creates a message resource for an extended data field
	 *
	 * @param locale        - Locale for new message resource
	 * @param ddxId         - ID for the extended data dictionary
	 * @param alias         - Alias for the extended data field
	 * @param userLongName  - Value for long name of the extended data field in the
	 *                      given locale
	 * @param userShortName - Value for short name of the extended data field in the
	 *                      given locale
	 */
	public void createMessageResourceForExtendedField(String locale, String ddxId, String alias, String userLongName,
			String userShortName) {
		// Get the extended data field by ddxId and alias
		ExtendedDataField extendedDataField = getExtendedDataFieldByAlias(alias, ddxId);

		if (extendedDataField != null) {
			// Create the message resource keys for the long and short names
			String userLongNameKey = "fdx." + extendedDataField.getOid() + ".fdxLongName";
			String userShortNameKey = "fdx." + extendedDataField.getOid() + ".fdxShortName";
			// Create the message resources
			createMessageResource(locale, userLongNameKey, userLongName, extendedDataField.getOid());
			createMessageResource(locale, userShortNameKey, userShortName, extendedDataField.getOid());
		}
	}

	/**
	 * Converts element to XML string.
	 *
	 * @param root Element
	 * @return String
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public String elementToString(Element root) throws IOException {
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		XMLOutputter outputter = new XMLOutputter(Format.getPrettyFormat());
		outputter.output(root, outputStream);
		return outputStream.toString();
	}

	/**
	 * Execute batch statement. Hook for allowing multiple SQL statements to be run
	 * at once, a special statements delimiter is needed. Use this method with
	 * caution as it will clear Aspen cache entirely to ensure all changes take
	 * effect among all nodes.
	 *
	 * @param sql                   String
	 * @param sqlStatementDelimiter String
	 * @throws SQLException exception
	 */
	public void executeBatchStatement(String sql, String sqlStatementDelimiter) throws SQLException {
		ArrayList<String> sqlStatements = StringUtils.convertDelimitedStringToList(sql, sqlStatementDelimiter);

		Connection connection = null;

		try {
			getBroker().beginTransaction();
			connection = getBroker().borrowConnection();

			Statement sqlStatement = connection.createStatement();
			for (String sqlStatementFromInput : sqlStatements) {
				sqlStatement.addBatch(sqlStatementFromInput);
			}

			// READING_QUERY_RESULTS:
			for (int count : sqlStatement.executeBatch()) {
				logMessage("Rows affected: " + count);
			}

			connection.commit();
			logMessage("Batch executed succesfully.");
			// Reloading Aspen cache for all classes as we may have multiple tables
			// affected.
			AppGlobals.getCache(getBroker().getPersistenceKey()).clearAll(true, true, true, false);
		} catch (SQLException sqle) {
			logMessage("ERROR: " + sqle.toString());
			connection.rollback();
		} finally {
			if (connection != null) {
				getBroker().returnConnection();
			}
		}
	}

	/**
	 * Executes the SQL select statement
	 *
	 * @param sql
	 */
	public void executeSqlSelect(String sql) {
		executeSqlSelect(sql, true);
	}

	/**
	 * Executes the SQL select statement, with options to control output
	 *
	 * @param sql
	 * @param hideRowNumber
	 * @return
	 */
	public void executeSqlSelect(String sql, boolean hideRowNumber) {
		Connection connection = null;
		try {
			connection = getBroker().borrowConnection();
			Statement statement = connection.createStatement();
			ResultSet results = statement.executeQuery(sql);

			DataGrid grid = new DataGrid();
			grid.append(results);
			grid.beforeTop();

			/*
			 * Go through the grid and set all the null values to have a string value of
			 * "NULL". Otherwise, it will be impossible to distinguish between blanks and
			 * nulls in the results.
			 */
			List<String> columnHeaders = grid.getColumns();
			while (grid.next()) {
				for (String columnHeader : columnHeaders) {
					if (grid.get(columnHeader) == null) {
						grid.set(columnHeader, "NULL");
					}
				}
			}
			grid.beforeTop();
			logMessage(grid.format(hideRowNumber, false, false, null));

		} catch (SQLException | IllegalArgumentException sqle) {
			logMessage(sqle.toString());
		} finally {
			if (connection != null) {
				getBroker().returnConnection();
			}
		}
	}

	/**
	 * Executes the SQL stored procedure statement, returning whether it succeeded
	 * occur
	 *
	 * @param sql        - Update SQL to execute
	 * @param bytes      - Bytes to write to a varbinary(max) column
	 * @param logMessage - Message to print before the results of the update SQL
	 * @param clearCache - Whether to clear the object cache
	 * @return
	 */
	public void executeSqlStoredProcedure(String sql, String logMessage, boolean clearCache) {
		Connection connection = null;
		CallableStatement statement = null;
		try {
			connection = getBroker().borrowConnection();
			statement = connection.prepareCall("{call " + sql + "}");
			boolean results = statement.execute();
			if (clearCache) {
				m_reloadObjectCache = true;
			}
			if (logMessage != null) {
				Object message = results;
				if (!results) {
					message = statement.getUpdateCount();
				}
				logMessage(logMessage + " " + String.valueOf(message));
			}
		} catch (SQLException sqle) {
			logMessage(sqle.toString());
		} finally {
			if (connection != null) {
				getBroker().returnConnection();
			}
		}
	}

	/**
	 * Executes the SQL update statement
	 *
	 * @param sql
	 * @return
	 */
	public boolean executeSqlUpdate(String sql) {
		return executeSqlUpdate(sql, null, null, true);
	}

	/**
	 * Executes the SQL update statement
	 *
	 * @param sql   - SQL to execute
	 * @param bytes - Bytes to write to a varbinary(max) column
	 * @return
	 */
	public boolean executeSqlUpdate(String sql, byte[] bytes) {
		return executeSqlUpdate(sql, bytes, null, true);
	}

	/**
	 * Executes the SQL update statement, returning whether a rollback needs to
	 * occur
	 *
	 * @param sql        - Update SQL to execute
	 * @param logMessage - Message to print before the results of the update SQL
	 * @return
	 */
	public boolean executeSqlUpdate(String sql, String logMessage) {
		return executeSqlUpdate(sql, null, logMessage, true);
	}

	/**
	 * Executes the SQL update statement, returning whether a rollback needs to
	 * occur
	 *
	 * @param sql        - Update SQL to execute
	 * @param logMessage - Message to print before the results of the update SQL
	 * @param clearCache - Whether to clear the object cache
	 * @return
	 */
	public boolean executeSqlUpdate(String sql, String logMessage, boolean clearCache) {
		return executeSqlUpdate(sql, null, logMessage, clearCache);
	}

	/**
	 * Executes the SQL update statement, returning whether a rollback needs to
	 * occur
	 *
	 * @param sql        - Update SQL to execute
	 * @param bytes      - Bytes to write to a varbinary(max) column
	 * @param logMessage - Message to print before the results of the update SQL
	 * @param clearCache - Whether to clear the object cache
	 * @return
	 */
	public boolean executeSqlUpdate(String sql, byte[] bytes, String logMessage, boolean clearCache) {
		boolean rollback = true;

		Connection connection = null;
		try {
			connection = getBroker().borrowConnection();
			PreparedStatement statement = connection.prepareStatement(sql);
			if (bytes != null) {
				statement.setBytes(1, bytes);
			}
			int count = statement.executeUpdate();
			if (clearCache) {
				m_reloadObjectCache = true;
			}
			if (logMessage != null) {
				logMessage(logMessage + " " + count);
			}
			rollback = false;
		} catch (SQLException sqle) {
			logMessage(sqle.toString());
		} finally {
			if (connection != null) {
				getBroker().returnConnection();
			}
		}

		return rollback;
	}

	/**
	 * Exports child records for a tool, adding them to the element
	 *
	 * @param element
	 * @param beanClass
	 * @param foreignKeyBeanPath
	 * @param foreignKeyOid
	 * @param includeOid
	 * @param beanPathsToSkip    (nullable)
	 */
	public void exportChildRecord(Element element, Class beanClass, String foreignKeyBeanPath,
			String foreignKeyOid, boolean includeOid, List<String> beanPathsToSkip) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(foreignKeyBeanPath, foreignKeyOid);
		try (IterableQuery<X2BaseBean> query = new IterableQuery(beanClass, criteria, getBroker())) {
			for (X2BaseBean bean : query) {
				Element childElement = new Element(getElementNameForClass(beanClass));
				setAttributesOnElement(childElement, bean, beanClass, includeOid, beanPathsToSkip);
				element.addContent(childElement);
			}
		}
	}

	/**
	 * Creates deployment procedure or just exports the results to the message
	 * window.
	 *
	 * @param deploymentProcedure
	 * @param xml
	 * @param addToTop
	 * @throws X2BaseException
	 * @throws IOException
	 * @throws JDOMException
	 */
	public void exportResults(String deploymentProcedure, StringBuilder xml, boolean addToTop)
			throws X2BaseException, IOException, JDOMException {
		exportResults(deploymentProcedure, null, xml, addToTop);
	}

	/**
	 * Creates deployment procedure or just exports the results to the message
	 * window.
	 *
	 * @param devOpsTask
	 * @param xml
	 * @param addToTop
	 * @throws X2BaseException
	 * @throws IOException
	 * @throws JDOMException
	 */
	public void exportResults(String devOpsTask, String deploymentMethod, StringBuilder xml, boolean addToTop)
			throws X2BaseException, IOException, JDOMException {
		if (DEPLOYMENT_OBC_CHANGE_LOG.equals(deploymentMethod)) {
			if (m_obcChangeLog.openForBundle(devOpsTask)) {
				String results;
				if (m_obcChangeLog.isNew()) {
					results = "New change log created: " + devOpsTask;
				} else {
					results = "Content added to change log " + devOpsTask;
				}
				m_obcChangeLog.setBundle(xml.toString(), addToTop);
				m_obcChangeLog.closeForBundle();
				logMessage(results);
			}

		} else if (!StringUtils.isBlank(devOpsTask)) {
			xml.append("\n");
			createDeploymentProcedure(devOpsTask, xml.toString(), addToTop);
		} else {
			logMessage(xml.toString());
		}
	}

	/**
	 * Get attributes for export / import
	 *
	 * @param tablePrefix
	 * @return
	 */
	public List<String> getAttributesForTablePrefix(String tablePrefix) {
		List<String> attributesToExport = new ArrayList<String>();

		X2Criteria criteria = new X2Criteria();
		criteria.addBeginsWithIgnoreCase(DataFieldConfig.COL_DATA_FIELD_OID, tablePrefix);
		criteria.addNotIn(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME,
				SKIP_ATTRIBUTES_LIST);
		BeanQuery query = new BeanQuery(DataFieldConfig.class, criteria);
		query.addOrderByAscending(DataFieldConfig.COL_SEQUENCE_NUMBER);
		try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
			while (iterator.hasNext()) {
				DataFieldConfig dataFieldConfig = (DataFieldConfig) iterator.next();
				attributesToExport.add(dataFieldConfig.getDataField().getJavaName());
			}
		}
		return attributesToExport;
	}

	/**
	 * Returns value for board language on the organization table
	 *
	 * @return
	 */
	public String getBoardLanguage() {
		String boardLanguage = getOntarioPreference(ALIAS_BOARD_LANGUAGE_ONTARIO_PREF);

		if (StringUtils.isBlank(boardLanguage)) {
			X2Criteria fddCriteria = new X2Criteria();
			fddCriteria.addEqualTo(DataFieldConfig.COL_ALIAS, ALIAS_ORGANIZATION_LANGUAGE);
			BeanQuery fddQuery = new BeanQuery(DataFieldConfig.class, fddCriteria);

			DataFieldConfig dataFieldConfig = getBroker().getBeanByQuery(fddQuery);
			if (dataFieldConfig != null) {
				String languageCode = (String) getOrganization().getRootOrganization()
						.getFieldValueByAlias(ALIAS_ORGANIZATION_LANGUAGE);
				X2Criteria rcdCriteria = new X2Criteria();
				rcdCriteria.addEqualTo(ReferenceCode.COL_CODE, languageCode);
				rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, dataFieldConfig.getReferenceTableOid());
				BeanQuery rcdQuery = new BeanQuery(ReferenceCode.class, rcdCriteria);

				ReferenceCode referenceCode = getBroker().getBeanByQuery(rcdQuery);
				if (referenceCode != null) {
					boardLanguage = referenceCode.getStateCode();
				}
			}
		}
		return boardLanguage;
	}

	/**
	 * Gets the calculated field
	 *
	 * @param name
	 * @return
	 */
	public CalculatedField getCalculatedField(String name) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(CalculatedField.COL_NAME, name);
		BeanQuery query = new BeanQuery(CalculatedField.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the calculated field member
	 *
	 * @param calculatedFieldOid
	 * @param dataFieldOid
	 * @return
	 */
	public CalculatedFieldMember getCalculatedFieldMember(String calculatedFieldOid, String dataFieldOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(CalculatedFieldMember.COL_CALCULATED_FIELD_OID, calculatedFieldOid);
		criteria.addEqualTo(CalculatedFieldMember.COL_DATA_FIELD_OID, dataFieldOid);
		BeanQuery query = new BeanQuery(CalculatedFieldMember.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the comment bank table
	 *
	 * @param name
	 * @return
	 */
	public CommentBankTable getCommentBankTable(String name) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(CommentBankTable.COL_NAME, name);
		BeanQuery query = new BeanQuery(CommentBankTable.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the data field for an alias
	 *
	 * @param alias String
	 * @return DataField bean
	 */
	public DataField getDataFieldByAlias(String alias) {
		DataField dataField = null;
		DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
		if (field != null) {
			dataField = field.getSystemDataField();
		}
		return dataField;
	}

	/**
	 * Gets the data field config or allocates a new one if available
	 *
	 * @param dataTableOid
	 * @param dataFieldOid
	 * @param alias
	 * @param longName
	 * @param length
	 * @return
	 */
	public DataFieldConfig getDataFieldConfig(String dataTableOid, String dataFieldOid, String alias, String longName,
			int length) {
		X2Criteria criteria = new X2Criteria();
		DataFieldConfig dataFieldConfig = null;

		// If alias is specified, look for enabled field with this alias
		if (!StringUtils.isBlank(alias)) {
			criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + "." + DataField.COL_DATA_TABLE_OID, dataTableOid);
			criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);
			criteria.addEqualTo(DataFieldConfig.COL_ALIAS, alias);

			dataFieldConfig = (DataFieldConfig) getBroker()
					.getBeanByQuery(new BeanQuery(DataFieldConfig.class, criteria));
		}

		if (dataFieldConfig == null) {
			// Enabled field with alias not found...looking by fieldOid
			if (dataFieldOid != null) {
				criteria = new X2Criteria();
				criteria.addEqualTo(DataFieldConfig.COL_DATA_FIELD_OID, dataFieldOid);
				criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + "." + DataField.COL_DATA_TABLE_OID, dataTableOid);
				// Search for disabled fields OR fields where the alias is blank but the long
				// name is the same
				X2Criteria andCriteria = new X2Criteria();
				andCriteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);
				X2Criteria orCriteria = new X2Criteria();
				orCriteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);
				orCriteria.addEqualTo(DataFieldConfig.COL_USER_LONG_NAME, longName);
				orCriteria.addEmpty(DataFieldConfig.COL_ALIAS, getBroker().getPersistenceKey());
				andCriteria.addOrCriteria(orCriteria);
				criteria.addAndCriteria(andCriteria);

				dataFieldConfig = (DataFieldConfig) getBroker()
						.getBeanByQuery(new BeanQuery(DataFieldConfig.class, criteria));
			}

			if (dataFieldConfig == null) {
				// A fieldOid has not been specified, or that fields is already taken
				// Find the next available data field config
				criteria = new X2Criteria();
				criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + "." + DataField.COL_DATA_TABLE_OID, dataTableOid);
				criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);

				BeanQuery query = new BeanQuery(DataFieldConfig.class, criteria);
				m_existingConfigs = (LinkedHashMap<String, DataFieldConfig>) getBroker().getMapByQuery(query,
						X2BaseBean.COL_OID, 1024);

				if (m_existingConfigs != null) {
					if (length <= 10) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldA")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null && length <= 25) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldB")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null && length <= 50) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldC")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldD")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldE")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
				}
			}
		}
		return dataFieldConfig;
	}

	/**
	 * Gets the data field config for an alias
	 *
	 * @param alias
	 * @return
	 */
	public DataFieldConfig getDataFieldConfigByAlias(String alias) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(DataFieldConfig.COL_ALIAS, alias);
		BeanQuery query = new BeanQuery(DataFieldConfig.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the first data field config for a data field oid
	 *
	 * @param fldOid
	 * @return
	 */
	public DataFieldConfig getDataFieldConfigByFieldOid(String fldOid) {
		DataFieldConfig dataFieldConfig = null;
		try {
			DataField dataField = getBroker().getBeanByOid(DataField.class, fldOid);
			if (dataField != null) {
				dataFieldConfig = dataField.getDataFieldConfigs().stream().findFirst().get();
			}
		} catch (Exception e) {
			// null
		}
		return dataFieldConfig;
	}

	/**
	 * Gets a data validation rule
	 *
	 * @param dataTableConfigOid
	 * @param id
	 * @return
	 */
	public DataValidationRule getDataValidationRule(String dataTableConfigOid, String id) {
		return getDataValidationRule(dataTableConfigOid, null, id);
	}

	/**
	 * Gets a data validation rule
	 *
	 * @param dataTableConfigOid
	 * @param id
	 * @param extendedDataDictionaryId
	 * @return
	 */
	public DataValidationRule getDataValidationRule(String dataTableConfigOid, String extendedDataDictionaryId,
			String id) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(DataValidationRule.COL_DATA_TABLE_CONFIG_OID, dataTableConfigOid);
		if (!StringUtils.isBlank(extendedDataDictionaryId)) {
			criteria.addEqualTo(DataValidationRule.REL_EXTENDED_DATA_TABLE + "."
					+ ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + "." + ExtendedDataDictionary.COL_ID,
					extendedDataDictionaryId);
		}
		criteria.addEqualTo(DataValidationRule.COL_ID, id);
		BeanQuery query = new BeanQuery(DataValidationRule.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the district data dictionary with the extended dictionary ID applied
	 *
	 * @param extendedDictionaryId
	 * @return
	 */
	public DataDictionary getDistrictDictionaryByDdx(String extendedDictionaryId) {
		ExtendedDataDictionary extendedDictionary = getExtendedDataDictionary(extendedDictionaryId);

		return DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
	}

	/**
	 * Returns an element name (camel case) for the dictionary id
	 *
	 * @param dictionaryId
	 * @return
	 */
	public String getElementNameForClass(Class beanClass) {
		List<String> names = Arrays.asList(beanClass.getCanonicalName().split("\\."));
		String elementName = names.get(names.size() - 1);

		return Character.toLowerCase(elementName.charAt(0)) + elementName.substring(1);
	}

	/**
	 * Returns a new element with the attribute value base64 encoded
	 *
	 * @param attribute
	 * @param bytes
	 * @return
	 */
	public Element getEncodedBytesAsElement(String attribute, byte[] bytes) {
		Element element = new Element(attribute);
		String encodedBytes = Base64.getEncoder().encodeToString(bytes);
		element.addContent(encodedBytes);
		return element;
	}

	/**
	 * Gets the extended data dictionary by id
	 *
	 * @param extendedDataDictionaryId
	 * @return
	 */
	public ExtendedDataDictionary getExtendedDataDictionary(String extendedDataDictionaryId) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryId);

		return getBroker().getBeanByQuery(new BeanQuery(ExtendedDataDictionary.class, criteria));
	}

	/**
	 * Gets the extended data field or creates a new one if a field is available
	 *
	 * @param extendedDataDictionaryId
	 * @param extendedDataTable
	 * @param alias
	 * @param longName
	 * @param length
	 * @param dataFieldConfigOid
	 * @param dataFieldConfigOptional
	 * @return
	 */
	public ExtendedDataField getExtendedDataField(ExtendedDataTable extendedDataTable, String alias, String longName,
			int length, String dataFieldConfigOid, boolean dataFieldConfigOptional) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ExtendedDataField.COL_EXTENDED_DATA_TABLE_OID, extendedDataTable.getOid());
		// Search for field where long name matches OR alias matches
		X2Criteria andCriteria = new X2Criteria();
		andCriteria.addEqualTo(ExtendedDataField.COL_ALIAS, alias);
		X2Criteria orCriteria = new X2Criteria();
		orCriteria.addEqualTo(ExtendedDataField.COL_USER_LONG_NAME, longName);
		andCriteria.addOrCriteria(orCriteria);
		criteria.addAndCriteria(andCriteria);

		ExtendedDataField extendedDataField = (ExtendedDataField) getBroker()
				.getBeanByQuery(new BeanQuery(ExtendedDataField.class, criteria));

		if (extendedDataField == null) {
			// Enabled field with alias not found...looking by dataFieldConfigOid
			if (dataFieldConfigOid != null) {
				criteria = new X2Criteria();
				criteria.addEqualTo(ExtendedDataField.COL_EXTENDED_DATA_TABLE_OID, extendedDataTable.getOid());
				criteria.addEqualTo(ExtendedDataField.COL_DATA_FIELD_CONFIG_OID, dataFieldConfigOid);
				extendedDataField = (ExtendedDataField) getBroker()
						.getBeanByQuery(new BeanQuery(ExtendedDataField.class, criteria));
			}

			if (extendedDataField != null) {
				// dataFieldConfigOid is already being used on this extended table, but has a
				// different long name and alias. Use a different field.
				extendedDataField = null;
				// If it's OK to use a different field than what is specified, set
				// dataFieldConfigOid to null so we don't try to use that one since
				// we've just discovered it's already in use.
				if (dataFieldConfigOptional) {
					dataFieldConfigOid = null;
				}
			}

			if (extendedDataField == null && (dataFieldConfigOid == null || dataFieldConfigOptional)) {
				// Get collection of already used data field configs for this extended data
				// table
				Collection<String> dataFieldConfigsInUse = new ArrayList<String>();
				criteria = new X2Criteria();
				criteria.addEqualTo(ExtendedDataField.COL_EXTENDED_DATA_TABLE_OID, extendedDataTable.getOid());
				BeanQuery query = new BeanQuery(ExtendedDataField.class, criteria);
				try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
					while (iterator.hasNext()) {
						ExtendedDataField extendedFieldInUse = (ExtendedDataField) iterator.next();
						dataFieldConfigsInUse.add(extendedFieldInUse.getDataFieldConfigOid());
					}
				}

				// Looking for next available data field config
				criteria = new X2Criteria();
				criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + "." + DataField.COL_DATA_TABLE_OID,
						extendedDataTable.getDataTableConfig().getDataTableOid());
				criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);
				criteria.addNotIn(X2BaseBean.COL_OID, dataFieldConfigsInUse);
				query = new BeanQuery(DataFieldConfig.class, criteria);
				m_existingConfigs = (LinkedHashMap<String, DataFieldConfig>) getBroker().getMapByQuery(query,
						X2BaseBean.COL_OID, 1024);

				DataFieldConfig dataFieldConfig = null;

				if (m_existingConfigs != null) {
					if (m_existingConfigs.containsKey(dataFieldConfigOid)) {
						dataFieldConfig = m_existingConfigs.get(dataFieldConfigOid);
					}
					if (dataFieldConfig == null && length <= 10) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldA")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null && length == 14) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldO")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null && length <= 25) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldB")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null && length <= 50) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldC")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldD")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
					if (dataFieldConfig == null) {
						for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
							if (entry.getValue().getDataFieldOid().contains("FieldE")) {
								dataFieldConfig = entry.getValue();
								break;
							}
						}
					}
				}

				// Found an available data field config, so create a new extendedDataField
				// pointing to it
				if (dataFieldConfig != null) {
					extendedDataField = X2BaseBean.newInstance(ExtendedDataField.class,
							getBroker().getPersistenceKey());
					extendedDataField.setExtendedDataTableOid(extendedDataTable.getOid());
					extendedDataField.setDataFieldConfigOid(dataFieldConfig.getOid());
					extendedDataField.setAlias(alias);
					getBroker().saveBeanForced(extendedDataField);
				}
			}
		}
		return extendedDataField;
	}

	/**
	 * Gets the extended data field for a given alias and extended dictionary ID
	 *
	 * @param alias
	 * @param extendedDictionaryId
	 * @return
	 */
	public ExtendedDataField getExtendedDataFieldByAlias(String alias, String extendedDictionaryId) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ExtendedDataField.COL_ALIAS, alias);
		criteria.addEqualTo(ExtendedDataField.REL_EXTENDED_DATA_TABLE + ModelProperty.PATH_DELIMITER
				+ ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
				+ ExtendedDataDictionary.COL_ID, extendedDictionaryId);
		BeanQuery query = new BeanQuery(ExtendedDataField.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the extended data table by extended data dictionary id and data table
	 * oid
	 *
	 * @param extendedDataDictionaryId
	 * @param dataTableOid
	 * @return
	 */
	public ExtendedDataTable getExtendedDataTable(String extendedDataDictionaryId, String dataTableOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + "." + ExtendedDataDictionary.COL_ID,
				extendedDataDictionaryId);
		criteria.addEqualTo(ExtendedDataTable.REL_DATA_TABLE_CONFIG + "." + DataTableConfig.COL_DATA_TABLE_OID,
				dataTableOid);

		return getBroker().getBeanByQuery(new BeanQuery(ExtendedDataTable.class, criteria));
	}

	/**
	 * Gets a map of extended dictionary OIDs and extended data fields that point to
	 * a reference table OID
	 *
	 * @param referenceTableOid
	 * @return
	 */
	public Map<String, Collection<ExtendedDataField>> getExtendedFieldsForReferenceTable(String referenceTableOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ExtendedDataField.COL_REFERENCE_TABLE_OID, referenceTableOid);
		BeanQuery query = new BeanQuery(ExtendedDataField.class, criteria);

		return getBroker().getGroupedCollectionByQuery(query,
				ExtendedDataField.REL_EXTENDED_DATA_TABLE + "." + ExtendedDataTable.COL_EXTENDED_DATA_DICTIONARY_OID,
				32);
	}

	/**
	 * Gets a root-organization owned field set by context and name
	 *
	 * @param context
	 * @param name
	 * @param extendedDataDictionary
	 * @return
	 */
	public FieldSet getFieldSet(String context, String name, ExtendedDataDictionary extendedDataDictionary) {

		String extendedDataDictionaryOid = null;
		if (extendedDataDictionary != null) {
			extendedDataDictionaryOid = extendedDataDictionary.getOid();
		}

		return getFieldSet(context, name, extendedDataDictionaryOid, getOrganization().getRootOrganization().getOid(),
				Ownable.OWNER_TYPE_ORG1);
	}

	/**
	 * Gets a field set
	 *
	 * @param context
	 * @param name
	 * @param extendedDataDictionaryOid
	 * @param ownerOid
	 * @param ownerType
	 * @return
	 */
	public FieldSet getFieldSet(String context, String name, String extendedDataDictionaryOid, String ownerOid,
			int ownerType) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(FieldSet.COL_CONTEXT, context);
		criteria.addEqualTo(FieldSet.COL_NAME, name);
		if (extendedDataDictionaryOid != null) {
			criteria.addEqualTo(FieldSet.COL_EXTENDED_DATA_DICTIONARY_OID, extendedDataDictionaryOid);
		} else {
			criteria.addIsNull(FieldSet.COL_EXTENDED_DATA_DICTIONARY_OID);
		}
		criteria.addEqualTo(FieldSet.COL_OWNER_OID, ownerOid);
		criteria.addEqualTo(FieldSet.COL_OWNER_TYPE, ownerType);
		BeanQuery query = new BeanQuery(FieldSet.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets a field set member by field set oid and object oid
	 *
	 * @param fieldSetOid
	 * @param objectOid
	 * @return
	 */
	public FieldSetMember getFieldSetMember(String fieldSetOid, String objectOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(FieldSetMember.COL_FIELD_SET_OID, fieldSetOid);
		criteria.addEqualTo(FieldSetMember.COL_OBJECT_OID, objectOid);
		BeanQuery query = new BeanQuery(FieldSetMember.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets a collection of data field configs that point to a reference table oid
	 *
	 * @param referenceTableOid
	 * @return
	 */
	public Collection<DataFieldConfig> getFieldsForReferenceTable(String referenceTableOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(DataFieldConfig.COL_REFERENCE_TABLE_OID, referenceTableOid);
		BeanQuery query = new BeanQuery(DataFieldConfig.class, criteria);

		return getBroker().getCollectionByQuery(query);
	}

	/**
	 * Gets the import export definition bean by id
	 *
	 * @param importExportDefinitionId
	 * @param type
	 * @return ImportExportDefinition
	 */
	public ImportExportDefinition getImportExportDefinitionById(String importExportDefinitionId, Integer type) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ImportExportDefinition.COL_ID, importExportDefinitionId);
		if (type != null) {
			criteria.addEqualTo(ImportExportDefinition.COL_TYPE, type.intValue());
		}
		BeanQuery query = new BeanQuery(ImportExportDefinition.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Returns a message resource
	 *
	 * @param key
	 * @param locale
	 * @return
	 */
	public MessageResource getMessageResource(String key, String locale) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(MessageResource.COL_KEY, key);
		criteria.addEqualTo(MessageResource.COL_LOCALE, locale);
		BeanQuery query = new BeanQuery(MessageResource.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets a collection of message resources
	 *
	 * @param key    - Key for message resource (optional)
	 * @param locale - Locale for message resource (optional)
	 * @param value  - Existing value (optional) for querying a message resource
	 * @return
	 */
	public Collection<MessageResource> getMessageResources(String key, String locale, String value) {
		X2Criteria criteria = new X2Criteria();
		if (!StringUtils.isBlank(key)) {
			criteria.addEqualTo(MessageResource.COL_KEY, key);
		}
		if (!StringUtils.isBlank(locale)) {
			criteria.addEqualTo(MessageResource.COL_LOCALE, locale);
		}
		if (!StringUtils.isBlank(value)) {
			criteria.addEqualTo(MessageResource.COL_VALUE, value);
		}
		BeanQuery query = new BeanQuery(MessageResource.class, criteria);
		return getBroker().getCollectionByQuery(query);
	}

	/**
	 * Returns a custom Ontario Preference
	 *
	 * @param alias
	 * @return
	 */
	public String getOntarioPreference(String alias) {
		String prfValue = null;

		OrganizationAttributes preference = getBroker().getBeanByOid(OrganizationAttributes.class,
				OID_ONTARIO_PREFERENCES);
		DataDictionary dictionary = DataDictionaryUtils
				.getExtendedDictionaryById(EXT_ID_ONTARIO_PREFERENCES, getBroker());

		if (preference != null) {
			// Get value
			prfValue = (String) preference.getFieldValueByAlias(alias, dictionary);
		}

		return prfValue;
	}

	/**
	 * Gets the procedure bean by id
	 *
	 * @param procedureId
	 * @return Procedure
	 */
	public Procedure getProcedureById(String procedureId) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(Procedure.COL_ID, procedureId);
		BeanQuery query = new BeanQuery(Procedure.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the reference code
	 *
	 * @param referenceTableOid
	 * @param code
	 * @return
	 */
	public ReferenceCode getReferenceCode(String referenceTableOid, String code) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
		criteria.addEqualTo(ReferenceCode.COL_CODE, code);
		BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the reference table for a given name
	 *
	 * @param userName - Reference table name
	 * @return
	 */
	public ReferenceTable getReferenceTable(String userName) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ReferenceTable.COL_USER_NAME, userName);
		BeanQuery query = new BeanQuery(ReferenceTable.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the report bean by id
	 *
	 * @param reportId
	 * @return Report
	 */
	public Report getReportById(String reportId) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(Report.COL_ID, reportId);
		BeanQuery query = new BeanQuery(Report.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets a saved filter
	 *
	 * @param context
	 * @param name
	 * @param extendedDataDictionaryOid
	 * @param ownerOid
	 * @param ownerType
	 * @return
	 */
	public SavedFilter getSavedFilter(String context, String name, String extendedDataDictionaryOid, String ownerOid,
			int ownerType) {
		SavedFilter savedFilter;
		// Query by name, context, owner and extended data dictionary (if applicable)
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(SavedFilter.COL_NAME, name);
		criteria.addEqualTo(SavedFilter.COL_CONTEXT, context);
		if (extendedDataDictionaryOid != null) {
			criteria.addEqualTo(SavedFilter.COL_EXTENDED_DATA_DICTIONARY_OID, extendedDataDictionaryOid);
		} else {
			criteria.addIsNull(FieldSet.COL_EXTENDED_DATA_DICTIONARY_OID);
		}
		criteria.addEqualTo(SavedFilter.COL_OWNER_OID, ownerOid);
		criteria.addEqualTo(SavedFilter.COL_OWNER_TYPE, ownerType);
		BeanQuery query = new BeanQuery(SavedFilter.class, criteria);
		savedFilter = getBroker().getBeanByQuery(query);
		return savedFilter;
	}

	/**
	 * Returns system preference for the preference definition and value
	 *
	 * @param systemPreferenceDefinition
	 * @param ownerOid
	 * @return
	 */
	public SystemPreference getSystemPreference(SystemPreferenceDefinition systemPreferenceDefinition,
			String ownerOid) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(SystemPreference.COL_PREFERENCE_DEFINITION_OID, systemPreferenceDefinition.getOid());
		if (!StringUtils.isBlank(ownerOid)) {
			criteria.addEqualTo(SystemPreference.COL_OWNER_OID, ownerOid);
		}
		BeanQuery query = new BeanQuery(SystemPreference.class, criteria);
		SystemPreference systemPreference = getBroker().getBeanByQuery(query);

		return systemPreference;
	}

	/**
	 * Returns the system preference definition for the key and category
	 *
	 * @param key
	 * @param category
	 * @return
	 */
	public SystemPreferenceDefinition getSystemPreferenceDefinition(String key, String category) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(SystemPreferenceDefinition.COL_KEY, key);
		if (!StringUtils.isBlank(category)) {
			criteria.addEqualTo(SystemPreferenceDefinition.COL_CATEGORY, category);
		}
		BeanQuery query = new BeanQuery(SystemPreferenceDefinition.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Returns a tool (Report, Procedure, ImportExportDefinition) by ID
	 *
	 * @param id
	 * @param objectPrefix
	 * @return X2BaseBean
	 */
	public X2BaseBean getToolById(String id, String objectPrefix) {
		return getToolById(id, objectPrefix, null);
	}

	/**
	 * Returns a tool (Report, Procedure, ImportExportDefinition) by ID
	 *
	 * @param id
	 * @param objectPrefix
	 * @param iedType
	 * @return X2BaseBean
	 */
	public X2BaseBean getToolById(String id, String objectPrefix, Integer iedType) {
		X2BaseBean bean = null;
		if (Report.OBJECT_PREFIX.equals(objectPrefix.toUpperCase())) {
			bean = getReportById(id);
		} else if (Procedure.OBJECT_PREFIX.equals(objectPrefix.toUpperCase())) {
			bean = getProcedureById(id);
		} else if (ImportExportDefinition.OBJECT_PREFIX.equals(objectPrefix.toUpperCase())) {
			bean = getImportExportDefinitionById(id, iedType);
		}
		return bean;
	}

	/**
	 * Return a user defined navigation
	 *
	 * @param parentNavigationId
	 * @param position
	 * @param fieldC001          -- UDF containing the name of the navigation
	 * @return
	 */
	public UserDefinedNavigation getUserDefinedNavigation(String parentNavigationId, String position, String fieldC001,
			Boolean org1View, Boolean schoolView) {
		UserDefinedNavigation udn = null;
		Collection<UserDefinedNavigation> udnCollection = getUserDefinedNavigationCollection(parentNavigationId,
				position, fieldC001, org1View, schoolView);
		if (udnCollection.size() == 1) {
			udn = udnCollection.stream().findFirst().get();
		} else {
			// Query on parent navigation ID, position, and name (fieldC001) only
			udnCollection = getUserDefinedNavigationCollection(parentNavigationId, position, fieldC001, null, null);
			if (udnCollection.size() == 1) {
				udn = udnCollection.stream().findFirst().get();
			} else {
				// Query on parent navigation ID, position only
				udnCollection = getUserDefinedNavigationCollection(parentNavigationId, position, null, null, null);
				if (udnCollection.size() == 1) {
					udn = udnCollection.stream().findFirst().get();
				}
			}
		}

		return udn;
	}

	/**
	 * Return a user defined navigation bean query
	 *
	 * @param parentNavigationId
	 * @param position
	 * @param fieldC001          -- UDF containing the name of the navigation
	 * @param org1View
	 * @param schoolView
	 * @return
	 */
	public Collection<UserDefinedNavigation> getUserDefinedNavigationCollection(String parentNavigationId,
			String position, String fieldC001, Boolean org1View, Boolean schoolView) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(UserDefinedNavigation.COL_PARENT_NAVIGATION_ID, parentNavigationId);
		criteria.addEqualTo(UserDefinedNavigation.COL_POSITION, position);
		if (!StringUtils.isBlank(fieldC001)) {
			criteria.addEqualTo(UserDefinedNavigation.COL_FIELD_C001, fieldC001);
		}
		if (org1View != null) {
			criteria.addEqualTo(UserDefinedNavigation.COL_ORG1_APPLICATION_INDICATOR, org1View.booleanValue());
		}
		if (schoolView != null) {
			criteria.addEqualTo(UserDefinedNavigation.COL_SCHOOL_APPLICATION_INDICATOR, schoolView.booleanValue());
		}
		BeanQuery query = new BeanQuery(UserDefinedNavigation.class, criteria);
		return getBroker().getCollectionByQuery(query);
	}

	/**
	 * Return a user defined navigation resource
	 *
	 * @param messageResource
	 * @param userDefinedNavigation
	 * @return
	 */
	public UserDefinedNavResource getUserDefinedNavResource(MessageResource messageResource,
			UserDefinedNavigation userDefinedNavigation) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(UserDefinedNavResource.COL_MESSAGE_RESOURCE_OID, messageResource.getOid());
		criteria.addEqualTo(UserDefinedNavResource.COL_USER_DEFINED_NAVIGATION_OID, userDefinedNavigation.getOid());
		BeanQuery query = new BeanQuery(UserDefinedNavResource.class, criteria);

		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the view template
	 *
	 * @param context String
	 * @param name    String
	 * @return ViewTemplate
	 */
	public ViewTemplate getViewTemplate(String context, String name) {
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(ViewTemplate.COL_CONTEXT, context);
		criteria.addEqualTo(ViewTemplate.COL_NAME, name);
		BeanQuery query = new BeanQuery(ViewTemplate.class, criteria);
		return getBroker().getBeanByQuery(query);
	}

	/**
	 * Gets the workflow phase
	 *
	 * @return WorkflowPhase collection
	 */
	public Collection getWorkflowPhaseCollection() {
		X2Criteria criteria = new X2Criteria();
		criteria.addIsNull(WorkflowPhase.COL_ID);
		BeanQuery query = new BeanQuery(WorkflowPhase.class, criteria);
		return getBroker().getCollectionByQuery(query);
	}

	/**
	 * Gets the workflow phase
	 *
	 * @return WorkflowPhase collection
	 */
	public Collection getWorkflowPhaseOutcomeCollection() {
		X2Criteria criteria = new X2Criteria();
		criteria.addIsNull(WorkflowPhaseOutcome.COL_ID);
		BeanQuery query = new BeanQuery(WorkflowPhaseOutcome.class, criteria);
		return getBroker().getCollectionByQuery(query);
	}

	/**
	 * Gets the root element from an XML string (input definition, view template,
	 * etc.)
	 *
	 * @param xmlString
	 * @return Element
	 * @throws JDOMException exception
	 * @throws IOException   Signals that an I/O exception has occurred.
	 */
	public Element getXmlRoot(String xmlString) throws JDOMException, IOException {
		Element xmlRoot = null;

		if (xmlString != null) {
			SAXBuilder builder = new SAXBuilder();
			Document xmlDocument = builder.build(new ByteArrayInputStream(xmlString.getBytes()));
			xmlRoot = xmlDocument.getRootElement();
		} else {
			logErrorMessage("No XML root in string");
		}
		return xmlRoot;
	}

	/**
	 * Returns whether the board language on the organization table is French (F)
	 *
	 * @return
	 */
	public boolean isBoardLanguageFrench() {
		return m_isBoardLanguageFrench;
	}

	/**
	 * Logs message and sets m_errors so procedure won't be deleted
	 *
	 * @param message
	 */
	public void logErrorMessage(String message) {
		logMessage("ERROR: " + message);
		m_errors = true;
		m_obcChangeLog.setErrors(m_errors);
	}

	/**
	 * Adds message to string builder
	 */
	@Override
	public void logMessage(String message) {
		logMessage(message, true);
	}

	/**
	 * Adds message to string builder
	 */
	public void logMessage(String message, boolean addToObcChangeLog) {
		m_messages.append(message + "\n");
		if (addToObcChangeLog) {
			m_obcChangeLog.logMessage(message + "\n");
		}
	}

	public void reloadDataDictionary() {
		DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
		if (isBoardLanguageFrench()) {
			logMessage("Dictionnaire de données rechargé");
		} else {
			logMessage("Data Dictionary reloaded");
		}
	}

	public void reloadLocalizationCache() {
		LocalizationCache.reload(getBroker().getPersistenceKey(), true);
		if (isBoardLanguageFrench()) {
			logMessage("Cache de localisation rechargée");
		} else {
			logMessage("Localization cache reloaded");
		}
	}

	public void reloadNavigation() {
		try {
			NavConfigXMLHandler parser = new NavConfigXMLHandler(false);
			Collection<PersistenceKey> persistenceKeys = Arrays
					.asList(new PersistenceKey[] { m_userData.getPersistenceKey() });
			parser.setIsMobile(false);
			AppGlobals.loadNavConfigs(parser, persistenceKeys, false);
			parser.setIsMobile(true);
			AppGlobals.loadNavConfigs(parser, persistenceKeys, true);
			LocalizationCache.reload(m_userData.getUser().getPersistenceKey(), false);
			AppGlobals.getCache(m_userData.getPersistenceKey()).fireReloadNavEvent();

			m_userData.reloadNavConfig();
			if (isBoardLanguageFrench()) {
				logMessage("Navigation actualisée");
			} else {
				logMessage("Navigation reloaded");
			}
		} catch (X2BaseException e) {
			logErrorMessage("Exception error while reloading navigation. You must reload manually on this database");
		}
	}

	public void reloadProcedureCache() {
		ToolObjectCache.clear(getBroker().getPersistenceKey(), true);
		if (isBoardLanguageFrench()) {
			logMessage("Cache de procédures effacée");
		} else {
			logMessage("Procedure cache cleared");
		}
	}

	public void reloadObjectCache() {
		(new ModelBroker(getPrivilegeSet())).clearCache();
		if (isBoardLanguageFrench()) {
			logMessage("Objet cache dégagé et réinitialisé");
		} else {
			logMessage("Object cache cleared and reinitialized");
		}
	}

	/**
	 * Returns a string padded on the right with the pad character
	 *
	 * @param string
	 * @param length
	 * @param padChar
	 * @return
	 */
	public String rightPad(String string, int length, String padChar) {
		return org.apache.commons.lang3.StringUtils.rightPad(string, length, padChar);
	}

	/**
	 * Resets the data dictionary field Not using method in DataDictionaryUtils
	 * because it doesn't reset all data field config attributes. Also, using SQL to
	 * update records / set field to NULL so it runs quickly on very large tables.
	 *
	 * @param beanClass Class
	 * @param fieldOid
	 */
	public void resetField(Class beanClass, String fieldOid) {
		DataDictionaryField field = m_dictionary.findDataDictionaryField(fieldOid);
		if (field != null && field.getDataFieldConfig() != null) {
			DataFieldConfig dataFieldConfig = field.getDataFieldConfig();

			/*
			 * For each bean of type beanClass, null the value for that record
			 */
			String databaseTableName = field.getDataTable().getDatabaseName();
			String databaseFieldName = field.getDatabaseName();
			String sql = "UPDATE " + databaseTableName + " SET " + databaseFieldName + " = NULL WHERE "
					+ databaseFieldName + " IS NOT NULL";
			boolean success = executeSqlUpdate(sql, "Records set to null for " + field.getId() + ":");

			if (success) {
				/*
				 * Reset the field in the dictionary. Borrowed from
				 * DataDictionaryUtils.resetField(), but extended to include missing attributes
				 *
				 * @see
				 * com.follett.fsc.core.k12.business.dictionary.DataDictionaryUtils#resetField()
				 */
				String originalName = StringUtils.capitalize(field.getJavaName());

				dataFieldConfig.setUserLongName(originalName);
				dataFieldConfig.setUserShortName(originalName);
				dataFieldConfig.setUserDecimal(0);
				dataFieldConfig.setUserLength(0);
				dataFieldConfig.setUserType(null);

				dataFieldConfig.setAlias(null);
				dataFieldConfig.setDefaultValue(null);
				dataFieldConfig.setDetailControl(null);
				dataFieldConfig.setReferenceTableOid(null);
				dataFieldConfig.setDependency(null);

				dataFieldConfig.setEnabledIndicator(false);
				dataFieldConfig.setListEditIndicator(false);
				dataFieldConfig.setPicklistFields(null);
				dataFieldConfig.setReadOnlyIndicator(false);
				dataFieldConfig.setRequiredIndicator(false);
				dataFieldConfig.setUpdateIndicator(false);
				dataFieldConfig.setValidReferenceOnlyIndicator(false);
				dataFieldConfig.setValidatorExpressionIds(null);

				dataFieldConfig.setHelpIndicator(false);
				dataFieldConfig.setFieldDescription(null);
				dataFieldConfig.setSpellCheckIndicator(false);
				dataFieldConfig.setFieldAuditTypeEnum(FieldAuditType.OFF);
				dataFieldConfig.setDefaultSuffix(null);
				dataFieldConfig.setRecordLevelSecurityIndicator(false);
				dataFieldConfig.setLocalizedIndicator(false);

				dataFieldConfig.setAccessCode(null);
				dataFieldConfig.setCalculatedFieldOid(null);
				dataFieldConfig.setChangeReferenceTableOid(null);
				dataFieldConfig.setCommentBankTableOid(null);
				dataFieldConfig.setConversionType(null);
				dataFieldConfig.setDisplayMaskOid(null);
				dataFieldConfig.setExternalId(null);
				dataFieldConfig.setSaveMaskOid(null);
				dataFieldConfig.setTechnicalDescription(null);

				X2Criteria criteria = new X2Criteria();
				criteria.addEqualTo(MessageResource.COL_OBJECT_OID, dataFieldConfig.getOid());
				BeanQuery deleteQuery = new BeanQuery(MessageResource.class, criteria);
				int recordsDeleted = getBroker().deleteByQuery(deleteQuery);
				if (recordsDeleted > 0) {
					LocalizationCache.reload(getBroker().getPersistenceKey(), true);
				}
				getBroker().saveBeanForced(dataFieldConfig);
			}
		}
	}

	/**
	 * Sets all attributes on the bean by iterating over its fields
	 *
	 * @param element
	 * @param bean
	 * @param beanClass
	 * @param beanPathsToSkip
	 */
	public void setAttributesOnBean(Element element, X2BaseBean bean, Class beanClass, boolean includeOid,
			List<String> beanPathsToSkip) {
		Set<String> pathsToSkip;
		if (beanPathsToSkip == null) {
			pathsToSkip = new HashSet<String>();
		} else {
			pathsToSkip = new HashSet<String>(beanPathsToSkip);
		}
		pathsToSkip.add(X2BaseBean.COL_LAST_MODIFIED_TIME);
		/*
		 * Unless specified, don't include the OID
		 */
		if (!includeOid) {
			pathsToSkip.add(X2BaseBean.COL_OID);
		}
		for (DataDictionaryField field : m_dictionary.getFieldsForContext(beanClass.getCanonicalName())) {
			/*
			 * Loop through all fields for the class unless specifically excluded
			 */
			if (!pathsToSkip.contains(field.getJavaName())) {
				Object value = element.getAttributeValue(field.getJavaName());
				if (value != null) {
					if (field.isString()) {
						// Do nothing, already assigned
					} else if (Date.class.getName().equals(field.getEffectiveJavaType())) {
						// Format for date fields will be YYYY-MM-DD
						try {
							value = new PlainDate(m_dateFormat.parse(String.valueOf(value)));
						} catch (ParseException e) {
							logErrorMessage("Parse exception error for field " + field.getJavaName()
									+ " trying to import value " + value);
							logErrorMessage(ExceptionUtils.getFullStackTrace(e));
						}
					} else if (Report.COL_FORMAT.equals(field.getJavaName())
							|| ToolSourceCode.COL_SOURCE_CODE.equals(field.getJavaName())
							|| ToolSourceCode.COL_INPUT_DEFINITION.equals(field.getJavaName())) {
						// For reports and tool source, decode the source (format, Java, input defn)
						value = new String(Base64.getDecoder().decode(String.valueOf(value)));
					} else if (Report.COL_COMPILED_FORMAT.equals(field.getJavaName())
							|| ToolSourceCode.COL_COMPILED_CODE.equals(field.getJavaName())) {
						// For reports and tool source, decode the compiled source (format, Java)
						value = Base64.getDecoder().decode(String.valueOf(value));
					} else {
						Converter converter = ConverterFactory.getConverterForClass(field.getEffectiveJavaType(),
								getLocale(), field.isString());
						if (converter != null) {
							value = converter.stringToJava(String.valueOf(value));
						} else {
							logErrorMessage("Can't get converter for attribute '" + field.getUserLongName() + "' ("
									+ field.getSystemDataField().getOid() + "), with effect Java type "
									+ field.getEffectiveJavaType());
						}
					}

					bean.setFieldValueByBeanPath(field.getJavaName(), value);
				}
			}
		}
	}

	/**
	 * Sets all attributes on the element by iterating over the properties of bean
	 *
	 * @param element
	 * @param bean
	 * @param beanClass
	 * @param includeOid
	 * @param beanPathsToSkip
	 */
	public void setAttributesOnElement(Element element, X2BaseBean bean, Class beanClass, boolean includeOid,
			List<String> beanPathsToSkip) {
		Set<String> pathsToSkip;
		if (beanPathsToSkip == null) {
			pathsToSkip = new HashSet<String>();
		} else {
			pathsToSkip = new HashSet<String>(beanPathsToSkip);
		}
		pathsToSkip.add(X2BaseBean.COL_LAST_MODIFIED_TIME);
		/*
		 * Unless specified, don't include the OID
		 */
		if (!includeOid) {
			pathsToSkip.add(X2BaseBean.COL_OID);
		}
		for (DataDictionaryField field : m_dictionary.getFieldsForContext(beanClass.getCanonicalName())) {
			/*
			 * Loop through all fields for the class unless specifically excluded
			 */
			if (!pathsToSkip.contains(field.getJavaName())) {
				String attributeValue = null;
				Object beanValue = bean.getFieldValueByBeanPath(field.getJavaName());
				if (beanValue != null) {
					if (Date.class.getName().equals(field.getJavaType())) {
						// Format date fields as YYYY-MM-DD
						attributeValue = m_dateFormat.format(beanValue);
					} else if (bean instanceof Report && Report.COL_FORMAT.equals(field.getJavaName())) {
						// For reports, base64 encode the format
						Report report = (Report) bean;
						if (report.getFormat() != null) {
							element.addContent(getEncodedBytesAsElement(Report.COL_FORMAT,
									report.getFormat().getBytes()));
						}
					} else if (bean instanceof Report && Report.COL_COMPILED_FORMAT.equals(field.getJavaName())) {
						// For reports, base64 encode the compiled format
						Report report = (Report) bean;
						if (report.getCompiledFormat() != null) {
							element.addContent(getEncodedBytesAsElement(Report.COL_COMPILED_FORMAT,
									report.getCompiledFormat()));
						}
					} else if (field.isString()) {
						attributeValue = String.valueOf(beanValue);
					} else {
						Converter converter = ConverterFactory.getConverterForClass(field.getEffectiveJavaType(),
								getLocale(), field.isString());
						if (converter != null) {
							attributeValue = converter.javaToString(beanValue);
						} else {
							logErrorMessage("Can't get converter for attribute '" + field.getUserLongName() + "' ("
									+ field.getSystemDataField().getOid() + "), with effect Java type "
									+ field.getEffectiveJavaType());
						}
					}

					if (!StringUtils.isBlank(attributeValue)) {
						element.setAttribute(field.getJavaName(), attributeValue);
					}
				}
			}
		}
	}

	/**
	 * Sets the extended data dictionary oid and ID in an element
	 *
	 * @param element
	 * @param extendedDataDictionaryOid
	 * @param extendedDataDictionaryOidAttrName
	 * @param extendedDataDictionaryIdAttrName
	 * @return
	 */
	public boolean setExtendedDataDictionaryInElement(Element element, String extendedDataDictionaryOid,
			String extendedDataDictionaryOidAttrName, String extendedDataDictionaryIdAttrName) {
		boolean success = true;
		if (extendedDataDictionaryOid != null) {
			ExtendedDataDictionary extendedDataDictionary = getBroker().getBeanByOid(ExtendedDataDictionary.class,
					extendedDataDictionaryOid);
			if (extendedDataDictionary != null) {
				element.setAttribute(extendedDataDictionaryOidAttrName, extendedDataDictionaryOid);
				element.setAttribute(extendedDataDictionaryIdAttrName, extendedDataDictionary.getId());
			} else {
				success = false;
			}
		}

		return success;
	}

	/**
	 * Separates the field from the path, if the field is on a related table
	 *
	 * @param field
	 * @param relation
	 * @return
	 */
	public static String splitFieldAndRelation(String field, StringBuilder relation) {
		// Split field on the path delimiter, and keep calling method until no more
		// delimiters are found
		if (field.indexOf('.') >= 0) {
			if (relation.length() > 0) {
				relation.append(".");
			}
			relation.append(field.substring(0, field.indexOf('.')));
			field = splitFieldAndRelation(field.substring(field.indexOf('.') + 1, field.length()), relation);
		}
		return field;
	}

	/**
	 * Updates the field names in the extended data dictionary, typically from
	 * English to French
	 *
	 * @param locale
	 * @param ddxId
	 * @param alias
	 * @param userLongName
	 * @param userShortName
	 */
	public void updateExtendedDataDictionaryFieldName(String locale, String ddxId, String alias, String userLongName,
			String userShortName) {
		DataDictionary dictionary = DataDictionaryUtils
				.getExtendedDictionaryById(ddxId, getBroker());
		DataDictionaryField dataDictionaryField = dictionary
				.findDataDictionaryFieldByAlias(alias);
		ExtendedDataField extendedDataField = (ExtendedDataField) dataDictionaryField.getExtendedDataField();
		if (extendedDataField != null) {
			extendedDataField.setUserLongName(userLongName);
			extendedDataField.setUserShortName(userShortName);
			if (extendedDataField.isDirty()) {
				getBroker().saveBeanForced(extendedDataField);
			}
		}
	}

	/**
	 * Updates existing message resource beans with new keys and/or values, and
	 * updates templates
	 *
	 * @param locale          - Locale for message resource (optional)
	 * @param currentKey      - Existing key (optional) for querying a message
	 *                        resource
	 * @param currentValue    - Existing value (optional) for querying a message
	 *                        resource
	 * @param newKey          - New key (optional)
	 * @param newValue        - New value (optional)
	 * @param updateTemplates - Whether to update templates in the event of a
	 *                        changed key
	 * @param skipLocked      - Whether to skip locked templates when
	 *                        updateTemplates is true
	 */
	public void updateMessageResource(String locale, String currentKey, String currentValue, String newKey,
			String newValue, boolean updateTemplates, boolean skipLocked) {
		X2Criteria criteria = new X2Criteria();
		if (!StringUtils.isBlank(locale)) {
			criteria.addEqualTo(MessageResource.COL_LOCALE, locale);
		}
		if (!StringUtils.isBlank(currentKey)) {
			criteria.addEqualTo(MessageResource.COL_KEY, currentKey);
		}
		if (!StringUtils.isBlank(currentValue)) {
			criteria.addEqualTo(MessageResource.COL_VALUE, currentValue);
		}
		BeanQuery query = new BeanQuery(MessageResource.class, criteria);
		try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
			while (iterator.hasNext()) {
				MessageResource messageResource = (MessageResource) iterator.next();
				if (!StringUtils.isBlank(newKey)) {
					messageResource.setKey(newKey);
				}
				if (!StringUtils.isBlank(newValue)) {
					messageResource.setValue(newValue);
				}
				if (messageResource.isDirty()) {
					getBroker().saveBeanForced(messageResource);
					if (updateTemplates) {
						updateTemplateXml(null, false, null, false, currentKey, currentKey, newKey, true, skipLocked);
					}
				}
			}
		}
	}

	/**
	 * Changes the oid for a bean
	 *
	 * @param bean   - Bean to update with new oid
	 * @param newOid - New oid for the Reference Table
	 */
	public X2BaseBean updateOidOnBean(X2BaseBean bean, String newOid) {
		return updateOidOnBean(bean, newOid, false, null);
	}

	/**
	 * Updates values in tables when a reference code is changed. This method does
	 * not create a reference code bean for the newCode, nor does it change the
	 * reference code bean from oldCode to newCode. Those operations should be done
	 * separately. This is to update records that have a reference code value
	 * (equivalent to Query > Mass Update).
	 *
	 * @param referenceTableName
	 * @param oldCode            - old value to be changed
	 * @param newCode            - new value
	 * @param disableOldCode     - disables reference code bean for oldCode
	 */
	public void updateReferenceCodeValue(String referenceTableName, String oldCode, String newCode) {
		updateReferenceCodeValue(referenceTableName, oldCode, newCode, false);
	}

	/**
	 * Updates values in tables when a reference code is changed. Optionally
	 * disables the reference code bean for the old code (if exists).
	 *
	 * @param referenceTableName
	 * @param oldCode            - old value to be changed
	 * @param newCode            - new value
	 * @param disableOldCode     - disables reference code bean for oldCode
	 */
	public void updateReferenceCodeValue(String referenceTableName, String oldCode, String newCode,
			boolean disableOldCode) {
		ReferenceTable referenceTable = getReferenceTable(referenceTableName);
		if (referenceTable != null) {
			// Update core tables with fields linked to reference table
			for (DataFieldConfig dataFieldConfig : getFieldsForReferenceTable(referenceTable.getOid())) {
				DataTable dataTable = dataFieldConfig.getDataField().getDataTable();
				String attribute = dataFieldConfig.getDataField().getJavaName();
				X2Criteria criteria = new X2Criteria();
				criteria.addEqualTo(attribute, oldCode);
				UpdateQuery updateQuery = new UpdateQuery(dataTable.getDataClass(), criteria, attribute, newCode);
				int results = getBroker().executeUpdateQuery(updateQuery);
				logMessage("Updated " + results + " " + dataTable.getDatabaseName() + " records from '" + oldCode
						+ "' to '" + newCode + "'.");
			}

			// Update extended tables with fields linked to reference table
			Map<String, Collection<ExtendedDataField>> extendedFieldsForReferenceTable = getExtendedFieldsForReferenceTable(
					referenceTable.getOid());
			for (Map.Entry<String, Collection<ExtendedDataField>> entry : extendedFieldsForReferenceTable.entrySet()) {
				String ddxOid = entry.getKey();
				Collection<ExtendedDataField> extendedDataFieldsCollection = entry.getValue();
				for (ExtendedDataField extendedDataField : extendedDataFieldsCollection) {
					DataTable dataTable = extendedDataField.getDataFieldConfig().getDataField().getDataTable();
					String attribute = extendedDataField.getDataFieldConfig().getDataField().getJavaName();
					X2Criteria criteria = new X2Criteria();
					criteria.addEqualTo(attribute, oldCode);
					criteria.addEqualTo("extendedDataDictionaryOid", ddxOid);
					UpdateQuery updateQuery = new UpdateQuery(dataTable.getDataClass(), criteria, attribute, newCode);
					try {
						int results = getBroker().executeUpdateQuery(updateQuery);
						logMessage("Updated " + results + " " + dataTable.getDatabaseName() + " records from '"
								+ oldCode + "' to '" + newCode + "'.");
					} catch (Exception e) {
						logMessage("Can't update '" + oldCode + "' to '" + newCode + "' in " + attribute + " on "
								+ dataTable.getDatabaseName() + ". If critical that these values be "
								+ "updated, use a query then mass update.");
					}
				}
			}

			// Disable old code
			if (disableOldCode) {
				ReferenceCode referenceCode = getReferenceCode(referenceTable.getOid(), oldCode);
				if (referenceCode != null) {
					referenceCode.setDisabledIndicator(Boolean.TRUE);
					if (referenceCode.isDirty()) {
						getBroker().saveBeanForced(referenceCode);
					}
				}
			}

		} else {
			logErrorMessage("Unable to find reference table '" + referenceTableName + "'. Codes were not changed from '"
					+ oldCode + "' to '" + newCode + "'.");
		}
	}

	/**
	 * Updates context and/or name of template, with option to update other
	 * templates that include to it
	 *
	 * @param currentContext       - Existing context (for querying the template)
	 * @param currentName          - Existing name (for querying the template)
	 * @param newContext           - New / updated context (optional)
	 * @param newName              - New / updated name (optional)
	 * @param updateOtherTemplates - Whether to update other templates, in the event
	 *                             of a changed context
	 * @param skipLocked           - Whether to skip locked templates if
	 *                             updateOtherTemplates is true
	 */
	public void updateTemplate(String currentContext, String currentName, String newContext, String newName,
			boolean updateOtherTemplates, boolean skipLocked) {
		X2Criteria criteria = new X2Criteria();
		if (!StringUtils.isBlank(currentContext)) {
			criteria.addEqualTo(ViewTemplate.COL_CONTEXT, currentContext);
		}
		if (!StringUtils.isBlank(currentName)) {
			criteria.addEqualTo(ViewTemplate.COL_NAME, currentName);
		}
		BeanQuery query = new BeanQuery(ViewTemplate.class, criteria);
		try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
			int count = 0;
			while (iterator.hasNext()) {
				ViewTemplate viewTemplate = (ViewTemplate) iterator.next();
				if (!StringUtils.isBlank(newContext)) {
					viewTemplate.setContext(newContext);
				}
				if (!StringUtils.isBlank(newName)) {
					viewTemplate.setName(newName);
				}
				if (viewTemplate.isDirty()) {
					getBroker().saveBeanForced(viewTemplate);
					count++;
					if (updateOtherTemplates) {
						updateTemplateXml(null, false, null, false, currentContext, currentContext, newContext, true,
								skipLocked);
					}
				}
			}
			if (!StringUtils.isBlank(newContext)) {
				logMessage("Updated context for " + count + " template(s)");
			}
			if (!StringUtils.isBlank(newName)) {
				logMessage("Updated name for " + count + " template(s)");
			}
		}
	}

	/**
	 * Replaces a string in all templates that have that string Must be used with
	 * extreme care
	 *
	 * @param context                   - Template context for querying the
	 *                                  template(s) to update
	 * @param beginsWithContext         - If true, query for context using
	 *                                  beginsWith
	 * @param name                      - Template name for querying the template(s)
	 *                                  to update
	 * @param updateDefaultTemplateOnly - When name is null, searches for default
	 *                                  template
	 * @param searchFor                 - String to search for. Looking for an
	 *                                  absence of a string in order to add new
	 *                                  content.
	 * @param stringToReplace           - Current string/value to replace [usually
	 *                                  an anchor string that will be replaced with
	 *                                  the same plus new content]
	 * @param newString                 - New string/value that will be replaced
	 *                                  [usually includes the stringToReplace but
	 *                                  could be different if making a wholesale
	 *                                  change rather than adding new content]
	 * @param replaceIfAlreadyExists    - Whether to replace even if the searchFor
	 *                                  string already exists in the definition
	 *                                  (e.g., in another location)
	 * @param skipLocked                - Skips locked templates
	 */
	public void updateTemplateXml(String context, boolean beginsWithContext, String name,
			boolean updateDefaultTemplateOnly, String searchFor, String stringToReplace, String newString,
			boolean replaceIfAlreadyExists, boolean skipLocked) {
		if (!StringUtils.isBlank(stringToReplace)) {
			X2Criteria criteria = new X2Criteria();
			if (!StringUtils.isBlank(context)) {
				if (beginsWithContext) {
					criteria.addBeginsWith(ViewTemplate.COL_CONTEXT, context);
				} else {
					criteria.addEqualTo(ViewTemplate.COL_CONTEXT, context);
				}
			}
			if (!StringUtils.isBlank(name)) {
				criteria.addEqualTo(ViewTemplate.COL_NAME, name);
			} else if (updateDefaultTemplateOnly) {
				// Get System Preference for default template oid
				X2Criteria prdCriteria = new X2Criteria();
				prdCriteria.addEqualTo(
						SystemPreference.REL_PREFERENCE_DEFINITION + "." + SystemPreferenceDefinition.COL_KEY, context);
				SystemPreference systemPreference = getBroker()
						.getBeanByQuery(new BeanQuery(SystemPreferenceDefinition.class, prdCriteria));
				if (systemPreference != null) {
					criteria.addEqualTo(X2BaseBean.COL_OID, systemPreference.getValue());
				}
			}
			if (skipLocked) {
				criteria.addNotEqualTo(ViewTemplate.COL_LOCKED, Boolean.TRUE);
			}
			criteria.addContains(ViewTemplate.COL_VIEW_DEFINITION, stringToReplace);
			BeanQuery query = new BeanQuery(ViewTemplate.class, criteria);
			try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
				int count = 0;
				while (iterator.hasNext()) {
					ViewTemplate viewTemplate = (ViewTemplate) iterator.next();

					String viewDefinition = viewTemplate.getViewDefinition();
					if (!viewDefinition.contains(searchFor) || replaceIfAlreadyExists) {
						viewTemplate.setViewDefinition(viewDefinition.replaceAll(stringToReplace, newString));
						if (viewTemplate.isDirty()) {
							getBroker().saveBeanForced(viewTemplate);
							count++;
						}
					}
				}
				logMessage("Updated XML definition for " + count + " template(s) with context " + context);
			}
		}
	}

	/**
	 * Updates an element within a template by traversing the DOM
	 *
	 * @param viewTemplate
	 * @param attributeToFind         - attribute to search for
	 * @param attributeValueToFind    - value of that attribute for a match
	 * @param attributeToReplace      - attribute to replace
	 * @param attributeValueToReplace - value for that attribute to update
	 * @throws JDOMException
	 * @throws IOException
	 */
	public void updateTemplateXml(ViewTemplate viewTemplate, String attributeToFind, String attributeValueToFind,
			String attributeToReplace, String attributeValueToReplace) throws JDOMException, IOException {
		logMessage(viewTemplate.getContext());
		Element root = getXmlRoot(viewTemplate.getViewDefinition());
		if (root != null) {
			if (updateXml(root, attributeToFind, attributeValueToFind, attributeToReplace, attributeValueToReplace,
					false)) {
				viewTemplate.setViewDefinition(elementToString(root));
				if (viewTemplate.isDirty()) {
					getBroker().saveBeanForced(viewTemplate);
					logMessage(
							"Updated XML definition for " + viewTemplate.getContext() + " - " + viewTemplate.getName());
				}
			}
		}
	}

	/**
	 * Exports XML for a collection or list of data validation rules
	 *
	 * @param dataValidationRules
	 * @throws Exception
	 */
	public void xmlExportDataValidationRule(Collection<DataValidationRule> dataValidationRules) throws Exception {

		String devOpsTask = (String) getParameter(PARAM_DEVOPS_TASK);
		String deploymentMethod = (String) getParameter(PARAM_DEPLOYMENT_METHOD);

		StringBuilder xml = new StringBuilder();

		for (DataValidationRule dataValidationRule : dataValidationRules) {
			xmlExportDataValidationRule(xml, dataValidationRule);
		}

		// Export results to message window, deployment procedure, or OBC Change Log
		exportResults(devOpsTask, deploymentMethod, xml, false);
	}

	/**
	 * Exports XML for a data validation rule
	 *
	 * @param xml
	 * @param dataValidationRule
	 * @throws Exception
	 */
	public void xmlExportDataValidationRule(StringBuilder xml, DataValidationRule dataValidationRule) throws Exception {
		Element element = dataValidationRule.marshal();
		if (!StringUtils.isBlank(dataValidationRule.getDataTableConfigOid())) {
			element.setAttribute(DataValidationRule.COL_DATA_TABLE_CONFIG_OID,
					dataValidationRule.getDataTableConfigOid());
		}
		if (!StringUtils.isBlank(dataValidationRule.getExtendedDataTableOid())) {
			element.setAttribute(EXTENDED_DATA_DICTIONARY_ID_ATTR,
					dataValidationRule.getExtendedDataTable().getExtendedDataDictionary().getId());
		}
		xml.append(elementToString(element) + "\n");
	}

	/**
	 * Add new content to a deployment package
	 *
	 * @param newContent
	 * @param addToTop
	 * @param content
	 * @return
	 */
	private String addContentToDeployment(String newContent, boolean addToTop, String content) {
		if (StringUtils.isBlank(content)) {
			content = deploymentProcedureInputStart + deploymentProcedureInputEnd;
		}
		if (addToTop) {
			// Add new content first (e.g., Reference Tables)
			content = content.replace("<" + PORTABLE_DOCUMENT + ">",
					"<" + PORTABLE_DOCUMENT + ">\n\n" + newContent + "\n");
		} else {
			// Add new content at the bottom
			content = content.replace("</" + PORTABLE_DOCUMENT + ">",
					"\n" + newContent + "\n</" + PORTABLE_DOCUMENT + ">");
		}
		return content;
	}

	/**
	 * Method to sift through all your declared member variables and sets them to
	 * null for proper garbage collection.
	 *
	 * @param classToClear                     Class
	 * @param logMemoryUsedStatisticsInToolLog boolean
	 */
	private void clearLocallyDeclaredVariables(Class classToClear) {
		for (Field field : classToClear.getDeclaredFields()) {
			if (field.getModifiers() == Modifier.PRIVATE) {
				field.setAccessible(true);
			}
			try {
				field.set(this, null);
			} catch (Exception e) {
				// Don't log exceptions
			}
		}
	}

	/**
	 * Gets the attribute string
	 *
	 * @param element    Element
	 * @param colContext String
	 * @return String
	 */
	private String getAttributeString(Element element, String colContext) {
		Attribute attribute = element.getAttribute(colContext);
		return attribute == null ? null : attribute.getValue();
	}

	/**
	 * Returns the bean that matches the values in the query values map
	 *
	 * @param element
	 * @param beanClass
	 * @param queryValuesMap
	 * @return
	 */
	private X2BaseBean getBeanForImport(Element element, Class beanClass, Map<String, String> queryValuesMap) {
		X2BaseBean bean = null;
		X2Criteria criteria = new X2Criteria();
		for (Map.Entry<String, String> entry : queryValuesMap.entrySet()) {
			criteria.addEqualTo(entry.getKey(), entry.getValue());
		}
		BeanQuery query = new BeanQuery(beanClass, criteria);
		if (getBroker().getCount(query) == 1) {
			// Only return the bean if there is exactly 1 match for the criteria
			bean = getBroker().getBeanByQuery(query);
		}

		return bean;
	}

	/**
	 * Gets the data field config for import
	 *
	 * @param element Element
	 * @return DataFieldConfig bean
	 */
	private DataFieldConfig getDataFieldConfig(Element element) {
		String dataTableOid = element.getAttributeValue(DATA_FIELD_CONFIG_DATA_TABLE_ATTR);
		String dataFieldOid = element.getAttributeValue(DataFieldConfig.COL_DATA_FIELD_OID);
		String longName = element.getAttributeValue(DataFieldConfig.COL_USER_LONG_NAME);
		String alias = element.getAttributeValue(DataFieldConfig.COL_ALIAS);
		String lengthString = element.getAttributeValue(DataFieldConfig.COL_USER_LENGTH);
		int length = Integer.parseInt(lengthString);

		return getDataFieldConfig(dataTableOid, dataFieldOid, alias, longName, length);
	}

	/**
	 * Gets the data field type for the data dictionary field
	 *
	 * @param dataDictionaryField
	 * @return
	 */
	private DataFieldType getDataFieldType(String databaseType) {
		DataFieldType dataFieldType = null;
		if (databaseType == null || DataField.CHAR_DATABASE_TYPE.equals(databaseType)
				|| DataField.VARCHAR_DATABASE_TYPE.equals(databaseType)
				|| DataField.CLOB_DATABASE_TYPE.equals(databaseType)) {
			dataFieldType = DataFieldType.CHAR;
		} else if (DataField.NUMERIC_DATABASE_TYPE.equals(databaseType)
				|| DataField.NULLABLE_NUMERIC_DATABASE_TYPE.equals(databaseType)) {
			dataFieldType = DataFieldType.INT;
		} else if (DataField.LOGICAL_DATABASE_TYPE.equals(databaseType)) {
			dataFieldType = DataFieldType.BOOLEAN;
		}
		return dataFieldType;
	}

	/**
	 * Gets the extended data field for import
	 *
	 * @param element           Element
	 * @param alias
	 * @param extendedDataTable
	 * @return DataFieldConfig bean
	 */
	private ExtendedDataField getExtendedDataField(Element element, ExtendedDataTable extendedDataTable, String alias) {
		String dataFieldConfigOid = element.getAttributeValue(EXTENDED_DATA_FIELD_CONFIG_OID_ATTR);
		// If dataFieldConfigOid is already occupied in the extended data dictionary,
		// it's OK to get a different field
		boolean dataFieldConfigOptional = true;

		String longName = element.getAttributeValue(EXTENDED_DATA_FIELD_LONG_NAME_ATTR);
		String lengthString = element.getAttributeValue(EXTENDED_DATA_FIELD_USER_LENGTH_ATTR);
		int length = Integer.parseInt(lengthString);

		return getExtendedDataField(extendedDataTable, alias, longName, length, dataFieldConfigOid,
				dataFieldConfigOptional);
	}

	/**
	 * Get the local extended dictionary oid
	 *
	 * @param element
	 * @param oid
	 * @param id
	 * @return
	 */
	private String getLocalSavedFilterDdxOid(Element element, String oid, String id) {
		/*
		 * The oid may be an assessment definition, extended data dictionary, or
		 * transcript definition OID. Query for each by OID. If not found by OID or if
		 * the ID / Name attributes don't match, query by ID / Name. This ensures we're
		 * saving the filter with the OIDs that exist in the local database.
		 */
		switch (oid.substring(0, 3).toUpperCase()) {
		case AssessmentDefinition.OBJECT_PREFIX:
			AssessmentDefinition assessmentDefinition = getLocalAssessmentDefinition(oid, id);
			if (assessmentDefinition != null) {
				oid = assessmentDefinition.getOid();
			}
			break;

		case ExtendedDataDictionary.OBJECT_PREFIX:
			ExtendedDataDictionary extendedDataDictionary = getLocalExtendedDataDictionary(oid, id);
			if (extendedDataDictionary != null) {
				oid = extendedDataDictionary.getOid();
			}
			break;

		case TranscriptDefinition.OBJECT_PREFIX:
			TranscriptDefinition transcriptDefinition = getLocalTranscriptDefinition(oid, id);
			if (transcriptDefinition != null) {
				oid = transcriptDefinition.getOid();
			}
			break;

		default:
			break;
		}

		return oid;
	}

	/**
	 * Gets the assessment definition by oid or by id
	 *
	 * @param oid
	 * @param id
	 * @return
	 */
	private AssessmentDefinition getLocalAssessmentDefinition(String oid, String id) {
		AssessmentDefinition assessmentDefinition = getBroker().getBeanByOid(AssessmentDefinition.class, oid);
		if (assessmentDefinition == null || !assessmentDefinition.getId().equals(id)) {
			// Query for assessment definition by ID
			X2Criteria criteria = new X2Criteria();
			criteria.addEqualTo(AssessmentDefinition.COL_ID, id);
			BeanQuery query = new BeanQuery(AssessmentDefinition.class, criteria);
			assessmentDefinition = getBroker().getBeanByQuery(query);
		}
		return assessmentDefinition;
	}

	/**
	 * Gets the extended data dictionary by oid or by id
	 *
	 * @param oid
	 * @param id
	 * @return
	 */
	private ExtendedDataDictionary getLocalExtendedDataDictionary(String oid, String id) {
		ExtendedDataDictionary extendedDataDictionary = getBroker().getBeanByOid(ExtendedDataDictionary.class, oid);
		if (extendedDataDictionary == null || !extendedDataDictionary.getId().equals(id)) {
			// Query for extended data dictionary by ID
			X2Criteria criteria = new X2Criteria();
			criteria.addEqualTo(ExtendedDataDictionary.COL_ID, id);
			BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
			extendedDataDictionary = getBroker().getBeanByQuery(query);
		}
		return extendedDataDictionary;
	}

	/**
	 * Gets the transcript definition by oid or by name
	 *
	 * @param oid
	 * @param name
	 * @return
	 */
	private TranscriptDefinition getLocalTranscriptDefinition(String oid, String name) {
		TranscriptDefinition transcriptDefinition = getBroker().getBeanByOid(TranscriptDefinition.class, oid);
		if (transcriptDefinition == null || !transcriptDefinition.getName().equals(name)) {
			// Query for transcript definition by name
			X2Criteria criteria = new X2Criteria();
			criteria.addEqualTo(TranscriptDefinition.COL_TRANSCRIPT_DEFINITION_NAME, name);
			BeanQuery query = new BeanQuery(TranscriptDefinition.class, criteria);
			transcriptDefinition = getBroker().getBeanByQuery(query);
		}
		return transcriptDefinition;
	}

	private Map<String, String> getQueryValuesMap(Element childElement, String foreignKeyBeanPath, String foreignKeyOid,
			List<String> uniqueBeanPaths) {
		Map<String, String> queryValuesMap = new HashMap<String, String>();
		if (foreignKeyBeanPath != null && foreignKeyOid != null) {
			queryValuesMap.put(foreignKeyBeanPath, foreignKeyOid);
		}
		for (String beanPath : uniqueBeanPaths) {
			if (!StringUtils.isBlank(childElement.getAttributeValue(beanPath))) {
				String value = childElement.getAttributeValue(beanPath);
				/*
				 * If a message resource, modify the key to replace the OID from the source
				 * database with the local tool OID
				 */
				if (MessageResource.COL_KEY.equals(beanPath)) {
					value = value.replace(childElement.getAttributeValue(foreignKeyBeanPath), foreignKeyOid);
					/*
					 * Update the element with the local values so the setAttributesOnBean method
					 * can run unmodified
					 */
					childElement.setAttribute(foreignKeyBeanPath, foreignKeyOid);
					childElement.setAttribute(MessageResource.COL_KEY, value);
				}
				queryValuesMap.put(beanPath, value);
			}
		}
		return queryValuesMap;
	}

	/**
	 * Import calculated field
	 *
	 * @param element
	 * @param dictionary
	 */
	private void importCalculatedField(Element element, DataDictionary dictionary) {
		String name = element.getAttributeValue(CALCULATED_FIELD_NAME);
		if (!StringUtils.isEmpty(name)) {
			CalculatedField calculatedField = getCalculatedField(name);
			if (calculatedField == null) {
				calculatedField = X2BaseBean.newInstance(CalculatedField.class, getBroker().getPersistenceKey());
				calculatedField.setName(name);
			}
			calculatedField.setOrganization1Oid(getOrganization().getOid());
			String value = element.getAttributeValue(CALCULATED_FIELD_DESCRIPTION);
			if (!StringUtils.isEmpty(value)) {
				calculatedField.setDescription(value);
			}
			value = element.getAttributeValue(CALCULATED_FIELD_CALCULATED_EXPRESSION);
			if (!StringUtils.isEmpty(value)) {
				calculatedField.setCalculatedExpression(value);
			}
			value = element.getAttributeValue(CALCULATED_FIELD_PROCEDURE_ID);
			if (!StringUtils.isEmpty(value)) {
				X2Criteria criteria = new X2Criteria();
				criteria.addEqualTo(Procedure.COL_ID, value);
				Procedure procedure = getBroker().getBeanByQuery(new BeanQuery(Procedure.class, criteria));
				if (procedure != null) {
					calculatedField.setProcedureOid(procedure.getOid());
				}
			}

			if (calculatedField.isDirty()) {
				getBroker().saveBeanForced(calculatedField);
				m_reloadDataDictionary = true;
			}

			DataFieldConfig dataFieldConfig = null;
			value = element.getAttributeValue(CALCULATED_FIELD_FIELD_OID);
			if (!StringUtils.isEmpty(value)) {
				dataFieldConfig = getDataFieldConfigByFieldOid(value);
			} else {
				value = element.getAttributeValue(CALCULATED_FIELD_FIELD_ALIAS);
				DataDictionaryField dictionaryField = dictionary.findDataDictionaryFieldByAlias(value);
				if (dictionaryField != null) {
					dataFieldConfig = dictionaryField.getDataFieldConfig();
				}
			}
			if (dataFieldConfig != null) {
				dataFieldConfig.setCalculatedFieldOid(calculatedField.getOid());
				getBroker().saveBeanForced(dataFieldConfig);
				m_reloadDataDictionary = true;
				logMessage("Calculated field: " + name + " updated");
			} else {
				logMessage("Calculated field: " + name + " error - data field not found");
			}
		}
	}

	/**
	 * Set audit type on a data field config
	 *
	 * @param element
	 */
	private void importDataFieldAudit(Element element) {
		String oid = element.getAttributeValue(FIELD_AUDIT_FIELD_OID);
		DataFieldConfig dataFieldConfig = getDataFieldConfigByFieldOid(oid);
		if (dataFieldConfig != null) {
			String type = element.getAttributeValue(FIELD_AUDIT_TYPE);
			if (!StringUtils.isEmpty(type)) {
				int value = Integer.parseInt(type);
				dataFieldConfig.setFieldAuditType(value);
				if (dataFieldConfig.isDirty()) {
					getBroker().saveBeanForced(dataFieldConfig);
					m_reloadDataDictionary = true;
				}
			}
		}
	}

	/**
	 * Import data table config
	 *
	 * @param element Element
	 */
	private void importDataTableConfig(Element element) {
		String dataTableOid = element.getAttributeValue(DataTableConfig.COL_DATA_TABLE_OID);

		Iterator iterator = (element.getChildren()).iterator();
		while (iterator.hasNext()) {
			Element childElement = (Element) iterator.next();
			if (DATA_FIELD_CONFIG.equals(childElement.getName())) {
				importDataFieldConfig(childElement, dataTableOid);
			}
		}
	}

	/**
	 * Import data field config
	 *
	 * @param element
	 * @param dataTableOid
	 */
	private void importDataFieldConfig(Element element, String dataTableOid) {
		DataFieldConfig dataFieldConfig = getDataFieldConfig(element);

		if (dataFieldConfig != null) {
			for (Attribute attribute : (List<Attribute>) element.getAttributes()) {
				if (DataFieldConfig.COL_CALCULATED_FIELD_OID.equals(attribute.getName())
						|| DATA_FIELD_CONFIG_CALC_FIELD_NAME_ATTR.equals(attribute.getName())) {
					// Set related calculated field by oid or by name
					setCalculatedField(dataFieldConfig, element, DataFieldConfig.COL_CALCULATED_FIELD_OID,
							DATA_FIELD_CONFIG_CALC_FIELD_NAME_ATTR);
				} else if (DataFieldConfig.COL_CHANGE_REFERENCE_TABLE_OID.equals(attribute.getName())
						|| DATA_FIELD_CONFIG_CHANGE_REF_TABLE_NAME_ATTR.equals(attribute.getName())) {
					// Set related change reference table by oid or by name
					setReferenceTable(dataFieldConfig, element, dataFieldConfig.getUserLongName(),
							DataFieldConfig.COL_CHANGE_REFERENCE_TABLE_OID,
							DataFieldConfig.COL_CHANGE_REFERENCE_TABLE_OID,
							DATA_FIELD_CONFIG_CHANGE_REF_TABLE_NAME_ATTR);
				} else if (DataFieldConfig.COL_COMMENT_BANK_TABLE_OID.equals(attribute.getName())
						|| PortableHelper.XML_COMMENT_BANK_TABLE_NAME.equals(attribute.getName())) {
					// Set related comment bank table by oid or by name
					setCommentBankTable(dataFieldConfig, element, DataFieldConfig.COL_COMMENT_BANK_TABLE_OID,
							PortableHelper.XML_COMMENT_BANK_TABLE_NAME);
				} else if (DataFieldConfig.COL_REFERENCE_TABLE_OID.equals(attribute.getName())
						|| PortableHelper.XML_REFERENCE_TABLE_NAME.equals(attribute.getName())) {
					// Set related reference table by oid or by name
					setReferenceTable(dataFieldConfig, element, dataFieldConfig.getUserLongName(),
							DataFieldConfig.COL_REFERENCE_TABLE_OID, DataFieldConfig.COL_REFERENCE_TABLE_OID,
							PortableHelper.XML_REFERENCE_TABLE_NAME);
				} else if (!DATA_FIELD_CONFIG_DATA_TABLE_ATTR.equals(attribute.getName())
						&& !DataFieldConfig.COL_DATA_FIELD_OID.equals(attribute.getName())) {
					setValueByBeanPath(DataFieldConfig.class.getName(), m_dictionary, dataFieldConfig, element,
							attribute.getName());
				}
			}

			if (dataFieldConfig.isDirty()) {
				getBroker().saveBeanForced(dataFieldConfig);
				m_reloadDataDictionary = true;
			}
			// Log import even if the bean didn't get modified ... reassures user looking at
			// log that import was successful
			logMessage("Imported field config into " + dataTableOid + ": alias = " + dataFieldConfig.getAlias()
					+ " and oid = " + dataFieldConfig.getDataFieldOid());
		} else {
			logErrorMessage("FAILED importing field config into " + dataTableOid + ": alias = "
					+ element.getAttributeValue(DataFieldConfig.COL_ALIAS));
		}
	}

	/**
	 * Import data validation rule
	 *
	 * @param element
	 */
	private void importDataValidationRule(Element element) {
		String dataTableConfigOid = element.getAttributeValue(DataValidationRule.COL_DATA_TABLE_CONFIG_OID);
		String extendedDataDictionaryId = element.getAttributeValue(EXTENDED_DATA_DICTIONARY_ID_ATTR);
		String id = element.getAttributeValue(DataValidationRule.COL_ID);
		boolean disabled = Boolean.parseBoolean(element.getAttributeValue(DataValidationRule.COL_DISABLED));

		String errorMessage = element.getChildText(DataValidationRule.COL_ERROR_MESSAGE);
		String condition = element.getChildText(DataValidationRule.COL_CONDITION);
		String expression = element.getChildText(DataValidationRule.COL_EXPRESSION);
		String comments = element.getChildText(DataValidationRule.COL_COMMENTS);

		createDataValidationRule(dataTableConfigOid, extendedDataDictionaryId, id, errorMessage, disabled, condition,
				expression, comments);
	}

	/**
	 * Imports a bean using fields from the data dictionary
	 *
	 * @param element
	 * @param beanClass
	 * @param queryValuesMap
	 */
	private void importElement(Element element, Class beanClass, Map<String, String> queryValuesMap) {
		X2BaseBean bean = null;
		if (!queryValuesMap.isEmpty()) {
			bean = getBeanForImport(element, beanClass, queryValuesMap);
		}
		if (bean == null) {
			bean = X2BaseBean.newInstance(beanClass, getBroker().getPersistenceKey());
			for (Map.Entry<String, String> entry : queryValuesMap.entrySet()) {
				/*
				 * Add query values to the child element so that foreign keys (local to this
				 * database) can be set by looping through all the fields of the class
				 */
				element.setAttribute(entry.getKey(), entry.getValue());
			}
		}
		setAttributesOnBean(element, bean, beanClass, false, null);

		if (bean.isDirty()) {
			getBroker().saveBeanForced(bean);
		}
	}

	/**
	 * Imports child elements for the tag name
	 *
	 * @param element
	 * @param beanClass
	 * @param name
	 * @param foreignKeyBeanPath
	 * @param foreignKeyOid
	 * @param uniqueBeanPaths
	 */
	private void importElements(Element element, Class beanClass, String foreignKeyBeanPath,
			String foreignKeyOid, List<String> uniqueBeanPaths) {
		Iterator iterator = (element.getChildren(getElementNameForClass(beanClass))).iterator();
		while (iterator.hasNext()) {
			Element childElement = (Element) iterator.next();
			// Create map of bean paths and values to query for the bean to import
			Map<String, String> queryValuesMap = getQueryValuesMap(childElement, foreignKeyBeanPath, foreignKeyOid,
					uniqueBeanPaths);
			importElement(childElement, beanClass, queryValuesMap);
		}
	}

	/**
	 * Import field set with members
	 *
	 * @param element
	 */
	private void importFieldSet(Element element) {
		String oid = element.getAttributeValue(X2BaseBean.COL_OID);
		String context = element.getAttributeValue(FieldSet.COL_CONTEXT);
		String name = element.getAttributeValue(FieldSet.COL_NAME);
		String extendedDataDictionaryOid = element.getAttributeValue(FieldSet.COL_EXTENDED_DATA_DICTIONARY_OID);
		String extendedDataDictionaryId = element.getAttributeValue(EXTENDED_DATA_DICTIONARY_ID_ATTR);
		String ownerOid = element.getAttributeValue(FieldSet.COL_OWNER_OID);
		int ownerType = Integer.parseInt(element.getAttributeValue(FieldSet.COL_OWNER_TYPE));
		boolean virtualIndicator = Boolean.parseBoolean(element.getAttributeValue(FieldSet.COL_VIRTUAL_INDICATOR));
		String virtualSource = element.getAttributeValue(FieldSet.COL_VIRTUAL_SOURCE);
		boolean ddxError = false;

		if (extendedDataDictionaryOid != null) {
			ExtendedDataDictionary extendedDataDictionary = getLocalExtendedDataDictionary(extendedDataDictionaryOid,
					extendedDataDictionaryId);
			if (extendedDataDictionary != null) {
				extendedDataDictionaryOid = extendedDataDictionary.getOid();
			} else {
				logErrorMessage(
						"Can't find Extended Data Dictionary for field set: " + name + ", with context = " + context);
				ddxError = true;
			}
		}

		if (!ddxError) {
			// Get field set by oid
			FieldSet fieldSet = getBroker().getBeanByOid(FieldSet.class, oid);

			if (fieldSet == null || !context.equals(fieldSet.getContext())
					|| !ownerOid.equals(fieldSet.getOwnerOid())) {
				// Query if null or attributes are different (don't want to overwrite the wrong
				// field set)
				fieldSet = getFieldSet(context, name, extendedDataDictionaryOid, ownerOid, ownerType);
			}

			if (fieldSet == null) {
				fieldSet = X2BaseBean.newInstance(FieldSet.class, getBroker().getPersistenceKey());
				fieldSet.setContext(context);
				fieldSet.setName(name);
				fieldSet.setExtendedDataDictionaryOid(extendedDataDictionaryOid);
				fieldSet.setOwnerOid(ownerOid);
				fieldSet.setOwnerType(ownerType);
			}
			fieldSet.setVirtualIndicator(virtualIndicator);
			fieldSet.setVirtualSource(virtualSource);

			if (fieldSet.isDirty()) {
				getBroker().saveBeanForced(fieldSet);
			}

			if (!StringUtils.isBlank(oid) && !fieldSet.getOid().trim().equals(oid.trim())) {
				fieldSet = (FieldSet) updateOidOnBean(fieldSet, oid, true, fieldSet.getContext());
			}

			Iterator childIterator = (element.getChildren()).iterator();
			while (childIterator.hasNext()) {
				Element childElement = (Element) childIterator.next();
				if (FIELD_SET_MEMBER.equals(childElement.getName())) {
					int fieldType = Integer.parseInt(childElement.getAttributeValue(FieldSetMember.COL_FIELD_TYPE));
					String objectOid = childElement.getAttributeValue(FieldSetMember.COL_OBJECT_OID);
					String relation = childElement.getAttributeValue(FieldSetMember.COL_RELATION);
					int sequenceNumber = Integer
							.parseInt(childElement.getAttributeValue(FieldSetMember.COL_SEQUENCE_NUMBER));
					int width = Integer.parseInt(childElement.getAttributeValue(FieldSetMember.COL_WIDTH));

					FieldSetMember fieldSetMember = getFieldSetMember(fieldSet.getOid(), objectOid);

					if (fieldSetMember == null) {
						fieldSetMember = X2BaseBean.newInstance(FieldSetMember.class, getBroker().getPersistenceKey());
						fieldSetMember.setFieldSetOid(fieldSet.getOid());
						fieldSetMember.setObjectOid(objectOid);
					}
					fieldSetMember.setFieldType(fieldType);
					fieldSetMember.setRelation(relation);
					fieldSetMember.setSequenceNumber(sequenceNumber);
					fieldSetMember.setWidth(width);

					if (fieldSetMember.isDirty()) {
						getBroker().saveBeanForced(fieldSetMember);
					}
				}
			}
		}
	}

	/**
	 * Import data field config
	 *
	 * @param element Element
	 */
	private void importExtendedDataDictionary(Element element) {
		String extendedDataDictionaryId = element.getAttributeValue(ExtendedDataDictionary.COL_ID);
		ExtendedDataDictionary extendedDataDictionary = importExtendedDataDictionary(element, extendedDataDictionaryId);

		Iterator tableIterator = (element.getChildren()).iterator();
		while (tableIterator.hasNext()) {
			Element tableElement = (Element) tableIterator.next();
			if (EXTENDED_DATA_TABLE.equals(tableElement.getName())) {
				String dataTableOid = tableElement.getAttributeValue(EXTENDED_DATA_TABLE_ID_ATTR);
				ExtendedDataTable extendedDataTable = importExtendedDataTable(tableElement, extendedDataDictionary,
						dataTableOid);

				if (extendedDataTable != null) {
					Iterator fieldIterator = (tableElement.getChildren()).iterator();
					while (fieldIterator.hasNext()) {
						Element fieldElement = (Element) fieldIterator.next();
						if (EXTENDED_DATA_FIELD.equals(fieldElement.getName())) {
							importExtendedDataField(extendedDataDictionaryId, dataTableOid, extendedDataTable,
									fieldElement);
						}
					}
				} else {
					logErrorMessage("Unable to import fields for table "
							+ tableElement.getAttributeValue(EXTENDED_DATA_TABLE_NAME_ATTR));
				}
			}
		}
	}

	/**
	 * Import extended data dictionary, including modifying the oid if one is
	 * specified in the element
	 *
	 * @param element
	 * @param extendedDataDictionaryId
	 * @return
	 */
	private ExtendedDataDictionary importExtendedDataDictionary(Element element, String extendedDataDictionaryId) {

		ExtendedDataDictionary extendedDataDictionary = getExtendedDataDictionary(extendedDataDictionaryId);
		if (extendedDataDictionary == null) {
			extendedDataDictionary = X2BaseBean.newInstance(ExtendedDataDictionary.class,
					getBroker().getPersistenceKey());
			extendedDataDictionary.setOrganization1Oid(getOrganization().getRootOrganization().getOid());
			extendedDataDictionary.setId(element.getAttributeValue(ExtendedDataDictionary.COL_ID));
		}
		setValueByBeanPath(ExtendedDataDictionary.COL_AUDIT_TYPE, DataFieldType.INT, extendedDataDictionary, element,
				ExtendedDataDictionary.COL_AUDIT_TYPE);
		setValueByBeanPath(ExtendedDataDictionary.COL_CATEGORY, DataFieldType.CHAR, extendedDataDictionary, element,
				ExtendedDataDictionary.COL_CATEGORY);
		setValueByBeanPath(ExtendedDataDictionary.COL_DESCRIPTION, DataFieldType.CHAR, extendedDataDictionary, element,
				ExtendedDataDictionary.COL_DESCRIPTION);
		setValueByBeanPath(ExtendedDataDictionary.COL_GE_PLAN_INDICATOR, DataFieldType.BOOLEAN, extendedDataDictionary,
				element, ExtendedDataDictionary.COL_GE_PLAN_INDICATOR);
		setValueByBeanPath(ExtendedDataDictionary.COL_HEALTH_INDICATOR, DataFieldType.BOOLEAN, extendedDataDictionary,
				element, ExtendedDataDictionary.COL_HEALTH_INDICATOR);
		setValueByBeanPath(ExtendedDataDictionary.COL_ICON_FILENAME, DataFieldType.CHAR, extendedDataDictionary,
				element, ExtendedDataDictionary.COL_ICON_FILENAME);
		setValueByBeanPath(ExtendedDataDictionary.COL_NAME, DataFieldType.CHAR, extendedDataDictionary, element,
				ExtendedDataDictionary.COL_NAME);
		setValueByBeanPath(ExtendedDataDictionary.COL_PD_INDICATOR, DataFieldType.BOOLEAN, extendedDataDictionary,
				element, ExtendedDataDictionary.COL_PD_INDICATOR);
		setValueByBeanPath(ExtendedDataDictionary.COL_RECORD_LEVEL_SECURITY_INDICATOR, DataFieldType.BOOLEAN,
				extendedDataDictionary, element, ExtendedDataDictionary.COL_RECORD_LEVEL_SECURITY_INDICATOR);
		setValueByBeanPath(ExtendedDataDictionary.COL_SEQUENCE_NUMBER, DataFieldType.INT, extendedDataDictionary,
				element, ExtendedDataDictionary.COL_SEQUENCE_NUMBER);
		setValueByBeanPath(ExtendedDataDictionary.COL_SPED_INDICATOR, DataFieldType.BOOLEAN, extendedDataDictionary,
				element, ExtendedDataDictionary.COL_SPED_INDICATOR);

		if (extendedDataDictionary.isDirty()) {
			getBroker().saveBeanForced(extendedDataDictionary);
			m_reloadDataDictionary = true;
		}

		String extendedDataDictionaryOid = element.getAttributeValue(X2BaseBean.COL_OID);
		if (!StringUtils.isBlank(extendedDataDictionaryOid)
				&& !extendedDataDictionary.getOid().trim().equals(extendedDataDictionaryOid.trim())) {
			extendedDataDictionary = (ExtendedDataDictionary) updateOidOnBean(extendedDataDictionary,
					extendedDataDictionaryOid.trim());
		}

		return extendedDataDictionary;
	}

	/**
	 * Import extended data field
	 *
	 * @param extendedDataDictionaryId
	 * @param dataTableOid
	 * @param extendedDataTable
	 * @param element
	 */
	private void importExtendedDataField(String extendedDataDictionaryId, String dataTableOid,
			ExtendedDataTable extendedDataTable, Element element) {
		String alias = element.getAttributeValue(ExtendedDataField.COL_ALIAS);
		ExtendedDataField extendedDataField = getExtendedDataField(element, extendedDataTable, alias);
		if (extendedDataField != null) {
			setValueByBeanPath(ExtendedDataField.COL_USER_LONG_NAME, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_LONG_NAME_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_USER_SHORT_NAME, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_SHORT_NAME_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_ALIAS, DataFieldType.CHAR, extendedDataField, element,
					ExtendedDataField.COL_ALIAS);

			setValueByBeanPath(ExtendedDataField.COL_AUDIT_TYPE, DataFieldType.INT, extendedDataField, element,
					EXTENDED_DATA_FIELD_AUDIT_TYPE_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_DEFAULT_VALUE, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_DEFAULT_VALUE_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_DEPENDENCY, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_DEPENDENCY_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_DETAIL_CONTROL, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_DETAIL_CONTROL_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_DETAIL_WIDTH, DataFieldType.INT, extendedDataField, element,
					EXTENDED_DATA_FIELD_DETAIL_WIDTH_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_ENABLED_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_ENABLED_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_LIST_EDIT_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_LIST_EDIT_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_LIST_WIDTH, DataFieldType.INT, extendedDataField, element,
					EXTENDED_DATA_FIELD_LIST_WIDTH_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_LOCALIZED_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_LOCALIZED_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_PICKLIST_FIELDS, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_PICKLIST_FIELDS_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_READ_ONLY_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_READ_ONLY_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_RECORD_LEVEL_SECURITY_INDICATOR, DataFieldType.BOOLEAN,
					extendedDataField, element, EXTENDED_DATA_FIELD_RECORD_LEVEL_SECURITY_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_REQUIRED_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_REQUIRED_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_SEQUENCE_NUMBER, DataFieldType.INT, extendedDataField, element,
					EXTENDED_DATA_FIELD_SEQUENCE_NUMBER_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_SPELL_CHECK_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_SPELL_CHECK_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_UPDATE_INDICATOR, DataFieldType.BOOLEAN, extendedDataField,
					element, EXTENDED_DATA_FIELD_UPDATE_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_USER_DECIMAL, DataFieldType.INT, extendedDataField, element,
					EXTENDED_DATA_FIELD_USER_DECIMAL_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_USER_LENGTH, DataFieldType.INT, extendedDataField, element,
					EXTENDED_DATA_FIELD_USER_LENGTH_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_USER_TYPE, DataFieldType.CHAR, extendedDataField, element,
					EXTENDED_DATA_FIELD_USER_TYPE_ATTR);
			setValueByBeanPath(ExtendedDataField.COL_VALID_REFERENCE_ONLY_INDICATOR, DataFieldType.BOOLEAN,
					extendedDataField, element, EXTENDED_DATA_FIELD_VALID_REFERENCE_ONLY_INDICATOR_ATTR);

			/*
			 * TODO: Support importing calculated field and comment bank table on extended
			 * data fields setCalculatedField(dataFieldConfig, element,
			 * calculatedFieldOidAttr, calculatedFieldNameAttr);
			 * setCommentBankTable(dataFieldConfig, element, commentBankTableOidAttr,
			 * commentBankTableNameAttr);
			 */

			setReferenceTable(extendedDataField, element, extendedDataField.getUserLongName(),
					ExtendedDataField.COL_REFERENCE_TABLE_OID, EXTENDED_DATA_FIELD_REF_TABLE_OID_ATTR,
					PortableHelper.XML_REFERENCE_TABLE_NAME);

			if (extendedDataField.isDirty()) {
				getBroker().saveBeanForced(extendedDataField);
				m_reloadDataDictionary = true;
			}
			// Log import even if the bean didn't get modified ... reassures user looking at
			// log that import was successful
			logMessage("Imported extended field into " + extendedDataDictionaryId + " on " + dataTableOid + ": alias = "
					+ extendedDataField.getAlias() + " and field = "
					+ extendedDataField.getDataFieldConfig().getDataFieldOid());
		} else {
			logErrorMessage("FAILED importing extended field into " + extendedDataDictionaryId + " on " + dataTableOid
					+ ": alias = " + alias);
		}
	}

	/**
	 * Import extended data table
	 *
	 * @param element
	 * @param extendedDataDictionary
	 * @param dataTableOid
	 * @return
	 */
	private ExtendedDataTable importExtendedDataTable(Element element, ExtendedDataDictionary extendedDataDictionary,
			String dataTableOid) {
		ExtendedDataTable extendedDataTable = getExtendedDataTable(extendedDataDictionary.getId(), dataTableOid);
		if (extendedDataTable == null) {
			extendedDataTable = X2BaseBean.newInstance(ExtendedDataTable.class, getBroker().getPersistenceKey());
			DataTable dataTable = getBroker().getBeanByOid(DataTable.class, dataTableOid);
			if (dataTable != null) {
				extendedDataTable
						.setDataTableConfigOid(dataTable.getDataTableConfigs().stream().findFirst().get().getOid());
				extendedDataTable.setExtendedDataDictionaryOid(extendedDataDictionary.getOid());
			} else {
				logErrorMessage("Can't find data table " + dataTableOid + ". Aborting this extended table import");
				return null;
			}
		}

		setValueByBeanPath(ExtendedDataTable.COL_USER_NAME, DataFieldType.CHAR, extendedDataTable, element,
				EXTENDED_DATA_TABLE_NAME_ATTR);
		setValueByBeanPath(ExtendedDataTable.COL_SEQUENCE_NUMBER, DataFieldType.INT, extendedDataTable, element,
				ExtendedDataTable.COL_SEQUENCE_NUMBER);
		setValueByBeanPath(ExtendedDataTable.COL_AUDIT_TYPE, DataFieldType.INT, extendedDataTable, element,
				ExtendedDataTable.COL_AUDIT_TYPE);
		setValueByBeanPath(ExtendedDataTable.COL_RECORD_LEVEL_SECURITY_INDICATOR, DataFieldType.BOOLEAN,
				extendedDataTable, element, ExtendedDataTable.COL_RECORD_LEVEL_SECURITY_INDICATOR);

		if (extendedDataTable.isDirty()) {
			getBroker().saveBeanForced(extendedDataTable);
		}

		return extendedDataTable;
	}

	/**
	 * Import message resource
	 *
	 * @param element Element
	 */
	private void importMessageResource(Element element) {
		String key = getAttributeString(element, MessageResource.COL_KEY);
		String locale = getAttributeString(element, MessageResource.COL_LOCALE);
		Element valueElement = element.getChild(MessageResource.COL_VALUE);
		String value = valueElement != null ? valueElement.getValue() : null;
		Element valueLongElement = element.getChild(MessageResource.COL_VALUE_LONG);
		String valueLong = valueLongElement != null ? valueLongElement.getValue() : null;
		String objectOid = getAttributeString(element, MessageResource.COL_OBJECT_OID);

		if (!StringUtils.isEmpty(key) && !StringUtils.isEmpty(locale)) {
			MessageResource bean = null;
			// Get existing resource for key and locale
			Collection<MessageResource> messageResources = getMessageResources(key, locale, null);
			if (messageResources.size() > 0) {
				bean = getMessageResources(key, locale, null).stream().findFirst().get();
			} else {
				bean = X2BaseBean.newInstance(MessageResource.class, getBroker().getPersistenceKey());
			}
			bean.setKey(key);
			bean.setLocale(locale);
			bean.setValue(value);
			bean.setValueLong(valueLong);
			if (!StringUtils.isBlank(objectOid)) {
				bean.setObjectOid(objectOid);
			}

			if (bean.isDirty()) {
				getBroker().saveBeanForced(bean);
				value = !StringUtils.isBlank(bean.getValue()) ? bean.getValue() : bean.getValueLong();
				logMessage("Imported message resource: " + key + "-" + locale + " = " + value);
				m_reloadLocalizationCache = true;
			}
		}
	}

	/**
	 * Import a reference table
	 *
	 * @param element
	 */
	private void importReferenceTable(Element element) {
		String refCodeDataTableOid = "tblRefCode";

		String referenceTableOid = element.getAttributeValue(X2BaseBean.COL_OID);
		String referenceTableName = element.getAttributeValue(REFERENCE_TABLE_NAME_ATTR);

		ReferenceTable referenceTable = importReferenceTable(element, refCodeDataTableOid, referenceTableOid,
				referenceTableName);

		if (referenceTable != null) {
			Iterator iterator = (element.getChildren()).iterator();
			int count = 0;
			while (iterator.hasNext()) {
				Element childElement = (Element) iterator.next();
				if (REFERENCE_CODE.equals(childElement.getName())) {

					String code = childElement.getAttributeValue(ReferenceCode.COL_CODE);

					if (!StringUtils.isBlank(code)) {
						ReferenceCode bean = getReferenceCode(referenceTable.getOid(), code);
						if (bean == null) {
							bean = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
							bean.setReferenceTableOid(referenceTable.getOid());
							bean.setOwnerOid(referenceTable.getOwnerOid());
							bean.setOwnerType(referenceTable.getOwnerType());
						}
						setValueByBeanPath(ReferenceCode.COL_CODE, bean, childElement, ReferenceCode.COL_CODE);
						setValueByBeanPath(ReferenceCode.COL_CATEGORY, bean, childElement, ReferenceCode.COL_CATEGORY);
						setValueByBeanPath(ReferenceCode.COL_DEPENDENCY_CODE, bean, childElement,
								REFERENCE_CODE_DEPENDENCY_ATTR);
						setValueByBeanPath(ReferenceCode.COL_DESCRIPTION, bean, childElement,
								ReferenceCode.COL_DESCRIPTION);

						// Set the disabled indicator to false because the XML only includes this
						// attribute when true
						bean.setDisabledIndicator(Boolean.FALSE);
						setBooleanValueByBeanPath(bean, childElement, REFERENCE_CODE_DISABLED_ATTR,
								ReferenceCode.COL_DISABLED_INDICATOR);
						setValueByBeanPath(ReferenceCode.COL_EDFI_CODE, bean, childElement,
								REFERENCE_CODE_EDFI_CODE_ATTR);
						// Check if code has an extended dictionary ID
						String codeExtendedDictionaryId = childElement
								.getAttributeValue(REFERENCE_CODE_EXTENDED_DICTIONARY_ID_ATTR);
						if (!StringUtils.isBlank(codeExtendedDictionaryId)) {
							if (codeExtendedDictionaryId.equals(referenceTable.getExtendedDataDictionaryOid())) {
								// If ID is same as reference table, don't query, just use reference table's oid
								bean.setExtendedDataDictionaryOid(referenceTable.getExtendedDataDictionaryOid());
							} else {
								// Query for the extended dictionary if the IDs are different (which should
								// never be the case)
								ExtendedDataDictionary extendedDataDictionary = getExtendedDataDictionary(
										codeExtendedDictionaryId);
								if (extendedDataDictionary != null) {
									bean.setExtendedDataDictionaryOid(extendedDataDictionary.getOid());
								}
							}
						}
						setValueByBeanPath(ReferenceCode.COL_FEDERAL_CODE, bean, childElement,
								REFERENCE_CODE_FEDERAL_CODE_ATTR);
						setValueByBeanPath(ReferenceCode.COL_LOCAL_CODE, bean, childElement,
								REFERENCE_CODE_LOCAL_CODE_ATTR);
						setValueByBeanPath(ReferenceCode.COL_SIF_CODE, bean, childElement,
								REFERENCE_CODE_SIF_CODE_ATTR);
						setValueByBeanPath(ReferenceCode.COL_STATE_CODE, bean, childElement,
								REFERENCE_CODE_STATE_CODE_ATTR);
						setValueByBeanPath(ReferenceCode.COL_SYSTEM_CODE, bean, childElement,
								REFERENCE_CODE_SYSTEM_CODE_ATTR);
						setValueByBeanPath(ReferenceCode.COL_TEMPLATE_CONTEXT, bean, childElement,
								REFERENCE_CODE_TEMPLATE_CONTEXT_ATTR);
						if (childElement.getAttributeValue(REFERENCE_CODE_SEQUENCE_NUMBER_ATTR) != null) {
							bean.setSequenceNumber(Integer
									.parseInt(childElement.getAttributeValue(REFERENCE_CODE_SEQUENCE_NUMBER_ATTR)));
						}

						// Import extended fields
						Iterator customFieldsIterator = (childElement.getChildren()).iterator();
						while (customFieldsIterator.hasNext()) {
							DataDictionary dictionary = DataDictionary.getDistrictDictionary(
									bean.getExtendedDataDictionary(), getBroker().getPersistenceKey());
							if (dictionary == null) {
								dictionary = DataDictionary.getDistrictDictionary(
										referenceTable.getExtendedDataDictionary(), getBroker().getPersistenceKey());
							}
							Element customFieldsElement = (Element) customFieldsIterator.next();
							Iterator customFieldIterator = (customFieldsElement.getChildren()).iterator();
							while (customFieldIterator.hasNext()) {
								Element customFieldElement = (Element) customFieldIterator.next();
								String alias = customFieldElement
										.getAttributeValue(REFERENCE_CODE_CUSTOM_FIELD_ALIAS_ATTR);
								String value = customFieldElement.getValue();
								if (!StringUtils.isBlank(alias) && !StringUtils.isBlank(value)) {
									bean.setFieldValueByAlias(alias, value, dictionary);
								}
							}
						}

						getBroker().saveBeanForced(bean);
						count++;
					}
				}
			}

			logMessage("Imported " + count + " codes into the '" + referenceTableName + "' reference table");
		}
	}

	/**
	 * Import reference table, including modifying the oid if one is specified
	 *
	 * @param element
	 * @param refCodeDataTableOid
	 * @param referenceTableOid
	 * @param referenceTableName
	 * @return
	 */
	private ReferenceTable importReferenceTable(Element element, String refCodeDataTableOid, String referenceTableOid,
			String referenceTableName) {
		ReferenceTable referenceTable = getReferenceTable(referenceTableName);
		if (referenceTable == null) {
			String dataTableOid = element.getAttributeValue(ReferenceTable.COL_DATA_TABLE_OID);
			if (StringUtils.isBlank(dataTableOid)) {
				// If not specified, assume this reference table stores codes in the Reference
				// Code table
				dataTableOid = refCodeDataTableOid;
			}
			DataTable dataTable = getBroker().getBeanByOid(DataTable.class, dataTableOid);
			// Don't proceed if code table isn't REF_CODE
			if (dataTable == null || !refCodeDataTableOid.equals(dataTable.getOid().trim())) {
				logErrorMessage("This tool only supports importing codes into the REF_CODE table.");
				return null;
			}

			referenceTable = X2BaseBean.newInstance(ReferenceTable.class, getBroker().getPersistenceKey());
			referenceTable.setOwnerOid(getOrganization().getRootOrganization().getOid());
			referenceTable.setOwnerType(Ownable.OWNER_TYPE_ORG1);
			referenceTable.setUserName(referenceTableName);
			referenceTable.setDataTableOid(dataTable.getOid());
		}
		referenceTable.setCategory(element.getAttributeValue(ReferenceTable.COL_CATEGORY));
		referenceTable.setCodeLength(Integer.valueOf(element.getAttributeValue(REFERENCE_TABLE_CODE_LENGTH_ATTR)));
		referenceTable.setDataEntryViaViewFieldsInd(
				Boolean.parseBoolean(element.getAttributeValue(ReferenceTable.COL_DATA_ENTRY_VIA_VIEW_FIELDS_IND)));
		// Get Extended Data Dictionary ID as it will most likely be the same for all
		// the codes below
		String tableExtendedDictionaryId = element.getAttributeValue(REFERENCE_TABLE_EXTENDED_DICTIONARY_ID_ATTR);
		if (!StringUtils.isBlank(tableExtendedDictionaryId)) {
			ExtendedDataDictionary extendedDataDictionary = getExtendedDataDictionary(tableExtendedDictionaryId);
			if (extendedDataDictionary != null) {
				referenceTable.setExtendedDataDictionaryOid(extendedDataDictionary.getOid());
			}
		}
		referenceTable.setSequenceOrderndicator(
				Boolean.parseBoolean(element.getAttributeValue(REFERENCE_TABLE_SEQUENCE_ORDER_ATTR)));
		referenceTable.setViewFields(element.getAttributeValue(ReferenceTable.COL_VIEW_FIELDS));
		if (referenceTable.isDirty()) {
			getBroker().saveBeanForced(referenceTable);
			logMessage("Imported reference table '" + referenceTableName + "'");
		}

		if (!StringUtils.isBlank(referenceTableOid)
				&& !referenceTable.getOid().trim().equals(referenceTableOid.trim())) {
			referenceTable = (ReferenceTable) updateOidOnBean(referenceTable, referenceTableOid.trim());
		}

		return referenceTable;
	}

	/**
	 * Import saved filter and saved query
	 *
	 * @param element
	 * @throws IOException
	 */
	private void importSavedFilterQuery(Element element) throws IOException {
		SavedQuery savedQuery = null;

		Iterator childIterator = (element.getChildren()).iterator();
		while (childIterator.hasNext()) {
			Element childElement = (Element) childIterator.next();
			if (SAVED_QUERY.equals(childElement.getName())) {
				savedQuery = importSavedQuery(childElement);
			}
			if (SAVED_FILTER.equals(childElement.getName()) && savedQuery != null) {
				importSavedFilter(childElement, savedQuery);
			}
		}
	}

	/**
	 * Import a saved filter
	 *
	 * @param childElement
	 * @param savedQuery
	 */
	private void importSavedFilter(Element element, SavedQuery savedQuery) {
		String action = "Updated existing filter ";
		String oid = element.getAttributeValue(X2BaseBean.COL_OID);
		String context = element.getAttributeValue(SavedFilter.COL_CONTEXT);
		String name = element.getAttributeValue(SavedFilter.COL_NAME);
		String ownerOid = element.getAttributeValue(SavedFilter.COL_OWNER_OID);
		int ownerType = Integer.parseInt(element.getAttributeValue(SavedFilter.COL_OWNER_TYPE));
		int definitionType = Integer.parseInt(element.getAttributeValue(SavedFilter.COL_DEFINITION_TYPE));
		String extendedDataDictionaryOid = element.getAttributeValue(SavedFilter.COL_EXTENDED_DATA_DICTIONARY_OID);
		String extendedDataDictionaryId = element.getAttributeValue(EXTENDED_DATA_DICTIONARY_ID_ATTR);

		if (extendedDataDictionaryOid != null) {
			// Get local extended dictionary oid (ASD, DDX or GTD)
			extendedDataDictionaryOid = getLocalSavedFilterDdxOid(element, extendedDataDictionaryOid,
					extendedDataDictionaryId);
		}

		// Get saved filter by oid
		SavedFilter savedFilter = getBroker().getBeanByOid(SavedFilter.class, oid);

		if (savedFilter == null || !name.equals(savedFilter.getName()) || !context.equals(savedFilter.getContext())
				|| !ownerOid.equals(savedFilter.getOwnerOid())) {
			// Query if null or attributes are different (don't want to overwrite the wrong
			// filter)
			savedFilter = getSavedFilter(context, name, extendedDataDictionaryOid, ownerOid, ownerType);
		}

		if (savedFilter == null) {
			savedFilter = X2BaseBean.newInstance(SavedFilter.class, getBroker().getPersistenceKey());
		}
		savedFilter.setContext(context);
		savedFilter.setName(name);
		if (extendedDataDictionaryOid != null) {
			savedFilter.setExtendedDataDictionaryOid(extendedDataDictionaryOid);
		}
		savedFilter.setDefinitionOid(savedQuery.getOid());
		savedFilter.setDefinitionType(definitionType);
		savedFilter.setOwnerOid(ownerOid);
		savedFilter.setOwnerType(ownerType);

		if (savedFilter.isDirty()) {
			if (savedFilter.isNew()) {
				action = "Imported new filter ";
			}
			getBroker().saveBeanForced(savedFilter);
			logMessage(action + savedFilter.getName());
		}

		if (!StringUtils.isBlank(oid) && !savedFilter.getOid().trim().equals(oid.trim())) {
			savedFilter = (SavedFilter) updateOidOnBean(savedFilter, oid, true, savedFilter.getContext());
		}
	}

	/**
	 * Import a saved query
	 *
	 * @param element
	 * @return
	 * @throws IOException
	 */
	private SavedQuery importSavedQuery(Element element) throws IOException {
		String action = "Updated existing query ";
		String oid = element.getAttributeValue(X2BaseBean.COL_OID);
		String name = element.getAttributeValue(SavedQuery.COL_NAME);
		String dataTableOid = element.getAttributeValue(SavedQuery.COL_DATA_TABLE_OID);
		String ownerOid = element.getAttributeValue(SavedQuery.COL_OWNER_OID);
		int ownerType = Integer.parseInt(element.getAttributeValue(SavedQuery.COL_OWNER_TYPE));

		// Get query definition
		Element queryDefinitionElement = null;
		String queryDefinition = null;

		Iterator childIterator = (element.getChildren()).iterator();
		if (childIterator.hasNext()) {
			Element childElement = (Element) childIterator.next();
			if ("query".equals(childElement.getName())) {
				queryDefinitionElement = childElement;
			}
		}
		if (queryDefinitionElement != null) {
			queryDefinition = XML_ENCODING_STRING + elementToString(queryDefinitionElement);
		}

		// Get saved query by oid
		SavedQuery savedQuery = getBroker().getBeanByOid(SavedQuery.class, oid);

		// Check if null or key attributes are different (don't want to overwrite the
		// wrong query)
		if (savedQuery == null || !name.equals(savedQuery.getName())
				|| !dataTableOid.equals(savedQuery.getDataTableOid()) || !ownerOid.equals(savedQuery.getOwnerOid())) {
			// Query by name, dataTableOid and owner
			X2Criteria criteria = new X2Criteria();
			criteria.addEqualTo(SavedQuery.COL_NAME, name);
			criteria.addEqualTo(SavedQuery.COL_DATA_TABLE_OID, dataTableOid);
			criteria.addEqualTo(SavedQuery.COL_OWNER_OID, ownerOid);
			BeanQuery query = new BeanQuery(SavedQuery.class, criteria);
			savedQuery = getBroker().getBeanByQuery(query);
		}

		if (savedQuery == null) {
			savedQuery = X2BaseBean.newInstance(SavedQuery.class, getBroker().getPersistenceKey());
		}
		savedQuery.setName(name);
		savedQuery.setDataTableOid(dataTableOid);
		savedQuery.setQueryDefinition(queryDefinition);
		savedQuery.setOwnerOid(ownerOid);
		savedQuery.setOwnerType(ownerType);

		if (savedQuery.isDirty()) {
			if (savedQuery.isNew()) {
				action = "Imported new query ";
			}
			getBroker().saveBeanForced(savedQuery);
			logMessage(action + savedQuery.getName());
		}

		if (!StringUtils.isBlank(oid) && !savedQuery.getOid().trim().equals(oid.trim())) {
			savedQuery = (SavedQuery) updateOidOnBean(savedQuery, oid, false, null);
		}

		return savedQuery;
	}

	/**
	 * Executes SQL statements in the XML element. Supports select as well as
	 * insert/update/delete (type = 'update'). Only DML is supported.
	 *
	 * @param element
	 */
	private void importSqlAndExecute(Element element) {
		Iterator sqlIterator = (element.getChildren()).iterator();
		while (sqlIterator.hasNext()) {
			Element sqlElement = (Element) sqlIterator.next();
			String type = sqlElement.getName();
			String message = sqlElement.getAttributeValue(SQL_UPDATE_MESSAGE_ATTR);
			Boolean clearCache = true;
			if (sqlElement.getAttributeValue(SQL_CLEAR_CACHE) != null) {
				clearCache = Boolean.parseBoolean(sqlElement.getAttributeValue(SQL_CLEAR_CACHE));
			}
			List<String> sqlStatements = Arrays.asList(sqlElement.getValue().split(";"));

			for (String sql : sqlStatements) {
				if (sql != null && !StringUtils.isBlank(sql.trim())) {
					if (SQL_SELECT.equals(type)) {
						executeSqlSelect(sql);
					}
					if (SQL_UPDATE.equals(type)) {
						executeSqlUpdate(sql, message, clearCache);
					}

					if (SQL_STORED_PROCEDURE.equals(type)) {
						executeSqlStoredProcedure(sql, message, clearCache);
					}
				}
			}
		}
	}

	/**
	 * Import a table
	 *
	 * @param element
	 * @param name
	 * @param tableId
	 * @param table
	 * @throws IOException
	 */
	private void importTable(Element element, String name, String tableId, DataDictionaryTable table)
			throws IOException {
		logMessage("Finding table[" + tableId + "] for [" + name + "] - result = " + table);
		ObjectImporter importer = new ObjectImporter(getBroker(), true);
		// First pass performs analyze only.
		importer.start(table.getBeanClass(), getOrganization().getOid(), element);
		// second pass with analyze off. This will perform the import.
		importer.setAnalyze(false);
		Portable portable = importer.start(table.getBeanClass(), getOrganization().getOid(), element);
		logMessage("Imported portable object " + tableId + " oid = " + portable.getOid());
		m_reloadDataDictionary = true;
	}

	/**
	 * Imports a tool including the tool source code base64 encoded so that it
	 * doesn't have to be compiled in the target database.
	 *
	 *
	 * @param objectPrefix
	 * @param element
	 * @param toolsToRun
	 * @throws IOException
	 */
	private void importToolByteCode(Class toolClass, String objectPrefix, Element element,
			Map<Tool, Boolean> toolsToRun)
			throws IOException {
		String id = element.getAttributeValue(Tool.COL_ID);
		/*
		 * Object that will generate the SQL for this import (or update)
		 */
		SqlImport toolSqlImport = new SqlImport(objectPrefix, toolClass, m_dictionary, getBroker());

		// get tool if exists, create new if not
		X2BaseBean bean = getToolById(id, objectPrefix);
        if(objectPrefix.equals(ImportExportDefinition.OBJECT_PREFIX)) {
            bean = getToolById(id, objectPrefix, Integer.valueOf(element.getAttributeValue(ImportExportDefinition.COL_TYPE)));
        }
		if (bean == null) {
			toolSqlImport.setInsert(X2BaseBean.COL_OID, getBroker().generateOid(toolClass));
		} else {
			toolSqlImport.setUpdate(bean.getOid());
		}

		/*
		 * If importing a report, find the local report and procedure related to it
		 * using the tool ID included in the export
		 */
		String reportId = element.getAttributeValue(Report.COL_REPORT_OID);
		if (reportId != null) {
			X2BaseBean report = getToolById(reportId, Report.OBJECT_PREFIX);
			if (report != null) {
				element.setAttribute(Report.COL_REPORT_OID, report.getOid());
			}
		}
		String procedureId = element.getAttributeValue(Report.COL_PROCEDURE_OID);
		if (procedureId != null) {
			X2BaseBean procedure = getToolById(procedureId, Procedure.OBJECT_PREFIX);
			if (procedure != null) {
				element.setAttribute(Report.COL_PROCEDURE_OID, procedure.getOid());
			}
		}
		/*
		 * Add all fields for the tool class to the SQL insert/update statement
		 */
		toolSqlImport.addFields(element, toolClass, false, null);

		/*
		 * Get elements for the tool source code object
		 */
		Element compiledCodeElement = element.getChild(ToolSourceCode.COL_COMPILED_CODE);
		Element sourceCodeElement = element.getChild(ToolSourceCode.COL_SOURCE_CODE);
		Element inputDefinitionElement = element.getChild(ToolSourceCode.COL_INPUT_DEFINITION);

		if (compiledCodeElement != null || sourceCodeElement != null || inputDefinitionElement != null) {
			/*
			 * Object that will generate the SQL for the source code
			 */
			SqlImport sourceSqlImport = new SqlImport(ToolSourceCode.OBJECT_PREFIX, ToolSourceCode.class,
					m_dictionary,
					getBroker());
			if (bean == null) {
				sourceSqlImport.setInsert(X2BaseBean.COL_OID, getBroker().generateOid(ToolSourceCode.class));
			} else {
				sourceSqlImport.setUpdate(((Tool) bean).getSourceCodeOid());
			}
			/*
			 * Set values that are base64 encoded in the element
			 */
			if (compiledCodeElement != null && compiledCodeElement.getValue() != null) {
				// Decode the compiled source bytes
				byte[] compiledCodeBytes = Base64.getDecoder().decode(compiledCodeElement.getValue());
				sourceSqlImport.addField(ToolSourceCode.COL_COMPILED_CODE, compiledCodeBytes);
			}
			if (sourceCodeElement != null && sourceCodeElement.getValue() != null) {
				// Decode the source code bytes
				byte[] sourceCodeBytes = Base64.getDecoder().decode(sourceCodeElement.getValue());
				sourceSqlImport.addField(ToolSourceCode.COL_SOURCE_CODE, new String(sourceCodeBytes));
			}
			if (inputDefinitionElement != null && inputDefinitionElement.getValue() != null) {
				// Decode the input definition bytes
				byte[] inputDefinitionBytes = Base64.getDecoder().decode(inputDefinitionElement.getValue());
				sourceSqlImport.addField(ToolSourceCode.COL_INPUT_DEFINITION, new String(inputDefinitionBytes));
			}
			// Set external sources
			Element externalSourcesElement = element.getChild(ToolSourceCode.COL_EXTERNAL_SOURCES);
			if (externalSourcesElement != null) {
				Iterator<Element> children = externalSourcesElement.getChildren().iterator();
				if (children.hasNext()) {
					Element externalSources = children.next();
					sourceSqlImport.addField(ToolSourceCode.COL_EXTERNAL_SOURCES, elementToString(externalSources));
				}
			}
			// Tool Source Code OID to the tool
			toolSqlImport.addField(Report.COL_SOURCE_CODE_OID, sourceSqlImport.getOid());

			// Insert/Update the Tool Source Code
			executeSqlUpdate(sourceSqlImport.getSqlStatement(), sourceSqlImport.getBytes());
			m_reloadProcedureCache = true;
		}

		importElements(element, ToolNav.class, ToolNav.COL_TOOL_OID,
				toolSqlImport.getOid(), Arrays.asList(ToolNav.COL_NAV_ID));
		importElements(element, ToolRole.class, ToolRole.COL_TOOL_OID,
				toolSqlImport.getOid(), Arrays.asList(ToolRole.COL_ROLE_OID));
		importElements(element, ToolSchoolVisibility.class, ToolSchoolVisibility.COL_TOOL_OID,
				toolSqlImport.getOid(), Arrays.asList(ToolSchoolVisibility.COL_OWNER_OID));
		importElements(element, MessageResource.class, MessageResource.COL_OBJECT_OID,
				toolSqlImport.getOid(), Arrays.asList(MessageResource.COL_KEY, MessageResource.COL_LOCALE));
		m_reloadLocalizationCache = true;

		executeSqlUpdate(toolSqlImport.getSqlStatement(), toolSqlImport.getBytes());

		if (Boolean.parseBoolean(element.getAttributeValue(TOOL_BYTE_CODE_RUN_TOOL))) {
			boolean deleteAfterRun = Boolean.parseBoolean(element.getAttributeValue(TOOL_BYTE_CODE_DELETE_AFTER_RUN));
			Tool tool = (Tool) getToolById(id, objectPrefix);
			if (tool != null) {
				toolsToRun.put(tool, deleteAfterRun);
			}
		}
	}

	/**
	 * Import a user defined navigation
	 *
	 * @param element
	 */
	private void importUserDefinedNavigation(Element element) {
		String oid = getAttributeString(element, X2BaseBean.COL_OID);
		String parentNavigationId = getAttributeString(element, UserDefinedNavigation.COL_PARENT_NAVIGATION_ID);
		String position = getAttributeString(element, UserDefinedNavigation.COL_POSITION);
		String fieldC001 = getAttributeString(element, UserDefinedNavigation.COL_FIELD_C001);
		String navigationType = getAttributeString(element, UserDefinedNavigation.COL_NAVIGATION_TYPE);

		boolean org1View = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_ORG1_APPLICATION_INDICATOR));
		boolean org2View = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_ORG2_APPLICATION_INDICATOR));
		boolean org3View = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_ORG2_APPLICATION_INDICATOR));
		boolean org4View = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_ORG2_APPLICATION_INDICATOR));
		boolean org5View = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_ORG2_APPLICATION_INDICATOR));
		boolean personnelView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_PERSONNEL_APPLICATION_INDICATOR));
		boolean schoolView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_SCHOOL_APPLICATION_INDICATOR));
		boolean staffView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_STAFF_APPLICATION_INDICATOR));
		boolean familyView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_FAMILY_APPLICATION_INDICATOR));
		boolean studentView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_STUDENT_APPLICATION_INDICATOR));
		boolean iepView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_IEP_APPLICATION_INDICATOR));
		boolean iepSchoolView = Boolean
				.parseBoolean(element.getAttributeValue(UserDefinedNavigation.COL_IEP_SCHOOL_APPLICATION_INDICATOR));

		// Lookup by OID and parentNavigationId first
		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(X2BaseBean.COL_OID, oid);
		criteria.addEqualTo(UserDefinedNavigation.COL_PARENT_NAVIGATION_ID, parentNavigationId);
		BeanQuery query = new BeanQuery(UserDefinedNavigation.class, criteria);
		UserDefinedNavigation udn = getBroker().getBeanByQuery(query);
		// If not found, lookup by attributes
		if (udn == null) {
			udn = getUserDefinedNavigation(parentNavigationId, position, fieldC001, Boolean.valueOf(org1View),
					Boolean.valueOf(schoolView));
		}
		if (udn == null) {
			udn = X2BaseBean.newInstance(UserDefinedNavigation.class, getBroker().getPersistenceKey());
			udn.setParentNavigationId(parentNavigationId);
			udn.setPosition(Integer.valueOf(position).intValue());
		}
		udn.setFieldC001(fieldC001);
		udn.setNavigationType(Integer.valueOf(navigationType).intValue());
		udn.setOrg1ApplicationIndicator(org1View);
		udn.setOrg2ApplicationIndicator(org2View);
		udn.setOrg3ApplicationIndicator(org3View);
		udn.setOrg4ApplicationIndicator(org4View);
		udn.setOrg5ApplicationIndicator(org5View);
		udn.setPersonnelApplicationIndicator(personnelView);
		udn.setSchoolApplicationIndicator(schoolView);
		udn.setStaffApplicationIndicator(staffView);
		udn.setFamilyApplicationIndicator(familyView);
		udn.setStudentApplicationIndicator(studentView);
		udn.setIepApplicationIndicator(iepView);
		udn.setIepSchoolApplicationIndicator(iepSchoolView);
		getBroker().saveBeanForced(udn);

		if (!StringUtils.isBlank(oid) && !udn.getOid().trim().equals(oid.trim())) {
			// Set OID if it doesn't match value in input definition
			udn = (UserDefinedNavigation) updateOidOnBean(udn, oid);
		}

		Iterator iterator = (element.getChildren()).iterator();
		while (iterator.hasNext()) {
			Element childElement = (Element) iterator.next();
			if (USER_DEFINED_NAV_DEFINITION.equals(childElement.getName())) {
				try {
					Element navdefinition = (Element) childElement.getChildren().stream().findFirst().get();
					udn.setNavDefinition(XML_ENCODING_STRING + elementToString(navdefinition));
					getBroker().saveBeanForced(udn);
				} catch (Exception e) {
					logErrorMessage("Exception occurred getting custom navigation definition for " + udn.getFieldC001()
							+ ":\n" + ExceptionUtils.getFullStackTrace(e));
				}
			} else if (USER_DEFINED_NAV_RESOURCE.equals(childElement.getName())) {
				// Get nav resources
				try {
					Element messageResourceElement = (Element) childElement.getChildren().stream().findFirst().get();
					// Get attributes for the message resource
					String key = messageResourceElement.getAttributeValue(MessageResource.COL_KEY);
					String locale = messageResourceElement.getAttributeValue(MessageResource.COL_LOCALE);
					String value = messageResourceElement.getAttributeValue(MessageResource.COL_VALUE);
					String valueLong = messageResourceElement.getAttributeValue(MessageResource.COL_VALUE_LONG);

					// Lookup message resource by key and locale
					MessageResource messageResource = getMessageResource(key, locale);
					UserDefinedNavResource userDefinedNavResource = null;

					if (messageResource == null) {
						// Create a message resource
						messageResource = X2BaseBean.newInstance(MessageResource.class,
								getBroker().getPersistenceKey());
						messageResource.setKey(key);
						messageResource.setLocale(locale);
						messageResource.setValue(value);
						messageResource.setValueLong(valueLong);
						getBroker().saveBeanForced(messageResource);
					}

					// Get any existing UDR for this message resource
					userDefinedNavResource = getUserDefinedNavResource(messageResource, udn);
					if (userDefinedNavResource == null) {
						// Create a user defined nav resource
						userDefinedNavResource = X2BaseBean.newInstance(UserDefinedNavResource.class,
								getBroker().getPersistenceKey());
						userDefinedNavResource.setMessageResourceOid(messageResource.getOid());
						userDefinedNavResource.setUserDefinedNavigationOid(udn.getOid());
						getBroker().saveBeanForced(userDefinedNavResource);
					}
				} catch (Exception e) {
					logErrorMessage("Exception occurred getting navigation message resources for" + udn.getFieldC001()
							+ ":\n" + ExceptionUtils.getFullStackTrace(e));
				}
			}
		}

		m_reloadNavigation = true;
	}

	/**
	 * Import a view template
	 *
	 * @param element Element
	 * @throws IOException
	 */
	private void importViewTemplate(Element element) throws IOException {
		String context = getAttributeString(element, ViewTemplate.COL_CONTEXT);
		String name = getAttributeString(element, ViewTemplate.COL_NAME);
		String previousName = getAttributeString(element, VIEW_TEMPLATE_PREVIOUS_NAME_ATTR);
		String oid = getAttributeString(element, X2BaseBean.COL_OID);
		boolean overrideCustom = Boolean.parseBoolean(getAttributeString(element, VIEW_TEMPLATE_OVERRIDE_CUSTOM_ATTR));
		boolean defaultTemplate = Boolean
				.parseBoolean(getAttributeString(element, VIEW_TEMPLATE_DEFAULT_TEMPLATE_ATTR));
		boolean hideTemplate = Boolean
				.parseBoolean(getAttributeString(element, ViewTemplate.COL_HIDE_INDICATOR));

		if (!StringUtils.isEmpty(context) && !StringUtils.isEmpty(name)) {
			ViewTemplate viewTemplate = getBroker().getBeanByOid(ViewTemplate.class, oid);
			if (viewTemplate == null) {
				viewTemplate = getViewTemplate(context, name);
			}
			// If we're renaming the template, search for previous name
			ViewTemplate beanPreviousName = getViewTemplate(context, previousName);
			if (beanPreviousName != null && viewTemplate == null) {
				// Use previous named template only if new name does not exist
				viewTemplate = beanPreviousName;
			}

			// Update if template is not locked, or overrideCustom is true, and create new
			// if null
			if (viewTemplate == null || overrideCustom || !viewTemplate.getCustomIndicator()) {
				if (viewTemplate == null) {
					viewTemplate = X2BaseBean.newInstance(ViewTemplate.class, getBroker().getPersistenceKey());
				}
				viewTemplate.setContext(context);
				viewTemplate.setName(name);

				// Set custom attribute
				String customIndicator = getAttributeString(element, ViewTemplate.COL_CUSTOM_INDICATOR);
				if (Boolean.toString(Boolean.TRUE).equalsIgnoreCase(customIndicator)) {
					viewTemplate.setCustomIndicator(Boolean.TRUE);
				} else {
					viewTemplate.setCustomIndicator(Boolean.FALSE);
				}

				// Set locked attribute
				String locked = getAttributeString(element, ViewTemplate.COL_LOCKED);
				if (Boolean.toString(Boolean.TRUE).equalsIgnoreCase(locked)) {
					viewTemplate.setLocked(Boolean.TRUE);
				} else {
					viewTemplate.setLocked(Boolean.FALSE);
				}

				// Set hide attribute
				String hide = getAttributeString(element, ViewTemplate.COL_HIDE_INDICATOR);
				if (Boolean.toString(Boolean.TRUE).equalsIgnoreCase(hide)) {
					viewTemplate.setHideIndicator(hideTemplate);
				}
				// Don't remove the hide indicator in case a board chooses to hide something on
				// their own

				// Set procedureId
				String procedureId = getAttributeString(element, ViewTemplate.COL_PROCEDURE_ID);
				if (procedureId != null) {
					viewTemplate.setProcedureId(procedureId);
				}

				// Set extended dictionary oid
				String extendedDictionaryOid = getAttributeString(element,
						ViewTemplate.COL_EXTENDED_DATA_DICTIONARY_OID);
				if (extendedDictionaryOid != null) {
					String id = getAttributeString(element, EXTENDED_DATA_DICTIONARY_ID_ATTR);
					ExtendedDataDictionary extendedDataDictionary = getLocalExtendedDataDictionary(
							extendedDictionaryOid, id);
					if (extendedDataDictionary != null) {
						viewTemplate.setExtendedDataDictionaryOid(extendedDataDictionary.getOid());
					}
				}
				viewTemplate.setOwnerType(Ownable.OWNER_TYPE_ORG1);
				viewTemplate.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);

				Element childElementViewDefinition = element.getChild(TemplateConstants.TEMPLATE_ROOT_ELEMENT);
				if (childElementViewDefinition != null) {
					Element viewDefinitionElement = (Element) ((Element) childElementViewDefinition.clone()).detach();
					if (viewDefinitionElement != null) {
						XMLOutputter xmlOut = new XMLOutputter();
						org.jdom.Document document = new org.jdom.Document(viewDefinitionElement);
						viewTemplate.setViewDefinition(xmlOut.outputString(document));
					}
				}

				Element childElementTemplateDescription = element.getChild(VIEW_TEMPLATE_TEMPLATE_DESCRIPTION);
				if (childElementTemplateDescription != null) {
					Element templateDescriptionElement = (Element) ((Element) childElementTemplateDescription.clone())
							.detach();
					if (templateDescriptionElement != null) {
						String templateDescription = elementToString(templateDescriptionElement);
						// Modify to match original HTML structure
						templateDescription = templateDescription.replace(XML_ENCODING_STRING, "");
						templateDescription = templateDescription.replace("<template-description>\r\n", "");
						templateDescription = templateDescription.replace("\n</template-description>", "");
						templateDescription = templateDescription.replace("\n  ", "");
						templateDescription = templateDescription.replace("\n\n", "\n");
						viewTemplate.setTemplateDescription(templateDescription);
					}
				}

				if (viewTemplate.isDirty()) {
					getBroker().saveBeanForced(viewTemplate);
					logMessage("Imported view template " + context + "-" + name + " oid = " + viewTemplate.getOid());

					if (!StringUtils.isBlank(oid) && !viewTemplate.getOid().trim().equals(oid.trim())) {
						// Set OID if it doesn't match value in input definition
						boolean unlockBeforeDelete = viewTemplate.getLocked();
						if (unlockBeforeDelete) {
							viewTemplate.setLocked(false);
							getBroker().saveBeanForced(viewTemplate);
						}
						viewTemplate = (ViewTemplate) updateOidOnBean(viewTemplate, oid);
						if (unlockBeforeDelete) {
							viewTemplate.setLocked(true);
							getBroker().saveBeanForced(viewTemplate);
						}
					}
				}

				if (defaultTemplate) {
					int count = ContextResourceManager.updateTemplateDefaults(getOrganization(), viewTemplate.getOid(),
							getBroker());
					if (count > 0) {
						logMessage(
								viewTemplate.getContext() + "-" + viewTemplate.getName() + " set as default template");
					} else {
						logMessage("NOTE: " + viewTemplate.getContext() + "-" + viewTemplate.getName()
								+ " NOT SET as default. "
								+ "If template has a custom context, there is no default preference.");
					}
				}
			}
		}
	}

	/**
	 * Runs a tool
	 *
	 * @param toolToRun
	 * @param deleteAfterRun
	 */
	private void runTool(Tool toolToRun, Boolean deleteAfterRun) {
		Tool tool = null;
		if (toolToRun instanceof Report) {
			tool = toolToRun;
		} else if (toolToRun instanceof Procedure) {
			tool = toolToRun;
		} else if (toolToRun instanceof ImportExportDefinition) {
			tool = toolToRun;
		}
		if (tool != null) {
			Converter timeConverter = ConverterFactory.getConverterForClass(Converter.TIME_CONVERTER);
			ToolJob job = null;
			try {
				logMessage("======================================================================");
				logMessage("Started running " + tool.getName() + " (" + tool.getId() + ") after import");
				logMessage("Start time: " + timeConverter.javaToString(new PlainTime()));
				logMessage("======================================================================");
				File secureDirectory = AppGlobals.getSecureRootDirectory(getOrganization(), null);
				job = ToolJob.createJob(tool, m_userData, secureDirectory, false, getLocale());
			} catch (NotSerializableException e) {
				AppGlobals.getLog()
						.warning("Running tool as part of a deployment that is not serializable: " + tool.getId()
								+ ", " + tool.getName());
			} catch (X2BaseException e) {
				e.printStackTrace();
			}

			if (job != null) {
				try {
					job.run();
				} finally {
					try {
						job.getResultHandler().close();
					} catch (IOException e) {
						logErrorMessage("Exception error running tool after tool byte-code import");
						logErrorMessage(ExceptionUtils.getFullStackTrace(e));
						AppGlobals.getLog().severe(ExceptionUtils.getFullStackTrace(e));
					}
					/*
					 * Output the results to the log window so users can see these in the job
					 * history
					 */
					ResultHandler resultHandler = job.getResultHandler();
					if (resultHandler != null) {
						File resultsFile = new File(resultHandler.getFilePath());
						try (FileReader reader = new FileReader(resultsFile)) {
							BufferedReader bufferedReader = new BufferedReader(reader);
							String line;

							while ((line = bufferedReader.readLine()) != null) {
								logMessage(line);
							}

						} catch (IOException e) {
							logMessage(ExceptionUtils.getFullStackTrace(e));
						}
					}
				}
			}
			logMessage("======================================================================");
			logMessage("Finished. End time: " + timeConverter.javaToString(new PlainTime()));
			logMessage("======================================================================\n");

			if (deleteAfterRun) {
				getBroker().deleteBean((X2BaseBean) tool);
			}
		} else {
			logMessage("Unable to identify bean type for tool ");
		}
	}

	/**
	 * Sets a boolean value by the bean path
	 *
	 * @param bean
	 * @param element
	 * @param attribute
	 * @param beanPath
	 */
	private void setBooleanValueByBeanPath(X2BaseBean bean, Element element, String attribute, String beanPath) {
		String value = element.getAttributeValue(attribute);
		if (!StringUtils.isBlank(value)) {
			bean.setFieldValueByBeanPath(beanPath, Boolean.parseBoolean(value));
		}
	}

	/**
	 * Sets the calculated field oid on the data field config
	 *
	 * @param dataFieldConfig
	 * @param element
	 * @param calculatedFieldOidAttr
	 * @param calculatedFieldNameAttr
	 */
	private void setCalculatedField(DataFieldConfig dataFieldConfig, Element element, String calculatedFieldOidAttr,
			String calculatedFieldNameAttr) {
		String oid = element.getAttributeValue(calculatedFieldOidAttr);
		String name = element.getAttributeValue(calculatedFieldNameAttr);

		if (oid != null || name != null) {
			// Get the calculated field by oid
			CalculatedField calculatedField = getBroker().getBeanByOid(CalculatedField.class, oid);
			if (calculatedField == null || !calculatedField.getName().equals(name)) {
				// Look for calculated field by name
				calculatedField = getCalculatedField(name);
			}
			if (calculatedField != null) {
				dataFieldConfig.setCalculatedFieldOid(calculatedField.getOid());
			} else {
				logErrorMessage("FAILED setting calculated field on " + dataFieldConfig.getUserLongName() + ". "
						+ "Queried by oid = '" + oid + "' and name = '" + name + "'.");
			}
		} else {
			dataFieldConfig.setCalculatedFieldOid(null);
		}
	}

	/**
	 * Sets the comment bank table oid on the data field config
	 *
	 * @param dataFieldConfig
	 * @param element
	 * @param commentBankTableOidAttr
	 * @param commentBankTableNameAttr
	 */
	private void setCommentBankTable(DataFieldConfig dataFieldConfig, Element element, String commentBankTableOidAttr,
			String commentBankTableNameAttr) {
		String oid = element.getAttributeValue(commentBankTableOidAttr);
		String name = element.getAttributeValue(commentBankTableNameAttr);

		if (oid != null || name != null) {
			// Get the comment bank table by oid
			CommentBankTable commentBankTable = getBroker().getBeanByOid(CommentBankTable.class, oid);
			if (commentBankTable == null) {
				// Look for comment bank table by name
				commentBankTable = getCommentBankTable(name);
			}
			if (commentBankTable != null) {
				dataFieldConfig.setCommentBankTableOid(commentBankTable.getOid());
			} else {
				logErrorMessage("FAILED setting comment bank table on " + dataFieldConfig.getUserLongName() + ". "
						+ "Queried by oid = '" + oid + "' and name = '" + name + "'.");
			}
		} else {
			dataFieldConfig.setCommentBankTableOid(null);
		}
	}

	/**
	 * Sets the reference table oid on the data field config / extended data field
	 *
	 * @param bean
	 * @param element
	 * @param beanName
	 * @param beanPath               - needed to support custom attribute in
	 *                               extended data field portable XML
	 *                               (reference-table-oid)
	 * @param referenceTableOidAttr  - same as beanPath for data field config, but
	 *                               not for extended field
	 * @param referenceTableNameAttr
	 */
	private void setReferenceTable(X2BaseBean bean, Element element, String beanName, String beanPath,
			String referenceTableOidAttr, String referenceTableNameAttr) {
		String oid = element.getAttributeValue(referenceTableOidAttr);
		String userName = element.getAttributeValue(referenceTableNameAttr);

		if (oid != null || userName != null) {
			// Get the reference table by oid
			ReferenceTable referenceTable = getBroker().getBeanByOid(ReferenceTable.class, oid);
			if (referenceTable == null) {
				// Look for reference table by name
				referenceTable = getReferenceTable(userName);
			}
			if (referenceTable != null) {
				bean.setFieldValueByBeanPath(beanPath, referenceTable.getOid());
			} else {
				logErrorMessage("FAILED setting reference table on " + beanName + ". " + "Queried by oid = '" + oid
						+ "' and name = '" + userName + "'.");
			}
		} else {
			bean.setFieldValueByBeanPath(beanPath, null);
		}
	}

	/**
	 * Sets the value on the bean by the bean path
	 *
	 * @param className
	 * @param dictionary
	 * @param bean
	 * @param element
	 * @param attribute
	 */
	private void setValueByBeanPath(String className, DataDictionary dictionary, X2BaseBean bean, Element element,
			String attribute) {
		String value = element.getAttributeValue(attribute);
		if (value != null) {
			try {
				DataDictionaryField dataDictionaryField = dictionary.findDataDictionaryField(className, attribute);
				if (dataDictionaryField != null) {
					DataFieldType dataFieldType = getDataFieldType(dataDictionaryField.getDatabaseType());
					if (dataFieldType != null) {
						setValueByBeanPath(attribute, dataFieldType, bean, element, attribute);
					} else {
						logErrorMessage("Unknown data field type for field " + dataDictionaryField.getJavaName());
					}
				} else {
					logErrorMessage("Can't find dataDictionaryField: " + className + " " + attribute);
				}
			} catch (Exception e) {
				logErrorMessage(ExceptionUtils.getFullStackTrace(e));
			}
		}
	}

	/**
	 * Sets the value on the bean by the bean path
	 *
	 * @param beanPath
	 * @param bean
	 * @param element
	 * @param attribute
	 * @param dictionary
	 */
	private void setValueByBeanPath(String beanPath, DataFieldType type, X2BaseBean bean, Element element,
			String attribute) {
		String value = element.getAttributeValue(attribute);
		if (value != null) {
			if (DataFieldType.INT.equals(type)) {
				bean.setFieldValueByBeanPath(beanPath, Integer.parseInt(value));
			} else if (DataFieldType.BOOLEAN.equals(type)) {
				bean.setFieldValueByBeanPath(beanPath, Boolean.parseBoolean(value));
			} else {
				bean.setFieldValueByBeanPath(beanPath, value);
			}
		}
	}

	/**
	 * Sets the value on the bean by the bean path
	 *
	 * @param beanPath
	 * @param bean
	 * @param element
	 * @param attribute
	 */
	private void setValueByBeanPath(String beanPath, X2BaseBean bean, Element element, String attribute) {
		String value = element.getAttributeValue(attribute);
		if (value != null) {
			bean.setFieldValueByBeanPath(beanPath, value);
		}
	}

	/**
	 * Updates the oid of a bean and changes preferences that reference the old oid
	 *
	 * @param bean
	 * @param newOid
	 * @param updatePreferences
	 * @param context
	 * @return
	 */
	private X2BaseBean updateOidOnBean(X2BaseBean bean, String newOid, boolean updatePreferences, String context) {
		String currentOid = bean.getOid();
		List<String> errors = new ArrayList<String>();
		try {
			bean = CopyBeanChangeOid.execute(bean, newOid, true, true, true, getBroker(), getPrivilegeSet(), errors);
		} catch (Exception e) {
			logMessage("Exception error occurred:\n" + ExceptionUtils.getFullStackTrace(e));
		}
		if (errors.size() > 0) {
			for (String error : errors) {
				logMessage(error);
			}
		}

		if (updatePreferences) {
			String sql = "UPDATE  PRF SET PRF_VALUE = REPLACE(PRF_VALUE,'" + currentOid + "','" + newOid + "') "
					+ "FROM    SYS_PREFERENCE PRF " + "JOIN    SYS_PREFERENCE_DEFINITION ON PRD_OID = PRF_PRD_OID "
					+ "WHERE   PRD_KEY LIKE '" + context + "%' " + "AND     PRF_VALUE LIKE '%" + currentOid + "%'";
			executeSqlUpdate(sql, "Preferences modified:");
		}

		return bean;
	}

	/**
	 * Recurses through an XML document and returns the element with the attribute
	 * having attributeValue
	 *
	 * @param element
	 * @param name
	 * @param attribute
	 * @param attributeValue
	 * @param attributeToReplace
	 * @param attributeValueToReplace
	 * @param updated                 - must be false for initial call to this
	 *                                method
	 * @return
	 */
	private boolean updateXml(Element element, String attribute, String attributeValue, String attributeToReplace,
			String attributeValueToReplace, boolean updated) {
		String thisValue = getAttributeString(element, attribute);
		if (thisValue != null && thisValue.contains(attributeValue)) {
			element.setAttribute(attributeToReplace, attributeValueToReplace);
			return true;
		}
		Iterator iterator = (element.getChildren()).iterator();
		while (iterator.hasNext()) {
			updated = updateXml((Element) iterator.next(), attribute, attributeValue, attributeToReplace,
					attributeValueToReplace, updated);
		}
		return updated;
	}

	/**
	 * Update a workflow phase
	 *
	 * @param element Element
	 */
	private void updateWorkflowPhase(Element element) {
		String wph_action = element.getChild(WORK_FLOW_PHASE_UPDATE).getAttributeValue(WORK_FLOW_PHASE_ACTION);
		if (wph_action.equals("true")) {
			Collection beans = getWorkflowPhaseCollection();

			if (beans.size() != 0) {
				Iterator workflowObjectsIterator = beans.iterator();
				while (workflowObjectsIterator.hasNext()) {
					WorkflowPhase workflowObject = (WorkflowPhase) workflowObjectsIterator.next();

					if (StringUtils.isEmpty(workflowObject.getId())) {
						workflowObject.setId(workflowObject.getOid());
						getBroker().saveBeanForced(workflowObject);
						logMessage("Workflow phase id updated: oid = " + workflowObject.getOid());
					}
				}
			} else {
				logMessage("Workflow phase - nothing to update");
			}

			updateWorkflowPhaseOutcome();
		}
	}

	/**
	 * Update a workflow phase outcome
	 *
	 * @param element Element
	 */
	private void updateWorkflowPhaseOutcome() {
		Collection beans = getWorkflowPhaseOutcomeCollection();

		if (beans.size() != 0) {
			Iterator workflowObjectsIterator = beans.iterator();
			while (workflowObjectsIterator.hasNext()) {
				WorkflowPhaseOutcome workflowObject = (WorkflowPhaseOutcome) workflowObjectsIterator.next();

				if (StringUtils.isEmpty(workflowObject.getId())) {
					workflowObject.setId(workflowObject.getOid());
					getBroker().saveBeanForced(workflowObject);
					logMessage("Workflow phase outcome id updated: oid = " + workflowObject.getOid());
				}
			}
		} else {
			logMessage("Workflow phase outcome - nothing to update");
		}
	}

	/**
	 * Class to copy a bean and specify the OID of the copy. Allows removing the
	 * original bean and updating all child records to point to the copied bean.
	 * Updating child records uses the data dictionary relationships table, so it is
	 * valid for all relationships defined in the data dictionary.
	 */
	public static class CopyBeanChangeOid {

		/**
		 * Copies a bean using SQL so a specific OID can be assigned. Optionally, moves
		 * all child records and deletes the original bean
		 *
		 * @param bean
		 * @param newOid
		 * @param moveChildRecords
		 * @param removeOriginalAfterCopy
		 * @param clearObjectCache
		 * @param broker
		 * @param privilegeSet
		 * @param errors
		 */
		public static X2BaseBean execute(X2BaseBean bean, String newOid, boolean moveChildRecords,
				boolean removeOriginalAfterCopy, boolean clearObjectCache, X2Broker broker, PrivilegeSet privilegeSet,
				List<String> messages) {
			if (removeOriginalAfterCopy && !moveChildRecords) {
				messages.add("Can't remove original record if not moving the child records!");
			} else {
				String currentOid = bean.getOid();
				// Get data table for the current OID
				DataTable dataTable = getDataTableByOid(currentOid, broker);

				// Copy the record, setting the OID to the desired value
				boolean insertRollback = copyRecord(dataTable, currentOid, newOid, broker, messages);

				boolean rollback = false;

				if (!insertRollback && moveChildRecords) {
					// Move child records
					rollback = moveChildRecords(dataTable, currentOid, newOid, broker, messages);

					if (clearObjectCache) {
						// Clear cache since we've updated OIDs through SQL
						(new ModelBroker(privilegeSet)).clearCache();
						messages.add("Object cache cleared and reinitialized");
					} else {
						// Not clearing entire cache, so remove just the original bean OID
						broker.removeBeanFromCache(dataTable.getDataClass(), currentOid);
						messages.add("Not clearing cache...you are responsible for doing this!");
					}
				} else if (insertRollback) {
					messages.add("ERROR: Insert failed!");
				}

				if (!insertRollback && !rollback && removeOriginalAfterCopy) {
					// Delete original now that it has been copied and all child records updated
					deleteRecord(dataTable, currentOid, broker, messages);
				} else if (rollback) {
					// Remove newOid if it exists since we're rolling everything back
					messages.add("Rollback - removing new OID");
					deleteRecord(dataTable, newOid, broker, messages);
				}

				bean = broker.getBeanByOid(dataTable.getDataClass(), newOid);
			}

			return bean;
		}

		/**
		 * Copy a record using SQL, setting the OID to the desired value
		 *
		 * @param dataTable
		 * @param currentOid
		 * @param newOid
		 * @param broker
		 * @param messages
		 * @return
		 */
		private static boolean copyRecord(DataTable dataTable, String currentOid, String newOid, X2Broker broker,
				List<String> messages) {
			// Execute SQL that copies all fields for a record
			String oidField = dataTable.getObjectPrefix() + "_OID";
			String sql = getSqlToCopyRecord(dataTable, oidField, currentOid, newOid);
			boolean rollback = executeSqlUpdate(sql, "Inserted into " + dataTable.getDatabaseName() + " :", false,
					broker, messages);

			return rollback;
		}

		/**
		 * Deletes a bean, logging any validation errors
		 *
		 * @param dataTable
		 * @param oid
		 * @param broker
		 * @param messages
		 */
		private static void deleteRecord(DataTable dataTable, String oid, X2Broker broker, List<String> messages) {
			List<ValidationError> errors = broker.deleteBeanByOid(dataTable.getDataClass(), oid);
			if (errors.isEmpty()) {
				messages.add("Removed record: " + oid);
			} else {
				messages.add("Error encountered deleting bean:");
				for (ValidationError validationError : errors) {
					messages.add(validationError.toString());
				}
			}
		}

		/**
		 * Executes the SQL update statement, returning whether an error was encountered
		 *
		 * @param sql           - Update SQL to execute
		 * @param logMessage    - Message to print before the results of the update SQL
		 * @param logZeroCounts
		 * @param broker
		 * @param messages
		 * @return
		 */
		private static boolean executeSqlUpdate(String sql, String logMessage, boolean logZeroCounts, X2Broker broker,
				List<String> messages) {
			boolean rollback = true;

			Connection connection = null;
			try {
				connection = broker.borrowConnection();
				Statement statement = connection.createStatement();
				int count = statement.executeUpdate(sql);
				if (logMessage != null) {
					if (count > 0 || logZeroCounts) {
						messages.add(logMessage + " " + count);
					}
				}
				rollback = false;
			} catch (SQLException sqle) {
				messages.add(sqle.toString());
			} finally {
				if (connection != null) {
					broker.returnConnection();
				}
			}

			return rollback;
		}

		/**
		 * Returns the data table for an OID
		 *
		 * @param oid
		 * @param broker
		 * @param messages
		 * @return
		 */
		private static DataTable getDataTableByOid(String oid, X2Broker broker) {
			DataTable dataTable = null;
			String tablePrefix = oid.substring(0, 3).toUpperCase();

			DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
			DataDictionaryTable dataDictionaryTable = dictionary.findDataDictionaryTableByPrefix(tablePrefix);
			if (dataDictionaryTable != null) {
				dataTable = dataDictionaryTable.getSystemDataTable();
			}
			return dataTable;
		}

		/**
		 * Returns the SQL to make a copy of an existing record, but with a new oid
		 *
		 * @param dataTable
		 * @param field
		 * @param currentOid
		 * @param newOid
		 * @return
		 */
		private static String getSqlToCopyRecord(DataTable dataTable, String field, String currentOid, String newOid) {
			// All columns in the table
			List<String> columnsList = new LinkedList<String>();
			List<String> selectList = new LinkedList<String>();

			for (DataField dataField : dataTable.getDataFields()) {
				columnsList.add(dataField.getDatabaseName());
				if (!field.equals(dataField.getDatabaseName())) {
					selectList.add(dataField.getDatabaseName());
				} else {
					// Replace the primary key of the table with the newOid
					selectList.add("'" + newOid + "'");
				}
			}
			// Insert a copy of the current record, but swap the new OID for the current
			String sql = "INSERT INTO " + dataTable.getDatabaseName() + " (" + String.join(", ", columnsList)
					+ ") SELECT " + String.join(", ", selectList) + " FROM " + dataTable.getDatabaseName() + " WHERE "
					+ field + " = '" + currentOid + "'";
			return sql;
		}

		/**
		 * Returns SQL for updating records to change a field to a new value
		 *
		 * @param tableName
		 * @param tablePrefix
		 * @param currentValue
		 * @param newValue
		 * @return
		 */
		private static String getUpdateSql(String table, String field, String currentValue, String newValue) {
			return "UPDATE " + table + " SET " + field + " = '" + newValue + "' WHERE " + field + " = '" + currentValue
					+ "'";
		}

		/**
		 * Updates all child records to point to a new OID
		 *
		 * @param dataTable
		 * @param currentOid
		 * @param newOid
		 * @param broker
		 * @param messages
		 * @return
		 */
		private static boolean moveChildRecords(DataTable dataTable, String currentOid, String newOid, X2Broker broker,
				List<String> messages) {
			boolean rollback = false;

			List<String> rollbackStatements = new ArrayList<String>();

			// Iterate over child table relationships and update all records related to the
			// parent record we want to update
			for (DataRelationship dataRelationship : dataTable.getRelatedDataRelationships()) {
				String table = dataRelationship.getPrimaryDataTable().getDatabaseName();
				String field = dataRelationship.getPrimaryDataIndex().getDatabaseName();

				// Update child records for relationship if field is not the primary key
				if (!rollback && !field.equals(dataRelationship.getPrimaryDataTable().getObjectPrefix() + "_OID")) {
					// Statement to execute if an error occurs. Sets OID back to current value
					rollbackStatements.add(getUpdateSql(table, field, newOid, currentOid));
					// Update records with new OID
					rollback = executeSqlUpdate(getUpdateSql(table, field, currentOid, newOid), table + " :", false,
							broker, messages);
				}
			}

			if (rollback) {
				for (String rollbackSql : rollbackStatements) {
					messages.add("Rolling back SQL: " + rollbackSql);
					executeSqlUpdate(rollbackSql, "Record(s) rolled back:", false, broker, messages);
				}
				messages.add("\n\n========================================");
				messages.add("ERROR ENCOUNTERED....CHANGES ROLLED BACK");
				messages.add("========================================\n");
			}

			return rollback;
		}
	}

//  Example input definition XML for importing portables

//  <tool-input refresh="0">
//
//    <PortableDocument>
//
//      <extended-data-dictionary id="ORA-MA-SPED-CFIG" category="Special Ed." description="MA IEP Configuration" name="MA IEP Configuration" pd="false" sped="false">
//        <table id="tblOrgAttrib" name="Attributes">
//          <field long-name="Form Required Fields" short-name="FormRequiredFields" length="9999"
//            sequence-number="20" enabled="true" list-edit="false" read-only="false" required="false" update="false"
//            alias="ora-ma-sped-form-req-fields" type="Text" />
//        </table>
//      </extended-data-dictionary>
//
//      <calculated-field name="Incident Date" description="Incident Date"
//        procedure-id="" calculated-expression="{userDefinedTableE.fieldA004}" field-alias="inc-ude-incident-date" />
//
//      <data-table-config dataTableOid="tblPerson" >
//        <data-field-config dataTableOid="tblPerson"
//          dataFieldOid="psnFieldB001" userLongName="Birth country" userShortName="Birth Country"
//          alias="all-psn-BirthCountry" userType="Character" userLength="25" userDecimal="0"
//          detailWidth="0" listWidth="0" enabledIndicator="true" requiredIndicator="false"
//          readOnlyIndicator="false" updateIndicator="false" listEditIndicator="false" fieldAuditType="0"
//          referenceTableOid="Country Codes" reference-table-name="Country Codes" detailControl="Dropdown"
//          validReferenceOnlyIndicator="false" spellCheckIndicator="false" localizedIndicator="false"
//          recordLevelSecurityIndicator="false" helpIndicator="true" fieldDescription="" technicalDescription=""
//        />
//      </data-table-config>
//
//      <field-audit type="1" field-oid="isvStfOID" />
//
//      <reference-table user-name="Grade Levels" code-length="10" sequence-order="false" category="Student" ext-dictionary-id="REF-GRADE-LEVELS" dataEntryViaViewFieldsInd="false" dataTableOid="tblRefCode    " oid="tblGradeLevel ">
//        <reference-code code="01" description="Grade 1" state-code="1" ext-dictionary-id="REF-GRADE-LEVELS">
//          <custom-fields>
//            <custom-field field-alias="NumericGradeLevel">1</custom-field>
//            <custom-field field-alias="RGGradeLevel">1</custom-field>
//            <custom-field field-alias="all-rcd-Panorama">GR1</custom-field>
//          </custom-fields>
//        </reference-code>
//      </reference-table>
//
//      <resource key="label.template.next-year" locale="en_US">
//        <value>Next Year</value>
//        <valueLong>Some very long value goes here (greater than 2000 characters)</valueLong>
//      </resource>
//
//      <!--
//          Custom attributes for view-template:
//          > override-custom - if "true", replaces a template marked as custom, changing the existing custom flag to "false"
//          > previous-name   - if present, the import will look for a template with the previous name if the regular name isn't found
//      -->
//      <view-template context="student.std.list.doc.detail" name="OBC Default Template" locked="false"
//          extendedDataDictionaryOid="ddxOnDoc" procedureId="OBC-DOC-TEMPLATE" customIndicator="true" override-custom="true" previous-name="Default Template">
//        <template>
//          <tab name="tab.default">
//            <row>
//              <column>
//                <property id="docName" />
//                <property id="docTypeCode" />
//                <property id="docDocument" />
//                <property id="docFileName" read-only="true" />
//              </column>
//            </row>
//          </tab>
//        </template>
//      </view-template>
//
//      <workflow-phase>
//        <update-phase action="true" />
//      </workflow-phase>
//
//    </PortableDocument>
//
//  </tool-input>

	private static final String deploymentProcedureSource = "package com.x2dev.tools.updates;\n"
			+ "\n"
			+ "/**\n"
			+ " * For DevOps _____\n"
			+ " */\n"
			+ "public class DevOps1 extends ConfigurationDeploymentToolBase {\n"
			+ "    \n"
			+ "	   private static final long serialVersionUID = 1L;\n"
			+ "    \n"
			+ "    @Override\n"
			+ "    protected void execute() throws Exception {\n"
			+ "        /**\n"
			+ "         * Imports all objects defined in input definition\n"
			+ "         */\n"
			+ "        importPortables();\n"
			+ "        \n"
			+ "        /**\n"
			+ "         * Only uncomment lines below if additional code in this procedure requires it\n"
			+ "         */\n"
			+ "        //m_reloadObjectCache = true;\n"
			+ "        //m_reloadDataDictionary = true;\n"
			+ "        //m_reloadLocalizationCache = true;\n"
			+ "        //m_reloadProcedureCache = true;\n"
			+ "        //m_reloadNavigation = true;\n"
			+ "        \n"
			+ "        m_deleteAfterRun = true;\n"
			+ "    }\n"
			+ "\n"
			+ "}\n";

	private static final String deploymentProcedureInputStart = "<tool-input refresh=\"0\">\n<" + PORTABLE_DOCUMENT
			+ ">";
	private static final String deploymentProcedureInputEnd = "</" + PORTABLE_DOCUMENT + ">\n</tool-input>";
	private static final String deploymentProcedureExternalSource = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<external-sources>\n"
			+ "  <external-source id=\"FSS-CFG-DEPLOY-BASE\" type=\"2\" />\n"
			+ "</external-sources>";
}