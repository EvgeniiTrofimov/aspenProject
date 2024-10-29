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
package com.x2dev.procedures.portal;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import javax.net.ssl.HttpsURLConnection;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class StudentPortalExport extends ExportJavaSource {

    private static final long serialVersionUID = 1L;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String PROCEDURE_ID = "procedureId";

    private static final String SAVE_RESULTS = "saveResults";

    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;

    /**
     * A parameter that indicates the export results should be saved in the CustomExportResults
     * table.
     */
    private boolean m_saveResults = false;


    // =====================================================================================
    // Export to Google Storage
    // =====================================================================================
    public class GoogleStorageUploader {
        private static final String AUTH_JSON = "{\n" + "  \"type\": \"service_account\",\n"
                + "  \"project_id\": \"aspen-dashboard-145006\",\n"
                + "  \"private_key_id\": \"87e5daf289f966b028ad92b3d9faa730c2b05f4e\",\n"
                + "  \"private_key\": \"-----BEGIN PRIVATE KEY-----\\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC7KN0CeRMiziJ0\\nZduGiIbGbevvwWsYirjRxAp7cg/g2KgVn9n/RIn3tyxyVtiGjt1vC23MDj4XzUzF\\nCCnx55pFFnnlUAbC2EG64FeTyBcd3h0XrQHPg9wB9X3EIKBkbUPASrqY2xoYklHK\\nSQjLhphTfZ503GXyp2hKCrBzDIhN7tGQqE/QjmBqw/V5bZDln2bZH0tbD6lwU/QM\\n5pvO8AgtL+pvMJbsz692uw7nkXddUCOAJs8Xps8T0TAj/XgZQXGlOn2U/gJYFGhT\\nIAjTt3GV9XWXZDHxaXF0Qb4PNwRF5b6R7LcKrTLbrQHASv947Yy7BE+0Yom5afPK\\n3risXcklAgMBAAECggEAGc4Miua+qLHbNklE90ujqYBGY4w7g+q8iM+K0nJrlgjJ\\nRBICMJv2mdz7l9g5inPwO/tLbDMahJWd3BzAC8ZvEs+Chwd9pyfCCE4eRRX1efzB\\n7h3AfigkEXETohMNiks2aDDlgWXUN0cVV9qyB73LCtfrHRm2RLqoOm023uX/dR8Y\\nD+XTcsxMlYwAXOJM4nM8qWzIHX3ddeHrnZU4++tvH6h/0AA6lK1KNerd82YBpsj+\\nRBFJ6wKeTLY7YORfPlm/bh1op77RacObY/T83hE2wzxU6h2Ik40oCngUbZr/nj7A\\nopfwwZjDMoFzW1G1b3IueaiMJxecTbffBl8bhfNekQKBgQDeMH3TdR+Wy814rgVo\\nKlOaLDZG1eDo4dThxep6pESOJGCtvHbKBzHSKrc0ZndvuQs37W8RP0O0GFZtmHZx\\ns0BNZoRSXPpJtShRv1tc//Kpbx+9bhmGPmf49X+aBhS/qR82U28FA7erKgRCYox/\\nsEK7RH4SwHd+at42ulTRvdY/mwKBgQDXo8RmILziugfipe8xGKwJgw126d86YzlV\\nmylF9vtlxml8mVcFiJS3eBFXxukoQbcfGw9I/88RB10Q/Yn3s9SN+wh6LFtmzLXF\\n5w0P62T/yAIGZjDeqh4KPbTpvTkC9lkXrL1J86aiO46ETWUtl0TFkYX1y5TSRFd5\\nkrAm7huGPwKBgCWkc3zCqAJlLr9WRCY8nYNOIDfp/juWHvnNDQeQEuk98n8cocdi\\nZqdjgzxGa8+quuLTv/BdFnxJb0MMBG0I4Ej8qEhH5kNchKMs1y6vUg4wfGEcaQic\\nRAMinGacxyEdZuNOSU3BLnm6GZShm4n6c6PEW9OTdesd8d21va73CXgnAoGAK4V+\\n/wm+dZgMPV82HleXaTgNAKIFDri1+qQdOWS2qnzYD3C3UoOVWE4gWfHoXtqcheel\\nNxuCBDLTQrmZHegqjxlKRbYsIFxSl4WhakQdF06bsgTv+hCljXWUzNQG7KUhqBiR\\n/5nwWK/ifKtOL1xnYeST/969w45NhVjouoK2BWsCgYEApV9C1gtgLonU8ZONPEKb\\n731hd47DtruwBMaA/ptr0SbqD4W9tG5N9HatpW0Z5euH9uC7qOgdcCkDHEnz+njb\\nplFPLumDgQ+5Z84PefgqRcJOjscvWGFl+20v/k9mD+h3IJALOOvi3uA7Qlok8mCB\\nPD7GBCK8+x0Ud6AXJvgipy8=\\n-----END PRIVATE KEY-----\\n\",\n"
                + "  \"client_email\": \"aspen-dashboard-145006@appspot.gserviceaccount.com\",\n"
                + "  \"client_id\": \"102420317144021557687\",\n"
                + "  \"auth_uri\": \"https://accounts.google.com/o/oauth2/auth\",\n"
                + "  \"token_uri\": \"https://accounts.google.com/o/oauth2/token\",\n"
                + "  \"auth_provider_x509_cert_url\": \"https://www.googleapis.com/oauth2/v1/certs\",\n"
                + "  \"client_x509_cert_url\": \"https://www.googleapis.com/robot/v1/metadata/x509/aspen-dashboard-145006%40appspot.gserviceaccount.com\"\n"
                + "}\n";

        private static final String EXPORT_FILE_CONTENT_TYPE = "text/csv";
        private static final String EXPORT_FILE_NAME = "export.csv";
        private static final String GOOGLE_APP_ENGINE_PROJECT_ID = "aspen-dashboard-145006"; // from
                                                                                             // app
                                                                                             // engine
                                                                                             // console
        private static final String SCOPE_FULL_CONTROL = "https://www.googleapis.com/auth/devstorage.full_control";
        private static final String JSON_API_BUCKETS_LIST_URL = "https://www.googleapis.com/storage/v1/b?project="
                + GOOGLE_APP_ENGINE_PROJECT_ID;

        private String m_fileName;

        public GoogleStorageUploader() {
            m_fileName = EXPORT_FILE_NAME;
        }

        public GoogleStorageUploader(String fileName) {
            m_fileName = fileName;
        }

        public boolean upload(InputStream inputStream) throws Exception {
            List<String> scopes = new ArrayList<String>();
            scopes.add(SCOPE_FULL_CONTROL);

            InputStream authStream = new ByteArrayInputStream(AUTH_JSON.getBytes(StandardCharsets.UTF_8));
            GoogleCredential credential = GoogleCredential.fromStream(authStream).createScoped(scopes);
            credential.refreshToken();
            String token = credential.getAccessToken();
            String bucket = getDefaultBucketName(token);
            if (bucket == null || bucket.length() == 0) {
                throw new IOException("can't find default storage bucket");
            }

            String uploadAddress = "https://www.googleapis.com/upload/storage/v1/b/" + bucket
                    + "/o?uploadType=media&name=" + m_fileName;
            URL url = new URL(uploadAddress);
            HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoInput(true);
            connection.setUseCaches(false);
            connection.setDefaultUseCaches(false);
            connection.setRequestProperty("Content-Type", EXPORT_FILE_CONTENT_TYPE);
            connection.setRequestProperty("Authorization", "Bearer " + token);
            connection.setDoOutput(true);
            connection.connect();
            DataOutputStream outputStream = new DataOutputStream(connection.getOutputStream());
            byte[] buffer = new byte[inputStream.available()];
            try {
                while (true) {
                    int bytesRead = inputStream.read(buffer);
                    if (bytesRead > 0) {
                        outputStream.write(buffer, 0, bytesRead);
                    } else {
                        break;
                    }
                }
            } finally {
                outputStream.flush();
                outputStream.close();
                inputStream.close();
            }

            String response = readFullyAsString(connection.getInputStream(), "UTF-8");
            connection.disconnect();

            if (!StringUtils.isEmpty(response)) {
                JSONObject object = JSONObject.fromObject(response);
                return !object.has("error");
            }
            return false;
        }

        private String readFullyAsString(InputStream inputStream, String encoding) throws IOException {
            return readFully(inputStream).toString(encoding);
        }

        private ByteArrayOutputStream readFully(InputStream inputStream) throws IOException {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            byte[] buffer = new byte[1024];
            int length = 0;
            while ((length = inputStream.read(buffer)) != -1) {
                baos.write(buffer, 0, length);
            }
            return baos;
        }

        private String getDefaultBucketName(String token) throws IOException {
            String res = null;
            URL url = new URL(JSON_API_BUCKETS_LIST_URL);
            HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setRequestProperty("Content-Type", EXPORT_FILE_CONTENT_TYPE);
            connection.setRequestProperty("Authorization", "Bearer " + token);
            connection.connect();
            String content = readFullyAsString(connection.getInputStream(), "UTF-8");
            JSONObject object = JSONObject.fromObject(content);
            if (object.has("items")) {
                JSONArray items = object.getJSONArray("items");
                if (items.size() > 0) {
                    res = items.getJSONObject(0).getString("name");
                }
            }
            connection.disconnect();
            return res;
        }
    }

    @Override
    protected void writeResults(DataGrid dataGrid) throws Exception {
        super.writeResults(dataGrid);
        if (getResultHandler() != null && getJob().getStatus() != ToolJob.STATUS_ABORT && dataGrid.rowCount() > 0) {
            dataGrid.sort("School", false);
            List<Map<String, Object>> rows = new LinkedList<Map<String, Object>>();
            rows.addAll(dataGrid.getRows());
            dataGrid.getRows().clear();
            dataGrid.beforeTop();
            String currentSchool = "";
            Map<String, List<String>> users = new HashMap<String, List<String>>();
            for (int i = 0; i < rows.size(); i++) {
                Map<String, Object> row = rows.get(i);
                String school = row.get("School").toString();
                String staffEmail = row.get("Staff Email").toString();
                if (!school.isEmpty() && !staffEmail.isEmpty()) {
                    if (!users.containsKey(staffEmail)) {
                        users.put(staffEmail, new ArrayList<String>());
                    }
                    if (!users.get(staffEmail).contains(school)) {
                        users.get(staffEmail).add(school);
                    }
                    if (!currentSchool.equals(school)) {
                        uploadGrid(dataGrid, "SKL_" + currentSchool + ".csv");
                        currentSchool = school;
                    }
                    dataGrid.append(row);
                }
            }
            uploadGrid(dataGrid, "SKL_" + currentSchool + ".csv");
            uploadUsers(users);
        }
    }

    private void uploadUsers(Map<String, List<String>> users) throws Exception {
        DataGrid grid = new DataGrid();
        for (String key : users.keySet()) {
            grid.append();
            grid.set("Staff Email", key);
            grid.set("School", StringUtils.convertCollectionToDelimitedString(users.get(key), "#"));
        }
        uploadGrid(grid, "user.csv");
    }

    private void uploadGrid(DataGrid dataGrid, String fileName) {
        if (!dataGrid.isEmpty()) {
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            writeData(outputStream, dataGrid, getColumnNames(), getColumnUserNames(), getCharacterEncoding(),
                    getLineSeparator(), getHeader(), getTrailer(), getEscapeCharacter(), getValueDelimiter(),
                    getValueWrapper(), isIncludeHeaderRow(), isUseEscapes(), isUseValueDelimiters(),
                    isUseValueWrappers());

            ByteArrayInputStream bis = new ByteArrayInputStream(outputStream.toByteArray());
            try {
                fileName = fileName.replace(" ", "");
                if (!new GoogleStorageUploader(fileName).upload(bis)) {
                    this.logToolMessage(Level.SEVERE, "failed to upload export file to Google Storage: " + fileName,
                            false);
                }
            } catch (Exception e) {
                this.logToolMessage(Level.SEVERE, "failed to upload export file to Google Storage: " + fileName, false);
            }
            dataGrid.getRows().clear();
            dataGrid.beforeTop();
        }
    }


    // =====================================================================================
    // END of Export to Google Storage
    // =====================================================================================


    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ExportFormatResult saveResult = null;
        ExportFormatRow saveRow = null;
        long rowNumber = 0;
        int rowcount = 1;
        if (m_reportData != null) {
            rowcount = m_reportData.getFieldCount();
        }

        DataGrid dataGrid = new DataGrid(rowcount);
        if (m_initErrors.size() == 0) {
            if (m_saveResults) {
                saveResult = X2BaseBean.newInstance(ExportFormatResult.class, getBroker().getPersistenceKey());
                saveResult.setOrganization1Oid(m_reportData.getOrganization().getOid());
                saveResult.setRunDate(System.currentTimeMillis());
                saveResult.setName(m_reportData.getExportTitle());
                saveResult.setDefinitionOid(m_reportData.getEfdOid());
                getBroker().saveBeanForced(saveResult);
            }

            if (m_reportData.open()) {
                try {
                    StateReportEntity entity = null;
                    while ((entity = m_reportData.next()) != null) {
                        StateReportValidationError err = entity.filterEntity();
                        if (err == null) {
                            entity.preProcess();
                            entity.setScriptManager(m_reportData.getScriptManager());
                            dataGrid.append();

                            if (m_saveResults) {
                                rowNumber++;
                                saveRow =
                                        X2BaseBean.newInstance(ExportFormatRow.class, getBroker().getPersistenceKey());
                                saveRow.setResultOid(saveResult.getOid());
                                saveRow.setDefinitionOid(m_reportData.getEfdOid());
                                saveRow.setSortOrder(new BigDecimal(rowNumber));
                                saveRow.setSourceOid(entity.getBean().getOid());
                                String rowName = entity.getEntityName();
                                if (rowName != null && rowName.length() > 50) {
                                    rowName = rowName.substring(0, 50);
                                }
                                saveRow.setDescription(rowName);
                            }

                            /*
                             * Add all fields
                             */
                            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                String fieldValue = entity.getFieldValue(pos);

                                if (m_saveResults) {
                                    /*
                                     * If a value has a specified maximum length, then the field
                                     * that it is
                                     * being saved into also has the specified maximum length,
                                     * So we must trim the value to that maximum length before
                                     * saving.
                                     *
                                     * Ex: Middle name is specified as 10 chars and is assigned to a
                                     * FieldA.
                                     * The value is 12 chars.
                                     * Must trim to 10 prior to saving so it will fit into the
                                     * field.
                                     *
                                     * The case that this might lose data would be in a CSV where
                                     * the length is not
                                     * absolute as it would be in a column width report. The export
                                     * might still
                                     * contain the excessive length but the saved value would not.
                                     *
                                     * In those cases, the field would generate a validation error
                                     * anyway.
                                     *
                                     * Save happens before padding so pad values do not also get
                                     * saved.
                                     */
                                    String saveFieldValue = ExportFormatManager.doPadding(fieldValue,
                                            ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY.ordinal(), null,
                                            field.getExportLength());

                                    /*
                                     * Save field value into CustomExportRow before
                                     * padding/trimming.
                                     */
                                    String saveField = field.getSaveBeanPath();
                                    if (!StringUtils.isEmpty(saveField)) {
                                        try {
                                            WebUtils.setProperty(saveRow, saveField, saveFieldValue);
                                        } catch (RuntimeException re) {
                                            // Ignore: the value was not saved, probably an invalid
                                            // field name.
                                        }
                                    }
                                }

                                /*
                                 * If the value requires padding, pad it and trim it to field max
                                 * length.
                                 */
                                fieldValue = ExportFormatManager.doPadding(fieldValue,
                                        (field.getResizeMode() == null
                                                ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                : field.getResizeMode().ordinal()),
                                        field.getPaddingChar(),
                                        field.getExportLength());

                                // Set the final value.
                                dataGrid.set(field.getFieldId(), fieldValue);

                            }
                            entity.postProcess();

                            if (m_saveResults) {
                                getBroker().saveBean(saveRow);
                            }

                        } else {
                            m_initErrors.add(err);
                        }
                    }
                } finally {
                    m_reportData.close();
                }
            }

            // If the report has a heading or trailer, save it to the parent record.
            if (m_saveResults && (!StringUtils.isEmpty(m_reportData.getHeading()) || !StringUtils
                    .isEmpty(m_reportData.getTrailer()))) {
                saveResult.setHeading(m_reportData.getHeading());
                saveResult.setTrailer(m_reportData.getTrailer());
                getBroker().saveBeanForced(saveResult);
            }

        } else {
            for (StateReportValidationError error : m_initErrors) {
                dataGrid.append();
                dataGrid.set("Entity name", error.getEntityName());
                dataGrid.set("Error ID", error.getErrorId());
                dataGrid.set("Field Name", error.getFieldName());
                dataGrid.set("Error message", error.getErrorMessage());
            }
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Returns a list of export field names.
     *
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List fields = null;
        if (m_reportData != null) {
            fields = new ArrayList(m_reportData.getFieldCount());
            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                fields.add(m_reportData.getFieldDefinition(pos) == null ? ""
                        : m_reportData.getFieldDefinition(pos).getFieldId());
            }

        }
        return fields;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        String lastName = "";
        if (m_initErrors != null && m_initErrors.size() > 0) {
            for (StateReportValidationError err : m_initErrors) {
                String thisName = err.getEntityName();
                if (!lastName.equals(thisName)) {
                    comment.append(err.getEntityName());
                    comment.append("\n");
                    lastName = thisName;
                }
                comment.append("    ");
                comment.append(err.getFieldName());
                comment.append("   ");
                comment.append(err.getErrorId());
                comment.append("   ");
                comment.append(err.getErrorMessage());
                comment.append("\n");
            }
        }
        return comment.toString();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        String header = null;
        if (m_reportData != null) {
            header = m_reportData.getHeading();
        }
        return header;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
     */
    @Override
    protected String getTrailer() {
        String trailer = null;
        if (m_reportData != null) {
            trailer = m_reportData.getTrailer();
        }
        return trailer;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        // Set exports to use MS/windows end of line character for all exports.
        setLineSeparator(FORMAT_EOL_WINDOWS);

        // Determine if the results should be saved in the StateReport results tables.
        Boolean saveResults = (Boolean) getParameter(SAVE_RESULTS);
        if (saveResults != null) {
            m_saveResults = saveResults.booleanValue();
        }

        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();

                // Set export parameters from the report data object.
                setEscapeCharacter(m_reportData.getEscapeCharacter());
                setIncludeHeaderRow(m_reportData.getIncludeHeaderRow());
                setUseEscapes(m_reportData.getUseEscapes());
                setUseValueDelimiters(m_reportData.getUseValueDelimiters());
                setUseValueWrappers(m_reportData.getUseValueWrappers());
                setValueDelimiter(m_reportData.getValueDelimiter());
                setValueWrapper(m_reportData.getValueWrapper());
            } catch (X2BaseException x2be) {
                String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }

    }
}
