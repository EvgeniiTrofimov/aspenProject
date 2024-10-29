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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.apache.commons.io.IOUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Element;

/**
 * The Class OnsisGenerateCsvFromXml.
 */
public class OnsisGenerateCsvFromXml extends ImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String INPUT_PARAMETER_PROCEDURES_OIDS = "proceduresOids";
    private static final String INPUT_PARAMETER_SELECT_ALL = "selectAll";

    private static final String INPUT_OUTPUT_FILE_NAME = "csv_files.zip";

    private static final String LIKE_EXSMS_PROCEDURE_ID = "EXSMS-%DT";

    private Set<String> m_extractTypes = null;

    private File m_sourceFile = null;

    /**
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#beforeImportData(java.io.File)
     */
    @Override
    protected void beforeImportData(File file) {
        getJob().getInput().setFormat(ToolInput.ZIP_FORMAT);
    }

    /**
     * Export results.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Element root = db.parse(m_sourceFile).getDocumentElement();
            OnsisExtractHelper extractHelper = new OnsisExtractHelper(getBroker());

            List<OnsisExtractRecords> onsisExtractRecordsList = getExtractTypes().stream()
                    .map(extractType -> extractHelper.getMatcherByExtractType(extractType))
                    .collect(Collectors.toList());

            List<File> csvFiles = onsisExtractRecordsList.stream()
                    .map(onsisExtractRecords -> onsisExtractRecords.parseXml(root))
                    .filter(records -> !records.getDataRecords().isEmpty())
                    .map(records -> records.toCsv())
                    .collect(Collectors.toList());
            if (!csvFiles.isEmpty()) {
                File zipFile = new File(INPUT_OUTPUT_FILE_NAME);
                try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFile))) {
                    csvFiles.stream().forEach(file -> {
                        try (FileInputStream fis = new FileInputStream(file)) {
                            ZipEntry entry = new ZipEntry(file.getPath());
                            zos.putNextEntry(entry);
                            byte[] fileContent = IOUtils.toByteArray(fis);
                            zos.write(fileContent);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    });
                }
                try (ByteArrayInputStream in =
                        new ByteArrayInputStream(IOUtils.toByteArray(new FileInputStream(zipFile)))) {
                    StreamUtils.copyStream(in, getResultHandler().getOutputStream());
                }
            } else {
                getResultHandler().setEmpty(true);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        m_sourceFile = sourceFile;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        return INPUT_OUTPUT_FILE_NAME;
    }

    /**
     * Gets the extract types.
     *
     * @return Sets the
     */
    private Set<String> getExtractTypes() {
        if (m_extractTypes == null) {
            Boolean selectAll = (boolean) getParameter(INPUT_PARAMETER_SELECT_ALL);
            X2Criteria formatsCriteria = new X2Criteria();
            if (selectAll != null && selectAll) {
                formatsCriteria.addLike(ExportFormatDefinition.COL_PROCEDURE_ID, LIKE_EXSMS_PROCEDURE_ID);
            } else {
                String formatOidsString = (String) getParameter(INPUT_PARAMETER_PROCEDURES_OIDS);
                if (formatOidsString != null) {
                    String[] formatOids = formatOidsString.split(",");
                    formatsCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList(formatOids));
                }
            }
            Collection<ExportFormatDefinition> formats = getBroker()
                    .getCollectionByQuery(new QueryByCriteria(ExportFormatDefinition.class, formatsCriteria));

            m_extractTypes = formats.stream().map(format -> format.getSifProfile()).collect(Collectors.toSet());
        }
        return m_extractTypes;
    }
}
