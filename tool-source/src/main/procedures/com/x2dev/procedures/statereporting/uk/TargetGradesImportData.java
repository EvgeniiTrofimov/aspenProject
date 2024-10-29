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
package com.x2dev.procedures.statereporting.uk;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */


import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.TargetGrade;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;

/**
 * The Class TargetGradesImportData.
 *
 * @author X2 Development Corporation
 */
public class TargetGradesImportData extends ImportJavaSource {
    // Format characters for XLS cell values
    private static final String ESCAPED_SINGLE_BACKSLASH = "\\\\\\\\";
    private static final String ESCAPED_SINGLE_QUOTE = "''";
    private static final String SINGLE_BACKSLASH = "\\\\";
    private static final String SINGLE_QUOTE = "'";

    private static final String IMPORT_NAME_PARAM = "importName";
    private static final String UPN_PARAM = "upnColumnName";

    private int m_matchCount = 0;
    private int m_insertCount = 0;
    private int m_updateCount = 0;
    private int m_skipCount = 0;

    private List<String> m_columnNames;
    @SuppressWarnings("unused")
    private int m_headerRow;

    /**
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        String sourceFileName = ((File) getParameter(FILE_KEY)).getName();

        buffer.append("  Results" + '\n');
        buffer.append("------------------------------------------------\n");
        buffer.append("   File name: " + sourceFileName + '\n');
        buffer.append("   Match count: " + m_matchCount + '\n');
        buffer.append("   Update count: " + m_updateCount + '\n');
        buffer.append("   Insert count: " + m_insertCount + '\n');
        buffer.append("   Skipped count: " + m_skipCount + '\n');
        buffer.append("------------------------------------------------\n");

        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Imports the data from the given file.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        InputStream inputStream = new FileInputStream(sourceFile.getAbsolutePath());
        POIFSFileSystem fileSystem = new POIFSFileSystem(inputStream);
        HSSFWorkbook workbook = new HSSFWorkbook(fileSystem);

        m_headerRow = 0;
        m_columnNames = null;

        HSSFSheet sheet = workbook.getSheetAt(0);
        if (sheet != null) {
        	Iterator<Row> rows = sheet.rowIterator();
            if (rows.hasNext()) {
                /*
                 * Determine the width of the current sheet. The width of the current sheet is
                 * the cell count of the row with the most cells.
                 */
                int sheetWidth = 0;
                int lineNumber = 0;
                while (lineNumber <= sheet.getLastRowNum()) {
                    Row row = sheet.getRow(lineNumber);
                    if (row != null) {
                        short cellCount = (short) (row.getLastCellNum() - 1);
                        while (cellCount >= 0 && (row.getCell(cellCount) == null
                                || row.getCell(cellCount).getCellType() == CellType.BLANK)) {
                            if (row.getCell(cellCount) != null) {
                                row.removeCell(row.getCell(cellCount));
                            }

                            cellCount--;
                        }

                        sheetWidth = Math.max(sheetWidth, row.getPhysicalNumberOfCells());
                    }
                    lineNumber++;
                }

                /*
                 * Determine the header row index for this sheet.
                 */
                int headerRowIndex = 0;
                HSSFRow headerRow = sheet.getRow(headerRowIndex);

                /*
                 * Auto resize the width of all the columns in a sheet.
                 */
                for (short j = 0; j < headerRow.getLastCellNum(); j++) {
                    sheet.autoSizeColumn(j);
                }

                String tableName = "target_grade";

                createRefCodes(sheet, tableName, headerRowIndex);
                populateTable(sheet, tableName, headerRowIndex);
            }
        }
    }

    /**
     * Creates a SQL table based on the text in the cells of the first row of the spreadsheet and
     * the column width and cell types.
     *
     * @param sheet HSSFSheet
     * @param tableName String
     * @param headerRowIndex int
     * @return void
     * @throws SQLException exception
     */
    private void createRefCodes(HSSFSheet sheet, String tableName, int headerRowIndex) throws SQLException {
        HSSFRow headerRow = sheet.getRow(headerRowIndex);

        m_columnNames = new ArrayList<String>(headerRow.getLastCellNum());
        // TODO: reference table should get from data dictionary
        ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, "rtbTarGrdType");
        Map<String, ReferenceCode> codes = table.getCodeMap();

        CellTracker cellTracker = new CellTracker(headerRow);

        short cellNumber = 0;
        while (cellNumber <= headerRow.getLastCellNum()) {
            HSSFCell cell = headerRow.getCell(cellNumber);
            if (cell != null) {
                String columnName = cell.getRichStringCellValue().toString();
                columnName = columnName.trim().toUpperCase();
                m_columnNames.add(columnName);
                if (!codes.containsKey(columnName)) {
                    ReferenceCode refCode =
                            X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
                    refCode.setReferenceTableOid(table.getOid());
                    refCode.setCode(columnName);
                    getBroker().saveBean(refCode);
                }
            }
            cellTracker.next();
            cellNumber++;
        }
    }

    /**
     * Creates and executes INSERT statements for each row after the Header row in the spreadsheet.
     *
     * @param sheet HSSFSheet
     * @param tableName String
     * @param headerRowIndex int
     * @return void
     */
    private void populateTable(HSSFSheet sheet, String tableName, int headerRowIndex) {
        if (sheet != null) {
            // Iterate over each row in the sheet
            int lineNumber = 0;
            while (lineNumber <= sheet.getLastRowNum()) {
                HSSFRow row = sheet.getRow(lineNumber);
                if (row != null && row.getRowNum() > headerRowIndex && row.getPhysicalNumberOfCells() > 0) {
                    /*
                     * Iterate over each cell in the row and fill in the 'values' of the
                     * insert statement
                     */
                    CellTracker cellTracker = new CellTracker(row);
                    String studentOid = null;
                    String upnColumnName = (String) getParameter(UPN_PARAM);
                    short cellNumber = 0;
                    boolean isStudent = false;
                    while (m_columnNames.size() > cellTracker.getColumnIndex()) {
                        if (m_columnNames.get(cellTracker.getColumnIndex()).toString().equals(upnColumnName)) {
                            HSSFCell cell = row.getCell(cellNumber);
                            String value;
                            if (cell != null) {
                                if (cell.getCellType() == CellType.NUMERIC) {
                                    value = new BigDecimal(cell.getNumericCellValue()).toPlainString();
                                } else {
                                    value = cell.toString();
                                }
                                // TODO: Add ability to search alias fields rather than LASID/SASID
                                Criteria criteria = StudentManager.buildStudentSearchCriteria(value);
                                SubQuery query = new SubQuery(Student.class, X2BaseBean.COL_OID, criteria);
                                studentOid = (String) getBroker().getSubQueryValueByQuery(query);
                            }
                            if (studentOid != null) {
                                isStudent = true;
                            }
                        }
                        cellTracker.next();
                        cellNumber++;
                    }

                    if (isStudent) {
                        cellTracker.reset();
                        String importName = (String) getParameter(IMPORT_NAME_PARAM);

                        cellNumber = 0;

                        // the length is necessary to ensure that the excel data in the import will
                        // fit into the field it is populating
                        DataDictionaryField field = DataDictionary
                                .getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                                        SchoolCourse.class.getName(), SchoolCourse.COL_TARGET_GRADE_TYPE);
                        int length = field.getDatabaseLength();
                        while (m_columnNames.size() > cellTracker.getColumnIndex()) {
                            TargetGrade targetGrade =
                                    X2BaseBean.newInstance(TargetGrade.class, getBroker().getPersistenceKey());
                            String value = "NULL";
                            String type = m_columnNames.get(cellTracker.getColumnIndex());
                            if (type.length() > length) {
                                type = type.substring(0, length);
                            }
                            targetGrade.setType(type);
                            targetGrade.setDistrictContextOid(getCurrentContext().getOid());
                            targetGrade.setPosition(cellTracker.m_cellNumber);
                            targetGrade.setImportName(importName);
                            /*
                             * Create the insert statement, specifying the columns to add
                             */
                            HSSFCell cell = row.getCell(cellNumber);
                            if (cell != null) {
                                switch (cell.getCellType()) {
                                    case NUMERIC:
                                        value = cell.toString();
                                        break;

                                    case STRING:
                                        String cellContents = cell.getRichStringCellValue().getString()
                                                .replaceAll(SINGLE_QUOTE, ESCAPED_SINGLE_QUOTE);
                                        cellContents =
                                                cellContents.replaceAll(SINGLE_BACKSLASH, ESCAPED_SINGLE_BACKSLASH);
                                        value = cellContents;
                                        break;

                                    default:
                                        break;
                                }
                            }
                            if (value.length() > length) {
                                value = value.substring(0, length);
                            }
                            targetGrade.setTargetGrade(value);
                            targetGrade.setStudentOid(studentOid);
                            getBroker().saveBeanForced(targetGrade);
                            cellTracker.next();
                            cellNumber++;
                        }
                        m_insertCount++;
                    } else {
                        m_skipCount++;
                    }
                }
                lineNumber++;
            }
        }
    }

    /**
     * This class helps track the current cell (aka column) to keep the SQL and spreadsheet
     * synchronized since the POI row iterator does not return cell data for empty cells.
     */
    private class CellTracker extends Object {
        // Private variables
        int m_cellNumber;
        private final HSSFRow m_row;

        /**
         * Constructs a new CellTracker for the current spreadsheet row.
         *
         * @param headerRow HSSFRow
         */
        public CellTracker(HSSFRow headerRow) {
            m_cellNumber = 0;
            m_row = headerRow;
        }

        /**
         * Returns the tracker's current column index.
         *
         * @return int
         */
        public int getColumnIndex() {
            return m_cellNumber;
        }

        /**
         * Returns true if the tracker is on the last column for the row; otherwise false.
         *
         * @return boolean
         */
        @SuppressWarnings("unused")
        public boolean isLastColumn() {
            return m_cellNumber == m_row.getLastCellNum();
        }

        /**
         * Increments the tracker to the next column.
         */
        public void next() {
            m_cellNumber++;
        }

        /**
         * Resets the column counter to zero.
         */
        public void reset() {
            m_cellNumber = 0;
        }

        /**
         * Returns a string indicating which column the cell tracker is in.
         *
         * @return String
         */
        @Override
        public String toString() {
            return "Column: " + Integer.toString(m_cellNumber);
        }
    }
}
