/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.blackbear.flatworm.ConfigurationReader;
import com.blackbear.flatworm.FileFormat;
import com.blackbear.flatworm.MatchedRecord;
import com.blackbear.flatworm.errors.FlatwormConfigurationValueException;
import com.blackbear.flatworm.errors.FlatwormConversionException;
import com.blackbear.flatworm.errors.FlatwormCreatorException;
import com.blackbear.flatworm.errors.FlatwormInputLineLengthException;
import com.blackbear.flatworm.errors.FlatwormInvalidRecordException;
import com.blackbear.flatworm.errors.FlatwormUnsetFieldValueException;
import com.follett.exams.BasedataFileType;
import com.follett.exams.CentreHeader;
import com.follett.exams.FileHeader;
import com.follett.exams.RecordType;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.content.repository.ContentRuntimeException;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.MessageDef;
import com.follett.fsc.core.k12.web.MessageDefImpl;
import com.x2dev.sis.model.beans.ExamComponent;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.ExamSyllabus;
import com.x2dev.sis.model.beans.OptionComponent;
import com.x2dev.sis.model.beans.TransferFile;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class imports basedata which defines the qualifications and exams for the UK education
 * system. Basedata is
 * provided by examination boards (also referred to as awarding bodies) and divided into examination
 * series
 * categorized by subject and time frame. The Exams officer at a centre (i.e., UK school) determines
 * which series
 * from one or more awarding bodies need to be imported into the system for their particular centre.
 *
 * @author Follett Software Company
 */
public class BasedataImport extends ToolJavaSource {

    /**
     * The Class BasedataManager.
     */
    static public class BasedataManager {
        private static final String CANDIDATE_NUMBER = "DFE CANDIDATE NUMBER";

        /**
         * Gets an ExamSyllabus by identifying fields.
         *
         * @param series ExamSeries
         * @param syllabusCode String
         * @param broker X2Broker
         * @return ExamSyllabus
         */
        static public ExamSyllabus getSyllabus(ExamSeries series, String syllabusCode, X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamSyllabus.COL_SERIES_OID, series.getOid());
            criteria.addEqualTo(ExamSyllabus.COL_SYLLABUS_CODE, syllabusCode);
            QueryByCriteria query = new QueryByCriteria(ExamSyllabus.class, criteria);
            ExamSyllabus syllabus = (ExamSyllabus) broker.getBeanByQuery(query);
            return syllabus;
        }

        /**
         * Gets the OID of an ExamSyllabus by identifying fields.
         *
         * @param series ExamSeries
         * @param syllabusCode String
         * @param broker X2Broker
         * @return String
         */
        static public String getSyllabusOid(ExamSeries series, String syllabusCode, X2Broker broker) {
            String syllabusOid = null;
            ExamSyllabus syllabus = getSyllabus(series, syllabusCode, broker);
            if (syllabus != null) {
                syllabusOid = syllabus.getOid();
            }
            return syllabusOid;
        }

        /**
         * Gets an ExamSeries by identifying fields.
         *
         * @param seriesId String
         * @param broker X2Broker
         * @return ExamSeries
         */
        static public ExamSeries getSeries(String seriesId, X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamSeries.COL_SERIES_ID, seriesId);
            QueryByCriteria query = new QueryByCriteria(ExamSeries.class, criteria);
            ExamSeries series = (ExamSeries) broker.getBeanByQuery(query);
            return series;
        }

        /**
         * Gets a list of series OIDs by the specified criteria. There are possibly 3 series with
         * the specified
         * criteria; one for each of the available series languages: English (_), Welsh (C),
         * Bi-lingual (B).
         *
         * @param seriesCode series code (2 chars)
         * @param seriesYear series year code (2 digits)
         * @param ab awarding body code (2 digits)
         * @param broker X2Broker
         * @return List<String>
         */
        static public List<String> getSeriesOids(String seriesCode, String seriesYear, String ab, X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamSeries.COL_CODE, seriesCode);
            criteria.addEqualTo(ExamSeries.COL_YEAR, seriesYear);
            criteria.addEqualTo(ExamSeries.COL_AB_IDENTIFIER, ab);
            QueryByCriteria query = new QueryByCriteria(ExamSeries.class, criteria);
            Collection<ExamSeries> seriesCollection = broker.getCollectionByQuery(query);

            List<String> oidList = new ArrayList<String>();

            for (ExamSeries series : seriesCollection) {
                oidList.add(series.getOid());
            }
            return oidList;
        }

        /**
         * Gets an exam option with the specified criteria. Multiple records may be found if
         * different language versions
         * of the series containing the option have been imported. If this happens, only one of
         * these options is returned.
         *
         * @param seriesOids List<String>
         * @param optionEntryCode String
         * @param broker X2Broker
         * @return ExamOption
         */
        static public ExamOption getOption(List<String> seriesOids, String optionEntryCode, X2Broker broker) {
            ExamOption option = null;

            // if no series were found, the option isn't in the database
            if (seriesOids.size() != 0) {
                Criteria criteria = new Criteria();
                criteria.addIn(ExamOption.COL_SERIES_OID, seriesOids);
                criteria.addEqualTo(ExamOption.COL_OPTION_ENTRY_CODE, optionEntryCode);
                QueryByCriteria query = new QueryByCriteria(ExamOption.class, criteria);
                Collection col = broker.getCollectionByQuery(query);
                if (col != null && !col.isEmpty()) {
                    // return just one
                    option = (ExamOption) col.iterator().next();
                }
            }
            return option;
        }

        /**
         * Gets an ExamOption by identifying fields.
         *
         * @param series ExamSeries
         * @param optionEntryCode String
         * @param broker X2Broker
         * @return ExamOption
         */
        static public ExamOption getExamOption(ExamSeries series, String optionEntryCode, X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamOption.COL_SERIES_OID, series.getOid());
            criteria.addEqualTo(ExamOption.COL_OPTION_ENTRY_CODE, optionEntryCode);
            QueryByCriteria query = new QueryByCriteria(ExamOption.class, criteria);
            ExamOption examOption = (ExamOption) broker.getBeanByQuery(query);
            return examOption;
        }

        /**
         * Gets the OID of an ExamOption by identifying fields.
         *
         * @param series ExamSeries
         * @param optionEntryCode String
         * @param broker X2Broker
         * @return String
         */
        static public String getExamOptionOid(ExamSeries series, String optionEntryCode, X2Broker broker) {
            String oid = null;
            ExamOption examOption = getExamOption(series, optionEntryCode, broker);
            if (examOption != null) {
                oid = examOption.getOid();
            }
            return oid;
        }

        /**
         * Gets an ExamComponent by identifying fields.
         *
         * @param series ExamSeries
         * @param componentCode String
         * @param broker X2Broker
         * @return ExamComponent
         */
        static public ExamComponent getComponent(ExamSeries series, String componentCode, X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamComponent.COL_SERIES_OID, series.getOid());
            criteria.addEqualTo(ExamComponent.COL_COMPONENT_CODE, componentCode);
            QueryByCriteria query = new QueryByCriteria(ExamComponent.class, criteria);
            ExamComponent component = (ExamComponent) broker.getBeanByQuery(query);
            return component;
        }

        /**
         * Gets the OID of an ExamComponent by identifying fields.
         *
         * @param series ExamSeries
         * @param componentCode String
         * @param broker X2Broker
         * @return String
         */
        static public String getComponentOid(ExamSeries series, String componentCode, X2Broker broker) {
            String oid = null;
            ExamComponent component = getComponent(series, componentCode, broker);
            if (component != null) {
                oid = component.getOid();
            }
            return oid;
        }

        /**
         * Gets an OptionComponent by identifying fields.
         *
         * @param series ExamSeries
         * @param optionEntryCode String
         * @param componentCode String
         * @param broker X2Broker
         * @return OptionComponent
         */
        static public OptionComponent getOptionComponent(ExamSeries series,
                                                         String optionEntryCode,
                                                         String componentCode,
                                                         X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(OptionComponent.COL_SERIES_OID, series.getOid());
            criteria.addEqualTo(OptionComponent.COL_OPTION_ENTRY_CODE, optionEntryCode);
            criteria.addEqualTo(OptionComponent.COL_COMPONENT_CODE, componentCode);
            QueryByCriteria query = new QueryByCriteria(OptionComponent.class, criteria);
            OptionComponent optionComponent = (OptionComponent) broker.getBeanByQuery(query);
            return optionComponent;
        }

        /**
         * Gets the OID of an OptionComponent by identifying fields.
         *
         * @param series ExamSeries
         * @param optionEntryCode String
         * @param componentCode String
         * @param broker X2Broker
         * @return String
         */
        static public String getOptionComponentOid(ExamSeries series,
                                                   String optionEntryCode,
                                                   String componentCode,
                                                   X2Broker broker) {
            String oid = null;
            OptionComponent optionComponent = getOptionComponent(series, optionEntryCode, componentCode, broker);
            if (optionComponent != null) {
                oid = optionComponent.getOid();
            }
            return oid;
        }

        /**
         * Gets a transfer file by identifying fields.
         *
         * @param dataType String
         * @param examSeries String
         * @param year String
         * @param language String
         * @param abIdentifier String
         * @param sequenceNum String
         * @param broker X2Broker
         * @return TransferFile
         */
        static public TransferFile getTransferFile(String dataType,
                                                   String examSeries,
                                                   String year,
                                                   String language,
                                                   String abIdentifier,
                                                   String sequenceNum,
                                                   X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(TransferFile.COL_DATA_TYPE, dataType);
            criteria.addEqualTo(TransferFile.COL_EXAM_SERIES, examSeries);
            criteria.addEqualTo(TransferFile.COL_YEAR, year);
            criteria.addEqualTo(TransferFile.COL_LANGUAGE, language);
            criteria.addEqualTo(TransferFile.COL_AB_IDENTIFIER, abIdentifier);
            criteria.addEqualTo(TransferFile.COL_SEQUENCE_NUM, sequenceNum);
            QueryByCriteria query = new QueryByCriteria(TransferFile.class, criteria);
            TransferFile transferFile = (TransferFile) broker.getBeanByQuery(query);
            return transferFile;
        }

        /**
         * Gets a transfer file by identifying fields.
         *
         * @param dataType String
         * @param year String
         * @param abIdentifier String
         * @param filename String
         * @param broker X2Broker
         * @return Transfer file
         */
        static public TransferFile getTransferFile(String dataType,
                                                   String year,
                                                   String abIdentifier,
                                                   String filename,
                                                   X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(TransferFile.COL_DATA_TYPE, dataType);
            criteria.addEqualTo(TransferFile.COL_YEAR, year);
            criteria.addEqualTo(TransferFile.COL_AB_IDENTIFIER, abIdentifier);
            criteria.addEqualTo(TransferFile.COL_FILENAME, filename);
            QueryByCriteria query = new QueryByCriteria(TransferFile.class, criteria);
            TransferFile transferFile = (TransferFile) broker.getBeanByQuery(query);
            return transferFile;
        }

        /**
         * Gets a student by their candidate number.
         *
         * @param candidateNumber String
         * @param broker X2Broker
         * @return Student
         */
        static public Student getStudentByCandidateNumber(String candidateNumber, X2Broker broker) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(CANDIDATE_NUMBER);
            if (field == null) {
                throw new ContentRuntimeException(
                        new MessageDefImpl("error.examResults.import.invalidAlias", CANDIDATE_NUMBER));
            }

            Criteria criteria = new Criteria();
            criteria.addEqualTo(field.getJavaName(), candidateNumber);
            QueryByCriteria query = new QueryByCriteria(Student.class, criteria);
            Student student = (Student) broker.getBeanByQuery(query);
            return student;
        }
    }

    /**
     * The Interface MsgKey.
     */
    /*
     * Constants for resource bundle keys.
     */
    private interface MsgKey {
        // public static final String ALREADY_IMPORTED = "error.basedata.import.alreadyImported";
        public static final String INVALID_FILE_TYPE = "error.basedata.import.invalidFileType";
        public static final String LINE_NUMBER = "error.examResults.import.lineNumber";
        public static final String INTERNAL = "error.examResults.import.internal";
        public static final String NO_ERRORS = "message.examResults.import.noErrors";
        public static final String RESULTS = "message.examResults.import.results";
        public static final String LINES_PROCESSED = "message.examResults.import.linesProcessed";
        public static final String RECORD_COUNT = "message.basedata.import.recordCount";
    }

    /*
     * Regular expressions that match filenames of basedata transfer files used to determine the
     * data type the
     * particular file contains.
     */
    static private final String ZIP_FILE_REGEX = "^[Zz]\\w{7}\\.(ZIP|zip)$";
    static private final String ZIP_OF_ZIP_FILES_REGEX = "^.+\\.(ZIP|zip|Zip)$";
    static private final String DATA_FILE_REGEX = "^[SsOoCcLl]\\w{7}\\.(X\\d\\d|x\\d\\d)$";
    static private final String SYLLABUS_FILE_REGEX = "^[Ss]\\w{7}\\.(X\\d\\d|x\\d\\d)$";
    static private final String OPTION_FILE_REGEX = "^[Oo]\\w{7}\\.(X\\d\\d|x\\d\\d)$";
    static private final String COMPONENT_FILE_REGEX = "^[Cc]\\w{7}\\.(X\\d\\d|x\\d\\d)$";
    static private final String LINK_FILE_REGEX = "^[Ll]\\w{7}\\.(X\\d\\d|x\\d\\d)$";

    static private final Pattern s_zipFilePattern = Pattern.compile(ZIP_FILE_REGEX);
    static private final Pattern s_zipOfZipFilesPattern = Pattern.compile(ZIP_OF_ZIP_FILES_REGEX);
    static private final Pattern s_dataFilePattern = Pattern.compile(DATA_FILE_REGEX);
    static private final Pattern s_syllabusFilePattern = Pattern.compile(SYLLABUS_FILE_REGEX);
    static private final Pattern s_optionFilePattern = Pattern.compile(OPTION_FILE_REGEX);
    static private final Pattern s_componentFilePattern = Pattern.compile(COMPONENT_FILE_REGEX);
    static private final Pattern s_linkFilePattern = Pattern.compile(LINK_FILE_REGEX);

    /**
     * Determines if the import file is a zip file or an individual basedata files and calls the
     * appropriate method.
     *
     * @param sourceFile File
     */
    protected void importData(File sourceFile) {
        StringBuilder errors = new StringBuilder();
        InputStream inputStream = null;

        String seasonOid = (String) getParameter("seasonOID");

        try {
            // Get an InputStream of the file being imported.
            inputStream = new FileInputStream(sourceFile);
            String filename = sourceFile.getName();

            if (isZipOfZipFiles(filename) && hasZipFiles(sourceFile)) // zip of zip files where root
                                                                      // zip file may/may not follow
                                                                      // naming convention
            {
                /*
                 * If the filename doesn't match the pattern of a basedata zip file, but is a zip
                 * file, the code
                 * assumes it's a zip containing multiple basedata zip files.
                 */
                processZipOfZipFiles(inputStream, seasonOid);
            } else if (isZipFile(filename)) // zip file that follows naming convention
            {
                processZipFile(inputStream, seasonOid, true);
            } else if (isZipOfZipFiles(filename)) // zip file that does not follow naming convention
            {
                processZipOfZipFiles(inputStream, seasonOid);
            } else if (isDataFile(filename)) {
                processBasedataFile(inputStream, filename, seasonOid);
            } else {
                String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(getLocale(), MsgKey.INVALID_FILE_TYPE, sourceFile.getName());
                appendMessage(errors, msg);
                exportResults(errors, 0, 0, "");
            }
        } catch (FileNotFoundException e) {
            String msgKey = MsgKey.INTERNAL;
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), msgKey,
                    new Object[] {e.getClass().getSimpleName(), sourceFile.getAbsolutePath()});
            appendMessage(errors, msg);
            exportResults(errors, 0, 0, "");
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    /* ignore */
                }
            }
        }
    }

    /**
     * Imports the data from an individual basedata file (syllabus, options, components, or links
     * file).
     *
     * @param input InputStream
     * @param filename String
     * @param seasonOid String
     */
    private void processBasedataFile(InputStream input, String filename, String seasonOid) {
        ConfigurationReader parser = new ConfigurationReader();
        String dataTypeName = "";
        StringBuilder errors = new StringBuilder();
        int line = 0;
        int recordCount = 0;

        boolean transactionCommitted = false;
        try {
            getBroker().beginTransaction();
            AppGlobals.getLog().log(Level.INFO, "--- beginTransaction");

            // Get the XML descriptor that defines the format of the file being imported.
            ImportExportDefinition def = (ImportExportDefinition) getJob().getTool();
            String descriptor = def.getDefinition();

            FileFormat ff = parser.loadConfigurationFile(new ByteArrayInputStream(descriptor.getBytes()));

            BufferedReader bufIn = new BufferedReader(new InputStreamReader(input));
            MatchedRecord results;

            // Initialize records based on X2 beans with a persistence key parameter so flatworm can
            // create
            // instances of the beans using reflection since this parameter is required in the bean
            // constructors.
            ff.setBeanConstructorParameters(RecordType.SYLLABUS.getId(), getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters(RecordType.LINK.getId(), getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters(RecordType.OPTION.getId(), getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters(RecordType.COMPONENT.getId(), getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters("transferFile", getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters("series", getBroker().getPersistenceKey());

            FileHeader fileHeader = null;
            CentreHeader centreHeader = null;

            String ab = null;
            TransferFile transferFile = null;
            ExamSeries series = null;

            while ((results = ff.getNextRecord(bufIn)) != null) {
                line++;
                switch (RecordType.get(results.getRecordName())) {
                    case FILE_HEADER:
                        fileHeader = (FileHeader) results.getBean(results.getRecordName());
                        ab = fileHeader.getAwardingBodyId();
                        break;

                    case FILE_TRAILER:
                        break;

                    case CENTRE_HEADER:
                        centreHeader = (CentreHeader) results.getBean(results.getRecordName());

                        transferFile = (TransferFile) results.getBean("transferFile");
                        transferFile.setFilename(filename);
                        transferFile.setDatetime(new Timestamp(System.currentTimeMillis()));

                        series = (ExamSeries) results.getBean("series");
                        series = getSeries(series);
                        if (StringUtils.isNotBlank(series.getSeasonOid())) {
                            seasonOid = series.getSeasonOid();
                        }
                        series.setSeasonOid(seasonOid);
                        series.setAwardingBody(ab);

                        dataTypeName = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(getLocale(),
                                        BasedataFileType.get(centreHeader.getDataType()).getNameKey());

                        if (doesTransferFileAlreadyExist(transferFile)) {
                            /*
                             * Commenting out for task T20113373. We want to continue importing
                             * files even if
                             * there is a transfer file bean that already exists. Currently, when
                             * deleting
                             * exam data (EXAM_SEASON or EXAM_SERIES), there is now way to cascade
                             * it down
                             * to also delete EXAM_TRANSFER_FILE.
                             */
                            // throw new ContentRuntimeException(new
                            // MessageDefImpl(MsgKey.ALREADY_IMPORTED, filename));
                        }

                        saveBean(transferFile);

                        switch (BasedataFileType.get(centreHeader.getDataType())) {
                            case SYLLABUS_FILE:
                                series.setSyllabusFileOid(transferFile.getOid());
                                break;

                            case OPTIONS_FILE:
                                series.setOptionFileOid(transferFile.getOid());
                                break;

                            case COMPONENTS_FILE:
                                series.setComponentFileOid(transferFile.getOid());
                                break;

                            case LINK_FILE:
                                series.setLinkFileOid(transferFile.getOid());
                                break;
                        }

                        saveBean(series);

                        break;

                    case CENTRE_TRAILER:
                        break;

                    case SYLLABUS:
                        ExamSyllabus syllabus = (ExamSyllabus) results.getBean(results.getRecordName());
                        syllabus.setAwardingBody(ab);
                        syllabus.setSeriesOid(series.getOid());

                        /*
                         * If there is an existing syllabus with the same identifying fields, update
                         * it with the new
                         * values.
                         */
                        ExamSyllabus existingSyllabus = BasedataManager.getSyllabus(series,
                                syllabus.getSyllabusCode(), getBroker());
                        if (existingSyllabus != null) {
                            // update with new values
                            syllabus.copyValues(existingSyllabus);
                            saveBean(existingSyllabus);
                        } else {
                            // save new syllabus
                            saveBean(syllabus);
                        }

                        recordCount++;
                        break;

                    case OPTION:
                        ExamOption option = (ExamOption) results.getBean(results.getRecordName());
                        option.setAwardingBody(ab);
                        option.setSeriesOid(series.getOid());
                        String syllabusOid = BasedataManager.getSyllabusOid(option.getSeries(),
                                option.getSyllabusCode(), getBroker());
                        option.setSyllabusOid(syllabusOid);

                        /*
                         * If there is an existing option with the same identifying fields, update
                         * it with the new
                         * values.
                         */
                        ExamOption existingOption = BasedataManager.getExamOption(series,
                                option.getOptionEntryCode(), getBroker());
                        if (existingOption != null) {
                            // update with new values
                            option.copyValues(existingOption);
                            saveBean(existingOption);
                        } else {
                            // save new option
                            saveBean(option);
                        }

                        recordCount++;
                        break;

                    case COMPONENT:
                        ExamComponent component = (ExamComponent) results.getBean(results.getRecordName());
                        component.setAwardingBody(ab);
                        component.setSeriesOid(series.getOid());

                        /*
                         * If there is an existing component with the same identifying fields,
                         * update it with the new
                         * values.
                         */
                        ExamComponent existingComponent = BasedataManager.getComponent(series,
                                component.getComponentCode(), getBroker());
                        if (existingComponent != null) {
                            // update with new values
                            component.copyValues(existingComponent);
                            saveBean(existingComponent);
                        } else {
                            // save new component
                            saveBean(component);
                        }

                        recordCount++;
                        break;

                    case LINK:
                        OptionComponent optionComponent = (OptionComponent) results.getBean(results.getRecordName());
                        optionComponent.setSeriesOid(series.getOid());
                        String optionOid = BasedataManager.getExamOptionOid(series,
                                optionComponent.getOptionEntryCode(), getBroker());
                        optionComponent.setOptionOid(optionOid);
                        String componentOid = BasedataManager.getComponentOid(series,
                                optionComponent.getComponentCode(), getBroker());
                        optionComponent.setComponentOid(componentOid);

                        /*
                         * If there is an existing OptionComponent with the same identifying fields,
                         * update it with
                         * the new values.
                         */
                        OptionComponent existingOptionComponent = BasedataManager.getOptionComponent(
                                series, optionComponent.getOptionEntryCode(),
                                optionComponent.getComponentCode(), getBroker());
                        if (existingOptionComponent != null) {
                            // update with new values
                            optionComponent.copyValues(existingOptionComponent);
                            saveBean(existingOptionComponent);
                        } else {
                            // save new OptionComponent
                            saveBean(optionComponent);
                        }

                        recordCount++;
                        break;

                    default:
                        break;
                }
            }

            getBroker().commitTransaction();
            transactionCommitted = true;
            AppGlobals.getLog().log(Level.INFO, "--- commitTransaction");
        } catch (FlatwormCreatorException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormUnsetFieldValueException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormConfigurationValueException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormInvalidRecordException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormInputLineLengthException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormConversionException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (ContentRuntimeException e) {
            for (MessageDef msg : e.getErrorMessages()) {
                appendErrorMessage(errors, msg.formatMessage(getLocale(), getBroker().getPersistenceKey()), line);
                AppGlobals.getLog().log(Level.SEVERE, msg.formatMessage(getLocale(), getBroker().getPersistenceKey()),
                        e);
            }
        } catch (Exception e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } finally {
            if (!transactionCommitted) {
                getBroker().rollbackTransaction(true);
                AppGlobals.getLog().log(Level.INFO, "--- rollbackTransaction");
            }
        }

        exportResults(errors, line, recordCount, dataTypeName);
    }

    /**
     * Returns the series object of an existing series if there is one.
     *
     * @param series ExamSeries
     * @return Exam series
     */
    private ExamSeries getSeries(ExamSeries series) {
        ExamSeries existingSeries = BasedataManager.getSeries(series.getSeriesId(), getBroker());
        if (existingSeries != null) {
            series = existingSeries;
        }
        return series;
    }

    /**
     * Returns true if the specified transfer file record is in the database (for determining if the
     * the file has
     * been imported previously).
     *
     * @param transferFile TransferFile
     * @return true, if successful
     */
    private boolean doesTransferFileAlreadyExist(TransferFile transferFile) {
        TransferFile existingTransferFile = BasedataManager.getTransferFile(transferFile.getDataType(),
                transferFile.getExamSeries(), transferFile.getYear(), transferFile.getLanguage(),
                transferFile.getAbIdentifier(), transferFile.getSequenceNum(), getBroker());
        return existingTransferFile != null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        AppGlobals.getLog().log(Level.INFO, "initialize");
        return;
    }

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        AppGlobals.getLog().log(Level.INFO, "releaseResources");
        super.releaseResources();
        return;
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        AppGlobals.getLog().log(Level.INFO, "run");
        importData((File) getParameter(FILE_KEY));
    }

    /**
     * Imports the data of a basedata zip file by importing the data from each of the individual
     * basedata files
     * it contains (syllabus, options, components, and links file).
     *
     * The boolean parameter closeStream should be set to false if the calling method is processing
     * a zip within a zip.
     * It seems that the inner zip stream is the same stream as the outer zip and the stream should
     * only be closed
     * after the all the inner zip streams have been processed.
     *
     * @param input InputStream
     * @param seasonOid String
     * @param closeStream boolean
     */
    private void processZipFile(InputStream input, String seasonOid, boolean closeStream) {
        ZipInputStream zip = null;

        try {
            zip = new ZipInputStream(input);
            byte[] syllabusBytes = null;
            byte[] optionBytes = null;
            byte[] componentBytes = null;
            byte[] linkBytes = null;
            String syllabusFilename = "";
            String optionFilename = "";
            String componentFilename = "";
            String linkFilename = "";

            /*
             * Buffer the data from each basedata file in the zip file. This is done because the
             * files must be
             * processed in a particular order and the order of their occurrence in the zip file may
             * be different.
             */
            while (true) {
                ZipEntry entry = zip.getNextEntry();
                if (entry == null) {
                    break;
                }

                if (entry.isDirectory()) {
                    // ignore the directories
                    continue;
                }

                if (isDataFile(entry.getName())) {
                    if (isSyllabusFile(entry.getName())) {
                        syllabusBytes = IOUtils.toByteArray(zip);
                        syllabusFilename = entry.getName();
                    } else if (isOptionFile(entry.getName())) {
                        optionBytes = IOUtils.toByteArray(zip);
                        optionFilename = entry.getName();
                    } else if (isComponentFile(entry.getName())) {
                        componentBytes = IOUtils.toByteArray(zip);
                        componentFilename = entry.getName();
                    } else if (isLinkFile(entry.getName())) {
                        linkBytes = IOUtils.toByteArray(zip);
                        linkFilename = entry.getName();
                    }
                }
            }

            // Process the basedata files in order (syllabus, options, components, links).
            if (syllabusBytes != null) {
                processBasedataFile(new ByteArrayInputStream(syllabusBytes), syllabusFilename, seasonOid);
            }

            if (optionBytes != null) {
                processBasedataFile(new ByteArrayInputStream(optionBytes), optionFilename, seasonOid);
            }

            if (componentBytes != null) {
                processBasedataFile(new ByteArrayInputStream(componentBytes), componentFilename, seasonOid);
            }

            if (linkBytes != null) {
                processBasedataFile(new ByteArrayInputStream(linkBytes), linkFilename, seasonOid);
            }
        } catch (IOException e) {
            StringBuilder errors = new StringBuilder();
            String msgKey = MsgKey.INTERNAL;
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), msgKey,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendMessage(errors, msg);
            exportResults(errors, 0, 0, "");
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } finally {
            if (zip != null && closeStream) {
                try {
                    zip.close();
                } catch (IOException e) {
                    /* ignore */
                }
            }
        }
    }

    /**
     * Imports the data of a zip file containing multiple basedata zip files.
     *
     * @param input InputStream
     * @param seasonOid String
     */
    private void processZipOfZipFiles(InputStream input, String seasonOid) {
        ZipInputStream zip = null;

        try {
            zip = new ZipInputStream(input);

            while (true) {
                ZipEntry entry = zip.getNextEntry();
                if (entry == null) {
                    break;
                }

                if (entry.isDirectory()) {
                    // ignore the directories
                    continue;
                }

                /*
                 * A zip of zip files might have a zip file that does not follow the naming
                 * convention. So, check whether a zip file follows naming convention first,
                 * else, if it is a file that ends with .zip, continue processing.
                 *
                 */
                if (isZipFile(entry.getName()) || isZipOfZipFiles(entry.getName())) {
                    processZipFile(zip, seasonOid, false);
                } else if (isDataFile(entry.getName())) {
                    processBasedataFile(new ByteArrayInputStream(IOUtils.toByteArray(zip)), entry.getName(), seasonOid);
                }
            }
        } catch (IOException e) {
            StringBuilder errors = new StringBuilder();
            String msgKey = MsgKey.INTERNAL;
            String msg = LocalizationCache.getMessages(getUser().getPersistenceKey()).getMessage(getLocale(), msgKey,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendMessage(errors, msg);
            exportResults(errors, 0, 0, "");
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } finally {
            if (zip != null) {
                try {
                    zip.close();
                } catch (IOException e) {
                    /* ignore */
                }
            }
        }
    }

    /**
     * Creates the results log and exports it.
     *
     * @param errors StringBuilder
     * @param lines int
     * @param recordCount int
     * @param dataTypeName String
     */
    private void exportResults(StringBuilder errors, int lines, int recordCount, String dataTypeName) {
        StringBuilder buffer = new StringBuilder(256);

        appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(getLocale(), MsgKey.RESULTS));
        buffer.append("------------------------------------------------\n");
        appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(getLocale(), MsgKey.LINES_PROCESSED, Integer.valueOf(lines)));
        appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                getLocale(), MsgKey.RECORD_COUNT,
                new Object[] {dataTypeName, Integer.valueOf(recordCount)}));
        buffer.append("------------------------------------------------\n");

        if (errors.length() == 0) {
            appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(getLocale(), MsgKey.NO_ERRORS));
        } else {
            buffer.append(errors);
        }

        buffer.append("------------------------------------------------\n\n\n");

        ByteArrayInputStream inputStream = null;
        try {
            inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
        } catch (FileNotFoundException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        } catch (IOException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    /* ignore */
                }
            }
        }
    }

    /**
     * Returns true if the passed file has at least one zip file inside it.
     *
     * @param file File
     * @return boolean
     */
    private boolean hasZipFiles(File file) {
        boolean hasZipFiles = false;

        ZipFile zipFile = null;

        try {
            zipFile = new ZipFile(file);

            Enumeration<? extends ZipEntry> entries = zipFile.entries();
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                if (isZipFile(entry.getName())) {
                    hasZipFiles = true;
                    break;
                }
            }
        } catch (Exception ex) {
            // Ignore
        } finally {
            if (zipFile != null) {
                try {
                    zipFile.close();
                } catch (IOException e) {
                    // Ignore
                }
            }
        }

        return hasZipFiles;
    }

    /**
     * Returns true if the specified filename is of a zip file.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isZipFile(String filename) {
        Matcher m = getZipFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Returns true if the specified filename is of a zip of zip files. It only checks if the
     * filename ends with .zip
     * since there is no other pattern to check. The code first checks if the pattern is for a
     * basedata zip file and
     * if it's not, but is a zip file, it assumes it's a zip file containing multiple basedata zip
     * files.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isZipOfZipFiles(String filename) {
        Matcher m = getZipOfZipFilesPattern().matcher(filename);
        return m.find();
    }

    /**
     * Returns true if the specified filename is of one of the basedata file types (i.e., syllabus,
     * options,
     * components, or links).
     *
     * @param filename String
     * @return boolean
     */
    private boolean isDataFile(String filename) {
        Matcher m = getDataFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Returns true if the specified filename is of a syllabus file.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isSyllabusFile(String filename) {
        Matcher m = getSyllabusFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Returns true if the specified filename is of an options file.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isOptionFile(String filename) {
        Matcher m = getOptionFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Returns true if the specified filename is of a components file.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isComponentFile(String filename) {
        Matcher m = getComponentFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Returns true if the specified filename is of a links file.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isLinkFile(String filename) {
        Matcher m = getLinkFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Saves a bean. If validation errors occur, they are saved in an exception and that exception
     * is thrown.
     * This allows the program flow to follow the standard try/catch pattern rather than checking
     * for returned
     * errors every time the call is made.
     *
     * @param bean bean to be saved
     */
    private void saveBean(X2BaseBean bean) {
        List<ValidationError> errors = getBroker().saveBean(bean);
        if (errors != null && !errors.isEmpty()) {
            ContentRuntimeException ex = new ContentRuntimeException();
            ex.setValidationErrors(errors);
            throw ex;
        }
    }

    /**
     * Gets DataFilePattern.
     * 
     * @return the DataFilePattern
     */
    public static Pattern getDataFilePattern() {
        return s_dataFilePattern;
    }

    /**
     * Gets ZipFilePattern.
     * 
     * @return the ZipFilePattern
     */
    public static Pattern getZipFilePattern() {
        return s_zipFilePattern;
    }

    /**
     * Gets ZipOfZipFilesPattern.
     * 
     * @return the ZipOfZipFilesPattern
     */
    public static Pattern getZipOfZipFilesPattern() {
        return s_zipOfZipFilesPattern;
    }

    /**
     * Gets SyllabusFilePattern.
     * 
     * @return the SyllabusFilePattern
     */
    public static Pattern getSyllabusFilePattern() {
        return s_syllabusFilePattern;
    }

    /**
     * Gets OptionFilePattern.
     * 
     * @return the OptionFilePattern
     */
    public static Pattern getOptionFilePattern() {
        return s_optionFilePattern;
    }

    /**
     * Gets ComponentFilePattern.
     * 
     * @return the ComponentFilePattern
     */
    public static Pattern getComponentFilePattern() {
        return s_componentFilePattern;
    }

    /**
     * Gets LinkFilePattern.
     * 
     * @return the LinkFilePattern
     */
    public static Pattern getLinkFilePattern() {
        return s_linkFilePattern;
    }

    /**
     * Formats an error message prepending the line number to the message and then adds that message
     * to the error
     * message buffer.
     *
     * @param errors error message buffer
     * @param msg error message
     * @param line line number of record in basedata file
     */
    private void appendErrorMessage(StringBuilder errors, String msg, int line) {
        String lineMsg =
                StringUtils.rightPad(LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(getLocale(), MsgKey.LINE_NUMBER, Integer.valueOf(line)), 11);
        msg = lineMsg + msg;
        appendMessage(errors, msg);
    }

    /**
     * Adds a log message to the message buffer.
     *
     * @param msgs message buffer
     * @param msg message
     */
    private void appendMessage(StringBuilder msgs, String msg) {
        msgs.append("   " + msg + '\n');
    }
}
