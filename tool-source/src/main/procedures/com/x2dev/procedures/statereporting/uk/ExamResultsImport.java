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
import com.follett.exams.CentreHeader;
import com.follett.exams.RecordType;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.content.repository.ContentRuntimeException;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.AppConstants;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.MessageDef;
import com.follett.fsc.core.k12.web.MessageDefImpl;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class imports exam results for the UK education system.
 *
 * @author Follett Software Company
 */
public class ExamResultsImport extends ToolJavaSource {
    /*
     * Input parameters
     */
    private static final String BLACKOUT_END_DATE_PARAM = "blackoutEndDate";
    private static final String BLACKOUT_START_DATE_PARAM = "blackoutStartDate";

    /**
     * The Class BasedataManager.
     */
    static public class BasedataManager {
        /*
         * Aliases
         */
        private static final String ALIAS_CANDIDATE_NUMBER = "DFE CANDIDATE NUMBER";
        private static final String ALIAS_UCI = "DFE UCI";
        private static final String ALIAS_ULN = "DFE ULN";
        private static final String ALIAS_CENTRE_NUMBER = "DFE CENTRE NUMBER";
        private static final String ALIAS_PREV_UCI = "DFE PREVIOUS UCI";
        private static final String ALIAS_PREV_ULN = "DFE PREVIOUS ULN";

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

            // If nothing found so try again with a different AB column.
            if (seriesCollection.isEmpty()) {
                criteria = new Criteria();
                criteria.addEqualTo(ExamSeries.COL_CODE, seriesCode);
                criteria.addEqualTo(ExamSeries.COL_YEAR, seriesYear);
                criteria.addEqualTo(ExamSeries.COL_AWARDING_BODY, ab);
                query = new QueryByCriteria(ExamSeries.class, criteria);
                seriesCollection = broker.getCollectionByQuery(query);
            }

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
         * @param school School
         * @param candidateNumber String
         * @param broker X2Broker
         * @return Student
         */
        static public Student getStudentByCandidateNumber(School school, String candidateNumber, X2Broker broker) {
            // get student by current candidate number
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_CANDIDATE_NUMBER);
            if (field == null) {
                throw new ContentRuntimeException(
                        new MessageDefImpl("error.examResults.import.invalidAlias", ALIAS_CANDIDATE_NUMBER));
            }

            Criteria criteria1 = new Criteria();
            criteria1.addEqualTo(field.getJavaName(), candidateNumber);
            criteria1.addEqualTo(Student.COL_SCHOOL_OID, school.getOid());
            QueryByCriteria query1 = new QueryByCriteria(Student.class, criteria1);
            Student student = (Student) broker.getBeanByQuery(query1);

            /*
             * Student not looked up by previous candidate number because in some cases it will have
             * come from another
             * MIS and may match a candidate number assigned to a different student in Aspen. So
             * this could potentially
             * attach an exam result to the wrong student.
             */

            return student;
        }

        /**
         * Gets a student by their UCI.
         *
         * @param uci String
         * @param broker X2Broker
         * @return Student
         */
        static public Student getStudentByUci(String uci, X2Broker broker) {
            // get student by current UCI
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_UCI);
            if (field == null) {
                throw new ContentRuntimeException(
                        new MessageDefImpl("error.examResults.import.invalidAlias", ALIAS_UCI));
            }

            Criteria criteria1 = new Criteria();
            criteria1.addEqualTo(field.getJavaName(), uci);
            QueryByCriteria query1 = new QueryByCriteria(SisStudent.class, criteria1);
            Student student = (Student) broker.getBeanByQuery(query1);

            // if student not found by current UCI, attempt to find by a previous UCI
            if (student == null) {
                DataDictionaryField prevUciField = dictionary.findDataDictionaryFieldByAlias(ALIAS_PREV_UCI);
                if (prevUciField != null) {
                    Criteria criteria2 = new Criteria();
                    criteria2.addEqualTo(prevUciField.getJavaName(), uci);
                    QueryByCriteria query2 = new QueryByCriteria(StudentVersionHistory.class, criteria2);
                    StudentVersionHistory svh = (StudentVersionHistory) broker.getBeanByQuery(query2);
                    if (svh != null) {
                        student = svh.getStudent();
                    }
                }
            }

            return student;
        }

        /**
         * Gets a student by their ULN.
         *
         * @param uln String
         * @param broker X2Broker
         * @return Student
         */
        static public Student getStudentByUln(String uln, X2Broker broker) {
            // get student by current ULN
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_ULN);
            if (field == null) {
                throw new ContentRuntimeException(
                        new MessageDefImpl("error.examResults.import.invalidAlias", ALIAS_ULN));
            }

            Criteria criteria1 = new Criteria();
            criteria1.addEqualTo(field.getJavaName(), uln);
            QueryByCriteria query1 = new QueryByCriteria(Student.class, criteria1);
            Student student = (Student) broker.getBeanByQuery(query1);

            // if student not found by current ULN, attempt to find by a previous ULN
            if (student == null) {
                DataDictionaryField prevUlnField = dictionary.findDataDictionaryFieldByAlias(ALIAS_PREV_ULN);
                if (prevUlnField != null) {
                    Criteria criteria2 = new Criteria();
                    criteria2.addEqualTo(prevUlnField.getJavaName(), uln);
                    QueryByCriteria query2 = new QueryByCriteria(StudentVersionHistory.class, criteria2);
                    StudentVersionHistory svh = (StudentVersionHistory) broker.getBeanByQuery(query2);
                    if (svh != null) {
                        student = svh.getStudent();
                    }
                }
            }

            return student;
        }

        /**
         * Gets an existing result record to be overwritten when re-importing results.
         *
         * @param transferFileOid String
         * @param uci String
         * @param optionResultCode String
         * @param broker X2Broker
         * @return Exam result
         */
        static public ExamResult getExistingResult(String transferFileOid,
                                                   String uci,
                                                   String optionResultCode,
                                                   X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamResult.COL_RESULTS_FILE_OID, transferFileOid);
            criteria.addEqualTo(ExamResult.COL_UCI, uci);
            criteria.addEqualTo(ExamResult.COL_OPTION_RESULT_CODE, optionResultCode);
            QueryByCriteria query = new QueryByCriteria(ExamResult.class, criteria);
            ExamResult result = (ExamResult) broker.getBeanByQuery(query);
            return result;
        }


        /**
         * Gets the school by centre number.
         *
         * @param centreNumber String
         * @param broker X2Broker
         * @return School
         */
        static public School getSchoolByCentreNumber(String centreNumber, X2Broker broker) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_CENTRE_NUMBER);
            if (field == null) {
                throw new ContentRuntimeException(
                        new MessageDefImpl("error.examResults.import.invalidAlias", ALIAS_CENTRE_NUMBER));
            }

            Criteria criteria = new Criteria();
            criteria.addLike(field.getJavaName(), centreNumber);
            QueryByCriteria query = new QueryByCriteria(School.class, criteria);
            School school = (School) broker.getBeanByQuery(query);
            return school;
        }
    }

    /**
     * The Interface MsgKey.
     */
    /*
     * Constants for resource bundle keys.
     */
    private interface MsgKey {
        public static final String STUDENT_NOT_FOUND = "error.examResults.import.studentNotFound";
        public static final String ENTRY_OPTION_NOT_FOUND = "error.examResults.import.entryOptionNotFound";
        public static final String INVALID_NUMERIC_SCORE = "error.examResults.import.invalidNumericScore";
        public static final String INTERNAL = "error.examResults.import.internal";
        public static final String LINE_NUMBER = "error.examResults.import.lineNumber";
        public static final String NO_ERRORS = "message.examResults.import.noErrors";
        public static final String RESULTS = "message.examResults.import.results";
        public static final String LINES_PROCESSED = "message.examResults.import.linesProcessed";
        public static final String EXAM_RESULTS_COUNT = "message.examResults.import.examResultsCount";
        public static final String INVALID_FILE_TYPE = "error.basedata.import.invalidFileType";
    }

    /**
     * The Interface ResultType.
     */
    /*
     * Constants for exam result types.
     */
    private interface ResultType {
        public static final String GRADES = "1";
        public static final String LEVELS = "2";
        public static final String POINTS = "3";
        public static final String UNIFORM = "U";
        public static final String MARK = "M";
        public static final String TYPE_B = "B";
        public static final String TYPE_C = "C";
    }

    /*
     * Regular expressions that match filenames of results transfer files used to determine the
     * format of the
     * files contents. Specifically, if the file is a single results file or a zip of multiple
     * results files.
     */
    static private final String ZIP_FILE_REGEX = "^.+\\.(ZIP|zip|Zip)$";
    static private final String RESULTS_FILE_REGEX = "^[Rr]\\w{7}\\.(X\\d\\d|x\\d\\d)$";

    static private final Pattern s_zipFilePattern = Pattern.compile(ZIP_FILE_REGEX);
    static private final Pattern s_resultsFilePattern = Pattern.compile(RESULTS_FILE_REGEX);

    private int m_newEntriesCount;

    /**
     * Gets the input stream of the import file and passes it to the method that imports the data.
     *
     * @param sourceFile File
     * @throws InvalidPreferenceException exception
     */
    protected void importData(File sourceFile) throws InvalidPreferenceException {
        StringBuilder errors = new StringBuilder();
        InputStream inputStream = null;

        try {
            // Get an InputStream of the file being imported.
            inputStream = new FileInputStream(sourceFile);
            String filename = sourceFile.getName();

            if (isResultsFile(filename)) {
                processExamResultsFile(inputStream, filename);
            } else if (isZipFile(filename)) {
                processZipFile(inputStream);
            } else {
                String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(getLocale(), MsgKey.INVALID_FILE_TYPE, sourceFile.getName());
                appendMessage(errors, msg);
                exportResults(errors, 0, 0, filename);
            }
        } catch (FileNotFoundException e) {
            String msgKey = MsgKey.INTERNAL;
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    getLocale(), msgKey,
                    new Object[] {e.getClass().getSimpleName(), sourceFile.getAbsolutePath()});
            appendMessage(errors, msg);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } finally {
            /*
             * Set the results navigation blackout preference.
             */
            PlainDate blackoutStartDate = (PlainDate) getParameter(BLACKOUT_START_DATE_PARAM);
            PlainDate blackoutEndDate = (PlainDate) getParameter(BLACKOUT_END_DATE_PARAM);
            if (blackoutStartDate != null && blackoutEndDate != null) {
                // Convert the PlainDate object to yyyy-MM-dd string format.
                Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                        getLocale(),
                        false,
                        AppConstants.SYSTEM_DATE_PATTERN);

                String startDateAsString = converter.javaToString(blackoutStartDate);
                String endDateAsString = converter.javaToString(blackoutEndDate);

                PreferenceManager.setPreferenceValue(getOrganization(),
                        getBroker(),
                        SisPreferenceConstants.RSL_BLACKOUT_START_DATE,
                        startDateAsString);

                PreferenceManager.setPreferenceValue(getOrganization(),
                        getBroker(),
                        SisPreferenceConstants.RSL_BLACKOUT_END_DATE,
                        endDateAsString);

                DateFormat dateFormat = new SimpleDateFormat("d/M/yyyy", getLocale());

                appendMessage(errors, "Results blackout date-range preference has been set.\n");
                appendMessage(errors, "Results navigation in Aspen will be hidden between "
                        + dateFormat.format(blackoutStartDate) + " and " + dateFormat.format(blackoutEndDate));
            }

            exportResults(errors, 0, 0, "");

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
     * Parses the exam results data and stores it in the database.
     *
     * @param input InputStream
     * @param filename String
     */
    private void processExamResultsFile(InputStream input, String filename) {
        ConfigurationReader parser = new ConfigurationReader();
        StringBuilder errors = new StringBuilder();
        int line = 0;
        int examResultsCount = 0;
        boolean reimport = false;

        boolean transactionCommitted = false;
        try {
            getBroker().beginTransaction();
            AppGlobals.getLog().log(Level.INFO, "--- beginTransaction");

            // Get the XML descriptor that defines the format of the file being imported.
            ImportExportDefinition def = (ImportExportDefinition) getJob().getTool();
            String descriptor = def.getDefinition();

            FileFormat ff = parser.loadConfigurationFile(new ByteArrayInputStream(descriptor.getBytes()));

            BufferedReader bufIn = new BufferedReader(new InputStreamReader(input));
            MatchedRecord results = null;

            // Initialize records based on X2 beans with a persistence key parameter so flatworm can
            // create
            // instances of the beans using reflection since this parameter is required in the bean
            // constructors.
            ff.setBeanConstructorParameters("result", getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters("transferFile", getBroker().getPersistenceKey());
            ff.setBeanConstructorParameters("series", getBroker().getPersistenceKey());

            TransferFile transferFile = null;
            List<String> seriesOids = null;
            CentreHeader centreHeader = null;
            School school = null;
            boolean fileEnd = false;

            while (!fileEnd && (results = ff.getNextRecord(bufIn)) != null) {
                line++;
                switch (RecordType.get(results.getRecordName())) {
                    case FILE_HEADER:
                        break;

                    case FILE_TRAILER:
                        fileEnd = true;
                        break;

                    case CENTRE_HEADER:
                        centreHeader = (CentreHeader) results.getBean(results.getRecordName());
                        school = BasedataManager.getSchoolByCentreNumber(centreHeader.getCentreNumber(), getBroker());

                        transferFile = (TransferFile) results.getBean("transferFile");
                        transferFile.setFilename(filename);

                        TransferFile existingTransferFile = BasedataManager.getTransferFile(transferFile.getDataType(),
                                transferFile.getYear(), transferFile.getAbIdentifier(), transferFile.getFilename(),
                                getBroker());
                        if (existingTransferFile != null) {
                            reimport = true;
                            transferFile = existingTransferFile;
                        }

                        transferFile.setDatetime(new Timestamp(System.currentTimeMillis()));

                        getBroker().saveBeanForced(transferFile);

                        ExamSeries series = (ExamSeries) results.getBean("series");
                        seriesOids = BasedataManager.getSeriesOids(series.getCode(), series.getYear(),
                                series.getAbIdentifier(), getBroker());

                        break;

                    case CENTRE_TRAILER:
                        break;

                    case RESULTS:
                        ExamResult examResult = (ExamResult) results.getBean(results.getRecordName());

                        if (reimport) {
                            ExamResult existingExamResult = BasedataManager.getExistingResult(transferFile.getOid(),
                                    examResult.getUci(), examResult.getOptionResultCode(), getBroker());
                            if (existingExamResult != null) {
                                getBroker().deleteBean(existingExamResult);
                            }
                        }

                        examResult.setResultsFileOid(transferFile.getOid());
                        setExamResultReferences(examResult, school, seriesOids, errors, line);
                        parseExamResultGrades(examResult, errors, line);

                        getBroker().saveBean(examResult);
                        if (examResult.getOid() != null) {
                            examResultsCount++;
                        }

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
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormUnsetFieldValueException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormConfigurationValueException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormInvalidRecordException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormInputLineLengthException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (FlatwormConversionException e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } catch (ContentRuntimeException e) {
            for (MessageDef msg : e.getErrorMessages()) {
                appendErrorMessage(errors, msg.formatMessage(Locale.UK, getBroker().getPersistenceKey()), line);
                AppGlobals.getLog().log(Level.SEVERE, msg.formatMessage(Locale.UK, getBroker().getPersistenceKey()));
            }
        } catch (Exception e) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(
                    Locale.UK, MsgKey.INTERNAL,
                    new Object[] {e.getClass().getSimpleName(), e.getMessage()});
            appendErrorMessage(errors, msg, line);
            AppGlobals.getLog().log(Level.SEVERE, msg, e);
        } finally {
            if (!transactionCommitted) {
                getBroker().rollbackTransaction(true);
                AppGlobals.getLog().log(Level.INFO, "--- rollbackTransaction");
            }
        }

        exportResults(errors, line, examResultsCount, filename);
    }

    /**
     * Sets the foreign keys of the student and exam options this exam result references.
     *
     * @param examResult ExamResult
     * @param school School
     * @param seriesOids List<String>
     * @param errors StringBuilder
     * @param line int
     */
    @SuppressWarnings("unused")
    private void setExamResultReferences(ExamResult examResult,
                                         School school,
                                         List<String> seriesOids,
                                         StringBuilder errors,
                                         int line) {
        // Join result to student.
        // Try to find the student first by UCI, second by ULN, and finally by candidate number.
        Student student = BasedataManager.getStudentByUci(examResult.getUci(), getBroker());

        if (student == null) {
            student = BasedataManager.getStudentByUln(examResult.getUln(), getBroker());
        }

        if (student == null) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(Locale.UK, MsgKey.STUDENT_NOT_FOUND, examResult.getUci());
            appendErrorMessage(errors, msg, line);
        } else {
            examResult.setStudentOid(student.getOid());
        }

        String optionCode = "";
        boolean isEntryOption = false;
        if (StringUtils.isNotBlank(examResult.getOptionEntryCode())) {
            optionCode = examResult.getOptionEntryCode();
            isEntryOption = true;
        } else {
            optionCode = examResult.getOptionResultCode();
        }

        ExamOption option = BasedataManager.getOption(seriesOids, optionCode, getBroker());
        if (option == null) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(Locale.UK, MsgKey.ENTRY_OPTION_NOT_FOUND, optionCode);
            appendErrorMessage(errors, msg, line);
        }

        String studentOid = examResult.getStudentOid();

        String optionOid = "";
        if (option != null) {
            optionOid = option.getOid();
        }
        examResult.setOptionOid(optionOid);

        if (!StringUtils.isEmpty(studentOid) && !StringUtils.isEmpty(optionOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExamEntry.COL_STUDENT_OID, studentOid);
            criteria.addEqualTo(ExamEntry.COL_OPTION_OID, optionOid);

            QueryByCriteria query = new QueryByCriteria(ExamEntry.class, criteria);

            ExamEntry matchingEntry = (ExamEntry) getBroker().getBeanByQuery(query);
            if (matchingEntry == null) {
                matchingEntry = new ExamEntry(getBroker().getPersistenceKey());
                matchingEntry.setStudentOid(studentOid);
                matchingEntry.setOptionOid(optionOid);
                matchingEntry.setSystemCreatedIndicator(true);

                getBroker().saveBeanForced(matchingEntry);
                m_newEntriesCount++;
            }
        }
    }

    /**
     * Parses the grade data and puts the data in the appropriate fields.
     *
     * @param examResult ExamResult
     * @param errors StringBuilder
     * @param line int
     */
    private void parseExamResultGrades(ExamResult examResult, StringBuilder errors, int line) {
        String type = examResult.getResultType();
        String grades = examResult.getGrades();
        if (StringUtils.equals(type, ResultType.GRADES)
                || StringUtils.equals(type, ResultType.LEVELS)
                || StringUtils.equals(type, ResultType.POINTS)) {
            examResult.setFirstGrade(substr(grades, 0, 2));
            examResult.setSecondGrade(substr(grades, 2, 4));
            examResult.setFirstGradeEndorsement(substr(grades, 4, 6));
            examResult.setSecondGradeEndorsement(substr(grades, 6, 8));
            examResult.setFirstGradeAbsence(substr(grades, 8, 9));
            examResult.setSecondGradeAbsence(substr(grades, 9, 10));
            examResult.setFirstEndorsementAbsence(substr(grades, 10, 11));
            examResult.setSecondEndorsementAbsence(substr(grades, 11, 12));
        } else if (StringUtils.equalsIgnoreCase(type, ResultType.UNIFORM)
                || StringUtils.equalsIgnoreCase(type, ResultType.MARK)) {
            examResult.setNumericScore(numericScoreSubstr(grades, 0, 3, errors, line));
            examResult.setFirstGrade(substr(grades, 3, 5));
        } else if (StringUtils.equalsIgnoreCase(type, ResultType.TYPE_B)
                || StringUtils.equalsIgnoreCase(type, ResultType.TYPE_C)) {
            examResult.setNumericScore(numericScoreSubstr(grades, 0, 4, errors, line));
            examResult.setFirstGrade(substr(grades, 4, 6));
            examResult.setFirstGradeAbsence(substr(grades, 10, 11));
        }
    }

    /**
     * Gets a substring with white space removed from the ends.
     *
     * @param str String
     * @param start int
     * @param end int
     * @return String
     */
    private String substr(String str, int start, int end) {
        return StringUtils.strip(StringUtils.substring(str, start, end));
    }

    /**
     * Gets the substring containing a numeric score and records an error if it isn't valid numeric
     * data.
     *
     * @param str String
     * @param start int
     * @param end int
     * @param errors StringBuilder
     * @param line int
     * @return int
     */
    private int numericScoreSubstr(String str, int start, int end, StringBuilder errors, int line) {
        int score = 0;
        String scoreStr = substr(str, start, end);

        try {
            score = Integer.parseInt(scoreStr);
        } catch (NumberFormatException ex) {
            String msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(Locale.UK, MsgKey.INVALID_NUMERIC_SCORE, scoreStr);
            appendErrorMessage(errors, msg, line);
        }
        return score;
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
                        .getMessage(Locale.UK, MsgKey.LINE_NUMBER, Integer.valueOf(line)), 11);
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
     * Imports the data of a results zip file by importing the data from each of the individual
     * results files
     * it contains.
     *
     * @param input InputStream
     */
    private void processZipFile(InputStream input) {
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

                // Make sure it's a results file and not some other random file within the zip.
                if (isResultsFile(entry.getName())) {
                    processExamResultsFile(zip, entry.getName());
                }
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
     * @param examResultsCount int
     * @param filename String
     */
    private void exportResults(StringBuilder errors, int lines, int examResultsCount, String filename) {
        StringBuilder buffer = new StringBuilder(256);

        appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(Locale.UK, MsgKey.RESULTS) + " - " + filename);
        buffer.append("------------------------------------------------\n");
        appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(Locale.UK, MsgKey.LINES_PROCESSED, Integer.valueOf(lines)));
        appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(Locale.UK, MsgKey.EXAM_RESULTS_COUNT, Integer.valueOf(examResultsCount)));
        appendMessage(buffer, "New entries count: " + m_newEntriesCount);
        buffer.append("------------------------------------------------\n");

        if (errors.length() == 0) {
            appendMessage(buffer, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(Locale.UK, MsgKey.NO_ERRORS));
        } else {
            buffer.append(errors);
        }

        buffer.append("------------------------------------------------\n");

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
     * Returns true if the specified filename is of a results file.
     *
     * @param filename String
     * @return boolean
     */
    private boolean isResultsFile(String filename) {
        Matcher m = getResultsFilePattern().matcher(filename);
        return m.find();
    }

    /**
     * Gets ResultsFilePattern.
     * 
     * @return the ResultsFilePattern
     */
    public static Pattern getResultsFilePattern() {
        return s_resultsFilePattern;
    }

    /**
     * Gets ZipFilePattern.
     * 
     * @return the ZipFilePattern
     */
    public static Pattern getZipFilePattern() {
        return s_zipFilePattern;
    }
}
