/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.content.repository.ContentRuntimeException;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.MessageDef;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentVersionHistory;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure that assigns candidate numbers and UCI's to UK students that don't have these
 * identifiers or their
 * current ones are invalid.
 *
 * @author Follett Software Company
 */
public class CandidateNumberProcedure extends ProcedureJavaSource {
    /**
     * Class containing the code that generates new candidate numbers and UCI's for students in the
     * UK. It also
     * archives any old values being replaced.
     *
     * @author Follett Software Company
     */
    static public class CandidateNumberManager {
        /*
         * Values used to calculate the check character at the end of the UCI. It maps letters (that
         * may be in the UCI) to
         * numbers for doing the arithmetic that determines the check character.
         */
        private static final Map<Character, Integer> CHAR_INT_MAP = new HashMap<Character, Integer>() {
            {
                put(Character.valueOf('A'), Integer.valueOf(1));
                put(Character.valueOf('B'), Integer.valueOf(2));
                put(Character.valueOf('C'), Integer.valueOf(3));
                put(Character.valueOf('D'), Integer.valueOf(4));
                put(Character.valueOf('E'), Integer.valueOf(5));
                put(Character.valueOf('F'), Integer.valueOf(6));
                put(Character.valueOf('G'), Integer.valueOf(7));
                put(Character.valueOf('H'), Integer.valueOf(8));
                put(Character.valueOf('I'), Integer.valueOf(9));
                put(Character.valueOf('J'), Integer.valueOf(10));
                put(Character.valueOf('K'), Integer.valueOf(11));
                put(Character.valueOf('L'), Integer.valueOf(12));
                put(Character.valueOf('M'), Integer.valueOf(13));
                put(Character.valueOf('N'), Integer.valueOf(14));
                put(Character.valueOf('O'), Integer.valueOf(15));
                put(Character.valueOf('P'), Integer.valueOf(16));
                put(Character.valueOf('Q'), Integer.valueOf(10));
                put(Character.valueOf('R'), Integer.valueOf(11));
                put(Character.valueOf('S'), Integer.valueOf(12));
                put(Character.valueOf('T'), Integer.valueOf(13));
                put(Character.valueOf('U'), Integer.valueOf(14));
                put(Character.valueOf('V'), Integer.valueOf(15));
                put(Character.valueOf('W'), Integer.valueOf(16));
                put(Character.valueOf('X'), Integer.valueOf(10));
                put(Character.valueOf('Y'), Integer.valueOf(11));
                put(Character.valueOf('Z'), Integer.valueOf(12));
            }
        };

        /*
         * Values used to determine the check character at the end of the UCI. It maps final result
         * from the algorithm
         * that determines the check character to the actual check character.
         */
        private static final Map<Integer, String> INT_CHAR_MAP = new HashMap<Integer, String>() {
            {
                put(Integer.valueOf(0), "A");
                put(Integer.valueOf(1), "B");
                put(Integer.valueOf(2), "C");
                put(Integer.valueOf(3), "D");
                put(Integer.valueOf(4), "E");
                put(Integer.valueOf(5), "F");
                put(Integer.valueOf(6), "G");
                put(Integer.valueOf(7), "H");
                put(Integer.valueOf(8), "I");
                put(Integer.valueOf(9), "J");
                put(Integer.valueOf(10), "K");
                put(Integer.valueOf(11), "R");
                put(Integer.valueOf(12), "T");
                put(Integer.valueOf(13), "V");
                put(Integer.valueOf(14), "W");
                put(Integer.valueOf(15), "X");
                put(Integer.valueOf(16), "Y");
            }
        };

        /**
         * Determines if a new candidate number and UCI needs to be assigned to the specified
         * student and
         * if so, generates and assigns them. If any old values are being replaced, it also archives
         * these old
         * values.
         *
         * @param student Student
         * @param startCandidateNumber String
         * @param broker X2Broker
         * @param increase boolean
         * @return String
         */
        public static String assignCandidateNumber(Student student,
                                                   String startCandidateNumber,
                                                   X2Broker broker,
                                                   boolean increase) {
            String centreNumber = (String) student.getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER);
            String candidateNumber = (String) student.getFieldValueByAlias(ALIAS_CANDIDATE_NUMBER);
            String uci = (String) student.getFieldValueByAlias(ALIAS_UCI);

            boolean transactionCommitted = false;
            String newCandidateNumber = null;
            try {
                broker.beginTransaction();
                AppGlobals.getLog().log(Level.INFO, "--- beginTransaction");

                String yearCode = getYearCode();

                // loop until a new unused candidate number and UCI are found
                // get a new unused candidate number
                newCandidateNumber =
                        getNextCandidateNumber(student.getSchool(), startCandidateNumber, broker, increase);

                String newUci = centreNumber + BOARD_IDENTIFIER + yearCode + newCandidateNumber;
                newUci += getCheckCharacter(newUci);

                PlainDate now = new PlainDate();
                StudentVersionHistory svh = null;

                // If there is an old candidate number, archive it
                if (StringUtils.length(candidateNumber) == CANDIDATE_NUMBER_LENGTH
                        && StringUtils.isNumeric(candidateNumber)) {
                    svh = X2BaseBean.newInstance(StudentVersionHistory.class,
                            broker.getPersistenceKey());
                    svh.setStudentOid(student.getOid());
                    svh.setFieldValueByAlias(ALIAS_PREV_CANDIDATE_NUMBER, candidateNumber);
                    svh.setDate(now);
                }

                // If there is an old UCI, archive it
                if (StringUtils.length(uci) == UCI_LENGTH) {
                    if (svh == null) {
                        svh = X2BaseBean.newInstance(StudentVersionHistory.class,
                                broker.getPersistenceKey());
                    }
                    svh.setStudentOid(student.getOid());
                    svh.setFieldValueByAlias(ALIAS_PREV_UCI, uci);
                    svh.setDate(now);
                }

                // if there are old values to archive, save them
                if (svh != null) {
                    saveBean(svh, broker);
                }

                // assign the new values to the student
                student.setFieldValueByAlias(ALIAS_CANDIDATE_NUMBER, newCandidateNumber);
                student.setFieldValueByAlias(ALIAS_UCI, newUci);
                broker.saveBeanForced(student);

                broker.commitTransaction();
                transactionCommitted = true;
                AppGlobals.getLog().log(Level.INFO, "--- commitTransaction");
            } finally {
                if (!transactionCommitted) {
                    broker.rollbackTransaction(true);
                    AppGlobals.getLog().log(Level.INFO, "--- rollbackTransaction");
                }
            }

            return newCandidateNumber;
        }

        /**
         * Gets the 2 digit year code used in the UCI.
         *
         * @return String
         */
        private static String getYearCode() {
            String yearCode;

            Calendar calendar = Calendar.getInstance();
            int year = calendar.get(Calendar.YEAR);
            yearCode = StringUtils.substring(String.valueOf(year), 2, 4);
            return yearCode;
        }

        /**
         * Calculates the check character at the end of the UCI.
         *
         * @param uci String
         * @return String
         */
        private static String getCheckCharacter(String uci) {
            int sum = 0;

            for (int i = 0; i < uci.length(); i++) {
                char ch = uci.charAt(i);
                int multiplier = 16 - i;

                int val = 0;
                if (Character.isDigit(ch)) {
                    val = Character.getNumericValue(ch);
                } else {
                    val = CHAR_INT_MAP.get(Character.valueOf(ch)).intValue();
                }
                sum += val * multiplier;
            }

            int remainder = sum % 17;
            String check = INT_CHAR_MAP.get(Integer.valueOf(remainder));
            return check;
        }

        /**
         * Returns the next unique candidate number for the specified school.
         *
         * @param school School
         * @param startCandidateNumber String
         * @param broker X2Broker
         * @param increase boolean
         * @return String
         */
        private static String getNextCandidateNumber(School school,
                                                     String startCandidateNumber,
                                                     X2Broker broker,
                                                     boolean increase) {
            return getNextCandidateNumberSequence(school, startCandidateNumber, broker, increase);
        }

        /**
         * Gets the next candidate number in the sequence for the school maintained in a system
         * preference.
         *
         * @param school School
         * @param startCandidateNumber String
         * @param broker X2Broker
         * @param increase boolean
         * @return String
         */
        private static String getNextCandidateNumberSequence(School school,
                                                             String startCandidateNumber,
                                                             X2Broker broker,
                                                             boolean increase) {
            String candidateNumber =
                    StringUtils.leftPad(String.valueOf(startCandidateNumber), CANDIDATE_NUMBER_LENGTH, '0');

            // set the next value in the sequence
            int nextCandidateNumber = Integer.parseInt(candidateNumber);
            if (increase) {
                nextCandidateNumber += 1;
            }
            if (nextCandidateNumber > 9999) {
                nextCandidateNumber = 1;
            }
            return String.valueOf(nextCandidateNumber);
        }


        /**
         * Saves a bean. If validation errors occur, they are saved in an exception and that
         * exception is thrown.
         * This allows the program flow to follow the standard try/catch pattern rather than
         * checking for returned
         * errors every time the call is made.
         *
         * @param bean bean to be saved
         * @param broker X2Broker
         */
        private static void saveBean(X2BaseBean bean, X2Broker broker) {
            List<ValidationError> errors = broker.saveBean(bean);
            if (errors != null && !errors.isEmpty()) {
                ContentRuntimeException ex = new ContentRuntimeException();
                ex.setValidationErrors(errors);
                throw ex;
            }
        }
    }

    /**
     * The Interface MsgKey.
     */
    /*
     * Constants for resource bundle keys.
     */
    private interface MsgKey {
        public static final String NO_ERRORS = "message.examResults.import.noErrors";
        public static final String RESULTS = "message.examResults.import.results";
    }

    /*
     * Aliases
     */
    private static final String ALIAS_CENTRE_NUMBER = "DFE CENTRE NUMBER";
    private static final String ALIAS_CANDIDATE_NUMBER = "DFE CANDIDATE NUMBER";
    private static final String ALIAS_UCI = "DFE UCI";
    private static final String ALIAS_PREV_CANDIDATE_NUMBER = "DFE PREVIOUS CANDIDATE NUMBER";
    private static final String ALIAS_PREV_UCI = "DFE PREVIOUS UCI";

    /*
     * Constants
     */
    private static final int CANDIDATE_NUMBER_LENGTH = 4;
    private static final int UCI_LENGTH = 13;
    private static final String BOARD_IDENTIFIER = "0";

    // private static final int UCI_CENTRE_NUMBER_START = 0;
    // private static final int UCI_CENTRE_NUMBER_END = 5;

    // private static final int UCI_CANDIDATE_NUMBER_START = 8;
    // private static final int UCI_CANDIDATE_NUMBER_END = 12;

    protected static final String QUERY_BY_PARAM = "queryBy";
    protected static final String QUERY_STRING_PARAM = "queryString";

    private Collection<SisStudent> m_activeStudents;
    private SisStudent m_selectedStudent;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    public void execute() throws Exception {
        StringBuilder errors = new StringBuilder();
        String startCandidateNumber = (String) getParameter("startCandidateNumber");

        try {
            boolean increase = false;
            for (Student student : m_activeStudents) {
                String newCandidateNumber = CandidateNumberManager.assignCandidateNumber(student, startCandidateNumber,
                        getBroker(), increase);

                if (newCandidateNumber != null) {
                    startCandidateNumber = newCandidateNumber;
                    increase = true;
                }
            }
        } catch (ContentRuntimeException e) {
            for (MessageDef msg : e.getErrorMessages()) {
                appendErrorMessage(errors, msg.formatMessage(getLocale(), getBroker().getPersistenceKey()));
                AppGlobals.getLog().log(Level.SEVERE, msg.formatMessage(getLocale(), getBroker().getPersistenceKey()),
                        e);
            }
        }
        exportResults(errors);
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

        if (m_selectedStudent != null) {
            // if there is a currently selected student, create collection containing that student
            m_activeStudents = new ArrayList<SisStudent>(1);
            m_activeStudents.add(m_selectedStudent);
        } else {
            Criteria criteria = new Criteria();
            if (isSchoolContext()) {
                criteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
            }
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, SisStudent.class, X2BaseBean.COL_OID);

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            query.addOrderBy(SisStudent.COL_NAME_VIEW, true);
            m_activeStudents = getBroker().getCollectionByQuery(query);
        }
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();
    }

    /**
     * Creates the results log and exports it.
     *
     * @param errors StringBuilder
     */
    private void exportResults(StringBuilder errors) {
        StringBuilder buffer = new StringBuilder(256);

        appendMessage(buffer,
                LocalizationCache.getMessages(getUser().getPersistenceKey()).getMessage(getLocale(), MsgKey.RESULTS));
        buffer.append("------------------------------------------------\n");

        if (errors.length() == 0) {
            appendMessage(buffer, LocalizationCache.getMessages(getUser().getPersistenceKey()).getMessage(getLocale(),
                    MsgKey.NO_ERRORS));
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
                    /* ignore */}
            }
        }
    }

    /**
     * Formats an error message prepending the line number to the message and then adds that message
     * to the error
     * message buffer.
     *
     * @param errors error message buffer
     * @param msg error message
     */
    private void appendErrorMessage(StringBuilder errors, String msg) {
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
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * get currently selected student if there is one
         */
        m_selectedStudent = userData.getCurrentRecord(SisStudent.class);
    }
}
