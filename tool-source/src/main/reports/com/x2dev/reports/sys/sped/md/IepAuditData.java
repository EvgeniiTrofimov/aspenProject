/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.md;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.utils.X2BaseException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the Maryland IEP audit report. This class examines an IepData instance and
 * identifies business rule violations within that class.
 * <p>
 * TODO: finish this report; it is currently only a "proof of concept" and needs to be completed
 *
 * @author X2 Development Corporation
 */
public class IepAuditData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constant for the grid column that identifes the IEP section containing the audit message.
     */
    public static final String COL_SECTION = "section";

    /**
     * Constant for the grid column that identifes the IEP tab containing the audit message.
     */
    public static final String COL_TAB = "tab";

    /**
     * Constant for the grid column that contains the IEP audit message text.
     */
    public static final String COL_MESSAGE = "message";

    /*
     * Audit messages - Section 1, Attributes tab
     */
    public static final int ERROR_NO_CASE_MANAGER = 1000;
    public static final String ERROR_NO_CASE_MANAGER_MSG =
            "A case manager was not specified.";

    public static final int ERROR_INCOMPLETE_DATE_RANGE = 1010;
    public static final String ERROR_INCOMPLETE_DATE_RANGE_MSG =
            "A start and end date was not specified.";

    public static final int ERROR_INVALID_DATE_RANGE = 1020;
    public static final String ERROR_INVALID_DATE_RANGE_MSG =
            "The end date must be after the start date.";

    /*
     * Audit messages - Section 1, Initial eligibility tab
     */
    public static final int ERROR_MISSING_IE_DATE = 1100;
    public static final String ERROR_MISSING_IE_DATE_MSG =
            "The {0} was not specified.";

    public static final int ERROR_NO_IE_DISABILITIES = 1110;
    public static final String ERROR_NO_IE_DISABILITIES_MSG =
            "No disabilities were specified.";

    public static final int ERROR_NO_PRIMARY_IE_DISABILITY = 1120;
    public static final String ERROR_NO_PRIMARY_IE_DISABILITY_MSG =
            "A primary disability was not specified.";

    public static final int ERROR_MULTIPLE_PRIMARY_IE_DISABILITIES = 1130;
    public static final String ERROR_MULTIPLE_PRIMARY_IE_DISABILITIES_MSG =
            "You cannot designate more than one primary disability.";

    /*
     * Audit messages - Section 1, Continued eligibility tab
     */
    public static final int ERROR_MISSING_CE_DATE = 1200;
    public static final String ERROR_MISSING_CE_DATE_MSG =
            "The {0} was not specified.";

    public static int ERROR_NO_CE_DISABILITIES = 1210;
    public static final String ERROR_NO_CE_DISABILITIES_MSG =
            "No disabilities were specified.";

    public static int ERROR_NO_PRIMARY_CE_DISABILITY = 1220;
    public static final String ERROR_NO_PRIMARY_CE_DISABILITY_MSG =
            "A primary disability was not specified.";

    public static final int ERROR_MULTIPLE_PRIMARY_CE_DISABILITIES = 1230;
    public static final String ERROR_MULTIPLE_PRIMARY_CE_DISABILITIES_MSG =
            "You cannot designate more than one primary disability.";

    /*
     * Audit messages - Section 1, Grad/Assessment tab
     */
    public static final int ERROR_MISSING_PURSUING_CHOICE = 1300;
    public static final String ERROR_MISSING_PURSUING_CHOICE_MSG =
            "You must specify if the student is pursuing a high school diploma or certificate.";

    /*
     * Audit messages - Section 3, Considerations tab
     */
    public static final int ERROR_MISSING_COMMUNICATION = 3000;
    public static final String ERROR_MISSING_COMMUNICATION_MSG =
            "You must enter the Communication special consideration.";

    public static final int ERROR_MISSING_AT = 3010;
    public static final String ERROR_MISSING_AT_MSG =
            "You must enter the Assistive Technology (AT) special consideration.";

    /*
     * Audit messages - Section 3, Supplementary tab
     */
    public static final int ERROR_INCOMPLETE_SA_DATE_RANGE = 3200;
    public static final String ERROR_INCOMPLETE_SA_DATE_RANGE_MSG =
            "A start and end date was not specified for {0}.";

    public static final int ERROR_INVALID_SA_DATE_RANGE = 3210;
    public static final String ERROR_INVALID_SA_DATE_RANGE_MSG =
            "The end date must be after the start date for {0}.";

    public static final int ERROR_SA_DATE_RANGE_OUTSIDE_IEP = 3220;
    public static final String ERROR_SA_DATE_RANGE_OUTSIDE_IEP_MSG =
            "The date range entered for {0} must fall within the IEP's date range.";

    /*
     * Audit messages - Section 4, Goals tab
     */
    public static final int ERROR_MISSING_PARENT_NOTIFICATION_METHOD = 4000;
    public static final String ERROR_MISSING_PARENT_NOTIFICATION_METHOD_MSG =
            "A progress notification method was not specified.";

    public static final int ERROR_MISSING_PARENT_NOTIFICATION_FREQUENCY = 4010;
    public static final String ERROR_MISSING_PARENT_NOTIFICATION_FREQUENCY_MSG =
            "A progress notification frequency was not specified.";

    /*
     * Audit messages - Section 5, Services tab
     */
    public static final int ERROR_INCOMPLETE_SVC_DATE_RANGE = 5000;
    public static final String ERROR_INCOMPLETE_SVC_DATE_RANGE_MSG =
            "A start and end date was not specified for {0}.";

    public static final int ERROR_INVALID_SVC_DATE_RANGE = 5010;
    public static final String ERROR_INVALID_SVC_DATE_RANGE_MSG =
            "The end date must be after the start date for {0}.";

    public static final int ERROR_SVC_DATE_RANGE_OUTSIDE_IEP = 5020;
    public static final String ERROR_SVC_DATE_RANGE_OUTSIDE_IEP_MSG =
            "The date range entered for {0} must fall within the IEP's date range.";

    public static final int ERROR_SVC_VALID_SESSION_COUNT = 5030;
    public static final String ERROR_SVC_VALID_SESSION_COUNT_MSG =
            "The number of sessions must be greater than zero.";

    /*
     * Audit messages - Section 7, Medical Assistance tab
     */
    public static final int ERROR_MA_NUMBER_REQUIRED = 7100;
    public static final String ERROR_MA_NUMBER_REQUIRED_MSG =
            "The student was flagged as eligible for Medical Assistance, but an MA number was not provided.";

    private DataDictionary m_dataDictionary = null;
    private HashSet<Integer> m_errors = null;
    private ReportDataGrid m_grid = null;
    private IepData m_iep = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_dataDictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());
        m_errors = new HashSet<Integer>();

        int[] validations = new int[] {ERROR_NO_CASE_MANAGER,
                ERROR_INCOMPLETE_DATE_RANGE,
                ERROR_INVALID_DATE_RANGE,

                ERROR_MISSING_IE_DATE,
                ERROR_NO_IE_DISABILITIES,
                ERROR_NO_PRIMARY_IE_DISABILITY,
                ERROR_MULTIPLE_PRIMARY_IE_DISABILITIES,

                ERROR_MISSING_CE_DATE,
                ERROR_NO_CE_DISABILITIES,
                ERROR_NO_PRIMARY_CE_DISABILITY,
                ERROR_MULTIPLE_PRIMARY_CE_DISABILITIES,

                ERROR_MISSING_PURSUING_CHOICE,

                ERROR_MISSING_COMMUNICATION,
                ERROR_MISSING_AT,

                ERROR_INCOMPLETE_SA_DATE_RANGE,
                ERROR_INVALID_SA_DATE_RANGE,
                ERROR_SA_DATE_RANGE_OUTSIDE_IEP,

                ERROR_MISSING_PARENT_NOTIFICATION_METHOD,
                ERROR_MISSING_PARENT_NOTIFICATION_FREQUENCY,

                ERROR_INCOMPLETE_SVC_DATE_RANGE,
                ERROR_INVALID_SVC_DATE_RANGE,
                ERROR_SVC_DATE_RANGE_OUTSIDE_IEP,
                ERROR_SVC_VALID_SESSION_COUNT,

                ERROR_MA_NUMBER_REQUIRED};

        m_grid = new ReportDataGrid();

        runValidations(validations);

        m_grid.beforeTop();

        return m_grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_iep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Runs each validation present in the error map.
     *
     * @param validations int[]
     */
    private void runValidations(int[] validations) {
        for (int errorNumber : validations) {
            try {
                Method method = getClass().getMethod("validateRule" + errorNumber, new Class[0]);
                method.invoke(this, new Object[0]);
            } catch (NoSuchMethodException nsme) {
                AppGlobals.getLog().log(Level.SEVERE, "IEP Audit Report Error", nsme);
            } catch (IllegalAccessException iae) {
                AppGlobals.getLog().log(Level.SEVERE, "IEP Audit Report Error", iae);
            } catch (InvocationTargetException ite) {
                AppGlobals.getLog().log(Level.SEVERE, "IEP Audit Report Error", ite);
            }
        }
    }

    /**
     * Validates rule 1000: ERROR_NO_CASE_MANAGER.
     */
    public void validateRule1000() {
        if (m_iep.getStaff() == null) {
            addError(ERROR_NO_CASE_MANAGER,
                    ERROR_NO_CASE_MANAGER_MSG);
        }
    }

    /**
     * Validates rule 1010: ERROR_INCOMPLETE_DATE_RANGE.
     */
    public void validateRule1010() {
        if (m_iep.getStartDate() == null || m_iep.getEndDate() == null) {
            addError(ERROR_INCOMPLETE_DATE_RANGE,
                    ERROR_INCOMPLETE_DATE_RANGE_MSG);
        }
    }

    /**
     * Validates rule 1020: ERROR_INVALID_DATE_RANGE.
     */
    public void validateRule1020() {
        if (!m_errors.contains(Integer.valueOf(ERROR_INCOMPLETE_DATE_RANGE))
                && !m_iep.getEndDate().before(m_iep.getStartDate())) {
            addError(ERROR_INVALID_DATE_RANGE,
                    ERROR_INVALID_DATE_RANGE_MSG);
        }
    }

    /**
     * Validates rule 1100: ERROR_MISSING_IE_DATE.
     */
    public void validateRule1100() {
        String[] dateAliases = new String[] {"init-parent-eval-consent",
                "init-evaluation",
                "init-iep-development",
                "init-parent-services-consent",
                "init-iep-implementation"};

        for (String dateAlias : dateAliases) {
            if (m_iep.getFieldValueByAlias(dateAlias, m_dataDictionary) == null) {
                DataDictionaryField field = m_dataDictionary.findDataDictionaryFieldByAlias(dateAlias);
                String message = replaceToken(ERROR_MISSING_IE_DATE_MSG, field.getUserLongName(), 0);

                addError(ERROR_MISSING_IE_DATE, message);
            }
        }
    }

    /**
     * Validates rule 1110: ERROR_NO_IE_DISABILITIES.
     */
    public void validateRule1110() {
        boolean found = false;

        for (IepDisability disability : m_iep.getIepDisability(getBroker())) {
            String initial = (String) disability.getFieldValueByAlias("disability-initial", m_dataDictionary);
            if ("1".equals(initial)) {
                found = true;
                break;
            }
        }

        if (!found) {
            addError(ERROR_NO_IE_DISABILITIES,
                    ERROR_NO_IE_DISABILITIES_MSG);
        }
    }

    /**
     * Validates rule 1120: ERROR_NO_PRIMARY_IE_DISABILITY.
     */
    public void validateRule1120() {
        boolean found = false;

        for (IepDisability disability : m_iep.getIepDisability(getBroker())) {
            String initial = (String) disability.getFieldValueByAlias("disability-initial", m_dataDictionary);
            if ("1".equals(initial) && disability.getPrimaryIndicator()) {
                found = true;
                break;
            }
        }

        if (!found) {
            addError(ERROR_NO_PRIMARY_IE_DISABILITY,
                    ERROR_NO_PRIMARY_IE_DISABILITY_MSG);
        }
    }

    /**
     * Validates rule 1130: ERROR_MULTIPLE_PRIMARY_IE_DISABILITIES.
     */
    public void validateRule1130() {
        int primaryCount = 0;

        for (IepDisability disability : m_iep.getIepDisability(getBroker())) {
            String initial = (String) disability.getFieldValueByAlias("disability-initial", m_dataDictionary);
            if ("1".equals(initial) && disability.getPrimaryIndicator()) {
                primaryCount++;
            }
        }

        if (primaryCount > 1) {
            addError(ERROR_MULTIPLE_PRIMARY_IE_DISABILITIES,
                    ERROR_MULTIPLE_PRIMARY_IE_DISABILITIES_MSG);
        }
    }

    /**
     * Replace token.
     *
     * @param message String
     * @param replacement String
     * @param tokenNumber int
     * @return String
     */
    private String replaceToken(String message, String replacement, int tokenNumber) {
        return message.replace("{" + tokenNumber + "}", replacement);
    }

    /**
     * Adds the error.
     *
     * @param errorNumber int
     * @param message String
     */
    private void addError(int errorNumber, String message) {
        m_errors.add(Integer.valueOf(errorNumber));

        String tabName = null;
        String sectionName = null;

        if (errorNumber >= 7100) {
            sectionName = "Authorizations";
            tabName = "MA";
        } else if (errorNumber >= 7000) {
            sectionName = "Authorizations";
            tabName = "Authorizations";
        } else if (errorNumber >= 6300) {
            sectionName = "Placement";
            tabName = "Child Count";
        } else if (errorNumber >= 6200) {
            sectionName = "Placement";
            tabName = "SSIS";
        } else if (errorNumber >= 6100) {
            sectionName = "Placement";
            tabName = "Transportation";
        } else if (errorNumber >= 6000) {
            sectionName = "Placement";
            tabName = "LRE";
        } else if (errorNumber >= 5000) {
            sectionName = "Services";
            tabName = " - ";
        } else if (errorNumber >= 4000) {
            sectionName = "Goals";
            tabName = " - ";
        } else if (errorNumber >= 3600) {
            sectionName = "Accommodations";
            tabName = "Trans. Services";
        } else if (errorNumber >= 3500) {
            sectionName = "Accommodations";
            tabName = "Trans. Activities";
        } else if (errorNumber >= 3400) {
            sectionName = "Accommodations";
            tabName = "Transition";
        } else if (errorNumber >= 3300) {
            sectionName = "Accommodations";
            tabName = "ESY";
        } else if (errorNumber >= 3200) {
            sectionName = "Accommodations";
            tabName = "Supplementary";
        } else if (errorNumber >= 3100) {
            sectionName = "Accommodations";
            tabName = "Instruction";
        } else if (errorNumber >= 3000) {
            sectionName = "Accommodations";
            tabName = "Considerations";
        } else if (errorNumber >= 2100) {
            sectionName = "Performance";
            tabName = "Discussion";
        } else if (errorNumber >= 2000) {
            sectionName = "Performance";
            tabName = "Areas";
        } else if (errorNumber >= 1400) {
            sectionName = "Identifying Info";
            tabName = "Scores";
        } else if (errorNumber >= 1300) {
            sectionName = "Identifying Info";
            tabName = "Grad./Assessment";
        } else if (errorNumber >= 1200) {
            sectionName = "Identifying Info";
            tabName = "Cont'd Eligibility";
        } else if (errorNumber >= 1100) {
            sectionName = "Identifying Info";
            tabName = "Initial Eligibility";
        } else if (errorNumber >= 1000) {
            sectionName = "Identifying Info";
            tabName = "Attributes";
        }

        m_grid.append();
        m_grid.set(COL_SECTION, sectionName);
        m_grid.set(COL_TAB, tabName);
        m_grid.set(COL_MESSAGE, message);
    }
}
