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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.StudentAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateConverter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/*
 * ====================================================================
 *
 * X2 Development Corporation, a wholly owned subsidiary of Follett
 * Software Corporation
 *
 * Copyright (c) 2002-2011 Follett Software Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Corporation.
 *
 * ====================================================================
 */

/**
 * Validates student membership/enrollment records for every student, creating any
 * missing data by extrapolating information from other sources.
 * <p>
 * Any enrollment record created with this procedure has a
 * <ul>
 * <li>Enrollment Reason of <code style="color:blue;">*created mm/dd/yyyy</code>
 * </ul>
 *
 * @author X2 Development Corporation, John Blomberg
 */
public class StudentEnrollmentValidation extends ProcedureJavaSource {
    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Input parameter for enrollment Entry Code
     */
    private static final String ENTRY_CODE_PARAMETER = "entryCode";

    /**
     * Input parameter for updating the student YOG if needed
     */
    private static final String UPDATE_YOG_PARAMETER = "updateYOG";

    /**
     * Input parameter for enrollment Withdrawal Code
     */
    private static final String WDRAW_CODE_PARAMETER = "wdrawCode";

    /**
     * Application resource properties strings
     */
    private static final String ERROR_PROCESSING = "procedure.studentEnrollmentValidation.error";
    private static final String RECORDS_CREATED = "procedure.studentEnrollmentValidation.created";
    private static final String VALIDATION_REASON = "procedure.studentEnrollmentValidation.reason";
    private static final String VALIDATION_TITLE = "procedure.studentEnrollmentValidation.title";

    private EnrollmentManager m_enrollmentManager;
    private String m_entryCode;
    private boolean m_updateStudentYOG;
    private String m_wdrawCode;
    private String m_x2Reason;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String titleString = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(VALIDATION_TITLE);
        List studentLog = new ArrayList();

        logMessage(titleString);
        logMessage(org.apache.commons.lang3.StringUtils.repeat("=", titleString.length()));

        long timeInMillis = System.currentTimeMillis();
        DateFormat dateFormat = new SimpleDateFormat("M/d/yyyy", getLocale());
        m_x2Reason = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                .getMessage(VALIDATION_REASON, dateFormat.format(Long.valueOf(timeInMillis)));
        logMessage(m_x2Reason);

        m_entryCode = (String) getParameter(ENTRY_CODE_PARAMETER);
        m_wdrawCode = (String) getParameter(WDRAW_CODE_PARAMETER);
        m_updateStudentYOG = ((Boolean) getParameter(UPDATE_YOG_PARAMETER)).booleanValue();

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        getBroker().beginTransaction();
        Exception exception = null;
        int i = 0;
        try {
            Criteria criteria = getStudentCriteria();
            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                StudentAttendanceManager studentAttendanceManager =
                        AttendanceManagerFactory.getStudentDailyAttendanceManager(getBroker());

                while (iterator.hasNext()) {
                    SisStudent student = (SisStudent) iterator.next();

                    List<StudentEnrollment> enrollmentList =
                            m_enrollmentManager.getOrderedEnrollment(student,
                                    m_updateStudentYOG,
                                    m_x2Reason,
                                    m_entryCode, m_wdrawCode);
                    Iterator enrollments = enrollmentList.iterator();
                    while (enrollments.hasNext()) {
                        StudentEnrollment enrollment = (StudentEnrollment) enrollments.next();
                        if (enrollment.isNew()) {
                            Organization org;
                            if (!StringUtils.isEmpty(enrollment.getSchoolOid())) {
                                org = OrganizationManager.getParentOrganization(enrollment.getSchool());
                            } else {
                                org = OrganizationManager.getParentOrganization(enrollment.getStudent());
                            }

                            if (!studentAttendanceManager.isCalendarDayCycleLocked(enrollment.getEnrollmentDate(),
                                    org,
                                    getPrivilegeSet(),
                                    getBroker())) {
                                getBroker().saveBeanForced(enrollment);

                                i++;
                                studentLog.add(student.getNameView() + ":  " + enrollment.getEnrollmentType() + " "
                                        + enrollment.getEnrollmentDate());
                            } else {
                                DataDictionary dict =
                                        DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                                DataDictionaryTable table =
                                        dict.findDataDictionaryTableByClass(StudentEnrollment.class.getName());

                                DateConverter converter = (DateConverter) ConverterFactory
                                        .getConverterForClass(Converter.DATE_CONVERTER, getLocale());

                                Object[] args = new Object[] {table.getUserName(),
                                        converter.javaToString(enrollment.getEnrollmentDate())};

                                LocalizationMessageResources messages =
                                        LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                                                getLocale().toString(), false);

                                logMessage(messages.getMessage("valid." + BusinessRules.STUDENT_ATTENDANCE_CYCLE_LOCKED,
                                        args));
                            }
                        }
                    }
                }
            } catch (Exception rte) {
                exception = rte;
                throw exception;
            } finally {
                iterator.close();
            }
        } catch (Exception rte) {
            exception = rte;
            throw exception;
        } finally {
            if (exception == null) {
                getBroker().commitTransaction();
                logMessage(LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(RECORDS_CREATED, Integer.valueOf(i)));

                /*
                 * Log all of the student enrollment records that were created
                 */
                Iterator<String> studentLogEntry = studentLog.iterator();
                for (; studentLogEntry.hasNext();) {
                    logMessage(studentLogEntry.next());
                }
            } else {
                getBroker().rollbackTransaction();
                logMessage(LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(ERROR_PROCESSING));
            }
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID,
                recordSetCriteria));
    }

    /**
     * Gets the student criteria.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria studentCriteria = new Criteria();

        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                studentCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return studentCriteria;
    }
}
