/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.List;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure that updates student grade levels for a snapshot.
 *
 * @author X2 Development Corporation
 */
public class UpdateStudentGradeLevels extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /**
     * OID of the snapshot containing the students to update.
     */
    public static final String RECORD_SET_NAME_PARAM = "recordSetName";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        /*
         * Get the components for calculating the grade level: the grade level map and the new
         * school year
         */
        TreeMap gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        int schoolYear = getCurrentContext().getSchoolYear();
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

        /*
         * Query for all active students in the district with an active, non-archive primary school
         */
        Criteria criteria = new Criteria();
        criteria.addIn(X2BaseBean.COL_OID, getStudentOidSubQuery());

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        int count = 0;

        /*
         * Iterate over the students, set the grade level and save; errors will be summarized with
         * the student's name
         */
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                List matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel,
                        student.getYog(), schoolYear, gradeLevels);

                String grade = (String) matchingGradeLevels.get(0);

                if (grade != null) {
                    student.setGradeLevel(grade);
                    if (student.isDirty()) {
                        getBroker().saveBeanForced(student);

                        count++;
                        logMessage(
                                "Update grade level for student " + student.getNameView() + " " + student.getLocalId());
                    }
                }
            }
        } finally {
            students.close();
        }

        logMessage("");
        logMessage("Updated " + count + " grade levels");
    }

    /**
     * Returns the Subquery for retrieving student oids from a record set.
     *
     * @return SubQuery
     */
    private SubQuery getStudentOidSubQuery() {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                getParameter(RECORD_SET_NAME_PARAM));

        SubQuery studentOidSubQuery = new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria);

        return studentOidSubQuery;
    }
}
