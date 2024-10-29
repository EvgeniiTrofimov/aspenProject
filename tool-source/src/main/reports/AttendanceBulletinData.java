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

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.reports.AttendanceReport;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Attendance Bulletin" report. This report lists the student attendance
 * for the entered date current school.
 *
 * @author X2 Development Corporation
 */
public class AttendanceBulletinData extends AttendanceReport {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.AttendanceReport#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria, false);
        applySort(query);

        addParameters();

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()),
                true,
                getLocale());
    }
}
