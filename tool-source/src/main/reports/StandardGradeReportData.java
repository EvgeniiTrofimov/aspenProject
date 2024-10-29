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

import com.x2dev.sis.tools.reports.GradeReportJavaSource;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Generic data source for all grade reports that return a Transcript Report Grid.
 * The standard reports that use this data source are:
 * <ul>
 * <li>Official School Transcript
 * <li>Student Transcript Sheet
 * <li>Report Cards
 * <li>Progress Reports
 * <li>Rubric Report Cards
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class StandardGradeReportData extends GradeReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        return runStandardGradeReport();
    }
}
