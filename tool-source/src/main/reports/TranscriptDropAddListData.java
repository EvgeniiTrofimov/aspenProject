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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.business.TranscriptDropAddManager;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Transcript Drop/Add List" report.
 *
 * @author X2 Development Corporation
 */
public class TranscriptDropAddListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // Input parameters
    public static final String CONTEXT_OID_PARAM = "contextOid";

    // Report parameters
    public static final String CONTEXT_PARAM = "context";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);

        DistrictSchoolYearContext context =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);

        addParameter(CONTEXT_PARAM, context);

        TranscriptDropAddManager tdaManager = new TranscriptDropAddManager(getBroker());

        Criteria criteria = tdaManager.getTranscriptDropAddCriteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, contextOid);
        criteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria dropAddQuery = new QueryByCriteria(Transcript.class, criteria);
        dropAddQuery.addOrderByAscending(Transcript.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
        dropAddQuery.addOrderByAscending(Transcript.COL_STUDENT_OID);
        dropAddQuery.addOrderByAscending(Transcript.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_NUMBER);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(dropAddQuery));
    }
}
