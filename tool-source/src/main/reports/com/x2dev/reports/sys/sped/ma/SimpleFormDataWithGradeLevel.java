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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * This class is used by the State of Massachusetts for printing the SLD Eligibility form.
 *
 * @author Follett Software Company
 *
 */
public class SimpleFormDataWithGradeLevel extends BaseFormReportJavaSource {
    private static final long serialVersionUID = 1L;
    private IepData m_currentIep = null;

    private static final String REPORT_DATE_DISPLAY = "formCreationDate";
    private static final String STD_GRADE_LEVEL = "gradeLevel";

    /**
     * Prepares the data source that will be used by the Jasper design. This method is called after
     * <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        IepData iep = (IepData) getFormOwner();
        DateFormat df = new SimpleDateFormat("MM/dd/yyyy");
        if (iep != null) {
            setGradeLevel(iep);
            if (getFormInstance() != null) {
                Date formCreationDate = (Date) getParameter(REPORT_DATE_DISPLAY);
                if (formCreationDate == null) {
                    addParameter(REPORT_DATE_DISPLAY,
                            df.format(new Date(getFormInstance().getCreatedTime())));
                } else {
                    addParameter(REPORT_DATE_DISPLAY, df.format(formCreationDate));
                }
            }
        }
        
        MaSpedAttribHelper m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        return m_attribHelper.getMaSpedDataSource(getFormStorage(), iep, getDictionary(), getLocale());
    }

    /**
     * Sets the grade level.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        // get age on as of iep start date.
        PlainDate startDate = new PlainDate();
        if (iep.getStartDate() != null) {
            startDate = iep.getStartDate();
        }

        // get grade level on creation time based on iep start date, if not form creation date, on
        // most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        String gradeLevel = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
        BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

        for (StudentEnrollment e : enrollments) {
            if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                // student's YOG at this particular time
                int yog = e.getYog();

                // get the school year from basedDate
                X2Criteria schoolYearCriteria = new X2Criteria();
                schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                QueryByCriteria schoolYearQuery =
                        new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                String currentContextOid = getCurrentContext().getContextId();
                if (!StringUtils.isEmpty(currentContextOid) && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                    gradeLevel = iep.getStudent().getGradeLevel();
                } else {
                    int schoolYear = ctx.getSchoolYear();
                    List<String> grades =
                            StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                    gradeLevel = grades.get(0);
                }
                break;
            }
        }
        if (StringUtils.isEmpty(gradeLevel)) {
            gradeLevel = iep.getStudent().getGradeLevel();
        }
        addParameter(STD_GRADE_LEVEL, gradeLevel);
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        }
    }
}
