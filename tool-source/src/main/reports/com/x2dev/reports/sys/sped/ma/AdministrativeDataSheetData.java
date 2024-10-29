/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;

/**
 * Java source for the Administrative Data Sheet. This class provides access to the following
 * information on the format:
 * <ul>
 * <li>The form storage and owner objects as provided by <code>SimpleBeanDataSource</code>
 * <li>The student's first two contacts in ascending priority order as report parameters
 * <li>The student's latest IEP meeting as a report parameter
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class AdministrativeDataSheetData extends MaBeanReport {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter containing the first priority contact
     */
    public static final String PARAM_CONTACT_0 = "contact0";

    /**
     * Report parameter containing the second priority contact
     */
    public static final String PARAM_CONTACT_1 = "contact1";

    /**
     * Report parameter containing a the most recent IEP meeting
     */
    public static final String PARAM_MEETING = "meeting";

    /**
     * Report parameter containing a the most recent IEP meeting type name from the related preference
     */
    public static final String PARAM_MEETING_TYPE = "meetingType";

    /**
     * Report parameter containing a the first Placement of IEPData
     */
    public static final String FIRST_PLACEMENT = "firstPlacement";

    /**
     * Report parameter containing a the second Placement of IEPData
     */
    public static final String SECOND_PLACEMENT = "secondPlacement";

    /**
     * Report parameter containing a the third Placement of IEPData
     */
    public static final String THIRD_PLACEMENT = "thirdPlacement";


    /**
     * Gather data.
     * IMPORTANT NOTE: Any updates to Administrative Data Sheet must also be mirrored in
     * IepFormData.getDataSourceADM()
     *
     * @return JRDataSource
     * @throws Exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iep = null;
        int ageOfStudent = 0;
        int currentAgeOfStudent = 0;
        // Parameter to detect if IEP report is blank or not
        if (!isBlank() && reportIsNotBlank()) {
            iep = (IepData) getFormOwner();
            List<IepMeeting> meetings = (List<IepMeeting>) iep.getIepMeeting();
            String[] typeCodePreferenceKeys = IepMeeting.TypeCode.getDisplayPreferences(); 
            if (meetings != null && meetings.size() > 0) {
                IepMeeting meeting = meetings.get(meetings.size() - 1);
                int typeCode = meeting.getTypeCode();
                String meetingTypeName = null; 
                if (typeCode == IepMeeting.TypeCode.OTHER.ordinal()) {
                    meetingTypeName = meeting.getOtherType();
                } else {
                    meetingTypeName = PreferenceManager.getPreferenceValue(
                        getOrganization(), typeCodePreferenceKeys[typeCode]);
                }
                addParameter(PARAM_MEETING, meeting);
                addParameter(PARAM_MEETING_TYPE, meetingTypeName);
            }

            // get age as of iep start date.
            PlainDate startDate = new PlainDate();
            currentAgeOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(startDate);
            // get age as of iep start date.
            if (iep.getStartDate() != null) {
                ageOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(iep.getStartDate());
            } else {
                ageOfStudent = currentAgeOfStudent;
            }

            String gradeLevelAtStart = iep.getStudent().getGradeLevel();
            int yogAtStart = getYog(iep.getStudent(), iep.getStartDate());
            int schoolYearAtStart = getSchoolYear(iep.getStartDate());

            // get grade level on creation time based on iep start date, if not form creation date,
            // on most recent entry enrollment record
            TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> grades =
                    StudentManager.getMatchingGradeLevels(maxGradeLevel, yogAtStart, schoolYearAtStart, gradeLevels);
            if (grades != null && !grades.isEmpty()) {
                gradeLevelAtStart = grades.get(0);
            }

            addParameter("studentAge", String.valueOf(ageOfStudent) + "yrs:" + gradeLevelAtStart + "g");
            addParameter("studentGradeLevel",
                    String.valueOf(currentAgeOfStudent) + "yrs:" + iep.getStudent().getGradeLevel() + "g");

            if (!MaSpedAttribHelper.isStudentOwnBehalf(iep.getStudent())) {
                List<StudentContact> contacts = MaSpedAttribHelper.getStudentContacts(iep, 2, getBroker());

                if (!contacts.isEmpty()) {
                    addParameter(PARAM_CONTACT_0, contacts.get(0));
                }

                if (contacts.size() >= 2) {
                    addParameter(PARAM_CONTACT_1, contacts.get(1));
                }
            }
        } else {
            iep = new IepData(getBroker().getPersistenceKey());
            if (getFormOwner() == null) {
                // When printing from District > Tools, we do not want the district name to print,
                // but we need it on all other blank forms.
                // This overrides the organization that would normally be pulled from
                // ToolJavaSource.prepareParameters()
                addParameter("organization", null);
                return super.gatherData();
            }
        }
        initIEPPlacementParams(iep);
        MaSpedAttribHelper helper = new MaSpedAttribHelper(getBroker(), true);
        return helper.getMaSpedDataSource(iep, getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Gets the school year for a particular date. If no school year matches, return the most recent
     * school year before the date.
     *
     * @param startDate the start date
     * @return the school year
     */
    private int getSchoolYear(PlainDate startDate) {
        DistrictSchoolYearContext ctx = null;
        if (startDate != null) {
            // get matching CTX
            X2Criteria criteria = new X2Criteria();
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
            BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    ctx = (DistrictSchoolYearContext) iterator.next();
                }
            }

            // get latest CTX before date
            if (ctx == null) {
                criteria = new X2Criteria();
                criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_END_DATE);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    if (iterator.hasNext()) {
                        ctx = (DistrictSchoolYearContext) iterator.next();
                    }
                }
            }
        } else {
            ctx = getCurrentContext();
        }
        return ctx.getSchoolYear();
    }

    /**
     * Gets the yog for the student on a particular date
     *
     * @param student the student
     * @param startDate the start date
     * @return the yog
     */
    private int getYog(SisStudent student, PlainDate startDate) {
        int yog = student.getYog();
        if (startDate != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            criteria.addGreaterOrEqualThan(StudentEnrollment.COL_YOG, Integer.valueOf(0));
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    StudentEnrollment enr = (StudentEnrollment) iterator.next();
                    if (enr.getYog() > 0) {
                        yog = enr.getYog();
                    }
                }
            }
        }
        return yog;
    }

    /**
     * Initializes report placement parameters
     *
     * @param iepData
     */
    private void initIEPPlacementParams(IepData iepData) {

        List<IepPlacement> iepPlacementList = new ArrayList<IepPlacement>();

        iepPlacementList.addAll(iepData.getPlacements());

        // Sorts IEPPlacement parameters by start date
        Collections.sort(iepPlacementList, new Comparator<IepPlacement>() {

            @Override
            public int compare(IepPlacement o1, IepPlacement o2) {
                PlainDate date1 = o1.getStartDate();
                PlainDate date2 = o2.getStartDate();
                if (date1 == null) {
                    return -1;
                }
                if (date2 == null) {
                    return 1;
                }

                return date2.compareTo(date1);
            }

        });

        // Initializes placement parameters from sorted list.
        addParameter(FIRST_PLACEMENT, iepPlacementList.size() > 0 ? iepPlacementList.get(0) : null);
        addParameter(SECOND_PLACEMENT, iepPlacementList.size() > 1 ? iepPlacementList.get(1) : null);
        addParameter(THIRD_PLACEMENT, iepPlacementList.size() > 2 ? iepPlacementList.get(2) : null);
    }
}
