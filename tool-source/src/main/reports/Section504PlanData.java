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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEdPlanMeeting;
import com.x2dev.sis.model.beans.StudentEdPlanMeetingParticipant;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the X2 default "504 Plan" report. This report is capable of printing a
 * single 504 plan. It includes support for 2 subreports for evaulations and accommodations.
 * <p>
 * BeanCollectionDataSource is used for each of the two subreports. SimpleBeanDataSource is the
 * data source used for the main report.
 *
 * @author X2 Development Corporation
 */
public class Section504PlanData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * ID of the evaluation subreport.
     */
    public static final String EVALUATION_SUBREPORT_ID = "SYS-STD-021-EVAL";

    /**
     * ID of the accommodation subreport.
     */
    public static final String ACCOMMODATION_SUBREPORT_ID = "SYS-STD-021-ACC";

    /**
     * ID of the accommodation categories subreport.
     */
    public static final String ACCOMMODATION_CATEGORIES_SUBREPORT_ID = "SYS-STD-021-ACC-CAT";

    /**
     * OID of the 504 Accomodation Category reference table.
     */
    public static final String CATEGORY_REFERENCE_TABLE_OID = "rtb504AccCat";

    /**
     * Parameter containing the accommodations sorted by category.
     */
    public static final String PARAM_ACCOMMODATIONS_BY_CATEGORY = "accByCat";

    /**
     * Parameter containing the data for the accommodation categories subreport.
     */
    public static final String PARAM_ACCOMMODATION_CATEGORIES_SUBREPORT_DATA = "accCatData";

    /**
     * Parameter containing the format for the accommodation categories subreport.
     */
    public static final String PARAM_ACCOMMODATION_CATEGORIES_SUBREPORT_FORMAT = "accCatFormat";

    /**
     * Parameter containing the format for the accommodation subreport.
     */
    public static final String PARAM_ACCOMMODATION_SUBREPORT_FORMAT = "accFormat";

    /**
     * Parameter containing a comma-delimited string of disability codes for the student.
     */
    public static final String PARAM_DISABILITIES = "disabilities";

    /**
     * Parameter containing the data source for the evaluation subreport.
     */
    public static final String PARAM_EVALUATION_SUBREPORT_DATA = "evalData";

    /**
     * Parameter containing the format for the evaluation subreport.
     */
    public static final String PARAM_EVALUATION_SUBREPORT_FORMAT = "evalFormat";

    /**
     * Boolean parameter containing true if the student has accommodations in the accommodation
     * table, false otherwise.
     */
    public static final String PARAM_HAS_ACCOMMODATIONS_IN_TABLE = "hasAccommodationsInTable";

    /**
     * Parameter containing a comma-delimited string of meeting participant names.
     */
    public static final String PARAM_PARTICIPANTS = "participants";

    private StudentEdPlan m_504Plan = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());

        // Build list of disabilities to pass as parameter
        StringBuilder disabilityString = new StringBuilder(64);

        Collection<IepDisability> disabilities = m_504Plan.getDisabilities(getBroker());
        for (IepDisability disability : disabilities) {
            if (disabilityString.length() > 0) {
                disabilityString.append(", ");
            }

            disabilityString.append(disability.getDisabilityCode());
        }

        addParameter(PARAM_DISABILITIES, disabilityString.toString());

        // Build meeting participants list
        StudentEdPlanMeeting lastMeeting = m_504Plan.getLastMeeting(getBroker());
        if (lastMeeting != null) {
            StringBuilder participantString = new StringBuilder(128);

            Collection<StudentEdPlanMeetingParticipant> participants =
                    lastMeeting.getStudentEdPlanMeetingParticipants(getBroker());
            for (StudentEdPlanMeetingParticipant participant : participants) {
                if (participantString.length() > 0) {
                    participantString.append(", ");
                }

                participantString.append(participant.getNameView());
                if (!StringUtils.isEmpty(participant.getRoleCode())) {
                    participantString.append(" - ");
                    participantString.append(participant.getRoleCode());
                }
            }

            addParameter(PARAM_PARTICIPANTS, participantString.toString());
        }

        // Add support for the evaluations subreport
        Report evalSubreport = ReportUtils.getReport(EVALUATION_SUBREPORT_ID, getBroker());
        addParameter(PARAM_EVALUATION_SUBREPORT_FORMAT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));
        addParameter(PARAM_EVALUATION_SUBREPORT_DATA,
                new BeanCollectionDataSource(m_504Plan.getStudentEdPlanEvaluations(getBroker()),
                        dictionary,
                        getLocale()));

        /*
         * Add support for the accommodation categories subreport. Get a collection of all 504
         * accommodation categories, but only those in which this 504 has accommodations.
         */
        Criteria planCategoriesCriteria = new Criteria();
        planCategoriesCriteria.addEqualTo(IepAccommodation.COL_STUDENT_ED_PLAN_OID, m_504Plan.getOid());
        SubQuery planCategoriesSubQuery =
                new SubQuery(IepAccommodation.class, IepAccommodation.COL_CATEGORY, planCategoriesCriteria);

        Criteria refCodeCriteria = ReferenceManager.getCodesCriteria(CATEGORY_REFERENCE_TABLE_OID,
                getOwnableCriteria(),
                true,
                true,
                false,
                getBroker().getPersistenceKey());

        refCodeCriteria.addIn(ReferenceCode.COL_CODE, planCategoriesSubQuery);

        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        refCodeQuery.addOrderBy(ReferenceCode.COL_SEQUENCE_NUMBER, true);
        Collection<ReferenceCode> accCategories = getBroker().getCollectionByQuery(refCodeQuery);

        // get the subreport format and set it as a parameter. set the collection as a parameter in
        // the
        // form of a report data source
        Report catSubreport = ReportUtils.getReport(ACCOMMODATION_CATEGORIES_SUBREPORT_ID, getBroker());
        addParameter(PARAM_ACCOMMODATION_CATEGORIES_SUBREPORT_FORMAT,
                new ByteArrayInputStream(catSubreport.getCompiledFormat()));
        addParameter(PARAM_ACCOMMODATION_CATEGORIES_SUBREPORT_DATA,
                new BeanCollectionDataSource(accCategories,
                        dictionary,
                        getLocale()));

        // Add support for the accommodations subreports
        Map<String, Collection<IepAccommodation>> accommodationsByCat =
                new HashMap<String, Collection<IepAccommodation>>();
        Collection<IepAccommodation> accommodations = m_504Plan.getAccommodations(getBroker());

        // create a map of accommodation category => collection of accommodations
        // (this first step creates an empty collection of accommodations for each category)
        for (ReferenceCode category : accCategories) {
            accommodationsByCat.put(category.getCode(), new ArrayList<IepAccommodation>());
        }

        // add each accommodation to its corresponding category in the map
        for (IepAccommodation accommodation : accommodations) {
            Collection<IepAccommodation> categoryAccommodations = accommodationsByCat.get(accommodation.getCategory());
            if (categoryAccommodations != null) {
                categoryAccommodations.add(accommodation);
            } else {
                String msg = "Category " + accommodation.getCategory() + " was not found as a reference code in '"
                        + dictionary
                        .findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_CATEGORY)
                        .getReferenceTable().getUserName() + "' - There must be desync between '" +
                        dictionary.findDataDictionaryField(IepAccommodation.class.getName(),
                                IepAccommodation.COL_CATEGORY).getReferenceTable().getUserName()
                        + "' reference codes and '" +
                        dictionary.findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_NAME)
                                .getReferenceTable().getUserName() + "' reference code dependent code values.";

                AppGlobals.getLog().log(Level.WARNING, msg);
            }

        }

        // create a report data source from each collection of accommodations
        Map<String, BeanCollectionDataSource> accReportByCat = new HashMap<String, BeanCollectionDataSource>();
        for (ReferenceCode category : accCategories) {
            accReportByCat.put(category.getCode(),
                    new BeanCollectionDataSource(accommodationsByCat.get(category.getCode()),
                            dictionary,
                            getLocale()));
        }

        // add the accommodation subreport format as a parameter, and also add the categorized
        // accommodations
        // as a paremeter
        Report accSubreport = ReportUtils.getReport(ACCOMMODATION_SUBREPORT_ID, getBroker());
        addParameter(PARAM_ACCOMMODATION_SUBREPORT_FORMAT, new ByteArrayInputStream(accSubreport.getCompiledFormat()));
        addParameter(PARAM_ACCOMMODATIONS_BY_CATEGORY, accReportByCat);
        addParameter(PARAM_HAS_ACCOMMODATIONS_IN_TABLE, Boolean.valueOf(!accommodations.isEmpty()));

        return new SimpleBeanDataSource(m_504Plan, dictionary, getLocale());
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
        m_504Plan = userData.getCurrentRecord(StudentEdPlan.class);
    }
}
