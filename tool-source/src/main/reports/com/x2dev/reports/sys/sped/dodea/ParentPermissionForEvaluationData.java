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

package com.x2dev.reports.sys.sped.dodea;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisReferenceTable;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 * <p>
 * This form for DoDEA is the "Parent Permission for Evaluation" form. This pulls in data from the
 * accommodations that contain the assessment evaluations.
 *
 * @author X2 Development Corporation
 */
public class ParentPermissionForEvaluationData extends BaseFormReportJavaSource {
    /**
     * Name for the "assessment areas" report parameter. The value is a String.
     */
    public static final String ASSESSMENT_AREAS_PARAM = "assessmentAreas";

    /**
     * Name for the "suspected disabilities" report parameter. The value is a String.
     */
    public static final String SUSPECTED_DISABILITIES_PARAM = "suspectedDisabilities";

    /**
     * Name for the "team chairperson" report paramter. The value is an IEP Team Member object.
     */
    public static final String TEAM_CHAIRPERSON_PARAM = "teamChairperson";

    /**
     * Name for the "reference description lookup" parameter. This map in in a Map keyed to the
     * reference
     * table OID.
     */

    public static final String REFERENCE_LOOKUP_MAP_PARAM = "referenceLookupMap";

    private static final String DELIMITER = "; ";

    private ReferenceDescriptionLookup m_lookup = null;



    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_lookup = new ReferenceDescriptionLookup(getBroker(), getOrganization());

        IepData iep = (IepData) getFormOwner();

        loadReferenceLookup();
        loadAssessmentAreas(iep);
        loadDisabilities(iep);
        loadTeamChairperson(iep);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Loads a general reference lookup of codes to their "long description" (field D001). This is
     * stored in a Map keyed to the reference table OID.
     */
    private void loadReferenceLookup() {
        Map<String, Map<String, String>> referenceMap = new HashMap<String, Map<String, String>>();

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class);
        query.addOrderByAscending(ReferenceCode.COL_REFERENCE_TABLE_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisReferenceTable lastTable = null;
            Map<String, String> codeMap = new HashMap<String, String>();

            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                SisReferenceTable table = (SisReferenceTable) (code.getReferenceTable());
                String longDescription = code.getFieldD001();

                if (!StringUtils.isEmpty(longDescription)) {
                    if (!ObjectUtils.match(table, lastTable)) {
                        codeMap = new HashMap<String, String>();
                        referenceMap.put(table.getOid(), codeMap);
                    }

                    codeMap.put(code.getCode(), longDescription);

                    lastTable = table;
                }
            }
        } finally {
            iterator.close();
        }

        addParameter(REFERENCE_LOOKUP_MAP_PARAM, referenceMap);
    }

    /**
     * Adds a string displaying all assessments areas to the report parameters.
     */
    private void loadAssessmentAreas(IepData iep) {
        StringBuilder areas = new StringBuilder(500);

        if (iep != null) {
            /*
             * Load the accommodation name reference table
             */
            DataDictionaryField field = getDictionary().findDataDictionaryField(IepAccommodation.class.getName(),
                    IepAccommodation.COL_NAME);
            SisReferenceTable table = (SisReferenceTable) (field.getReferenceTable());

            /*
             * Build all evaluation areas into a single string for display
             */
            for (IepAccommodation accommodation : iep.getAccommodations(getBroker())) {
                if (areas.length() > 0) {
                    areas.append(DELIMITER);
                }

                areas.append(m_lookup.getDescription(table.getOid(), accommodation.getName()));
            }

            /*
             * Append a closing period
             */
            if (areas.length() > 0 && areas.toString().contains(DELIMITER)) {
                areas.insert(areas.lastIndexOf(DELIMITER) + 2, "and ");
                areas.append(".");
            }
        }

        /*
         * Add string as parameter
         */
        addParameter(ASSESSMENT_AREAS_PARAM, areas.toString());
    }

    /**
     * Adds a string displaying all disabilities to the report parameters.
     */
    private void loadDisabilities(IepData iep) {
        StringBuilder disabilities = new StringBuilder(200);

        if (iep != null) {
            /*
             * Load the disability code reference table
             */
            DataDictionaryField field = getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                    IepDisability.COL_DISABILITY_CODE);
            SisReferenceTable table = (SisReferenceTable) (field.getReferenceTable());

            /*
             * Build all disabilities into a single string for display
             */
            for (IepDisability disability : iep.getIepDisability(getBroker())) {
                if (disabilities.length() > 0) {
                    disabilities.append(DELIMITER);
                }
                disabilities.append(m_lookup.getDescription(table.getOid(), disability.getDisabilityCode()));
            }

            /*
             * Add a closing period
             */
            if (disabilities.length() > 0 && disabilities.toString().contains(DELIMITER)) {
                disabilities.insert(disabilities.lastIndexOf(DELIMITER) + 2, "and ");
                disabilities.append(".");
            }
        }

        /*
         * Add to report parameters
         */
        addParameter(SUSPECTED_DISABILITIES_PARAM, disabilities.toString());
    }

    /**
     * Load the team member of the IEP team that is flagged as the chairperson to the report
     * parameters.
     *
     * @param iep
     */
    private void loadTeamChairperson(IepData iep) {
        IepTeamMember chairperson = null;

        if (iep != null) {
            for (IepTeamMember teamMember : iep.getTeamMembers(getBroker())) {
                if (teamMember.getChairpersonIndicator()) {
                    chairperson = teamMember;
                    break;
                }
            }
        }

        addParameter(TEAM_CHAIRPERSON_PARAM, chairperson);
    }
}
