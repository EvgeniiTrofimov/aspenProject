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

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;

/**
 * Massachusetts state report for SIMS export. This class implements the data
 * export for Mass SIMS export.
 *
 * @author X2 Development Corporation
 */
public class MaPhysicalRestraint extends StateReportData {

	/**
	 * The Class RetrieveIep.
	 */
	protected class RetrieveIep implements FieldRetriever {
		public static final String CALC_ID_IEP = "iepRetriver";

		/**
		 * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
		 *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
		 *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
		 */
		@Override
		public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
				throws X2BaseException {

			ConductAction action = (ConductAction) entity.getBean();
			boolean isIep = action.getStudent().getSpedStatusCodeEnum() == SisStudent.SpedStatusCode.ACTIVE ? true
					: false;

			return Boolean.valueOf(isIep);
		}
	}

	/**
	 * The Class CutOffRetriever.
	 */
	protected class CutOffRetriever implements FieldRetriever {
		public static final String CALC_ID_CUT_OFF = "cutOff";

		/**
		 * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
		 *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
		 *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
		 */
		@Override
		public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
				throws X2BaseException {
			String orgId = (String) getProperty(entity.getBean(), field.getBeanPath());
			String param = (String) field.getParameter();
			if (!StringUtils.isEmpty(param) && !StringUtils.isEmpty(orgId)) {
				try {
					int length = Integer.valueOf(param).intValue();
					if (orgId.length() > length) {
						orgId = orgId.substring(0, length);
					}
				} catch (ClassCastException e) {
					addSetupError("cutOff calc param", "wrong configuration, must be integer");
					// nothing to do
				}

			}

			return orgId;
		}
	}

	private static final String ALIAS_DOE_RESTRAINT_USED = "DOE RESTRAINT USED";

	private static final String PARAM_CURRENT_STUDENTS = "Current students";

	private static final String PARAM_START_DATE = "startDate";

	private static final String PARAM_END_DATE = "endDate";

	/**
	 * Name for the enumerated "sort" parameter. The value is an Integer.
	 */
	public static final String SORT_PARAM = "sort";

	protected StudentHistoryHelper m_helper;

	private String m_fieldRestraintUsed;

	/**
	 * Gets the sorted export query.
	 *
	 * @return Query by criteria
	 */
	public QueryByCriteria getSortedExportQuery() {
		// reset the restraint used field here, since this method is used in the
		// report without
		// initializing the MaPhysicalRestraint object.
		m_fieldRestraintUsed = translateAliasToJavaName(ALIAS_DOE_RESTRAINT_USED, true);

		QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, getConductActionCriteria());
		int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
		switch (sort) {
		case 0: // Name
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
			break;

		case 1: // YOG
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
			break;

		case 2: // School
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
			break;

		case 3: // LASID
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
			break;

		case 4: // SASID
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);
			break;

		default:
			actionQuery.addOrderByAscending(ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
			break;
		}
		return actionQuery;
	}

	/**
	 * Initialize the data module. Initialize necessary working resources.
	 * Define query for students to load. Define list of field definitions for
	 * the export.
	 *
	 * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
	 *      com.follett.fsc.core.k12.business.X2Broker)
	 */
	@Override
	public void initialize() {
		m_fieldRestraintUsed = translateAliasToJavaName(ALIAS_DOE_RESTRAINT_USED, true);
		/*
		 * If no errors so far, continue with query.
		 */
		if (getSetupErrors().size() == 0) {
			/*
			 * Build query object that will be used to retrieve export students.
			 */
			Map<String, FieldRetriever> retrives = new HashMap<String, FieldRetriever>();
			retrives.put(RetrieveIep.CALC_ID_IEP, new RetrieveIep());
			retrives.put(CutOffRetriever.CALC_ID_CUT_OFF, new CutOffRetriever());
			addCalcs(retrives);
			QueryByCriteria actionQuery = getSortedExportQuery();

			setQuery(actionQuery);

			setEntityClass(StateReportEntity.class);

		}
	}

	/**
	 * Gets the student criteria.
	 *
	 * @return X 2 criteria
	 */
	private X2Criteria getStudentCriteria() {
		m_helper = new StudentHistoryHelper(this);
		m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
		X2Criteria criteria = m_helper.getStudentCriteria();
		if (isSchoolContext()) {
			criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
		} else {
			criteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
			criteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
					Boolean.FALSE);
			criteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
					Boolean.FALSE);
		}

		return criteria;
	}

	/**
	 * Gets the conduct action criteria.
	 *
	 * @return Criteria
	 */
	private Criteria getConductActionCriteria() {
		Criteria studentConductAction = new X2Criteria();

		PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
		PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);

		studentConductAction.addEqualTo(m_fieldRestraintUsed, BooleanAsStringConverter.TRUE);

		X2Criteria endOrCriteria1 = new X2Criteria();
		endOrCriteria1.addEmpty(ConductAction.COL_ACTION_END_DATE, getBroker().getPersistenceKey());
		endOrCriteria1.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, endDate);
		endOrCriteria1.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, startDate);

		X2Criteria endOrCriteria2 = new X2Criteria();
		endOrCriteria2.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, startDate);
		endOrCriteria2.addLessOrEqualThan(ConductAction.COL_ACTION_END_DATE, endDate);

		X2Criteria endOrCriteria = new X2Criteria();
		endOrCriteria.addOrCriteria(endOrCriteria1);
		endOrCriteria.addOrCriteria(endOrCriteria2);

		studentConductAction.addAndCriteria(endOrCriteria);

		List<String> students = (List<String>) getParameter(PARAM_CURRENT_STUDENTS);

		if (students != null) {
			studentConductAction.addIn(ConductAction.COL_STUDENT_OID, students);
		} else {
			studentConductAction.addIn(ConductAction.COL_STUDENT_OID,
					new SubQuery(Student.class, X2BaseBean.COL_OID, getStudentCriteria()));
			applyInputCriteria(studentConductAction, false, ConductAction.REL_STUDENT);
		}

		return studentConductAction;
	}

}
