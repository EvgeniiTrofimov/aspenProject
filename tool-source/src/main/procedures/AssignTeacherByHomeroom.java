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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffScheduleAttributes;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

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

/**
 * Procedure used for elementary schools for assigning teachers to sections based on the homeroom.
 *
 * @author X2 Development Corporation
 */
public class AssignTeacherByHomeroom extends ProcedureJavaSource {
    private Schedule m_currentSchedule;

    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        /*
         * Retrieve teachers by homeroom
         */
        X2Criteria staffAttributeCriteria = new X2Criteria();
        staffAttributeCriteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_OID,
                m_currentSchedule.getStaffScheduleOid());
        staffAttributeCriteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_PARTICIPATE_INDICATOR, Boolean.TRUE);

        SubQuery staffOidSubQuery = new SubQuery(StaffScheduleAttributes.class, StaffScheduleAttributes.COL_STAFF_OID,
                staffAttributeCriteria);

        X2Criteria staffCriteria = new X2Criteria();
        staffAttributeCriteria.addIn(X2BaseBean.COL_OID, staffOidSubQuery);

        QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);
        Map<String, SisStaff> staffByHomeroom = getBroker().getMapByQuery(staffQuery, SisStaff.COL_HOMEROOM, 1000);

        /*
         * Retrieve sections by section type without platoon code
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_currentSchedule.getOid());
        sectionCriteria.addEmpty(BuildMasterSchedule.COL_PRIMARY_STAFF_OID, getBroker().getPersistenceKey());
        sectionCriteria.addEmpty(BuildMasterSchedule.COL_PLATOON_CODE, getBroker().getPersistenceKey());

        QueryByCriteria sectionQuery = new QueryByCriteria(BuildMasterSchedule.class, sectionCriteria);
        Collection<BuildMasterSchedule> sections = getBroker().getCollectionByQuery(sectionQuery);

        for (BuildMasterSchedule section : sections) {
            String sectionType = section.getSectionType();
            SisStaff staff = staffByHomeroom.get(sectionType);

            if (staff != null) {
                section.setPrimaryStaffOid(staff.getOid());
                getBroker().saveBeanForced(section);
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_currentSchedule = ScheduleUtils.getSchedule(userData);
        super.saveState(userData);
    }
}
