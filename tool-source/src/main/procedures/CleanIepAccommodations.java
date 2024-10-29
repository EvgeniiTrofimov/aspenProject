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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class CleanIepAccommodations.
 */
public class CleanIepAccommodations extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private int updatedIeps = 0;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Criteria iepCriteria = new Criteria();
        iepCriteria.addNotNull(IepData.COL_FIELD_D027);
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400038341");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000293");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000406");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000547");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000686");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000813");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000817");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000823");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000966");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400000979");
        iepCriteria.addNotEqualTo(X2BaseBean.COL_OID, "iepSM400001218");
        iepCriteria.addLike(X2BaseBean.COL_OID, "iep%");

        QueryByCriteria query = new QueryByCriteria(IepData.class, iepCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                IepData iep = (IepData) iterator.next();

                StringBuffer accom = new StringBuffer(iep.getFieldD027());
                for (int i = 3; i < accom.length(); i++) {
                    if (Character.isUpperCase(accom.charAt(i))) {
                        accom.insert(i, ", ");
                        i += 2;
                    }
                }

                iep.setFieldD027(accom.toString());
                if (iep.isDirty()) {
                    getBroker().saveBeanForced(iep);
                }
                logMessage("\n\nBefore:\n " + iep.getFieldD027() + "\n\nAfter:\n" + accom);
                updatedIeps++;
            }
        } finally {
            iterator.close();
        }

        logMessage("Updated IEP accommdations (" + updatedIeps + ")");
    }
}
