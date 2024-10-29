/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.k12.beans.CalculatedField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CalculatedFieldProcedure;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.IepServiceLog;


/**
 * The Class MaSpedFieldCalculation is used to initialize a service provider text field in the
 * service log when the log is created.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class MaSpedServiceProviderFieldCalculation implements CalculatedFieldProcedure {
    private static final String ALIAS_ISL_SERVICE_PROVIDER = "all-isl-ServiceProvider";

    /**
     * Update all beans.
     *
     * @param field CalculatedField
     * @param broker X2Broker
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateAllBeans(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateAllBeans(CalculatedField field, X2Broker broker) {
        // TODO Auto-generated method stub
    }

    /**
     * Update bean.
     *
     * @param field CalculatedField
     * @param bean X2BaseBean
     * @param broker X2Broker
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateBean(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateBean(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        if (bean.getClass().equals(IepServiceLog.class)) {
            IepServiceLog isl = (IepServiceLog) bean;
            if (isl.getIepService() != null && isl.getIepService().getStaff() != null) {
                isl.setFieldValueByAlias(ALIAS_ISL_SERVICE_PROVIDER, isl.getIepService().getStaff().getNameView());
            }
        }
    }

    /**
     * Update referenced beans.
     *
     * @param field CalculatedField
     * @param bean X2BaseBean
     * @param broker X2Broker
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateReferencedBeans(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateReferencedBeans(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        // TODO Auto-generated method stub
    }

}
