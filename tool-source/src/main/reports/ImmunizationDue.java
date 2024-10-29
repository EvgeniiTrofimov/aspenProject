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
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationGroup;
import com.x2dev.sis.model.beans.HealthImmunizationRuleAttributes;
import com.x2dev.sis.model.beans.HealthImmunizationRuleInstance;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.health.HealthDateRange;
import com.x2dev.sis.model.business.health.ImmunizationRuleEngine;
import com.x2dev.sis.tools.reports.ImmunizationReportJavaSource;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.List;

/**
 * Immunization due report.
 *
 * @author X2 Development Corporation
 */
public class ImmunizationDue extends ImmunizationReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "end date" report parameter. This value is a Date.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the "start date" report parameter. This value is a Date.
     */
    public static final String START_DATE_PARAM = "startDate";

    private static final String MESSAGE_DUE = "label.health.immunizationDue";
    private static final String MESSAGE_NO_DATE = "label.health.immunizationNoDate";
    private static final String MESSAGE_SKIP = "label.health.immunizationSkipped";
    private static final String MESSAGE_WAIVED = "label.health.immunizationWaived";

    private DateRange m_dateRange;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ImmunizationReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        PlainDate paramStartDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate paramEndDate = (PlainDate) getParameter(END_DATE_PARAM);
        m_dateRange = new DateRange(paramStartDate, paramEndDate);

        return super.gatherData();
    }

    /**
     * Populate immunization grid.
     *
     * @param student SisStudent
     * @param ruleAttributes HealthImmunizationRuleAttributes
     * @param ruleInstance HealthImmunizationRuleInstance
     * @param dosesList List<HealthImmunizationDose>
     * @param grid ReportDataGrid
     * @param engine ImmunizationRuleEngine
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ImmunizationReportJavaSource#populateImmunizationGrid
     */
    @Override
    protected void populateImmunizationGrid(SisStudent student,
                                            HealthImmunizationRuleAttributes ruleAttributes,
                                            HealthImmunizationRuleInstance ruleInstance,
                                            List<HealthImmunizationDose> dosesList,
                                            ReportDataGrid grid,
                                            ImmunizationRuleEngine engine) {
        // Determine if next dose within range
        boolean due = false;
        Collection<HealthDateRange> ranges = engine.evaluateNextDoses(student, dosesList);
        if (ranges != null) {
            for (DateRange range : ranges) {
                if (range.intersects(m_dateRange.getLowerBound(), m_dateRange.getUpperBound())) {
                    due = true;
                    break;
                }
            }
        }

        if (due) {
            int rows =
                    new BigDecimal(String.valueOf(dosesList.size() / 5.0)).setScale(0, RoundingMode.DOWN).intValue()
                            + 1;

            for (int row = 0; row < rows; row++) {
                // Add all doses for this series
                for (int index = row * 5; index < dosesList.size() && index < (row + 1) * 5; index++) {
                    HealthImmunizationDose dose = dosesList.get(index);

                    String doseLabel = m_dateFormat.format(dose.getDate());

                    if (StringUtils.isEmpty(doseLabel)) {
                        if (dose.getSkippedIndicator()) {
                            doseLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                    .getMessage(MESSAGE_SKIP);
                        } else if (dose.getNoDateIndicator()) {
                            doseLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                    .getMessage(MESSAGE_NO_DATE);
                        } else if (dose.getWaivedIndicator()) {
                            doseLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                    .getMessage(MESSAGE_WAIVED);
                        }
                    }

                    if (ruleAttributes instanceof HealthImmunizationGroup) {
                        doseLabel +=
                                " (" + dose.getImmunizationSeries().getImmunizationDefinition().getSeriesId() + ")";
                    }

                    if (index == row * 5) {
                        grid.append();
                        grid.set(COL_DEFINITION, ruleAttributes);
                        grid.set(COL_SERIES, ruleInstance);
                        grid.set(COL_STUDENT, student);
                    }

                    grid.set(COL_DOSE + ((index % 5) + 1), doseLabel);
                }
            }

            // If no doses exist or if a multiple of 5 doses exist, due must go on next row
            if (dosesList.size() % 5 == 0) {
                grid.append();
                grid.set(COL_DEFINITION, ruleAttributes);
                grid.set(COL_SERIES, ruleInstance);
                grid.set(COL_STUDENT, student);
            }

            String dueLabel =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(MESSAGE_DUE);
            grid.set(COL_DOSE + ((dosesList.size() % 5) + 1), dueLabel);
        }
    }
}
