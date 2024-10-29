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

import com.follett.fsc.core.k12.beans.Subscription;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.utils.StringUtils;

/**
 * The grades subscription letter procedure finds gradebook score records that fell
 * short of the grade threshold and sends notifications to persons who
 * have subscribed to notifications.
 *
 * @author X2 Development Corporation
 */
public class GradesSubscriptionLetterProcedure extends GradesSubscriptionProcedure {

    /**
     * Gets the grade threshold.
     *
     * @param subscription Subscription
     * @param scale GradeScale
     * @return double
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradesSubscriptionProcedure#getGradeThreshold()
     */
    @Override
    protected double getGradeThreshold(Subscription subscription, GradeScale scale) {
        double threshold = -1;

        String param = subscription.getParameter();
        if (!StringUtils.isNumeric(param)) {
            for (GradeScaleGradeDefinition gradeDefinition : scale.getGradeScaleDefinitions()) {
                if (gradeDefinition.getGradeCode().equals(param)) {
                    // Use the cutoff value if there is one, otherwise use the grade value as the
                    // threshold
                    threshold = gradeDefinition.getGradeCutoffValue() == null
                            ? gradeDefinition.getGradeValue().doubleValue()
                            : gradeDefinition.getGradeCutoffValue().doubleValue();

                    break;
                }
            }
        }
        return threshold;
    }
}
