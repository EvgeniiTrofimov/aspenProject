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
package com.x2dev.reports.bc;

import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.tools.procedures.CreditUpdateProcedure;
import com.x2dev.utils.StringUtils;
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
import java.math.BigDecimal;
import java.util.List;

/**
 * Standard credit update procedure. Credit is awarded based on the grade scale grade definition
 * associated with a transcript's final grade value. If the grade definition is designated to
 * earn credit, credit is awarded. The amount of credit is taken from the related district course
 * bean.
 *
 * @author X2 Development Corporation
 */
public class CreditCalculationProcedure extends CreditUpdateProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // BC Transcript field aliases
    private static final String ALIAS_BLENDED_MARK = "trn-blended-mark";
    private static final String ALIAS_TRAX_OVERRIDE = "trn-trax-override";

    /*
     * TRAX Override exception codes.
     *
     * If a transcript record for an examinable course has one of this values in the "TRAX Override"
     * field, the Credit Update procedure will consider the Final Mark instead of the Blended Mark.
     *
     * This is a comma-delimited string of values. More codes can be added to this list if needed.
     */
    private static final String TRAX_OVERRIDE_EXCEPTION_CODES = "Adult Not Writing (Q), Equivalent Not Writing (Q)";

    // Number of decimal places to store with a credit value
    private static final int CREDIT_VALUE_SCALE = 4;

    /**
     * Update credit.
     *
     * @param transcript Transcript
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.CreditUpdateProcedure#updateCredit(com.follett.fsc.
     *      core.k12.beans.Transcript)
     */
    @Override
    public void updateCredit(Transcript transcript) {
        TranscriptDefinition transcriptDefinition = transcript.getTranscriptDefinition();
        if (transcriptDefinition != null) {
            TranscriptColumnDefinition column = transcriptDefinition.getFinalColumnDefinition();

            if (column != null) {
                String grade = null;

                /*
                 * Check whether the transcript record is for an examinable course.
                 */
                boolean examRequired = false;

                SchoolCourse schoolCourse = transcript.getSchoolCourse();
                if (schoolCourse != null) {
                    Course course = schoolCourse.getCourse();
                    if (course != null) {
                        Course parentCourse = course.getParentCourse();
                        if (parentCourse != null) {
                            examRequired = parentCourse.getExamRequiredIndicator();
                        } else {
                            examRequired = course.getExamRequiredIndicator();
                        }
                    }
                }

                /*
                 * Check if the transcript record has a TRAX Override exception code, and determine
                 * whether to evaluate the Final Mark or the Blended Mark for credit.
                 */
                String traxOverrideCode = (String) transcript.getFieldValueByAlias(ALIAS_TRAX_OVERRIDE);
                List<String> traxOverrideExceptionCodes =
                        StringUtils.convertDelimitedStringToList(TRAX_OVERRIDE_EXCEPTION_CODES, ',', true);

                if (!examRequired ||
                        (examRequired && traxOverrideCode != null
                                && traxOverrideExceptionCodes.contains(traxOverrideCode))) {
                    grade = transcript.getFinalGrade();
                } else {
                    grade = (String) transcript.getFieldValueByAlias(ALIAS_BLENDED_MARK);
                }

                GradeScaleGradeDefinition gradeDefinition = getGradeDefinition(grade, column);

                BigDecimal credit = null;
                if (gradeDefinition != null) {
                    credit = getCredit(transcript, gradeDefinition);
                } else if (transcript.isOverride(Transcript.COL_TOTAL_CREDIT)) {
                    credit = transcript.getTotalCredit();
                }

                transcript.setTotalCredit(credit);

                if (transcript.isDirty()) {
                    getBroker().saveBeanForced(transcript);
                }
            }
        }
    }

    /**
     * Calculates the amount of credit to award to the passed transcript given the passed grade
     * definition.
     *
     * @param transcript Transcript
     * @param gradeDefinition GradeScaleGradeDefinition
     * @return BigDecimal
     */
    private BigDecimal getCredit(Transcript transcript, GradeScaleGradeDefinition gradeDefinition) {
        BigDecimal credit;

        // Determine the credit value
        if (transcript.isOverride(Transcript.COL_TOTAL_CREDIT)) {
            credit = transcript.getTotalCredit();
        } else if (gradeDefinition.getCreditIndicator() &&
                StringUtils.isNumeric(transcript.getPotentialCredit())) {
            credit = new BigDecimal(transcript.getPotentialCredit());
        } else if (gradeDefinition.getCreditIndicator() &&
                transcript.getSchoolCourse() != null) {
            credit = transcript.getSchoolCourse().getCredit();
        } else {
            credit = new BigDecimal(0);
        }

        // Include the correct number of decimals
        if (credit != null) {
            credit = credit.setScale(CREDIT_VALUE_SCALE);
        }

        return credit;
    }

    /**
     * Returns the grade definition for the passed grade and column. Null is returned if one cannot
     * be determined.
     *
     *
     * @param grade String
     * @param column TranscriptColumnDefinition
     * @return GradeScaleGradeDefinition
     */
    private GradeScaleGradeDefinition getGradeDefinition(String grade, TranscriptColumnDefinition column) {
        GradeScaleGradeDefinition gradeDefinition = null;
        GradeScale gradeScale = column.getGradeScale();
        if (gradeScale != null) {
            if (StringUtils.isNumeric(grade)) {
                gradeDefinition = getGradesManager().getGradeDefinition(
                        new BigDecimal(grade), gradeScale, null, null);
            } else {
                gradeDefinition = getGradesManager().getGradeDefinition(
                        grade, gradeScale, null, null);
            }
        }

        return gradeDefinition;
    }
}
