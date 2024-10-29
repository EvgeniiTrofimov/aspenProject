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
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.tools.procedures.CreditUpdateProcedure;
import com.x2dev.utils.StringUtils;

/**
 * Standard credit update procedure. Credit is awarded based on the grade scale grade definition
 * associated with a transcript's final grade value. If the grade definition is designated to
 * earn credit, credit is awarded. The amount of credit is taken from the related district course
 * bean.
 *
 * @author X2 Development Corporation
 */
public class StandardCreditUpdateProcedure extends CreditUpdateProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
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
                String grade = transcript.getFinalGrade();

                GradeScaleGradeDefinition gradeDefinition =
                        getGradeDefinition(grade, column, transcript.getSchoolCourse().getSchool(),
                                transcript.getSchoolCourseOid());

                BigDecimal credit = null;
                if (gradeDefinition != null) {
                    credit = getCredit(transcript, gradeDefinition);
                } else if (transcript.isOverride(Transcript.COL_TOTAL_CREDIT)) {
                    credit = transcript.getTotalCredit();
                }

                transcript.setTotalCredit(credit);
                getBroker().saveBeanForced(transcript);
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
     * @param school School
     * @param schoolCourseOid String
     * @return GradeScaleGradeDefinition
     */
    private GradeScaleGradeDefinition getGradeDefinition(String grade,
                                                         TranscriptColumnDefinition column,
                                                         SisSchool school,
                                                         String schoolCourseOid) {
        GradeScaleGradeDefinition gradeDefinition = null;
        GradeScale gradeScale = column.getGradeScale();
        if (gradeScale != null) {
            if (StringUtils.isNumeric(grade)) {
                gradeDefinition = getGradesManager().getGradeDefinition(
                        new BigDecimal(grade), gradeScale, school.getOid(), schoolCourseOid);
            } else {
                gradeDefinition = getGradesManager().getGradeDefinition(
                        grade, gradeScale, school.getOid(), schoolCourseOid);
            }
        }

        return gradeDefinition;
    }
}
