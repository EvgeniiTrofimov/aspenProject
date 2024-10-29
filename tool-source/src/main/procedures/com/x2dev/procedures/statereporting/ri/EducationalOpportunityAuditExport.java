/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The Class EducationalOpportunityAuditExport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class EducationalOpportunityAuditExport extends ExportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String ALIAS_COURSE_EXCLUDE = "DOE EXCLUDE CRS";
    private static final String ALIAS_COURSE_ID = "RI Course ID";
    private static final String ALIAS_SCHOOL_STATE_ID = "State School Id";

    private static final DecimalFormat CREDIT_FORMATTER = new DecimalFormat("0.0");

    private static final List<String> GRADES_INCLUDED = Arrays.asList("9", "09", "10", "11", "12");

    private static final String PARAM_YOG = "yog";

    private static final List<String> RPT_COLUMNS =
            Arrays.asList("SASID", "Local Course Title", "School Year", "Local Course ID", "Term", "Grade Level",
                    "SCED Code", "Credit Available", "Credit Earned", "Grade Earned", "School Code", "School Name");

    private static final List<String> RI_GRADUATED_WITHDRAWAL_CODES = Arrays.asList("15");

    private Map<String, ReferenceCode> m_codesGradeCode;
    private Map<String, ReferenceCode> m_codesSCEDCode;
    private DataDictionary m_dictionary;
    private Map<String, GradeScale> m_gradeScales;
    private GradesManager m_gradesManager;


    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        final List<Function<Transcript, String>> RPT_FUNCS = Arrays.asList(
                // "SASID", , , , ",
                trn -> trn.getStudent().getStateId(),
                // "Local Course Title"
                trn -> trn.getUserDescriptionIndicator() ? trn.getCourseDescription()
                        : (trn.getSchoolCourse() == null ? trn.getCourseDescription()
                                : trn.getSchoolCourse().getDescription()),
                // "School Year"
                trn -> Integer
                        .toString(trn.getDistrictContext() == null ? 0 : trn.getDistrictContext().getSchoolYear()),
                // "Local Course ID"
                trn -> trn.getSchoolCourse() == null ? "" : trn.getSchoolCourse().getNumber(),
                // "Term
                trn -> StringUtils.isEmpty(trn.getTermCode())
                        ? (trn.getMasterSchedule() == null ? ""
                                : (trn.getMasterSchedule().getScheduleTerm() == null ? ""
                                        : trn.getMasterSchedule().getScheduleTerm().getCode()))
                        : trn.getTermCode(),
                // "Grade Level"
                trn -> trn.getGradeLevel(),
                // "SCED Code", , , , ",
                trn -> getSCEDCode(trn),
                // "Credit Available"
                trn -> transformCredit(trn.getSchoolCourse() == null ? null
                        : (trn.getSchoolCourse().getCourse() == null ? null
                                : trn.getSchoolCourse().getCourse().getCredit())),
                // "Credit Earned"
                trn -> transformCredit(trn.getTotalCredit()),
                // "Grade Earned"
                trn -> getGradeEarned(trn),
                // "School Code
                trn -> (String) (trn.getSchool() == null ? ""
                        : trn.getSchool().getFieldValueByAlias(ALIAS_SCHOOL_STATE_ID)),
                // "School Name"
                trn -> trn.getSchool() == null ? "" : trn.getSchool().getName());

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(Transcript.COL_STUDENT_OID, getGraduatedStudents());
        criteria.addIn(Transcript.COL_GRADE_LEVEL, GRADES_INCLUDED);
        criteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                getAliasField(ALIAS_COURSE_EXCLUDE), BooleanAsStringConverter.TRUE);
        criteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_HIDE_TRANSCRIPT_INDICATOR, Boolean.TRUE);

        BeanQuery query = new BeanQuery(Transcript.class, criteria);
        query.addOrderByAscending(Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_STATE_ID);

        DataGrid grid = new DataGrid();
        try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Transcript trn = (Transcript) iterator.next();
                grid.append();

                Iterator<String> columns = RPT_COLUMNS.iterator();
                Iterator<Function<Transcript, String>> functions = RPT_FUNCS.iterator();
                while (columns.hasNext() && functions.hasNext()) {
                    String column = columns.next();
                    Function<Transcript, String> function = functions.next();
                    grid.set(column, function.apply(trn));
                }
            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return RPT_COLUMNS;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return RPT_COLUMNS;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_gradesManager = new GradesManager(getBroker());

        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria gradeScaleCriteria = new X2Criteria();
        gradeScaleCriteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        BeanQuery gradeScaleQuery = new BeanQuery(TranscriptColumnDefinition.class, gradeScaleCriteria);
        Collection<TranscriptColumnDefinition> tcds = getBroker().getCollectionByQuery(gradeScaleQuery);
        for (TranscriptColumnDefinition tcd : tcds) {
            m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
        }
    }

    /**
     * Gets the alias field.
     *
     * @param alias String
     * @return String
     */
    private String getAliasField(String alias) {
        String javaName = null;

        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else {
            throw new IllegalStateException("The alias " + alias + " must be defined");
        }

        return javaName;
    }

    /**
     * Gets the data dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Gets the grade code.
     *
     * @param letterGrade String
     * @return String
     */
    private String getGradeCode(String letterGrade) {
        String gradeCode = "";
        if (m_codesGradeCode == null) {
            ModelProperty prop = new ModelProperty(GradeScaleGradeDefinition.class,
                    GradeScaleGradeDefinition.COL_GRADE_CODE, getBroker().getPersistenceKey());
            DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
            if (field.hasReferenceTable()) {
                m_codesGradeCode = field.getReferenceTable().getCodeMap(getBroker());
            } else {
                m_codesGradeCode = new HashMap();
            }
        }

        if (m_codesGradeCode.containsKey(letterGrade)) {
            gradeCode = m_codesGradeCode.get(letterGrade).getStateCode();
        }

        return gradeCode;
    }

    /**
     * Gets the grade earned.
     *
     * @param transcript Transcript
     * @return String
     */
    private String getGradeEarned(Transcript transcript) {
        String letterGradeValue = null;

        if (transcript != null) {
            String finalGrade = transcript.getFinalGrade();
            letterGradeValue = finalGrade;

            GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
            String letterGrade = null;

            if (StringUtils.isNumeric(letterGradeValue)) {
                if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                    BigDecimal numericGradeBD = m_gradesManager.getNumericValue(finalGrade, scale,
                            transcript.getSchool(), transcript.getSchoolCourseOid());
                    if (numericGradeBD != null) {
                        letterGrade = finalGrade;
                    }

                    if (letterGrade == null) {
                        // Try the final grade as a number.
                        BigDecimal gradeAsNumber = null;
                        try {
                            gradeAsNumber = new BigDecimal(finalGrade);
                        } catch (NumberFormatException nfe) {
                            // nothing. The grade is not numeric.
                        }

                        if (gradeAsNumber != null) {
                            letterGrade = m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                    transcript.getSchool(), transcript.getSchoolCourseOid());
                        }
                    }

                    if (!StringUtils.isEmpty(letterGrade)) {
                        letterGradeValue = getGradeCode(letterGrade);
                    }
                }
            } else if (!StringUtils.isEmpty(letterGradeValue)) {
                if (scale != null) {
                    GradeScaleGradeDefinition gsgLetter =
                            m_gradesManager.getGradeDefinition(letterGradeValue, scale,
                                    transcript.getSchoolOid(),
                                    transcript.getSchoolCourseOid());
                    if (gsgLetter != null) {
                        letterGrade = getGradeCode(gsgLetter.getGradeCode());
                        if (!StringUtils.isEmpty(letterGrade)) {
                            letterGradeValue = letterGrade;
                        }
                    }
                }
            }
        }

        return letterGradeValue;
    }

    /**
     * Gets the graduated students.
     *
     * @return Column query
     */
    private ColumnQuery getGraduatedStudents() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_YOG, getParameter(PARAM_YOG));

        X2Criteria codeCriteria = new X2Criteria();
        codeCriteria.addOrCriteria(getWithdrawalCodesCriteria());
        codeCriteria.addOrCriteria(getStatusCodesCriteria());
        criteria.addAndCriteria(codeCriteria);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        return new ColumnQuery(StudentEnrollment.class, new String[] {StudentEnrollment.COL_STUDENT_OID}, criteria);
    }

    /**
     * Gets the graduated withdrawal codes.
     *
     * @return List
     */
    private List<String> getGraduatedWithdrawalCodes() {
        String rtbOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        Collection<ReferenceCode> codes =
                getBroker().getCollectionByQuery(new BeanQuery(ReferenceCode.class, criteria));

        return codes.stream().filter(rcd -> RI_GRADUATED_WITHDRAWAL_CODES.contains(rcd.getStateCode()))
                .map(ReferenceCode::getCode).collect(Collectors.toList());
    }

    /**
     * Gets the SCED code.
     *
     * @param trn Transcript
     * @return String
     */
    private String getSCEDCode(Transcript trn) {
        String scedCode = "";
        if (m_codesSCEDCode == null) {
            DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_COURSE_ID);
            if (field.hasReferenceTable()) {
                m_codesSCEDCode = field.getReferenceTable().getCodeMap(getBroker());
            } else {
                m_codesSCEDCode = new HashMap();
            }
        }

        if (trn.getSchoolCourse() != null && trn.getSchoolCourse().getCourse() != null) {
            String code = (String) trn.getSchoolCourse().getCourse().getFieldValueByAlias(ALIAS_COURSE_ID);
            if (m_codesSCEDCode.containsKey(code)) {
                scedCode = m_codesSCEDCode.get(code).getStateCode();
            } else {
                scedCode = code;
            }
        }

        return scedCode;
    }

    /**
     * Gets the status codes criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStatusCodesCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addLike(StudentEnrollment.COL_STATUS_CODE, "%Graduate%");
        return criteria;
    }

    /**
     * Gets the withdrawal codes criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getWithdrawalCodesCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_CODE, getGraduatedWithdrawalCodes());
        return criteria;
    }

    /**
     * Transform credit.
     *
     * @param credit BigDecimal
     * @return String
     */
    private String transformCredit(BigDecimal credit) {
        String value = "0.0";
        try {
            value = CREDIT_FORMATTER.format(credit);
        } catch (Exception e) {
            // Leave default value
            value = "0.0";
        }
        return value;
    }

}
