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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StaffDegree;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class CESEW.
 */
public class CESEW extends StateReportData {

    /**
     * The Class CESEWEntity.
     */
    public static class CESEWEntity extends StateReportEntity {
        List<String> m_keyList;
        CESEW m_cesewData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CESEWEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_cesewData = (CESEW) data;
            setRowCount(m_cesewData.m_cesewCounts.size());
        }

        /**
         * Gets the current name.
         *
         * @return String
         */
        protected String getCurrentName() {
            return m_cesewData.m_cesewOrderedNames.get(getCurrentRow());
        }

        /**
         * Gets the current head count.
         *
         * @return Integer
         */
        protected Integer getCurrentHeadCount() {
            return m_cesewData.m_cesewCounts.get(getCurrentName());
        }

    }

    /**
     * The Class RetrieveCESEWData.
     */
    protected class RetrieveCESEWData implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            CESEWEntity cesewEntity = (CESEWEntity) entity;

            if ("name".equals(param)) {
                value = cesewEntity.getCurrentName();
            } else if ("count".equals(param)) {
                value = cesewEntity.getCurrentHeadCount();
            }

            return value;
        }

    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();
        loadCollections();
        loadSenRefCode();
        loadSenNoStatementCodes();
        initializeCesewCounts();

        for (SisStaff staff : m_staffCollection) {
            Collection<StaffPosition> positions = staff.getStaffPositions();
            String staffRole = null;
            PlainDate startDate = null;

            for (StaffPosition position : positions) {
                PlainDate currentStartDate = position.getStartDate();
                if (currentStartDate != null && (startDate == null || currentStartDate.after(startDate))) {
                    startDate = currentStartDate;
                    staffRole = (String) position.getFieldValueByAlias(ALIAS_DFE_ROLE_ID);

                    ReferenceCode refCode = m_roleCodesMap.get(staffRole);
                    if (refCode != null) {
                        staffRole = refCode.getStateCode();
                    }
                }
            }

            String vacancyIndicator = (String) staff.getFieldValueByAlias(ALIAS_DFE_VACANCY);
            String vacancyPost = (String) staff.getFieldValueByAlias(ALIAS_DFE_POST);

            if (!StringUtils.isEmpty(vacancyIndicator) &&
                    !StringUtils.isEmpty(vacancyPost) &&
                    BooleanAsStringConverter.TRUE.equals(vacancyIndicator)) {
                if (CODE_STATE_EXECUTIVE.equals(vacancyPost)) {
                    add(VACANCIES_EXECUTIVE);
                } else if (CODE_STATE_HEAD.equals(vacancyPost)) {
                    add(VACANCIES_HEAD);
                } else if (CODE_STATE_DEPUTY.equals(vacancyPost)) {
                    add(VACANCIES_DEPUTY);
                } else if (CODE_STATE_ASSISTANT.equals(vacancyPost)) {
                    add(VACANCIES_ASSISTANT);
                }
            } else if (!StringUtils.isEmpty(staffRole) &&
                    (CODE_STATE_EXECUTIVE.equals(staffRole) || CODE_STATE_HEAD.equals(staffRole) ||
                            CODE_STATE_DEPUTY.equals(staffRole) || CODE_STATE_ASSISTANT.equals(staffRole) ||
                            CODE_STATE_CLASSROOM.equals(staffRole) || CODE_STATE_SKILLS.equals(staffRole) ||
                            CODE_STATE_EXCELLENT.equals(staffRole) || CODE_STATE_ADVISORY.equals(staffRole))) {
                SisPerson person = staff.getPerson();
                String catholicIndicator = BooleanAsStringConverter.FALSE;
                if (person != null) {
                    String fieldValue = (String) person.getFieldValueByAlias(ALIAS_DFE_CATHOLIC);
                    if (!StringUtils.isEmpty(fieldValue)) {
                        catholicIndicator = fieldValue;
                    }
                }
                String gender = person.getGenderCode();
                ReferenceCode refCode = m_genderCodesMap.get(gender);
                if (refCode != null) {
                    gender = refCode.getStateCode();
                }

                boolean npqhCompleted = false;
                boolean npqhInProgress = false;
                boolean leadershipCompleted = false;
                boolean leadershipInProgress = false;

                for (StaffDegree degree : staff.getDegrees()) {
                    String degreeSubject = (String) degree.getFieldValueByAlias(ALIAS_DFE_QUALIFICATION_SUBJECT);
                    refCode = m_degreeSubjectCodesMap.get(degreeSubject);
                    if (refCode != null) {
                        degreeSubject = refCode.getLocalCode();
                    }

                    String degreeType = (String) degree.getFieldValueByAlias(ALIAS_DFE_QUALIFICATION_TYPE);
                    refCode = m_degreeTypeCodesMap.get(degreeType);
                    if (refCode != null) {
                        degreeType = refCode.getStateCode();
                        if (StringUtils.isEmpty(degreeType)) {
                            degreeType = refCode.getLocalCode();
                        }
                    }

                    String degreeCompleted = (String) degree.getFieldValueByAlias(ALIAS_DFE_DEGREE_COMPLETED);

                    if (CODE_STATE_NPQH.equals(degreeSubject) && CODE_STATE_QUALIFICATION.equals(degreeType) &&
                            ("Y".equals(degreeCompleted) || BooleanAsStringConverter.TRUE.equals(degreeCompleted))) {
                        npqhCompleted = true;
                    } else if (CODE_STATE_NPQH.equals(degreeSubject) && CODE_STATE_QUALIFICATION.equals(degreeType) &&
                            ("N".equals(degreeCompleted) || BooleanAsStringConverter.FALSE.equals(degreeCompleted)
                                    || degreeCompleted == null)) {
                        npqhInProgress = true;
                    } else if (CODE_STATE_LEADERSHIP.equals(degreeSubject) && CODE_STATE_MASTERS.equals(degreeType) &&
                            ("Y".equals(degreeCompleted) || BooleanAsStringConverter.TRUE.equals(degreeCompleted))) {
                        leadershipCompleted = true;
                    } else if (CODE_STATE_LEADERSHIP.equals(degreeSubject) && CODE_STATE_MASTERS.equals(degreeType) &&
                            ("N".equals(degreeCompleted) || BooleanAsStringConverter.FALSE.equals(degreeCompleted)
                                    || degreeCompleted == null)) {
                        leadershipInProgress = true;
                    }
                }

                if (leadershipCompleted) {
                    add(HOLDING_MASTERS);
                } else if (leadershipInProgress) {
                    add(STUDYING_MASTERS);
                }

                if (CODE_STATE_MALE.equals(gender)) {
                    if (BooleanAsStringConverter.TRUE.equals(catholicIndicator)) {
                        add(TEACHERS_CATHOLIC);
                        if (npqhCompleted) {
                            add(M_NPQH_CATHOLIC);
                        } else if (npqhInProgress) {
                            add(M_UNDER_NPQH_CATHOLIC);
                        }
                    } else {
                        add(TEACHERS_OTHER);
                        if (npqhCompleted) {
                            add(M_NPQH_OTHER);
                        } else if (npqhInProgress) {
                            add(M_UNDER_NPQH_OTHER);
                        }
                    }

                    if (CODE_STATE_EXECUTIVE.equals(staffRole)) {
                        add(EXECUTIVE_M);
                    } else if (CODE_STATE_HEAD.equals(staffRole)) {
                        add(HEAD_M);
                    } else if (CODE_STATE_DEPUTY.equals(staffRole)) {
                        add(DEPUTY_M);
                    } else if (CODE_STATE_ASSISTANT.equals(staffRole)) {
                        add(ASSISTANT_M);
                    }
                } else if (CODE_STATE_FEMALE.equals(gender)) {
                    if (BooleanAsStringConverter.TRUE.equals(catholicIndicator)) {
                        add(TEACHERS_CATHOLIC);
                        if (npqhCompleted) {
                            add(F_NPQH_CATHOLIC);
                        }
                        if (npqhInProgress) {
                            add(F_UNDER_NPQH_CATHOLIC);
                        }
                    } else {
                        add(TEACHERS_OTHER);
                        if (npqhCompleted) {
                            add(F_NPQH_OTHER);
                        }
                        if (npqhInProgress) {
                            add(F_UNDER_NPQH_OTHER);
                        }
                    }

                    if (CODE_STATE_EXECUTIVE.equals(staffRole)) {
                        add(EXECUTIVE_F);
                    } else if (CODE_STATE_HEAD.equals(staffRole)) {
                        add(HEAD_F);
                    } else if (CODE_STATE_DEPUTY.equals(staffRole)) {
                        add(DEPUTY_F);
                    } else if (CODE_STATE_ASSISTANT.equals(staffRole)) {
                        add(ASSISTANT_F);
                    }
                }

                String ethnicity = null;
                if (person != null) {
                    ethnicity = (String) person.getFieldValueByAlias(ALIAS_DFE_ETHNICITY);
                    refCode = m_ethnicityCodesMap.get(ethnicity);
                    if (refCode != null) {
                        ethnicity = refCode.getStateCode();
                    }
                }

                if (StringUtils.isEmpty(ethnicity) ||
                        ethnicity.equals(CODE_STATE_OTHER_REFUSED) ||
                        ethnicity.equals(CODE_STATE_NOT_OBTAINED)) {
                    add(TEACHER_NOT_KNOWN);
                    if (isLeadership(staffRole)) {
                        add(LEADERSHIP_NOT_KNOWN);
                    }
                } else // ethnicity is known
                {
                    if (ethnicity.equals(CODE_STATE_WHITE_BRITISH)) {
                        add(TEACHER_WHITE_BRITISH);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_WHITE_BRITISH);
                        }
                    } else if (ethnicity.equals(CODE_STATE_WHITE_IRISH)) {
                        add(TEACHER_WHITE_IRISH);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_WHITE_IRISH);
                        }
                    } else if (ethnicity.equals(CODE_STATE_WHITE_E_EUROPEAN)) {
                        add(TEACHER_WHITE_E_EUROPEAN);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_WHITE_E_EUROPEAN);
                        }
                    } else if (ethnicity.equals(CODE_STATE_WHITE_OTHER)) {
                        add(TEACHER_WHITE_OTHER);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_WHITE_OTHER);
                        }
                    } else if (ethnicity.equals(CODE_STATE_TRAVELER_IRISH) ||
                            ethnicity.equals(CODE_STATE_GYPSY_ROMA)) {
                        add(TEACHER_TRAVELLER);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_TRAVELLER);
                        }
                    } else if (ethnicity.equals(CODE_STATE_MIXED_WHITE_ASIAN) ||
                            ethnicity.equals(CODE_STATE_MIXED_WHITE_AFRICAN) ||
                            ethnicity.equals(CODE_STATE_MIXED_WHITE_CARIBBEAN) ||
                            ethnicity.equals(CODE_STATE_MIXED_WHITE_OTHER)) {
                        add(TEACHER_MIXED);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_MIXED);
                        }
                    } else if (ethnicity.equals(CODE_STATE_ASIAN_AB_BANGLADSKI) ||
                            ethnicity.equals(CODE_STATE_ASIAN_AB_INDIAN) ||
                            ethnicity.equals(CODE_STATE_ASIAN_AB_PAKASTANI) ||
                            ethnicity.equals(CODE_STATE_ASIAN_AB_OTHER)) {
                        add(TEACHER_ASIAN);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_ASIAN);
                        }
                    } else if (ethnicity.equals(CODE_STATE_BLACK_BB_AFRICAN) ||
                            ethnicity.equals(CODE_STATE_BLACK_BB_CARIBBEAN) ||
                            ethnicity.equals(CODE_STATE_BLACK_BB_OTHER)) {
                        add(TEACHER_BLACK);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_BLACK);
                        }
                    } else if (ethnicity.equals(CODE_STATE_CHINESE)) {
                        add(TEACHER_CHINESE);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_CHINESE);
                        }
                    } else if (ethnicity.equals(CODE_STATE_OTHER)) {
                        add(TEACHER_OTHER);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_OTHER);
                        }
                    } else {
                        add(TEACHER_NOT_KNOWN);
                        if (isLeadership(staffRole)) {
                            add(LEADERSHIP_NOT_KNOWN);
                        }
                    }
                }
                // TODO add RE teaching in schools and colleges
                // TODO add guidance for catholic and other
            }
        }

        for (SisStudent student : m_studentCollection) {
            SisPerson person = student.getPerson();
            String catholicIndicator = BooleanAsStringConverter.FALSE;
            if (person != null) {
                String fieldValue = (String) person.getFieldValueByAlias(ALIAS_DFE_CATHOLIC);
                if (!StringUtils.isEmpty(fieldValue)) {
                    catholicIndicator = fieldValue;
                }
            }
            String gender = lookupReferenceCodeByBeanPath(Person.class, Person.COL_GENDER_CODE, person.getGenderCode(),
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String gradeLevel = lookupReferenceCodeByBeanPath(Student.class, Student.COL_GRADE_LEVEL,
                    student.getGradeLevel(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            if (!StringUtils.isEmpty(gradeLevel) && !StringUtils.isEmpty(gender)) {
                if (BooleanAsStringConverter.TRUE.equals(catholicIndicator) && CODE_STATE_MALE.equals(gender)) {
                    if (CODE_STATE_Y14.equals(gradeLevel)) {
                        add(Y14_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y13.equals(gradeLevel)) {
                        add(Y13_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y12.equals(gradeLevel)) {
                        add(Y12_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y11.equals(gradeLevel)) {
                        add(Y11_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y10.equals(gradeLevel)) {
                        add(Y10_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y9.equals(gradeLevel)) {
                        add(Y9_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y8.equals(gradeLevel)) {
                        add(Y8_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y7.equals(gradeLevel)) {
                        add(Y7_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y6.equals(gradeLevel)) {
                        add(Y6_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y5.equals(gradeLevel)) {
                        add(Y5_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y4.equals(gradeLevel)) {
                        add(Y4_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y3.equals(gradeLevel)) {
                        add(Y3_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y2.equals(gradeLevel)) {
                        add(Y2_CATHOLIC_BOYS);
                    } else if (CODE_STATE_Y1.equals(gradeLevel)) {
                        add(Y1_CATHOLIC_BOYS);
                    } else if (CODE_STATE_R.equals(gradeLevel)) {
                        add(R_CATHOLIC_BOYS);
                    } else if (CODE_STATE_N2.equals(gradeLevel)) {
                        add(N2_CATHOLIC_BOYS);
                    } else if (CODE_STATE_N1.equals(gradeLevel)) {
                        add(N1_CATHOLIC_BOYS);
                    }
                } else if (BooleanAsStringConverter.TRUE.equals(catholicIndicator)
                        && CODE_STATE_FEMALE.equals(gender)) {
                    if (CODE_STATE_Y14.equals(gradeLevel)) {
                        add(Y14_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y13.equals(gradeLevel)) {
                        add(Y13_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y12.equals(gradeLevel)) {
                        add(Y12_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y11.equals(gradeLevel)) {
                        add(Y11_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y10.equals(gradeLevel)) {
                        add(Y10_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y9.equals(gradeLevel)) {
                        add(Y9_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y8.equals(gradeLevel)) {
                        add(Y8_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y7.equals(gradeLevel)) {
                        add(Y7_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y6.equals(gradeLevel)) {
                        add(Y6_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y5.equals(gradeLevel)) {
                        add(Y5_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y4.equals(gradeLevel)) {
                        add(Y4_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y3.equals(gradeLevel)) {
                        add(Y3_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y2.equals(gradeLevel)) {
                        add(Y2_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_Y1.equals(gradeLevel)) {
                        add(Y1_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_R.equals(gradeLevel)) {
                        add(R_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_N2.equals(gradeLevel)) {
                        add(N2_CATHOLIC_GIRLS);
                    } else if (CODE_STATE_N1.equals(gradeLevel)) {
                        add(N1_CATHOLIC_GIRLS);
                    }
                } else if ((catholicIndicator == null || BooleanAsStringConverter.FALSE.equals(catholicIndicator))
                        && CODE_STATE_MALE.equals(gender)) {
                    if (CODE_STATE_Y14.equals(gradeLevel)) {
                        add(Y14_OTHER_BOYS);
                    } else if (CODE_STATE_Y13.equals(gradeLevel)) {
                        add(Y13_OTHER_BOYS);
                    } else if (CODE_STATE_Y12.equals(gradeLevel)) {
                        add(Y12_OTHER_BOYS);
                    } else if (CODE_STATE_Y11.equals(gradeLevel)) {
                        add(Y11_OTHER_BOYS);
                    } else if (CODE_STATE_Y10.equals(gradeLevel)) {
                        add(Y10_OTHER_BOYS);
                    } else if (CODE_STATE_Y9.equals(gradeLevel)) {
                        add(Y9_OTHER_BOYS);
                    } else if (CODE_STATE_Y8.equals(gradeLevel)) {
                        add(Y8_OTHER_BOYS);
                    } else if (CODE_STATE_Y7.equals(gradeLevel)) {
                        add(Y7_OTHER_BOYS);
                    } else if (CODE_STATE_Y6.equals(gradeLevel)) {
                        add(Y6_OTHER_BOYS);
                    } else if (CODE_STATE_Y5.equals(gradeLevel)) {
                        add(Y5_OTHER_BOYS);
                    } else if (CODE_STATE_Y4.equals(gradeLevel)) {
                        add(Y4_OTHER_BOYS);
                    } else if (CODE_STATE_Y3.equals(gradeLevel)) {
                        add(Y3_OTHER_BOYS);
                    } else if (CODE_STATE_Y2.equals(gradeLevel)) {
                        add(Y2_OTHER_BOYS);
                    } else if (CODE_STATE_Y1.equals(gradeLevel)) {
                        add(Y1_OTHER_BOYS);
                    } else if (CODE_STATE_R.equals(gradeLevel)) {
                        add(R_OTHER_BOYS);
                    } else if (CODE_STATE_N2.equals(gradeLevel)) {
                        add(N2_OTHER_BOYS);
                    } else if (CODE_STATE_N1.equals(gradeLevel)) {
                        add(N1_OTHER_BOYS);
                    }
                } else if ((catholicIndicator == null || BooleanAsStringConverter.FALSE.equals(catholicIndicator))
                        && CODE_STATE_FEMALE.equals(gender)) {
                    if (CODE_STATE_Y14.equals(gradeLevel)) {
                        add(Y14_OTHER_GIRLS);
                    } else if (CODE_STATE_Y13.equals(gradeLevel)) {
                        add(Y13_OTHER_GIRLS);
                    } else if (CODE_STATE_Y12.equals(gradeLevel)) {
                        add(Y12_OTHER_GIRLS);
                    } else if (CODE_STATE_Y11.equals(gradeLevel)) {
                        add(Y11_OTHER_GIRLS);
                    } else if (CODE_STATE_Y10.equals(gradeLevel)) {
                        add(Y10_OTHER_GIRLS);
                    } else if (CODE_STATE_Y9.equals(gradeLevel)) {
                        add(Y9_OTHER_GIRLS);
                    } else if (CODE_STATE_Y8.equals(gradeLevel)) {
                        add(Y8_OTHER_GIRLS);
                    } else if (CODE_STATE_Y7.equals(gradeLevel)) {
                        add(Y7_OTHER_GIRLS);
                    } else if (CODE_STATE_Y6.equals(gradeLevel)) {
                        add(Y6_OTHER_GIRLS);
                    } else if (CODE_STATE_Y5.equals(gradeLevel)) {
                        add(Y5_OTHER_GIRLS);
                    } else if (CODE_STATE_Y4.equals(gradeLevel)) {
                        add(Y4_OTHER_GIRLS);
                    } else if (CODE_STATE_Y3.equals(gradeLevel)) {
                        add(Y3_OTHER_GIRLS);
                    } else if (CODE_STATE_Y2.equals(gradeLevel)) {
                        add(Y2_OTHER_GIRLS);
                    } else if (CODE_STATE_Y1.equals(gradeLevel)) {
                        add(Y1_OTHER_GIRLS);
                    } else if (CODE_STATE_R.equals(gradeLevel)) {
                        add(R_OTHER_GIRLS);
                    } else if (CODE_STATE_N2.equals(gradeLevel)) {
                        add(N2_OTHER_GIRLS);
                    } else if (CODE_STATE_N1.equals(gradeLevel)) {
                        add(N1_OTHER_GIRLS);
                    }
                }
            }

            String careAuthority = (String) student.getFieldValueByAlias(ALIAS_DFE_CARE_AUTHORITY);
            if ("Y".equals(careAuthority) || BooleanAsStringConverter.TRUE.equals(careAuthority)) {
                add(IN_CARE);
            }

            String ethnicity = null;
            if (person != null) {
                ethnicity = (String) person.getFieldValueByAlias(ALIAS_DFE_ETHNICITY);
                ReferenceCode refCode = m_ethnicityCodesMap.get(ethnicity);
                if (refCode != null) {
                    ethnicity = refCode.getStateCode();
                }
            }

            if (StringUtils.isEmpty(ethnicity) ||
                    ethnicity.equals(CODE_STATE_OTHER_REFUSED) ||
                    ethnicity.equals(CODE_STATE_NOT_OBTAINED)) {
                add(PUPIL_NOT_KNOWN);
            } else // ethnicity is known
            {
                if (ethnicity.equals(CODE_STATE_WHITE_BRITISH)) {
                    add(PUPIL_WHITE_BRITISH);
                } else if (ethnicity.equals(CODE_STATE_WHITE_IRISH)) {
                    add(PUPIL_WHITE_IRISH);
                } else if (ethnicity.equals(CODE_STATE_WHITE_E_EUROPEAN)) {
                    add(PUPIL_WHITE_E_EUROPEAN);
                } else if (ethnicity.equals(CODE_STATE_WHITE_OTHER)) {
                    add(PUPIL_WHITE_OTHER);
                } else if (ethnicity.equals(CODE_STATE_TRAVELER_IRISH) ||
                        ethnicity.equals(CODE_STATE_GYPSY_ROMA)) {
                    add(PUPIL_TRAVELLER);
                } else if (ethnicity.equals(CODE_STATE_MIXED_WHITE_ASIAN) ||
                        ethnicity.equals(CODE_STATE_MIXED_WHITE_AFRICAN) ||
                        ethnicity.equals(CODE_STATE_MIXED_WHITE_CARIBBEAN) ||
                        ethnicity.equals(CODE_STATE_MIXED_WHITE_OTHER)) {
                    add(PUPIL_MIXED);
                } else if (ethnicity.equals(CODE_STATE_ASIAN_AB_BANGLADSKI) ||
                        ethnicity.equals(CODE_STATE_ASIAN_AB_INDIAN) ||
                        ethnicity.equals(CODE_STATE_ASIAN_AB_PAKASTANI) ||
                        ethnicity.equals(CODE_STATE_ASIAN_AB_OTHER)) {
                    add(PUPIL_ASIAN);
                } else if (ethnicity.equals(CODE_STATE_BLACK_BB_AFRICAN) ||
                        ethnicity.equals(CODE_STATE_BLACK_BB_CARIBBEAN) ||
                        ethnicity.equals(CODE_STATE_BLACK_BB_OTHER)) {
                    add(PUPIL_BLACK);
                } else if (ethnicity.equals(CODE_STATE_CHINESE)) {
                    add(PUPIL_CHINESE);
                } else if (ethnicity.equals(CODE_STATE_OTHER)) {
                    add(PUPIL_OTHER);
                }
            }
        }

        int leadershipSize = getCount(EXECUTIVE_M) +
                getCount(EXECUTIVE_F) +
                getCount(HEAD_M) +
                getCount(HEAD_F) +
                getCount(DEPUTY_M) +
                getCount(DEPUTY_F) +
                getCount(ASSISTANT_M) +
                getCount(ASSISTANT_F);
        m_cesewCounts.put(LEADERSHIP_SIZE, Integer.valueOf(leadershipSize));

        /*
         * int totalY11 = getCount(Y11_CATHOLIC_BOYS) +
         * getCount(Y11_CATHOLIC_GIRLS) +
         * getCount(Y11_OTHER_BOYS) +
         * getCount(Y11_OTHER_GIRLS);
         * m_cesewCounts.put(Y11_TOTAL, Integer.valueOf(totalY11));
         */ // TODO complete y11 students for last year

        m_cesewCounts.put(FREE_SCHOOL_MEALS, getStudentCount(m_freeLunchRefCode, null, null));
        m_cesewCounts.put(EMA, getStudentCount(m_edMaintenanceAllowance, null, null));
        m_cesewCounts.put(SEN_WITH_STATEMENT,
                getStudentCount(m_senRefCode, m_senProvisionBeanPath, Arrays.asList(CODE_SEN_STATEMENT)));
        m_cesewCounts.put(SEN_NO_STATEMENT,
                getStudentCount(m_senRefCode, m_senProvisionBeanPath, m_senNoStatementCodes));

        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
        QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

        setQuery(query);

        setEntityClass(CESEWEntity.class);

        HashMap calcMap = new HashMap<String, FieldRetriever>();
        calcMap.put("CESEW-DATA", new RetrieveCESEWData());
        super.addCalcs(calcMap);
    }

    Collection<SisStaff> m_staffCollection;
    Collection<SisStudent> m_studentCollection;
    Map<String, Integer> m_cesewCounts = new HashMap<String, Integer>();
    List<String> m_cesewOrderedNames = new ArrayList<String>();
    StudentHistoryHelper m_helper;
    PlainDate m_yearStartDate;
    PlainDate m_yearEndDate;
    Map<String, ReferenceCode> m_ethnicityCodesMap = new HashMap<String, ReferenceCode>();
    Map<String, ReferenceCode> m_roleCodesMap = new HashMap<String, ReferenceCode>();
    Map<String, ReferenceCode> m_genderCodesMap = new HashMap<String, ReferenceCode>();
    Map<String, ReferenceCode> m_gradeLevelCodesMap = new HashMap<String, ReferenceCode>();
    Map<String, ReferenceCode> m_senProvisionCodesMap = new HashMap<String, ReferenceCode>();
    Map<String, ReferenceCode> m_degreeSubjectCodesMap = new HashMap<String, ReferenceCode>();
    Map<String, ReferenceCode> m_degreeTypeCodesMap = new HashMap<String, ReferenceCode>();
    String m_freeLunchRefCode;
    String m_edMaintenanceAllowance;
    String m_senRefCode;
    String m_senProvisionBeanPath = null;
    String m_staffExcludeBeanPath = null;
    List<String> m_senNoStatementCodes = new ArrayList<String>();

    // Field Aliases
    String ALIAS_DFE_CARE_AUTHORITY = "DFE CARE AUTHORITY";
    String ALIAS_DFE_DEGREE_COMPLETED = "DFE DEGREE COMPLETED";
    String ALIAS_DFE_CATHOLIC = "DFE CATHOLIC INDICATOR";
    String ALIAS_DFE_ETHNICITY = "DFE ETHNICITY";
    String ALIAS_DFE_ROLE_ID = "DFE POST";
    String ALIAS_DFE_QUALIFICATION_SUBJECT = "DFE QUALIFICATION SUBJECT";
    String ALIAS_DFE_QUALIFICATION_TYPE = "DFE QUALIFICATION CODE";
    String ALIAS_DFE_SEN_PROVISION = "DFE SEN PROVISION";
    String ALIAS_DFE_VACANCY = "DFE VACANCY INDICATOR";
    String ALIAS_DFE_POST = "DFE VACANCY POST";
    String ALIAS_DFE_STAFF_EXCLUDE = "DFE STAFF EXCLUDE";
    String BEAN_PATH_GENDER_CODE = "genderCode";
    String BEAN_PATH_GRADE_LEVEL = "gradeLevel";

    // State Codes for Staff Degree
    String CODE_STATE_NPQH = "NPQH";
    String CODE_STATE_LEADERSHIP = "LDCL";
    String CODE_STATE_MASTERS = "MAST";
    String CODE_STATE_QUALIFICATION = "QUAL";

    // State Ethnicity Codes
    String CODE_STATE_WHITE_BRITISH = "WBRI";
    String CODE_STATE_WHITE_IRISH = "WIRI";
    String CODE_STATE_WHITE_E_EUROPEAN = "WIEE"; // TODO currently not used
    String CODE_STATE_WHITE_OTHER = "WOTH";
    String CODE_STATE_TRAVELER_IRISH = "WIRT";
    String CODE_STATE_GYPSY_ROMA = "WROM";
    String CODE_STATE_MIXED_WHITE_ASIAN = "MWAS";
    String CODE_STATE_MIXED_WHITE_AFRICAN = "MWBA";
    String CODE_STATE_MIXED_WHITE_CARIBBEAN = "MWBC";
    String CODE_STATE_MIXED_WHITE_OTHER = "MOTH";
    String CODE_STATE_ASIAN_AB_BANGLADSKI = "ABAN";
    String CODE_STATE_ASIAN_AB_INDIAN = "AIND";
    String CODE_STATE_ASIAN_AB_PAKASTANI = "APKN";
    String CODE_STATE_ASIAN_AB_OTHER = "AOTH";
    String CODE_STATE_BLACK_BB_AFRICAN = "BAFR";
    String CODE_STATE_BLACK_BB_CARIBBEAN = "BCRB";
    String CODE_STATE_BLACK_BB_OTHER = "BOTH";
    String CODE_STATE_CHINESE = "CHNE";
    String CODE_STATE_OTHER = "OOTH";
    String CODE_STATE_OTHER_REFUSED = "REFU";
    String CODE_STATE_NOT_OBTAINED = "NOBT";

    // State Staff Role Codes
    String CODE_STATE_EXECUTIVE = "EXH";
    String CODE_STATE_HEAD = "HDT";
    String CODE_STATE_DEPUTY = "DHT";
    String CODE_STATE_ASSISTANT = "AHT";
    String CODE_STATE_CLASSROOM = "TCH";
    String CODE_STATE_SKILLS = "AST";
    String CODE_STATE_EXCELLENT = "EXL";
    String CODE_STATE_SUPPORT = "SUP";
    String CODE_STATE_ADVISORY = "AVT";
    String CODE_STATE_TA = "TAS";

    // State Gender Codes
    String CODE_STATE_MALE = "1";
    String CODE_STATE_FEMALE = "2";

    // State Grade Codes
    String CODE_STATE_Y14 = "14";
    String CODE_STATE_Y13 = "13";
    String CODE_STATE_Y12 = "12";
    String CODE_STATE_Y11 = "11";
    String CODE_STATE_Y10 = "10";
    String CODE_STATE_Y9 = "9";
    String CODE_STATE_Y8 = "8";
    String CODE_STATE_Y7 = "7";
    String CODE_STATE_Y6 = "6";
    String CODE_STATE_Y5 = "5";
    String CODE_STATE_Y4 = "4";
    String CODE_STATE_Y3 = "3";
    String CODE_STATE_Y2 = "2";
    String CODE_STATE_Y1 = "1";
    String CODE_STATE_R = "R";
    String CODE_STATE_N2 = "N2";
    String CODE_STATE_N1 = "N1";

    // State SEN Codes
    String CODE_STATE_NO_STATEMENT = "N";

    // Local Student Program Participation Codes
    String CODE_FREE_REDUCED_LUNCH = "FRL";
    String CODE_SPECIAL_EDUCATION = "SE";
    String CODE_SEN_NOT_NEEDED = "N";
    String CODE_SEN_STATEMENT = "S";
    String CODE_ED_MAINTENANCE_ALLOWANCE = "EMA";

    // Codes printed on report
    // Teacher Codes
    String M_NPQH_CATHOLIC = "Male staff with NPQH qualification - Catholic";
    String M_NPQH_OTHER = "Male staff with NPQH qualification - Other";
    String F_NPQH_CATHOLIC = "Female staff with NPQH qualification - Catholic";
    String F_NPQH_OTHER = "Female staff with NPQH qualification - Other";
    String M_UNDER_NPQH_CATHOLIC = "Male staff undertaking NPQH qualification - Catholic";
    String M_UNDER_NPQH_OTHER = "Male staff undertaking NPQH qualification - Other";
    String F_UNDER_NPQH_CATHOLIC = "Female staff undertaking NPQH qualification - Catholic";
    String F_UNDER_NPQH_OTHER = "Female staff undertaking NPQH qualification - Other";
    String HOLDING_MASTERS = "Holding a Master's degree in the leadership of Church";
    String STUDYING_MASTERS = "Studying for Master's degree in leadership of Church";
    String LEADERSHIP_SIZE = "Size of leadership group";
    String VACANCIES_EXECUTIVE = "Vacancies Executive Head Teacher";
    String VACANCIES_HEAD = "Vacancies Head Teacher";
    String VACANCIES_DEPUTY = "Vacancies Deputy Head Teacher";
    String VACANCIES_ASSISTANT = "Vacancies Assistant Head Teacher";
    String EXECUTIVE_M = "Executive heads male";
    String EXECUTIVE_F = "Executive heads female";
    String HEAD_M = "Head teachers male";
    String HEAD_F = "Head teachers female";
    String DEPUTY_M = "Deputy head teachers male";
    String DEPUTY_F = "Deputy head teachers female";
    String ASSISTANT_M = "Assistant head teachers male";
    String ASSISTANT_F = "Assistant head teachers female";
    String TEACHERS_CATHOLIC = "Catholic teachers";
    String TEACHERS_OTHER = "Other teachers";
    String CCRS = "Staff having CCRS";

    // Teacher Ethnicity Codes
    String TEACHER_WHITE_BRITISH = "Teacher - White British";
    String TEACHER_WHITE_IRISH = "Teacher - White Irish";
    String TEACHER_WHITE_E_EUROPEAN = "Teacher - White Eastern European";
    String TEACHER_WHITE_OTHER = "Teacher - White Other";
    String TEACHER_TRAVELLER = "Teacher - Traveller of Irish/Gypsy/Roma heritage";
    String TEACHER_MIXED = "Teacher - Mixed/Dual Background";
    String TEACHER_ASIAN = "Teacher - Asian/Asian British";
    String TEACHER_BLACK = "Teacher - Black/Black British";
    String TEACHER_CHINESE = "Teacher - Chinese";
    String TEACHER_OTHER = "Teacher - Other ethnic group";
    String TEACHER_NOT_KNOWN = "Teacher - Not known";

    // Leadership Ethnicity Codes
    String LEADERSHIP_WHITE_BRITISH = "Leadership - White British";
    String LEADERSHIP_WHITE_IRISH = "Leadership - White Irish";
    String LEADERSHIP_WHITE_E_EUROPEAN = "Leadership - White Eastern European";
    String LEADERSHIP_WHITE_OTHER = "Leadership - White Other";
    String LEADERSHIP_TRAVELLER = "Leadership - Traveller of Irish/Gypsy/Roma heritage";
    String LEADERSHIP_MIXED = "Leadership - Mixed/Dual Background";
    String LEADERSHIP_ASIAN = "Leadership - Asian/Asian British";
    String LEADERSHIP_BLACK = "Leadership - Black/Black British";
    String LEADERSHIP_CHINESE = "Leadership - Chinese";
    String LEADERSHIP_OTHER = "Leadership - Other ethnic group";
    String LEADERSHIP_NOT_KNOWN = "Leadership - Not known";
    String SPECIALIST_RE = "Specialist qualified RE teachers";
    String STAFF_RE = "Staff who teach RE";
    String HALF_RE = "Staff teaching RE at least half of teaching time";
    String GUIDANCE_CATHOLIC = "Guidance Catholic";
    String GUIDANCE_OTHER = "Guidance Other";

    // Pupil Year Codes
    String Y14_CATHOLIC_BOYS = "Y14 Catholic Boys";
    String Y14_CATHOLIC_GIRLS = "Y14 Catholic Girls";
    String Y14_OTHER_BOYS = "Y14 Other Boys";
    String Y14_OTHER_GIRLS = "Y14 Other Girls";
    String Y13_CATHOLIC_BOYS = "Y13 Catholic Boys";
    String Y13_CATHOLIC_GIRLS = "Y13 Catholic Girls";
    String Y13_OTHER_BOYS = "Y13 Other Boys";
    String Y13_OTHER_GIRLS = "Y13 Other Girls";
    String Y12_CATHOLIC_BOYS = "Y12 Catholic Boys";
    String Y12_CATHOLIC_GIRLS = "Y12 Catholic Girls";
    String Y12_OTHER_BOYS = "Y12 Other Boys";
    String Y12_OTHER_GIRLS = "Y12 Other Girls";
    String Y11_CATHOLIC_BOYS = "Y11 Catholic Boys";
    String Y11_CATHOLIC_GIRLS = "Y11 Catholic Girls";
    String Y11_OTHER_BOYS = "Y11 Other Boys";
    String Y11_OTHER_GIRLS = "Y11 Other Girls";
    String Y10_CATHOLIC_BOYS = "Y10 Catholic Boys";
    String Y10_CATHOLIC_GIRLS = "Y10 Catholic Girls";
    String Y10_OTHER_BOYS = "Y10 Other Boys";
    String Y10_OTHER_GIRLS = "Y10 Other Girls";
    String Y9_CATHOLIC_BOYS = "Y9 Catholic Boys";
    String Y9_CATHOLIC_GIRLS = "Y9 Catholic Girls";
    String Y9_OTHER_BOYS = "Y9 Other Boys";
    String Y9_OTHER_GIRLS = "Y9 Other Girls";
    String Y8_CATHOLIC_BOYS = "Y8 Catholic Boys";
    String Y8_CATHOLIC_GIRLS = "Y8 Catholic Girls";
    String Y8_OTHER_BOYS = "Y8 Other Boys";
    String Y8_OTHER_GIRLS = "Y8 Other Girls";
    String Y7_CATHOLIC_BOYS = "Y7 Catholic Boys";
    String Y7_CATHOLIC_GIRLS = "Y7 Catholic Girls";
    String Y7_OTHER_BOYS = "Y7 Other Boys";
    String Y7_OTHER_GIRLS = "Y7 Other Girls";
    String Y6_CATHOLIC_BOYS = "Y6 Catholic Boys";
    String Y6_CATHOLIC_GIRLS = "Y6 Catholic Girls";
    String Y6_OTHER_BOYS = "Y6 Other Boys";
    String Y6_OTHER_GIRLS = "Y6 Other Girls";
    String Y5_CATHOLIC_BOYS = "Y5 Catholic Boys";
    String Y5_CATHOLIC_GIRLS = "Y5 Catholic Girls";
    String Y5_OTHER_BOYS = "Y5 Other Boys";
    String Y5_OTHER_GIRLS = "Y5 Other Girls";
    String Y4_CATHOLIC_BOYS = "Y4 Catholic Boys";
    String Y4_CATHOLIC_GIRLS = "Y4 Catholic Girls";
    String Y4_OTHER_BOYS = "Y4 Other Boys";
    String Y4_OTHER_GIRLS = "Y4 Other Girls";
    String Y3_CATHOLIC_BOYS = "Y3 Catholic Boys";
    String Y3_CATHOLIC_GIRLS = "Y3 Catholic Girls";
    String Y3_OTHER_BOYS = "Y3 Other Boys";
    String Y3_OTHER_GIRLS = "Y3 Other Girls";
    String Y2_CATHOLIC_BOYS = "Y2 Catholic Boys";
    String Y2_CATHOLIC_GIRLS = "Y2 Catholic Girls";
    String Y2_OTHER_BOYS = "Y2 Other Boys";
    String Y2_OTHER_GIRLS = "Y2 Other Girls";
    String Y1_CATHOLIC_BOYS = "Y1 Catholic Boys";
    String Y1_CATHOLIC_GIRLS = "Y1 Catholic Girls";
    String Y1_OTHER_BOYS = "Y1 Other Boys";
    String Y1_OTHER_GIRLS = "Y1 Other Girls";
    String R_CATHOLIC_BOYS = "R Catholic Boys";
    String R_CATHOLIC_GIRLS = "R Catholic Girls";
    String R_OTHER_BOYS = "R Other Boys";
    String R_OTHER_GIRLS = "R Other Girls";
    String N2_CATHOLIC_BOYS = "N2 Catholic Boys";
    String N2_CATHOLIC_GIRLS = "N2 Catholic Girls";
    String N2_OTHER_BOYS = "N2 Other Boys";
    String N2_OTHER_GIRLS = "N2 Other Girls";
    String N1_CATHOLIC_BOYS = "N1 Catholic Boys";
    String N1_CATHOLIC_GIRLS = "N1 Catholic Girls";
    String N1_OTHER_BOYS = "N1 Other Boys";
    String N1_OTHER_GIRLS = "N1 Other Girls";

    // Other Pupil Ethnicity Codes
    String WORSHIP_WITHDRAWN = "Pupils withdrawn from collective worship";
    String WORSHIP_WITHDRAWN_Y12_Y13 = "Pupils withdrawn from collective worship - Y12/Y13";
    String IN_CARE = "Pupils in care";
    String FREE_SCHOOL_MEALS = "Free school meals";
    String EMA = "Education maintenance allowance";
    String SEN_NO_STATEMENT = "Pupils with SEN without statement";
    String SEN_WITH_STATEMENT = "Pupils with SEN with statement";
    String Y11_TOTAL = "Total Y11 students";
    String DESTINATION_RETURNED = "Returned to school";
    String DESTINATION_OTHER_CATHOLIC = "Enrolled in another Catholic school/college";
    String DESTINATION_OTHER = "Other/Not known";
    String SRE_WITHDRAWAL_BOYS = "Boys withdrawn from SRE";
    String SRE_WITHDRAWAL_GIRLS = "Girls withdrawn from SRE";

    // Pupil Ethinicity codes
    String PUPIL_WHITE_BRITISH = "Pupil - White British";
    String PUPIL_WHITE_IRISH = "Pupil - White Irish";
    String PUPIL_WHITE_E_EUROPEAN = "Pupil - White Eastern European";
    String PUPIL_WHITE_OTHER = "Pupil - White Other";
    String PUPIL_TRAVELLER = "Pupil - Traveller of Irish/Gypsy/Roma heritage";
    String PUPIL_MIXED = "Pupil - Mixed/Dual Background";
    String PUPIL_ASIAN = "Pupil - Asian/Asian British";
    String PUPIL_BLACK = "Pupil - Black/Black British";
    String PUPIL_CHINESE = "Pupil - Chinese";
    String PUPIL_OTHER = "Pupil - Other ethnic group";
    String PUPIL_NOT_KNOWN = "Pupil - Not known";

    /**
     * Increments the pupil count indicated by the "key" of map m_pupilCounts by 1.
     *
     * @param key String
     */
    private void add(String key) {
        Integer value = m_cesewCounts.get(key);
        value = Integer.valueOf(value.intValue() + 1);
        m_cesewCounts.put(key, value);
    }

    /**
     * Returns the count of pupils from the map m_pupilCounts indicated by the "key."
     *
     * @param key String
     * @return int
     */
    private int getCount(String key) {
        Integer value = m_cesewCounts.get(key);
        int count = 0;
        if (value != null) {
            count = value.intValue();
        }
        return count;
    }

    /**
     * Gets the code.
     *
     * @param localCode String
     * @return String
     */
    private String getCode(String localCode) {
        String code = null;

        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, localCode);
        BeanQuery reportQuery = new BeanQuery(ReferenceCode.class, criteria);
        ReferenceCode refCode = (ReferenceCode) getBroker().getBeanByQuery(reportQuery);

        if (refCode != null) {
            code = refCode.getCode();
        }

        return code;
    }

    /**
     * Return a map of reference codes using either a field's bean path, or a field's alias to find
     * the bean path.
     *
     * @param beanClass Class
     * @param beanPath String
     * @param alias String
     * @return Map of reference codes
     */
    private Map<String, ReferenceCode> getRefCodes(Class beanClass, String beanPath, String alias) {
        Map<String, ReferenceCode> refCodesMap = new HashMap<String, ReferenceCode>();
        if (StringUtils.isEmpty(beanPath) && !StringUtils.isEmpty(alias)) {
            beanPath = translateAliasToJavaName(alias, true);
        }
        if (!StringUtils.isEmpty(beanPath)) {
            DataDictionaryField ddField = getDataDictionaryField(beanClass, beanPath);

            if (ddField != null && !StringUtils.isEmpty(ddField.getReferenceTableOid())) {
                refCodesMap = getReferenceCodes(ddField.getReferenceTableOid());
            }
        }

        return refCodesMap;
    }

    /**
     * Gets the student count.
     *
     * @param programCode String
     * @param beanPath String
     * @param codeList List<String>
     * @return Integer
     */
    private Integer getStudentCount(String programCode, String beanPath, List<String> codeList) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

        // Load program records for reporting students and programs into a map.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        if (!StringUtils.isEmpty(beanPath) && codeList != null) {
            criteria.addIn(beanPath, codeList);
        }
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_yearEndDate);
        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
        criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_yearStartDate);
        criteria2.addOrCriteria(criteria3);
        criteria.addAndCriteria(criteria2);
        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentProgramParticipation.class,
                new String[] {StudentProgramParticipation.COL_STUDENT_OID}, criteria, true);

        int studentCount = getBroker().getCount(query);

        return Integer.valueOf(studentCount);
    }

    /**
     * Initialize cesew counts.
     */
    private void initializeCesewCounts() {
        m_cesewOrderedNames = Arrays.asList

        (
                M_NPQH_CATHOLIC,
                M_NPQH_OTHER,
                F_NPQH_CATHOLIC,
                F_NPQH_OTHER,
                M_UNDER_NPQH_CATHOLIC,
                M_UNDER_NPQH_OTHER,
                F_UNDER_NPQH_CATHOLIC,
                F_UNDER_NPQH_OTHER,
                HOLDING_MASTERS,
                STUDYING_MASTERS,
                LEADERSHIP_SIZE,
                VACANCIES_EXECUTIVE,
                VACANCIES_HEAD,
                VACANCIES_DEPUTY,
                VACANCIES_ASSISTANT,
                EXECUTIVE_M,
                EXECUTIVE_F,
                HEAD_M,
                HEAD_F,
                DEPUTY_M,
                DEPUTY_F,
                ASSISTANT_M,
                ASSISTANT_F,
                TEACHERS_CATHOLIC,
                TEACHERS_OTHER,
                CCRS,
                TEACHER_WHITE_BRITISH,
                TEACHER_WHITE_IRISH,
                TEACHER_WHITE_E_EUROPEAN,
                TEACHER_WHITE_OTHER,
                TEACHER_TRAVELLER,
                TEACHER_MIXED,
                TEACHER_ASIAN,
                TEACHER_BLACK,
                TEACHER_CHINESE,
                TEACHER_OTHER,
                TEACHER_NOT_KNOWN,
                LEADERSHIP_WHITE_BRITISH,
                LEADERSHIP_WHITE_IRISH,
                LEADERSHIP_WHITE_E_EUROPEAN,
                LEADERSHIP_WHITE_OTHER,
                LEADERSHIP_TRAVELLER,
                LEADERSHIP_MIXED,
                LEADERSHIP_ASIAN,
                LEADERSHIP_BLACK,
                LEADERSHIP_CHINESE,
                LEADERSHIP_OTHER,
                LEADERSHIP_NOT_KNOWN,
                SPECIALIST_RE,
                STAFF_RE,
                HALF_RE,
                GUIDANCE_CATHOLIC,
                GUIDANCE_OTHER,
                Y14_CATHOLIC_BOYS,
                Y14_CATHOLIC_GIRLS,
                Y14_OTHER_BOYS,
                Y14_OTHER_GIRLS,
                Y13_CATHOLIC_BOYS,
                Y13_CATHOLIC_GIRLS,
                Y13_OTHER_BOYS,
                Y13_OTHER_GIRLS,
                Y12_CATHOLIC_BOYS,
                Y12_CATHOLIC_GIRLS,
                Y12_OTHER_BOYS,
                Y12_OTHER_GIRLS,
                Y11_CATHOLIC_BOYS,
                Y11_CATHOLIC_GIRLS,
                Y11_OTHER_BOYS,
                Y11_OTHER_GIRLS,
                Y10_CATHOLIC_BOYS,
                Y10_CATHOLIC_GIRLS,
                Y10_OTHER_BOYS,
                Y10_OTHER_GIRLS,
                Y9_CATHOLIC_BOYS,
                Y9_CATHOLIC_GIRLS,
                Y9_OTHER_BOYS,
                Y9_OTHER_GIRLS,
                Y8_CATHOLIC_BOYS,
                Y8_CATHOLIC_GIRLS,
                Y8_OTHER_BOYS,
                Y8_OTHER_GIRLS,
                Y7_CATHOLIC_BOYS,
                Y7_CATHOLIC_GIRLS,
                Y7_OTHER_BOYS,
                Y7_OTHER_GIRLS,
                Y6_CATHOLIC_BOYS,
                Y6_CATHOLIC_GIRLS,
                Y6_OTHER_BOYS,
                Y6_OTHER_GIRLS,
                Y5_CATHOLIC_BOYS,
                Y5_CATHOLIC_GIRLS,
                Y5_OTHER_BOYS,
                Y5_OTHER_GIRLS,
                Y4_CATHOLIC_BOYS,
                Y4_CATHOLIC_GIRLS,
                Y4_OTHER_BOYS,
                Y4_OTHER_GIRLS,
                Y3_CATHOLIC_BOYS,
                Y3_CATHOLIC_GIRLS,
                Y3_OTHER_BOYS,
                Y3_OTHER_GIRLS,
                Y2_CATHOLIC_BOYS,
                Y2_CATHOLIC_GIRLS,
                Y2_OTHER_BOYS,
                Y2_OTHER_GIRLS,
                Y1_CATHOLIC_BOYS,
                Y1_CATHOLIC_GIRLS,
                Y1_OTHER_BOYS,
                Y1_OTHER_GIRLS,
                R_CATHOLIC_BOYS,
                R_CATHOLIC_GIRLS,
                R_OTHER_BOYS,
                R_OTHER_GIRLS,
                N2_CATHOLIC_BOYS,
                N2_CATHOLIC_GIRLS,
                N2_OTHER_BOYS,
                N2_OTHER_GIRLS,
                N1_CATHOLIC_BOYS,
                N1_CATHOLIC_GIRLS,
                N1_OTHER_BOYS,
                N1_OTHER_GIRLS,
                WORSHIP_WITHDRAWN,
                WORSHIP_WITHDRAWN_Y12_Y13,
                IN_CARE,
                FREE_SCHOOL_MEALS,
                EMA,
                SEN_NO_STATEMENT,
                SEN_WITH_STATEMENT,
                Y11_TOTAL,
                DESTINATION_RETURNED,
                DESTINATION_OTHER_CATHOLIC,
                DESTINATION_OTHER,
                SRE_WITHDRAWAL_BOYS,
                SRE_WITHDRAWAL_GIRLS,
                PUPIL_WHITE_BRITISH,
                PUPIL_WHITE_IRISH,
                PUPIL_WHITE_E_EUROPEAN,
                PUPIL_WHITE_OTHER,
                PUPIL_TRAVELLER,
                PUPIL_MIXED,
                PUPIL_ASIAN,
                PUPIL_BLACK,
                PUPIL_CHINESE,
                PUPIL_OTHER,
                PUPIL_NOT_KNOWN);

        for (String key : m_cesewOrderedNames) {
            m_cesewCounts.put(key, Integer.valueOf(0));
        }
    }

    /**
     * Initialize fields used in program.
     */
    private void initializeFields() {
        m_yearStartDate = getCurrentContext().getStartDate();
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(m_yearStartDate);
        calendar.set(Calendar.MONTH, Calendar.JANUARY);
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        m_yearStartDate = new PlainDate(calendar.getTime());

        calendar.set(Calendar.MONTH, Calendar.DECEMBER);
        calendar.set(Calendar.DAY_OF_MONTH, 31);
        m_yearEndDate = new PlainDate(calendar.getTime());

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_yearStartDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_yearEndDate);

        m_senProvisionBeanPath = translateAliasToJavaName(ALIAS_DFE_SEN_PROVISION, true);
        m_staffExcludeBeanPath = translateAliasToJavaName(ALIAS_DFE_STAFF_EXCLUDE, false);

        // Load reference codes
        m_senRefCode = getCode(CODE_SPECIAL_EDUCATION);
        m_freeLunchRefCode = getCode(CODE_FREE_REDUCED_LUNCH);
        m_edMaintenanceAllowance = getCode(CODE_ED_MAINTENANCE_ALLOWANCE);
        m_ethnicityCodesMap = getRefCodes(SisPerson.class, null, ALIAS_DFE_ETHNICITY);
        m_senProvisionCodesMap = getRefCodes(SisStudent.class, null, ALIAS_DFE_SEN_PROVISION);
        m_roleCodesMap = getRefCodes(StaffPosition.class, null, ALIAS_DFE_ROLE_ID);
        m_genderCodesMap = getRefCodes(SisPerson.class, BEAN_PATH_GENDER_CODE, null);
        m_gradeLevelCodesMap = getRefCodes(SisStudent.class, BEAN_PATH_GRADE_LEVEL, null);
    }

    /**
     *
     * Return whether a staff member is part of leadership or not.
     *
     * @param staffRole String
     * @return true, if is leadership
     */
    private boolean isLeadership(String staffRole) {
        boolean leader = false;

        if (CODE_STATE_EXECUTIVE.equals(staffRole) || CODE_STATE_HEAD.equals(staffRole) ||
                CODE_STATE_DEPUTY.equals(staffRole) || CODE_STATE_ASSISTANT.equals(staffRole)) {
            leader = true;
        }

        return leader;
    }

    /**
     * Load the current staff collection and current student collection.
     */
    private void loadCollections() {
        String staffActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        X2Criteria staffCriteria = new X2Criteria();
        if (!StringUtils.isEmpty(m_staffExcludeBeanPath)) {
            staffCriteria.addEqualTo(m_staffExcludeBeanPath, BooleanAsStringConverter.TRUE);
        }
        staffCriteria.addEqualTo(SisStaff.COL_STATUS, staffActiveCode);
        staffCriteria.addLessOrEqualThan(SisStaff.COL_HIRE_DATE, m_yearEndDate);
        BeanQuery staffQuery = new BeanQuery(SisStaff.class, staffCriteria);
        m_staffCollection = getBroker().getCollectionByQuery(staffQuery);

        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        m_studentCollection = getBroker().getCollectionByQuery(studentQuery);
    }

    /**
     * Load the SEN reference code.
     */
    private void loadSenRefCode() {
        // Load the SEN reference code
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPECIAL_EDUCATION);
        BeanQuery reportQuery = new BeanQuery(ReferenceCode.class, criteria);
        ReferenceCode refCode = (ReferenceCode) getBroker().getBeanByQuery(reportQuery);

        if (refCode != null) {
            m_senRefCode = refCode.getCode();
        }
    }

    /**
     * Load the associated codes from the SEN Provision Codes table for students having SEN without
     * a statement.
     */
    private void loadSenNoStatementCodes() {
        DataDictionaryField senProvisionField =
                getDataDictionaryField(StudentProgramParticipation.class, m_senProvisionBeanPath);
        if (senProvisionField != null && !StringUtils.isEmpty(senProvisionField.getReferenceTableOid())) {
            Map<String, ReferenceCode> senCodesMap = getReferenceCodes(senProvisionField.getReferenceTableOid());
            for (ReferenceCode referenceCode : senCodesMap.values()) {
                String code = referenceCode.getCode();
                if (!CODE_SEN_NOT_NEEDED.equals(code) && !CODE_SEN_STATEMENT.equals(code)) {
                    m_senNoStatementCodes.add(code);
                }
            }
        }
    }
}
