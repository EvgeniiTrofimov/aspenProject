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

package com.x2dev.reports.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class WABasicSupportData.
 */
public class WABasicSupportData extends ReportJavaSourceNet {
    /**
     * This class is used to accumulate the data from individual rows of data per student
     * by resident district. This class is also used to directly deliver these values
     * to the report.
     *
     * @author Administrator
     *
     */
    public class DistrictData {
        static final String TOTAL_NAME = "Total";
        static final int BIG_DECIMAL_SCALE = 2;

        int m_bilingual;
        final Map<String, Map<String, Integer>> m_cnts;
        int m_collegeRS;
        final Map<String, Map<String, BigDecimal>> m_ftes;
        BigDecimal m_nonVocationalRsFte;
        String m_residentCountyName;
        String m_residentDistrictName;
        String m_residentDistrictNumber;
        String m_servingCountyName;
        String m_servingDistrictName;
        String m_servingDistrictNumber;
        String m_servingDistrictESDNumber;
        BigDecimal m_skillsAleQFte;
        BigDecimal m_skillsAleNQFte;
        BigDecimal m_skillsTotalFte;
        BigDecimal m_openDoorsTotal;
        BigDecimal m_ellExitsTotal;
        BigDecimal m_openDoorsNonVoc;
        BigDecimal m_openDoorsVoc;
        int m_totalRS;
        BigDecimal m_vocational78AleQFte;
        BigDecimal m_vocational78AleNQFte;
        BigDecimal m_vocational78TotalFte;
        BigDecimal m_vocationalHighAleQFte;
        BigDecimal m_vocationalHighAleNQFte;
        BigDecimal m_vocationalHighTotalFte;
        BigDecimal m_vocationalRsFte;

        /**
         * The constructor for DistrictData.
         */
        protected DistrictData() {
            m_cnts = new HashMap<String, Map<String, Integer>>();
            m_ftes = new HashMap<String, Map<String, BigDecimal>>();
        }

        /**
         * Gets the bilingual.
         *
         * @return the bilingual
         */
        public Integer getBilingual() {
            return m_bilingual == 0 ? null : Integer.valueOf(m_bilingual);
        }

        /**
         * Gets the bilingual int.
         *
         * @return the bilingual
         */
        public int getBilingualInt() {
            return m_bilingual;
        }

        /**
         * Gets the college RS.
         *
         * @return the collegeRS
         */
        public Integer getCollegeRS() {
            return m_collegeRS == 0 ? null : Integer.valueOf(m_collegeRS);
        }

        /**
         * Gets the college RS int.
         *
         * @return the collegeRS
         */
        public int getCollegeRSInt() {
            return m_collegeRS;
        }

        /**
         * Gets the count.
         *
         * @param grade the grade for which the count is returned
         * @param type the type for which the count is returned
         * @return the number of students with this grade and type
         */
        public Integer getCount(String grade, String type) {
            Map<String, Integer> types = m_cnts.get(grade);
            if (types != null) {
                return types.get(type);
            }
            return null;
        }

        /**
         * Returns the counts maps.
         *
         * @return Map<String, Map<String, Integer>>
         */
        public Map<String, Map<String, Integer>> getCountsMap() {
            return m_cnts;
        }

        /**
         * Gets the ell exits total.
         *
         * @return the vocationalRsFte
         */
        public BigDecimal getEllExitsTotal() {
            return m_ellExitsTotal != null ? m_ellExitsTotal.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the fte.
         *
         * @param grade the grade for which the count is returned
         * @param type the type for which the count is returned
         * @return the total fte of students with this grade and type
         */
        public BigDecimal getFte(String grade, String type) {
            Map<String, BigDecimal> types = m_ftes.get(grade);
            if (types != null) {
                return types.get(type).setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP);
            }
            return null;
        }

        /**
         * Returns the ftes map.
         *
         * @return Map<String, Map<String, BigDecimal>>
         */
        public Map<String, Map<String, BigDecimal>> getFtesMap() {
            return m_ftes;
        }

        /**
         * Gets the non vocational rs fte.
         *
         * @return the nonVocationalRsFte
         */
        public BigDecimal getNonVocationalRsFte() {
            return m_nonVocationalRsFte != null ? m_nonVocationalRsFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP)
                    : null;
        }

        /**
         * Gets the open doors non voc.
         *
         * @return the vocationalRsFte
         */
        public BigDecimal getOpenDoorsNonVoc() {
            return m_openDoorsNonVoc != null ? m_openDoorsNonVoc.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP)
                    : null;
        }

        /**
         * Gets the open doors total.
         *
         * @return the vocationalRsFte
         */
        public BigDecimal getOpenDoorsTotal() {
            return m_openDoorsTotal != null ? m_openDoorsTotal.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the open doors voc.
         *
         * @return the vocationalRsFte
         */
        public BigDecimal getOpenDoorsVoc() {
            return m_openDoorsVoc != null ? m_openDoorsVoc.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the resident county name.
         *
         * @return the residentCountyName
         */
        public String getResidentCountyName() {
            return m_residentCountyName;
        }

        /**
         * Gets the resident district name.
         *
         * @return the residentDistrictName
         */
        public String getResidentDistrictName() {
            return m_residentDistrictName;
        }

        /**
         * Gets the resident district number.
         *
         * @return the residentDistrictNumber
         */
        public String getResidentDistrictNumber() {
            return m_residentDistrictNumber;
        }

        /**
         * Gets the serving county name.
         *
         * @return the servingCountyName
         */
        public String getServingCountyName() {
            return m_servingCountyName;
        }

        /**
         * Gets the serving district ESD number.
         *
         * @return the servingDistrictESDNumber
         */
        public String getServingDistrictESDNumber() {
            return m_servingDistrictESDNumber;
        }

        /**
         * Gets the serving district name.
         *
         * @return the servingDistrictName
         */
        public String getServingDistrictName() {
            return m_servingDistrictName;
        }

        /**
         * Gets the serving district number.
         *
         * @return the servingDistrictNumber
         */
        public String getServingDistrictNumber() {
            return m_servingDistrictNumber;
        }

        /**
         * Gets the singletons map.
         *
         * @return Map
         */
        public Map<String, Object> getSingletonsMap() {
            Map<String, Object> singletonMap = new HashMap<String, Object>();
            singletonMap.put("bilingual", getBilingual());
            singletonMap.put("collegeRS", getCollegeRS());
            singletonMap.put("nonVocationalRsFte", getNonVocationalRsFte());
            singletonMap.put("residentDistrictName", getResidentDistrictName());
            singletonMap.put("residentDistrictNumber", getResidentDistrictNumber());
            singletonMap.put("residentCountyName", getResidentCountyName());
            singletonMap.put("servingDistrictName", getServingDistrictName());
            singletonMap.put("servingDistrictNumber", getServingDistrictNumber());
            singletonMap.put("servingDistrictESDNumber", getServingDistrictESDNumber());
            singletonMap.put("servingCountyName", getServingCountyName());
            singletonMap.put("skillsTotalFte", getSkillsTotalFte());
            singletonMap.put("skillsAleQFte", getSkillsAleQFte());
            singletonMap.put("skillsAleNQFte", getSkillsAleNQFte());
            singletonMap.put("totalRS", getTotalRS());
            singletonMap.put("vocational78TotalFte", getVocational78TotalFte());
            singletonMap.put("vocational78AleQFte", getVocational78AleQFte());
            singletonMap.put("vocational78AleNQFte", getVocational78AleNQFte());
            singletonMap.put("vocationalHighTotalFte", getVocationalHighTotalFte());
            singletonMap.put("vocationalHighAleQFte", getVocationalHighAleQFte());
            singletonMap.put("vocationalHighAleNQFte", getVocationalHighAleNQFte());
            singletonMap.put("vocationalRsFte", getVocationalRsFte());
            singletonMap.put("bilingual", getBilingual());
            singletonMap.put("ellExitCount", getEllExitsTotal());
            singletonMap.put("openDoorsTotal", getOpenDoorsTotal());
            singletonMap.put("openDoorsVoc", getOpenDoorsVoc());
            singletonMap.put("openDoorsNonVoc", getOpenDoorsNonVoc());
            return singletonMap;
        }

        /**
         * Gets the skills ale NQ fte.
         *
         * @return the skillsAleNQFte
         */
        public BigDecimal getSkillsAleNQFte() {
            return m_skillsAleNQFte != null ? m_skillsAleNQFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the skills ale Q fte.
         *
         * @return the skillsAleQFte
         */
        public BigDecimal getSkillsAleQFte() {
            return m_skillsAleQFte != null ? m_skillsAleQFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the skills total fte.
         *
         * @return the skillsTotalFte
         */
        public BigDecimal getSkillsTotalFte() {
            return m_skillsTotalFte != null ? m_skillsTotalFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the total RS.
         *
         * @return the totalRS
         */
        public Integer getTotalRS() {
            return m_totalRS == 0 ? null : Integer.valueOf(m_totalRS);
        }

        /**
         * Gets the total RS int.
         *
         * @return the totalRS
         */
        public int getTotalRSInt() {
            return m_totalRS;
        }

        /**
         * Gets the vocational 78 ale NQ fte.
         *
         * @return the vocational78AleNQFte
         */
        public BigDecimal getVocational78AleNQFte() {
            return m_vocational78AleNQFte != null
                    ? m_vocational78AleNQFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP)
                    : null;
        }

        /**
         * Gets the vocational 78 ale Q fte.
         *
         * @return the vocational78AleQFte
         */
        public BigDecimal getVocational78AleQFte() {
            return m_vocational78AleQFte != null
                    ? m_vocational78AleQFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP)
                    : null;
        }

        /**
         * Gets the vocational 78 total fte.
         *
         * @return the vocational78TotalFte
         */
        public BigDecimal getVocational78TotalFte() {
            return m_vocational78TotalFte != null
                    ? m_vocational78TotalFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP)
                    : null;
        }

        /**
         * Gets the vocational high ale NQ fte.
         *
         * @return the vocationalHighAleNQFte
         */
        public BigDecimal getVocationalHighAleNQFte() {
            return m_vocationalHighAleNQFte != null ? m_vocationalHighAleNQFte.setScale(BIG_DECIMAL_SCALE,
                    RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the vocational high ale Q fte.
         *
         * @return the vocationalHighAleQFte
         */
        public BigDecimal getVocationalHighAleQFte() {
            return m_vocationalHighAleQFte != null ? m_vocationalHighAleQFte.setScale(BIG_DECIMAL_SCALE,
                    RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the vocational high total fte.
         *
         * @return the vocationalHighTotalFte
         */
        public BigDecimal getVocationalHighTotalFte() {
            return m_vocationalHighTotalFte != null ? m_vocationalHighTotalFte.setScale(BIG_DECIMAL_SCALE,
                    RoundingMode.HALF_UP) : null;
        }

        /**
         * Gets the vocational rs fte.
         *
         * @return the vocationalRsFte
         */
        public BigDecimal getVocationalRsFte() {
            return m_vocationalRsFte != null ? m_vocationalRsFte.setScale(BIG_DECIMAL_SCALE, RoundingMode.HALF_UP)
                    : null;
        }

        /**
         * Increment the count accumulator for this grade and type.
         *
         * @param grade String
         * @param type String
         */
        protected void addCnt(String grade, String type) {
            Map<String, Integer> types = m_cnts.get(grade);
            if (types == null) {
                types = new HashMap<String, Integer>();
                m_cnts.put(grade, types);
            }
            Integer current = types.get(type);
            int value = current == null ? 1 : current.intValue() + 1;
            types.put(type, Integer.valueOf(value));
            if (!TOTAL_NAME.equals(grade)) {
                addCnt(TOTAL_NAME, type);
            }
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addEllExitsTotal(double fte) {
            m_ellExitsTotal = m_ellExitsTotal == null ? BigDecimal.valueOf(fte)
                    : m_ellExitsTotal.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment the FTE accumulator for this grade and type
         * by the BigDecimal value represented by fte.
         *
         * @param grade String
         * @param type String
         * @param fte double
         */
        protected void addFte(String grade, String type, double fte) {
            Map<String, BigDecimal> types = m_ftes.get(grade);
            if (types == null) {
                types = new HashMap<String, BigDecimal>();
                m_ftes.put(grade, types);
            }
            BigDecimal current = types.get(type);
            BigDecimal value = (current == null) ? BigDecimal.valueOf(fte) : current.add(BigDecimal.valueOf(fte));
            types.put(type, value);
            if (!TOTAL_NAME.equals(grade)) {
                addFte(TOTAL_NAME, type, fte);
            }
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addNonVocationalRSFte(double fte) {
            m_nonVocationalRsFte = m_nonVocationalRsFte == null ? BigDecimal.valueOf(fte)
                    : m_nonVocationalRsFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addOpenDoorsNonVoc(double fte) {
            m_openDoorsNonVoc = m_openDoorsNonVoc == null ? BigDecimal.valueOf(fte)
                    : m_openDoorsNonVoc.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addOpenDoorsTotal(double fte) {
            m_openDoorsTotal = m_openDoorsTotal == null ? BigDecimal.valueOf(fte)
                    : m_openDoorsTotal.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addOpenDoorsVoc(double fte) {
            m_openDoorsVoc =
                    m_openDoorsVoc == null ? BigDecimal.valueOf(fte) : m_openDoorsVoc.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addSkillsAleNQFte(double fte) {
            m_skillsAleNQFte = m_skillsAleNQFte == null ? BigDecimal.valueOf(fte)
                    : m_skillsAleNQFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addSkillsAleQFte(double fte) {
            m_skillsAleQFte = m_skillsAleQFte == null ? BigDecimal.valueOf(fte)
                    : m_skillsAleQFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addSkillsTotalFte(double fte) {
            m_skillsTotalFte = m_skillsTotalFte == null ? BigDecimal.valueOf(fte)
                    : m_skillsTotalFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocational78AleNQFte(double fte) {
            m_vocational78AleNQFte = m_vocational78AleNQFte == null ? BigDecimal.valueOf(fte)
                    : m_vocational78AleNQFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocational78AleQFte(double fte) {
            m_vocational78AleQFte = m_vocational78AleQFte == null ? BigDecimal.valueOf(fte)
                    : m_vocational78AleQFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocational78TotalFte(double fte) {
            m_vocational78TotalFte = m_vocational78TotalFte == null ? BigDecimal.valueOf(fte)
                    : m_vocational78TotalFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocationalHighAleNQFte(double fte) {
            m_vocationalHighAleNQFte = m_vocationalHighAleNQFte == null
                    ? BigDecimal.valueOf(fte)
                    : m_vocationalHighAleNQFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocationalHighAleQFte(double fte) {
            m_vocationalHighAleQFte = m_vocationalHighAleQFte == null ? BigDecimal.valueOf(fte)
                    : m_vocationalHighAleQFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocationalHighTotalFte(double fte) {
            m_vocationalHighTotalFte = m_vocationalHighTotalFte == null
                    ? BigDecimal.valueOf(fte)
                    : m_vocationalHighTotalFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Increment or initialize the FTE accumulator.
         *
         * @param fte double
         */
        protected void addVocationalRSFte(double fte) {
            m_vocationalRsFte = m_vocationalRsFte == null ? BigDecimal.valueOf(fte)
                    : m_vocationalRsFte.add(BigDecimal.valueOf(fte));
        }

        /**
         * Sets the bilingual.
         *
         * @param bilingual the bilingual to set
         */
        protected void setBilingual(int bilingual) {
            this.m_bilingual = bilingual;
        }

        /**
         * Sets the college RS.
         *
         * @param collegeRS the collegeRS to set
         */
        protected void setCollegeRS(int collegeRS) {
            this.m_collegeRS = collegeRS;
        }

        /**
         * Sets the non vocational rs fte.
         *
         * @param nonVocationalRsFte the nonVocationalRsFte to set
         */
        protected void setNonVocationalRsFte(BigDecimal nonVocationalRsFte) {
            this.m_nonVocationalRsFte = nonVocationalRsFte;
        }

        /**
         * Sets the resident county name.
         *
         * @param residentCountyName the residentCountyName to set
         */
        protected void setResidentCountyName(String residentCountyName) {
            this.m_residentCountyName = residentCountyName;
        }

        /**
         * Sets the resident district name.
         *
         * @param residentDistrictName the residentDistrictName to set
         */
        protected void setResidentDistrictName(String residentDistrictName) {
            this.m_residentDistrictName = residentDistrictName;
        }

        /**
         * Sets the resident district number.
         *
         * @param residentDistrictNumber the residentDistrictNumber to set
         */
        protected void setResidentDistrictNumber(String residentDistrictNumber) {
            this.m_residentDistrictNumber = residentDistrictNumber;
        }

        /**
         * Sets the serving county name.
         *
         * @param servingCountyName the servingCountyName to set
         */
        protected void setServingCountyName(String servingCountyName) {
            this.m_servingCountyName = servingCountyName;
        }

        /**
         * Sets the serving district ESD number.
         *
         * @param servingDistrictESDNumber the servingDistrictESDNumber to set
         */
        protected void setServingDistrictESDNumber(String servingDistrictESDNumber) {
            this.m_servingDistrictESDNumber = servingDistrictESDNumber;
        }

        /**
         * Sets the serving district name.
         *
         * @param servingDistrictName the servingDistrictName to set
         */
        protected void setServingDistrictName(String servingDistrictName) {
            this.m_servingDistrictName = servingDistrictName;
        }

        /**
         * Sets the serving district number.
         *
         * @param servingDistrictNumber the servingDistrictNumber to set
         */
        protected void setServingDistrictNumber(String servingDistrictNumber) {
            this.m_servingDistrictNumber = servingDistrictNumber;
        }

        /**
         * Sets the skills ale NQ fte.
         *
         * @param skillsAleNQFte the skillsAleNQFte to set
         */
        protected void setSkillsAleNQFte(BigDecimal skillsAleNQFte) {
            this.m_skillsAleNQFte = skillsAleNQFte;
        }

        /**
         * Sets the skills ale Q fte.
         *
         * @param skillsAleQFte the skillsAleQFte to set
         */
        protected void setSkillsAleQFte(BigDecimal skillsAleQFte) {
            this.m_skillsAleQFte = skillsAleQFte;
        }

        /**
         * Sets the skills total fte.
         *
         * @param skillsTotalFte the skillsTotalFte to set
         */
        protected void setSkillsTotalFte(BigDecimal skillsTotalFte) {
            this.m_skillsTotalFte = skillsTotalFte;
        }

        /**
         * Sets the total RS.
         *
         * @param totalRS the totalRS to set
         */
        protected void setTotalRS(int totalRS) {
            this.m_totalRS = totalRS;
        }

        /**
         * Sets the vocational 78 ale NQ fte.
         *
         * @param vocational78AleNQFte the vocational78AleNQFte to set
         */
        protected void setVocational78AleNQFte(BigDecimal vocational78AleNQFte) {
            this.m_vocational78AleNQFte = vocational78AleNQFte;
        }

        /**
         * Sets the vocational 78 ale Q fte.
         *
         * @param vocational78AleQFte the vocational78AleQFte to set
         */
        protected void setVocational78AleQFte(BigDecimal vocational78AleQFte) {
            this.m_vocational78AleQFte = vocational78AleQFte;
        }

        /**
         * Sets the vocational 78 total fte.
         *
         * @param vocational78TotalFte the vocational78TotalFte to set
         */
        protected void setVocational78TotalFte(BigDecimal vocational78TotalFte) {
            this.m_vocational78TotalFte = vocational78TotalFte;
        }

        /**
         * Sets the vocational high ale NQ fte.
         *
         * @param vocationalHighAleNQFte the vocationalHighAleNQFte to set
         */
        protected void setVocationalHighAleNQFte(BigDecimal vocationalHighAleNQFte) {
            this.m_vocationalHighAleNQFte = vocationalHighAleNQFte;
        }

        /**
         * Sets the vocational high ale Q fte.
         *
         * @param vocationalHighAleQFte the vocationalHighAleQFte to set
         */
        protected void setVocationalHighAleQFte(BigDecimal vocationalHighAleQFte) {
            this.m_vocationalHighAleQFte = vocationalHighAleQFte;
        }

        /**
         * Sets the vocational high total fte.
         *
         * @param vocationalHighTotalFte the vocationalHighTotalFte to set
         */
        protected void setVocationalHighTotalFte(BigDecimal vocationalHighTotalFte) {
            this.m_vocationalHighTotalFte = vocationalHighTotalFte;
        }

        /**
         * Sets the vocational rs fte.
         *
         * @param vocationalRsFte the vocationalRsFte to set
         */
        protected void setVocationalRsFte(BigDecimal vocationalRsFte) {
            this.m_vocationalRsFte = vocationalRsFte;
        }
    }

    /**
     * This class is used to accumulate the data from individual rows of data per student
     * by resident district and school. This class is also used to directly deliver these values
     * to the report.
     *
     * @author Administrator
     *
     */
    public class SchoolData extends DistrictData {

        private String m_schoolName;

        /**
         * Gets the school name.
         *
         * @return the schoolName
         */
        public String getSchoolName() {
            return m_schoolName;
        }

        /**
         * Sets the school name.
         *
         * @param schoolName void
         */
        protected void setSchoolName(String schoolName) {
            this.m_schoolName = schoolName;
        }
    }

    /**
     * A wrapper class for a DecimalFormat that is null safe.
     */
    private class DecimalFormatNS extends Format {
        private final DecimalFormat m_format = new DecimalFormat(FORMAT_PATTERN);

        /**
         * Instantiates a new decimal format NS.
         */
        public DecimalFormatNS() {
            // public constructor.
            super();
        }

        /**
         * Format the object with a DecimalFormat.
         * Perform null-safe check.
         *
         * @param number Object
         * @param toAppendTo StringBuffer
         * @param pos FieldPosition
         * @return StringBuffer
         */
        @Override
        public StringBuffer format(Object number, StringBuffer toAppendTo, FieldPosition pos) {
            StringBuffer value = null;
            if (number == null) {
                value = new StringBuffer(EMPTY_STRING);
            } else {
                value = m_format.format(number, toAppendTo, pos);
            }
            return value;
        }

        /**
         * parseObject method.
         * Required abstract implementation.
         *
         * @param source String
         * @param pos ParsePosition
         * @return Object
         */
        @Override
        public Object parseObject(String source, ParsePosition pos) {
            // Required abstract method. No implementation.
            return null;
        }
    }

    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    // Field names in Export Format
    private static final String DATA_FIELD_ALE_NQ = "ALE_NQ";
    private static final String DATA_FIELD_ALE_Q = "ALE_Q";
    private static final String DATA_FIELD_BILINGUAL_PROGRAM = "bilingualProgram";
    private static final String DATA_FIELD_COLLEGE_RS = "collegeRS";
    private static final String DATA_FIELD_COUNTY_HOME = "countyHome";
    private static final String DATA_FIELD_COUNTY_SERVE = "countyServe";
    private static final String DATA_FIELD_DISTRICT_HOME = "districtHome";
    private static final String DATA_FIELD_DISTRICT_HOME_CODE = "districtHomeCode";
    private static final String DATA_FIELD_DISTRICT_SERVE = "districtServe";
    private static final String DATA_FIELD_DISTRICT_SERVE_CODE = "districtServeCode";
    private static final String DATA_FIELD_DISTRICT_SERVE_ESD = "districtServeESD";
    private static final String DATA_FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String DATA_FIELD_HEAD_COUNT = "HEADCOUNT";
    private static final String DATA_FIELD_HIGH_POVERTY = "HIGH_POVERTY";
    private static final String DATA_FIELD_HIGH_POVERTY_REG = "HIGH_POVERTY_REG";
    private static final String DATA_FIELD_K12 = "K12";
    private static final String DATA_FIELD_NON_VOC_RS = "NON_VOC_RS";
    private static final String DATA_FIELD_REMOTE = "REMOTE";
    private static final String DATA_FIELD_SKILLS_CENTER = "SKILLS_CENTER";
    private static final String DATA_FIELD_SKILLS_CENTER_ALE_NQ = "SKILLS_CENTER_ALE_NQ";
    private static final String DATA_FIELD_SKILLS_CENTER_ALE_Q = "SKILLS_CENTER_ALE_Q";
    private static final String DATA_FIELD_SCHOOL_NAME = "School Name";
    private static final String DATA_FIELD_STD_LOCAL = "localId";
    private static final String DATA_FIELD_STD_NAME = "name";
    private static final String DATA_FIELD_STD_STATE = "stateId";
    private static final String DATA_FIELD_VOC_78 = "VOC_78";
    private static final String DATA_FIELD_VOC_78_ALE_NQ = "VOC_78_ALE_NQ";
    private static final String DATA_FIELD_VOC_78_ALE_Q = "VOC_78_ALE_Q";
    private static final String DATA_FIELD_VOC_912 = "VOC_912";
    private static final String DATA_FIELD_VOC_912_ALE_NQ = "VOC_912_ALE_NQ";
    private static final String DATA_FIELD_VOC_912_ALE_Q = "VOC_912_ALE_Q";
    private static final String DATA_FIELD_VOC_RS = "VOC_RS";
    private static final String DATA_FIELD_ELL_PROGRAMS_EXITED = "ELL_PROGRAMS_EXITED";
    private static final String DATA_FIELD_OPEN_DOORS = "OPEN_DOORS";
    private static final String DATA_FIELD_OPEN_DOORS_VOC = "OPEN_DOORS_VOC";
    private static final String DATA_FIELD_REPORT_DATE = "Report Date";

    private static final String DATA_TYPE_ALE_NQ = "AleNQ";
    private static final String DATA_TYPE_ALE_Q = "AleQ";
    private static final String DATA_TYPE_POVERTY = "Poverty";
    private static final String DATA_TYPE_REGULAR = "Regular";
    private static final String DATA_TYPE_REMOTE = "Remote";

    private static final String DEFAULT = "default";
    private static final String DISTRICT_RESIDENCE = "districtResidence";

    private static final String FIELD_DISTRICT_COUNTS = "districtCounts";
    private static final String FIELD_DISTRICT_FTES = "districtFtes";
    private static final String FIELD_DISTRICT_MAP = "districtMap";
    private static final String FIILD_SCHOOL_NAME = "schoolName";
    private static final String FORMAT_PATTERN = "####0.00";
    private static final String EMPTY_STRING = "";
    private static final String METHOD_NAME_GET_SKIP_HEADCOUNT = "getSkipHeadcount";


    private static final String PARAM_CURRENT_DATE = "currentDate";
    private static final String PARAM_DATE_TIME_FORMATTER = "dateTimeFormat";
    private static final String PARAM_DETAIL_REPORT = "stdP223DetailReport";
    private static final String PARAM_EXCLUDE_NON_RESIDENT = "excludeNonResident";
    private static final String PARAM_EXCLUDE_SKL = "excludeSchool";
    private static final String PARAM_FILE_DATE = "fileDate";
    private static final String PARAM_FROM_EXPORT_RESULT = "fromExportResult";
    private static final String PARAM_GROUP_BY = "groupBy";
    private static final String PARAM_INCLUDE_ZERO_VALUES = "omitZeroes";
    private static final String PARAM_NUMBER_FORMAT = "numberFormat";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_REPORT_MONTH = "reportMonth";
    private static final String PARAM_REPORT_YEAR = "reportYear";
    private static final String PARAM_RESULT_OID = "efrOid";
    private static final String PROCEDURE_ID = "procedureId";

    private static final String SCHOOL = "school";
    private static final String SCHOOL_AND_DISTRICT_RESIDENCE = "schoolAndDistrictResidence";

    private int m_aleNQPosition = -1;
    private int m_aleQPosition = -1;
    private int m_bilingualPosition = -1;
    private int m_collegeRSPosition = -1;
    private int m_countyHomePosition = -1;
    private int m_countyServePosition = -1;
    private int m_districtHomeCodePosition = -1;
    private int m_districtHomePosition = -1;
    private int m_districtServeCodePosition = -1;
    private int m_districtServeESDPosition = -1;
    private int m_districtServePosition = -1;
    private Map<String, DistrictData> m_districtGradeDataMap;
    private int m_gradeK12Position = -1;
    private int m_gradeLevelPosition = -1;
    private String m_groupBy;
    private Map<String, SisSchool> m_excludeSchool;
    private int m_headCountPosition = -1;
    private int m_highPovertyPosition = -1;
    private int m_highPovertyRegPosition = -1;
    private Collection<StateReportValidationError> m_initErrors = null;
    private Boolean m_isDetailReport = Boolean.FALSE;
    private int m_nonVocationalRSPosition = -1;
    private int m_remotePosition = -1;
    private StateReportData m_reportData = null;
    private Map<String, SchoolData> m_schoolGradeDataMap = null;
    private int m_skillsCenterAleNQPosition = -1;
    private int m_skillsCenterAleQPosition = -1;
    private int m_skillsCenterPosition = -1;
    private int m_stdStateId = -1;
    private int m_stdLocalId = -1;
    private int m_stdNameView = -1;
    private int m_vocational78AleNQPosition = -1;
    private int m_vocational78AleQPosition = -1;
    private int m_vocational78Position = -1;
    private int m_vocationalHighAleNQPosition = -1;
    private int m_vocationalHighAleQPosition = -1;
    private int m_vocationalHighPosition = -1;
    private int m_vocationalRSPosition = -1;
    private int m_openDoorsPosition = -1;
    private int m_openDoorsVocPosition = -1;
    private int m_ellExitedPosition = -1;
    private int m_reportDatePosition = -1;
    private int m_schoolNameCache = -1;
    private Boolean m_useExportResult = Boolean.FALSE;

    /**
     * Check if we need to include school in report.
     *
     * @param schoolName String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolName) {
        return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolName);
    }

    /**
     * All data for this report is provided by WABasicSupport.
     * The inpout data is not sorted and is collected in maps during
     * data gathering. The rows are output at the end of data gathering.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.procedures.statereporting.wa.WABasicSupport
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        if (getParameter(PARAM_EXCLUDE_SKL) != null && ((Boolean) getParameter(PARAM_EXCLUDE_SKL)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        m_useExportResult = (Boolean) getParameter(PARAM_FROM_EXPORT_RESULT);
        m_initErrors = new ArrayList<StateReportValidationError>();
        Date reportDate = null;

        if (getParameter(PARAM_DETAIL_REPORT) != null) {
            m_isDetailReport = (Boolean) getParameter(PARAM_DETAIL_REPORT);
        }

        if (m_useExportResult == null) {
            m_useExportResult = Boolean.FALSE;
        }

        if (m_useExportResult.booleanValue()) {
            m_groupBy = (String) getParameter(PARAM_GROUP_BY);
            String efrOid = (String) getParameter(PARAM_RESULT_OID);

            if (StringUtils.isEmpty(efrOid)) {
                efrOid = getLastExportResultOid();
            }

            if (!StringUtils.isEmpty(efrOid)) {
                addParameter(PARAM_RESULT_OID, efrOid);

                ExportFormatResult result =
                        (ExportFormatResult) getBroker().getBeanByOid(ExportFormatResult.class, efrOid);

                if (result != null) {
                    reportDate = new Date(result.getRunDate());
                }
            } else {
                JobResult result = (JobResult) getBroker().getBeanByOid(JobResult.class, getJob().getJobResultOid());
                if (result != null) {
                    result.setComments("Export Format Result was not found for " + getParameter(PROCEDURE_ID));
                }

                return null;
            }
        } else {
            m_groupBy = DEFAULT;
        }

        Boolean exludeNonResidentStudent = (Boolean) getParameter(PARAM_EXCLUDE_NON_RESIDENT);
        Boolean includeZeroKTwelveOrSkills = (Boolean) getParameter(PARAM_INCLUDE_ZERO_VALUES);

        Date currentDate = new Date(System.currentTimeMillis());
        if (!m_useExportResult.booleanValue()) {
            reportDate = (Date) getParameter(PARAM_REPORT_DATE);
            addParameter(PARAM_FILE_DATE, currentDate);
        }

        String procedureId = (String) getParameter(PROCEDURE_ID);
        String reportSchoolName = EMPTY_STRING;
        if (isSchoolContext()) {
            reportSchoolName = getSchool().getName();
        }

        addParameter(PARAM_NUMBER_FORMAT, new DecimalFormatNS());
        addParameter(PARAM_DATE_TIME_FORMATTER, DateFormat.getDateTimeInstance());
        addParameter(PARAM_CURRENT_DATE, new Date(System.currentTimeMillis()));

        Map<String, ReferenceCode> gradeReferenceMap = null;
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        if ((field != null) && field.hasReferenceTable()) {
            ReferenceTable referenceTable = field.getReferenceTable();
            gradeReferenceMap = referenceTable.getCodeMap();
        }

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

        if ((m_reportData != null) && (m_initErrors.size() == 0)) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in WABasicSupport";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }

        ReportDataGrid districtDataGrid = new ReportDataGrid(10, 20);
        ReportDataGrid schoolDataGrid = new ReportDataGrid(10, 20);
        ReportDataGrid detailDataGrid = new ReportDataGrid();

        if (m_reportData != null && m_reportData.open() && initializeFieldPositions()) {
            try {
                if (m_isDetailReport.booleanValue()) {
                    StateReportEntity entity = null;
                    while ((entity = m_reportData.next()) != null) {
                        if ((isSchoolContext() && reportSchoolName != null &&
                                reportSchoolName.equals(entity.getFieldValue(m_schoolNameCache)) || !isSchoolContext())
                                && includeSchool(entity.getFieldValue(m_schoolNameCache))) {

                            double regularFte = numericValue(entity.getFieldValue(m_gradeK12Position));
                            double skills = numericValue(entity.getFieldValue(m_skillsCenterPosition));
                            double highPoverty = numericValue(entity.getFieldValue(m_highPovertyPosition));
                            double headCount = numericValue(entity.getFieldValue(m_headCountPosition));

                            if (includeZeroKTwelveOrSkills.booleanValue() || (skills > 0.0) || (regularFte > 0.0) ||
                                    (highPoverty > 0.0) || (headCount > 0.0)) {
                                detailDataGrid.append();
                                detailDataGrid.set(DATA_FIELD_STD_LOCAL, entity.getFieldValue(m_stdLocalId));
                                detailDataGrid.set(DATA_FIELD_SCHOOL_NAME, entity.getFieldValue(m_schoolNameCache));
                                detailDataGrid.set(DATA_FIELD_STD_STATE, entity.getFieldValue(m_stdStateId));
                                detailDataGrid.set(DATA_FIELD_STD_NAME, entity.getFieldValue(m_stdNameView));

                                String gradeLevel = entity.getFieldValue(m_gradeLevelPosition);
                                if (gradeReferenceMap != null) {
                                    ReferenceCode code = gradeReferenceMap.get(gradeLevel);
                                    if (code != null) {
                                        gradeLevel = code.getStateCode();
                                    }
                                }

                                detailDataGrid.set(DATA_FIELD_GRADE_LEVEL, entity.getFieldValue(m_gradeLevelPosition));
                                detailDataGrid.set(DATA_FIELD_DISTRICT_HOME_CODE,
                                        entity.getFieldValue(m_districtHomeCodePosition));

                                detailDataGrid.set(DATA_FIELD_HEAD_COUNT, Double.valueOf(headCount));
                                detailDataGrid.set(DATA_FIELD_K12, Double.valueOf(regularFte));
                                detailDataGrid.set(DATA_FIELD_HIGH_POVERTY, Double.valueOf(highPoverty));
                                detailDataGrid.set(DATA_FIELD_SKILLS_CENTER, Double.valueOf(skills));
                            }

                        }

                    }

                    detailDataGrid.sort(Arrays.asList(new String[] {DATA_FIELD_SCHOOL_NAME, DATA_FIELD_GRADE_LEVEL,
                            DATA_FIELD_STD_NAME}), true);

                } else {
                    m_districtGradeDataMap = new HashMap<String, DistrictData>();
                    m_schoolGradeDataMap = new HashMap<String, SchoolData>();

                    StateReportEntity entity = null;
                    Method getSkipHeadcountMethod = null;
                    while ((entity = m_reportData.next()) != null) {
                        if ((isSchoolContext() && reportSchoolName != null &&
                                reportSchoolName.equals(entity.getFieldValue(m_schoolNameCache)) || !isSchoolContext())
                                && includeSchool(entity.getFieldValue(m_schoolNameCache))) {
                            entity.preProcess();

                            String gradeLevel = entity.getFieldValue(m_gradeLevelPosition);
                            if (gradeReferenceMap != null) {
                                ReferenceCode code = gradeReferenceMap.get(gradeLevel);
                                if (code != null) {
                                    gradeLevel = code.getStateCode();
                                }
                            }

                            // left pad the gradeLevel to two places with z zero
                            // "0".
                            // The maps expect zero padding.
                            // Fife has single digit grade levels in their state
                            // codes
                            // 1-9.
                            gradeLevel = StringUtils.padLeft(gradeLevel, 2, '0');

                            String collegeRS = entity.getFieldValue(m_collegeRSPosition);
                            double vocationalRS = numericValue(entity.getFieldValue(m_vocationalRSPosition));
                            double nonVocationalRS = numericValue(entity.getFieldValue(m_nonVocationalRSPosition));
                            if (!StringUtils.isEmpty(collegeRS) && !collegeRS.equals("0")) {
                                DistrictData gradeData = getDistrictGradeData(entity);
                                SchoolData schoolGradeData = getSchoolGradeData(entity);
                                // College RS Student - don't count as K-12
                                // Headcount or
                                // FTE
                                gradeData.setCollegeRS(gradeData.getCollegeRSInt() + 1);
                                gradeData.setTotalRS(gradeData.getTotalRSInt() + 1);

                                schoolGradeData.setCollegeRS(schoolGradeData.getCollegeRSInt() + 1);
                                schoolGradeData.setTotalRS(schoolGradeData.getTotalRSInt() + 1);
                                // Add RS FTE
                                if (vocationalRS > 0.0) {
                                    gradeData.addNonVocationalRSFte(nonVocationalRS);

                                    schoolGradeData.addNonVocationalRSFte(nonVocationalRS);
                                }
                                if (vocationalRS > 0.0) {
                                    gradeData.addVocationalRSFte(nonVocationalRS);

                                    schoolGradeData.addVocationalRSFte(nonVocationalRS);
                                }
                            } else {
                                double regularFte = numericValue(entity.getFieldValue(m_gradeK12Position));
                                double skills = numericValue(entity.getFieldValue(m_skillsCenterPosition));
                                double highPoverty = numericValue(entity.getFieldValue(m_highPovertyPosition));
                                double highPovertyReg = numericValue(entity.getFieldValue(m_highPovertyRegPosition));

                                String residentDistrictName = entity.getFieldValue(m_districtHomePosition);
                                // We don't want to report students without
                                // anything to
                                // report, unless they ask us to do some with
                                // the
                                // include flag
                                if (includeZeroKTwelveOrSkills.booleanValue() || (skills > 0.0) || (regularFte > 0.0) ||
                                        (highPoverty > 0.0)) {
                                    DistrictData gradeData = getDistrictGradeData(entity);
                                    SchoolData schoolGradeData = getSchoolGradeData(entity);
                                    // Set headcount data, if student
                                    // non-resident, skip
                                    // him in main headcount
                                    // Here need to use reflection because
                                    // simple
                                    // downcasting entity class to
                                    // WABasicSupportEntity
                                    // don't work on customer side.
                                    boolean includeHeadcount = false;
                                    if (getSkipHeadcountMethod == null) {
                                        getSkipHeadcountMethod =
                                                entity.getClass().getMethod(METHOD_NAME_GET_SKIP_HEADCOUNT);
                                    }
                                    includeHeadcount =
                                            !((Boolean) getSkipHeadcountMethod.invoke(entity)).booleanValue();
                                    if (includeHeadcount &&
                                            (!exludeNonResidentStudent.booleanValue()
                                                    || residentDistrictName.equals(getOrganization().getName()))) {
                                        gradeData.addCnt(gradeLevel, DATA_TYPE_REGULAR);

                                        schoolGradeData.addCnt(gradeLevel, DATA_TYPE_REGULAR);
                                    }
                                    if (regularFte > 0.0) {
                                        gradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, regularFte);

                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, regularFte);
                                    }
                                    double highPovertyFte = numericValue(entity.getFieldValue(m_highPovertyPosition));
                                    if (highPovertyFte > 0.0) {
                                        gradeData.addFte(gradeLevel, DATA_TYPE_POVERTY, highPovertyFte);

                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_POVERTY, highPovertyFte);
                                    }
                                    if ((highPovertyReg > 0.0) && includeHeadcount) {
                                        gradeData.addCnt(gradeLevel, DATA_TYPE_POVERTY);

                                        schoolGradeData.addCnt(gradeLevel, DATA_TYPE_POVERTY);
                                    }
                                    double aleQFte = numericValue(entity.getFieldValue(m_aleQPosition));
                                    if (aleQFte > 0.0) {
                                        gradeData.addFte(gradeLevel, DATA_TYPE_ALE_Q, aleQFte);
                                        gradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, aleQFte);

                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_ALE_Q, aleQFte);
                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, aleQFte);

                                        if (includeHeadcount) {
                                            gradeData.addCnt(gradeLevel, DATA_TYPE_ALE_Q);

                                            schoolGradeData.addCnt(gradeLevel, DATA_TYPE_ALE_Q);
                                        }
                                    }
                                    double aleNQFte = numericValue(entity.getFieldValue(m_aleNQPosition));
                                    if (aleNQFte > 0.0) {
                                        gradeData.addFte(gradeLevel, DATA_TYPE_ALE_NQ, aleNQFte);
                                        gradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, aleNQFte);

                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_ALE_NQ, aleNQFte);
                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, aleNQFte);

                                        if (includeHeadcount) {
                                            gradeData.addCnt(gradeLevel, DATA_TYPE_ALE_NQ);

                                            schoolGradeData.addCnt(gradeLevel, DATA_TYPE_ALE_NQ);
                                        }
                                    }
                                    double remoteFte = numericValue(entity.getFieldValue(m_remotePosition));
                                    if (remoteFte > 0.0) {
                                        gradeData.addFte(gradeLevel, DATA_TYPE_REMOTE, remoteFte);
                                        gradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, remoteFte);

                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_REMOTE, remoteFte);
                                        schoolGradeData.addFte(gradeLevel, DATA_TYPE_REGULAR, remoteFte);

                                        if (includeHeadcount) {
                                            gradeData.addCnt(gradeLevel, DATA_TYPE_REMOTE);

                                            schoolGradeData.addCnt(gradeLevel, DATA_TYPE_REMOTE);
                                        }
                                    }
                                    // Add RS FTE
                                    boolean isRS = false;
                                    if (nonVocationalRS > 0.0) {
                                        isRS = true;
                                        gradeData.addNonVocationalRSFte(nonVocationalRS);

                                        schoolGradeData.addNonVocationalRSFte(nonVocationalRS);
                                    }
                                    if (vocationalRS > 0.0) {
                                        isRS = true;
                                        gradeData.addVocationalRSFte(vocationalRS);

                                        schoolGradeData.addVocationalRSFte(vocationalRS);
                                    }
                                    if (isRS) {
                                        gradeData.setTotalRS(gradeData.getTotalRSInt() + 1);

                                        schoolGradeData.setTotalRS(schoolGradeData.getTotalRSInt() + 1);
                                    }
                                    // Add Vocational 78
                                    double vocational78 = numericValue(entity.getFieldValue(m_vocational78Position));
                                    double vocational78AleQ =
                                            numericValue(entity.getFieldValue(m_vocational78AleQPosition));
                                    double vocational78AleNQ =
                                            numericValue(entity.getFieldValue(m_vocational78AleNQPosition));
                                    if (vocational78 > 0.0) {
                                        gradeData.addVocational78TotalFte(vocational78);

                                        schoolGradeData.addVocational78TotalFte(vocational78);
                                    }
                                    if (vocational78AleQ > 0.0) {
                                        gradeData.addVocational78AleQFte(vocational78AleQ);
                                        gradeData.addVocational78TotalFte(vocational78AleQ);

                                        schoolGradeData.addVocational78AleQFte(vocational78AleQ);
                                        schoolGradeData.addVocational78TotalFte(vocational78AleQ);
                                    }
                                    if (vocational78AleNQ > 0.0) {
                                        gradeData.addVocational78AleNQFte(vocational78AleNQ);
                                        gradeData.addVocational78TotalFte(vocational78AleNQ);

                                        schoolGradeData.addVocational78AleNQFte(vocational78AleNQ);
                                        schoolGradeData.addVocational78TotalFte(vocational78AleNQ);
                                    }
                                    // Add Vocational High
                                    double vocationalHigh =
                                            numericValue(entity.getFieldValue(m_vocationalHighPosition));
                                    double vocationalHighAleQ =
                                            numericValue(entity.getFieldValue(m_vocationalHighAleQPosition));
                                    double vocationalHighAleNQ =
                                            numericValue(entity.getFieldValue(m_vocationalHighAleNQPosition));
                                    if (vocationalHigh > 0.0) {
                                        gradeData.addVocationalHighTotalFte(vocationalHigh);

                                        schoolGradeData.addVocationalHighTotalFte(vocationalHigh);
                                    }
                                    if (vocationalHighAleQ > 0.0) {
                                        gradeData.addVocationalHighAleQFte(vocationalHighAleQ);
                                        gradeData.addVocationalHighTotalFte(vocationalHighAleQ);

                                        schoolGradeData.addVocationalHighAleQFte(vocationalHighAleQ);
                                        schoolGradeData.addVocationalHighTotalFte(vocationalHighAleQ);
                                    }
                                    if (vocationalHighAleNQ > 0.0) {
                                        gradeData.addVocationalHighAleNQFte(vocationalHighAleNQ);
                                        gradeData.addVocationalHighTotalFte(vocationalHighAleNQ);

                                        schoolGradeData.addVocationalHighAleNQFte(vocationalHighAleNQ);
                                        schoolGradeData.addVocationalHighTotalFte(vocationalHighAleNQ);
                                    }
                                    // Add Vocational High
                                    double skillsAleQ = numericValue(entity.getFieldValue(m_skillsCenterAleQPosition));
                                    double skillsAleNQ =
                                            numericValue(entity.getFieldValue(m_skillsCenterAleNQPosition));
                                    if (skills > 0.0) {
                                        gradeData.addSkillsTotalFte(skills);

                                        schoolGradeData.addSkillsTotalFte(skills);
                                    }
                                    if (skillsAleQ > 0.0) {
                                        gradeData.addSkillsAleQFte(skillsAleQ);
                                        gradeData.addSkillsTotalFte(skillsAleQ);

                                        schoolGradeData.addSkillsAleQFte(skillsAleQ);
                                        schoolGradeData.addSkillsTotalFte(skillsAleQ);
                                    }
                                    if (skillsAleNQ > 0.0) {
                                        gradeData.addSkillsAleNQFte(skillsAleNQ);
                                        gradeData.addSkillsTotalFte(skillsAleNQ);

                                        schoolGradeData.addSkillsAleNQFte(skillsAleNQ);
                                        schoolGradeData.addSkillsTotalFte(skillsAleNQ);
                                    }
                                    // Add Open Doors counts
                                    double openDoors = numericValue(entity.getFieldValue(m_openDoorsPosition));
                                    double openDoorsVOC = numericValue(entity.getFieldValue(m_openDoorsVocPosition));
                                    if (openDoors > 0.0) {
                                        gradeData.addOpenDoorsNonVoc(openDoors);
                                        gradeData.addOpenDoorsTotal(openDoors);

                                        schoolGradeData.addOpenDoorsNonVoc(openDoors);
                                        schoolGradeData.addOpenDoorsTotal(openDoors);
                                    }
                                    if (openDoorsVOC > 0.0) {
                                        gradeData.addOpenDoorsVoc(openDoorsVOC);
                                        gradeData.addOpenDoorsTotal(openDoorsVOC);

                                        schoolGradeData.addOpenDoorsVoc(openDoorsVOC);
                                        schoolGradeData.addOpenDoorsTotal(openDoorsVOC);
                                    }
                                    // Add Ell program exit count
                                    double ellExits = numericValue(entity.getFieldValue(m_ellExitedPosition));
                                    if (ellExits > 0.0) {
                                        gradeData.addEllExitsTotal(ellExits);

                                        schoolGradeData.addEllExitsTotal(ellExits);
                                    }
                                    // check bilingual program participation
                                    String billingual = entity.getFieldValue(m_bilingualPosition);
                                    if (BooleanAsStringConverter.TRUE.equals(billingual)) {
                                        gradeData.setBilingual(gradeData.getBilingualInt() + 1);

                                        schoolGradeData.setBilingual(schoolGradeData.getBilingualInt() + 1);
                                    }
                                }

                                entity.postProcess();
                            }
                        }
                    }

                    if (isSchoolContext()) {
                        outputGridSchoolLvl(schoolDataGrid);
                    }
                    // if All active schools selected, print school after
                    // district level
                    else {
                        outputGridDistrictLvl(districtDataGrid, reportSchoolName);
                        outputGridSchoolLvl(schoolDataGrid);
                    }

                    switch (m_groupBy) {
                        case DISTRICT_RESIDENCE:
                            break;

                        case SCHOOL:
                            districtDataGrid.clear();
                            districtDataGrid.append(schoolDataGrid);
                            break;

                        case SCHOOL_AND_DISTRICT_RESIDENCE:
                            schoolDataGrid.append(districtDataGrid);
                            districtDataGrid = schoolDataGrid;
                            break;

                        default:
                            districtDataGrid.append(schoolDataGrid);
                            break;
                    }
                }
            }

            finally {
                m_reportData.close();
            }
        }

        if (m_isDetailReport.booleanValue()) {
            districtDataGrid.clear();
            districtDataGrid.append(detailDataGrid);
        }

        // This params should be in the end, because reportDate can be
        // overwritten;
        addParameter(PARAM_REPORT_DATE, reportDate);
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(reportDate != null ? reportDate : new Date());
        addParameter(PARAM_REPORT_MONTH, calendar.getDisplayName(Calendar.MONTH, Calendar.LONG, getLocale()));
        addParameter(PARAM_REPORT_YEAR, String.valueOf(calendar.get(Calendar.YEAR)));

        districtDataGrid.beforeTop();
        return districtDataGrid;
    }

    /**
     * Returns the correct DistrictData for use with this entity.
     * Creates and initializes a new DistrictData if a match is not found.
     *
     * @param entity StateReportEntity
     * @return District data
     */
    private DistrictData getDistrictGradeData(StateReportEntity entity) {
        String residentDistrict = entity.getFieldValue(m_districtHomePosition);
        DistrictData data = m_districtGradeDataMap.get(residentDistrict);
        if (data == null) {
            data = new DistrictData();
            data.setResidentDistrictName(residentDistrict);
            data.setResidentDistrictNumber(entity.getFieldValue(m_districtHomeCodePosition));
            data.setResidentCountyName(entity.getFieldValue(m_countyHomePosition));
            data.setServingDistrictName(entity.getFieldValue(m_districtServePosition));
            data.setServingDistrictNumber(entity.getFieldValue(m_districtServeCodePosition));
            data.setServingDistrictESDNumber(entity.getFieldValue(m_districtServeESDPosition));
            data.setServingCountyName(entity.getFieldValue(m_countyServePosition));
            m_districtGradeDataMap.put(residentDistrict, data);
        }
        return data;
    }

    /**
     * If last export format result was not selected looking for the last from DB.
     * 
     * @return lastEfrOid
     */
    private String getLastExportResultOid() {
        String lastEfrOid = null;
        String procedureId = (String) getParameter(PROCEDURE_ID);
        if (!StringUtils.isEmpty(procedureId)) {
            X2Criteria efrCriteria = new X2Criteria();
            efrCriteria.addEqualTo(
                    ExportFormatResult.REL_DEFINITION + PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID,
                    procedureId);

            QueryByCriteria efrQuery = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
            efrQuery.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

            Collection<ExportFormatResult> efrs = getBroker().getCollectionByQuery(efrQuery);

            if (!efrs.isEmpty()) {
                lastEfrOid = ((ExportFormatResult) efrs.toArray()[0]).getOid();
            }
        }
        return lastEfrOid;
    }

    /**
     * Returns the correct DistrictData for use with this entity.
     * Creates and initializes a new DistrictData if a match is not found.
     *
     * @param entity StateReportEntity
     * @return School data
     */
    private SchoolData getSchoolGradeData(StateReportEntity entity) {
        String school = null;

        // get school of actual enrollment on report date.
        school = entity.getFieldValue(m_schoolNameCache);

        SchoolData data = m_schoolGradeDataMap.get(school);
        if (data == null && includeSchool(school)) {
            data = new SchoolData();
            data.setResidentDistrictName(entity.getFieldValue(m_districtHomePosition));
            data.setResidentDistrictNumber(entity.getFieldValue(m_districtHomeCodePosition));
            data.setResidentCountyName(entity.getFieldValue(m_countyHomePosition));
            data.setServingDistrictName(entity.getFieldValue(m_districtServePosition));
            data.setServingDistrictNumber(entity.getFieldValue(m_districtServeCodePosition));
            data.setServingDistrictESDNumber(entity.getFieldValue(m_districtServeESDPosition));
            data.setServingCountyName(entity.getFieldValue(m_countyServePosition));
            m_schoolGradeDataMap.put(school, data);
        }
        return data;
    }

    /**
     * Initializes the field position members for the data source.
     *
     * @return true, if successful
     */
    private boolean initializeFieldPositions() {
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();

            if (DATA_FIELD_COUNTY_HOME.equals(fieldName)) {
                m_countyHomePosition = pos;
            } else if (DATA_FIELD_COUNTY_SERVE.equals(fieldName)) {
                m_countyServePosition = pos;
            } else if (DATA_FIELD_DISTRICT_HOME.equals(fieldName)) {
                m_districtHomePosition = pos;
            } else if (DATA_FIELD_DISTRICT_HOME_CODE.equals(fieldName)) {
                m_districtHomeCodePosition = pos;
            } else if (DATA_FIELD_DISTRICT_SERVE.equals(fieldName)) {
                m_districtServePosition = pos;
            } else if (DATA_FIELD_STD_NAME.equals(fieldName)) {
                m_stdNameView = pos;
            } else if (DATA_FIELD_DISTRICT_SERVE_CODE.equals(fieldName)) {
                m_districtServeCodePosition = pos;
            } else if (DATA_FIELD_DISTRICT_SERVE_ESD.equals(fieldName)) {
                m_districtServeESDPosition = pos;
            } else if (DATA_FIELD_GRADE_LEVEL.equals(fieldName)) {
                m_gradeLevelPosition = pos;
            } else if (DATA_FIELD_BILINGUAL_PROGRAM.equals(fieldName)) {
                m_bilingualPosition = pos;
            } else if (DATA_FIELD_COLLEGE_RS.equals(fieldName)) {
                m_collegeRSPosition = pos;
            } else if (DATA_FIELD_VOC_RS.equals(fieldName)) {
                m_vocationalRSPosition = pos;
            } else if (DATA_FIELD_NON_VOC_RS.equals(fieldName)) {
                m_nonVocationalRSPosition = pos;
            } else if (DATA_FIELD_VOC_78.equals(fieldName)) {
                m_vocational78Position = pos;
            } else if (DATA_FIELD_VOC_78_ALE_Q.equals(fieldName)) {
                m_vocational78AleQPosition = pos;
            } else if (DATA_FIELD_VOC_78_ALE_NQ.equals(fieldName)) {
                m_vocational78AleNQPosition = pos;
            } else if (DATA_FIELD_VOC_912.equals(fieldName)) {
                m_vocationalHighPosition = pos;
            } else if (DATA_FIELD_VOC_912_ALE_Q.equals(fieldName)) {
                m_vocationalHighAleQPosition = pos;
            } else if (DATA_FIELD_VOC_912_ALE_NQ.equals(fieldName)) {
                m_vocationalHighAleNQPosition = pos;
            } else if (DATA_FIELD_SKILLS_CENTER.equals(fieldName)) {
                m_skillsCenterPosition = pos;
            } else if (DATA_FIELD_SKILLS_CENTER_ALE_Q.equals(fieldName)) {
                m_skillsCenterAleQPosition = pos;
            } else if (DATA_FIELD_SKILLS_CENTER_ALE_NQ.equals(fieldName)) {
                m_skillsCenterAleNQPosition = pos;
            } else if (DATA_FIELD_K12.equals(fieldName)) {
                m_gradeK12Position = pos;
            } else if (DATA_FIELD_HEAD_COUNT.equals(fieldName)) {
                m_headCountPosition = pos;
            } else if (DATA_FIELD_HIGH_POVERTY.equals(fieldName)) {
                m_highPovertyPosition = pos;
            } else if (DATA_FIELD_HIGH_POVERTY_REG.equals(fieldName)) {
                m_highPovertyRegPosition = pos;
            } else if (DATA_FIELD_REMOTE.equals(fieldName)) {
                m_remotePosition = pos;
            } else if (DATA_FIELD_ALE_Q.equals(fieldName)) {
                m_aleQPosition = pos;
            } else if (DATA_FIELD_ALE_NQ.equals(fieldName)) {
                m_aleNQPosition = pos;
            } else if (DATA_FIELD_ELL_PROGRAMS_EXITED.equals(fieldName)) {
                m_ellExitedPosition = pos;
            } else if (DATA_FIELD_OPEN_DOORS.equals(fieldName)) {
                m_openDoorsPosition = pos;
            } else if (DATA_FIELD_OPEN_DOORS_VOC.equals(fieldName)) {
                m_openDoorsVocPosition = pos;
            } else if (DATA_FIELD_REPORT_DATE.equals(fieldName)) {
                m_reportDatePosition = pos;
            } else if (DATA_FIELD_SCHOOL_NAME.equals(fieldName)) {
                m_schoolNameCache = pos;
            } else if (DATA_FIELD_STD_STATE.equals(fieldName)) {
                m_stdStateId = pos;
            } else if (DATA_FIELD_STD_LOCAL.equals(fieldName)) {
                m_stdLocalId = pos;
            }

        }
        if ((m_countyHomePosition < 0) ||
                (m_countyServePosition < 0) ||
                (m_districtHomePosition < 0) ||
                (m_districtHomeCodePosition < 0) ||
                (m_districtServePosition < 0) ||
                (m_districtServeCodePosition < 0) ||
                (m_districtServeESDPosition < 0) ||
                (m_gradeLevelPosition < 0) ||
                (m_bilingualPosition < 0) ||
                (m_collegeRSPosition < 0) ||
                (m_vocationalRSPosition < 0) ||
                (m_nonVocationalRSPosition < 0) ||
                (m_vocational78Position < 0) ||
                (m_vocational78AleQPosition < 0) ||
                (m_vocational78AleNQPosition < 0) ||
                (m_vocationalHighPosition < 0) ||
                (m_vocationalHighAleQPosition < 0) ||
                (m_vocationalHighAleNQPosition < 0) ||
                (m_skillsCenterPosition < 0) ||
                (m_stdNameView < 0) ||
                (m_stdStateId < 0) ||
                (m_skillsCenterAleQPosition < 0) ||
                (m_skillsCenterAleNQPosition < 0) ||
                (m_gradeK12Position < 0) ||
                (m_highPovertyPosition < 0) ||
                (m_highPovertyRegPosition < 0) ||
                (m_remotePosition < 0) ||
                (m_aleQPosition < 0) ||
                (m_ellExitedPosition < 0) ||
                (m_openDoorsPosition < 0) ||
                (m_openDoorsVocPosition < 0) ||
                (m_aleNQPosition < 0) ||
                (m_reportDatePosition < 0) ||
                (m_schoolNameCache < 0)) {
            return false;
        }
        return true;
    }

    /**
     * Safely convert a string formatted number into a double
     *
     *
     * @param value String
     * @return double
     */
    private double numericValue(String value) {
        double numeric = 0.0d;
        try {
            if (!StringUtils.isEmpty(value)) {
                numeric = Double.parseDouble(value);
            }
        } catch (NumberFormatException nfe) {
            // No action, return the zero.
        }
        return numeric;
    }

    /**
     * Appends the DistrictData elements to the grid for output.
     *
     * @param dataGrid DataGrid
     * @param reportSchoolName String
     */
    private void outputGridDistrictLvl(DataGrid dataGrid, String reportSchoolName) {
        for (String districtName : new TreeSet<String>(m_districtGradeDataMap.keySet())) {
            dataGrid.append();
            dataGrid.set(FIELD_DISTRICT_MAP, m_districtGradeDataMap.get(districtName).getSingletonsMap());
            dataGrid.set(FIELD_DISTRICT_COUNTS, m_districtGradeDataMap.get(districtName).getCountsMap());
            dataGrid.set(FIELD_DISTRICT_FTES, m_districtGradeDataMap.get(districtName).getFtesMap());
            dataGrid.set(FIILD_SCHOOL_NAME, reportSchoolName);
        }
    }

    /**
     * Appends the DistrictData elements to the grid for output.
     *
     * @param dataGrid DataGrid
     */
    private void outputGridSchoolLvl(DataGrid dataGrid) {

        for (String schoolName : new TreeSet<String>(m_schoolGradeDataMap.keySet())) {
            dataGrid.append();
            dataGrid.set(FIELD_DISTRICT_MAP, m_schoolGradeDataMap.get(schoolName).getSingletonsMap());
            dataGrid.set(FIELD_DISTRICT_COUNTS, m_schoolGradeDataMap.get(schoolName).getCountsMap());
            dataGrid.set(FIELD_DISTRICT_FTES, m_schoolGradeDataMap.get(schoolName).getFtesMap());
            dataGrid.set(FIILD_SCHOOL_NAME, schoolName);
        }
    }

    /**
     * Loads schools with excluded indicator set to true.
     */
    private void loadSchoolExcludeMap() {
        String schoolExclude = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_SCHOOL);
        if (field != null) {
            schoolExclude = field.getJavaName();
        }

        if (!StringUtils.isEmpty(schoolExclude)) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
            BeanQuery query = new BeanQuery(School.class, schoolCriteria);
            m_excludeSchool = getBroker().getGroupedCollectionByQuery(query, SisSchool.COL_NAME, 128);

        }
    }
}
