/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateFiscalYear;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;


/**
 * The Class FLIndustryCertificationValidation.
 */
public class FLIndustryCertificationValidation {

    DateAsStringConverter m_dateConverter;


    /**
     * Instantiates a new FL industry certification validation.
     */
    public FLIndustryCertificationValidation() {
        m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("CAPEIC", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("CAPEIC", "2", new ValidateRegularExpression("School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89])$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001, or it must be N998 or N999.")),
                new FLValidationRule("CAPEIC", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("CAPEIC", "4", new ValidateRegularExpression("Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission specified by the district.")),
                new FLValidationRule("CAPEIC", "5", new ValidateFiscalYear()),
                new FLValidationRule("CAPEIC", "6", new ValidateRegularExpression("District Number CIS",
                        "^0[1-9]|[1-6][0-8]|7[1-5]$",
                        "District Number, Current Instruction/Service must be numeric in the range 01-68 or 71-75 "
                                + "and must be correct for the district submitting the data.")),
                new FLValidationRule("CAPEIC", "7", new ValidateRegularExpression("School Number CIS",
                        "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3}|(?!99|0000|9001)\\d{4})$",
                        "The School Number, Current Instruction/Service must be numeric in the range 0001-9899, excluding 9001, "
                                + "or it must be C901-C928, U970-U981, or P001-P999.")),
                new FLValidationRule("CAPEIC", "8", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-industry-cert-id",
                                        "Industry Cert Id"))),
                        "Industry Certification Identifier must exist on Appendix Z of the FDOE Information Database Requirements: "
                                + "Volume I -- Automated Student Information System Manual.")),
                new FLValidationRule("CAPEIC", "9", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("Industry Cert Id", "CAPEI001"))
                                .testThen(Restriction.pattern("Industry Cert Outcom", "^(A|C|F|I|P|Z)$")),
                        ValidationRule.testIf(Restriction.equals("Industry Cert Id", "CAPEI001"))
                                .testThen(Restriction.pattern("Industry Cert Outcom", "^(P|Z)$"))),
                        "Industry Certification Outcome must be P or F only unless Industry Certification Identifier is CAPEI001 "
                                + "then the Industry Certification Outcome may be A, C, F, I, P or Z.")),
                new FLValidationRule("CAPEIC", "10", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(KG|0[1-5])$"))
                                .testThen(Restriction.equals("Course Number", "0000000")),
                        ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(0[6-8])$"),
                                Restriction.pattern("Industry Cert Id", "^\\S{5}8\\S{2}$"))
                                .testThen(Restriction.or(
                                        Restriction.pattern("Course Number", "0000000"),
                                        Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(0[6-8])$"),
                                Restriction.pattern("Industry Cert Id", "^\\S{5}(?!8)\\S{3}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                        ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(09|1[0-2])$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                        ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(C|U)\\S{1,3}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        "Course Number must not contain blanks. If School Number, Current Instruction/Service does not begin with C, P or U; then"
                                + "If Grade Level is KG-05, the Course Number must be 0000000; or"
                                + "If Grade Level is 06-08 and position 6 of the Industry Certification Identifier is 8 the Course Number must be 0000000,"
                                + " or must be a valid Course Number in the Course Code Directory for the School Year - Course Taken; or "
                                + "If Grade Level is 06-08 and position 6 of the Industry Certification Identifier is not 8,"
                                + " the Course Number must be a valid Course Number in the Course Code Directory for the School Year - Course Taken; or "
                                + "If Grade Level is 09-12, the Course Number must be a valid Course Number in the Course Code Directory for the School Year - Course Taken."
                                + "If School Number, Current Instruction/Service begins with C or U, then the Course Number must be on the Statewide Course Numbering "
                                + "System file for the School Year - Course Taken.")),
                new FLValidationRule("CAPEIC", "11", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "0000000"),
                                Restriction.pattern("Grade Level", "^(KG|0[1-8])$"))
                                .testThen(Restriction.equals("Cape Program Code", "0000000"))
                // TODO add correct ref table for Cape Program Code
                // ,ValidationRule.testIf(Restriction.pattern("Course Number",
                // "^(?!0000000|1006300|2001310|2001340|2003310|2102360|2102365|2102370|3027010|3027020|2000350|2000360)\\d\\S{6}$"))
                // .testThen(Restriction.byExtFldRefTable("FL-PGM-CAPE",
                // "pgm-cape-district", "Cape Program Code")),
                // ValidationRule.testIf(Restriction.pattern("Course Number",
                // "^[A-Z]{3}0\\S{3}$"),
                // Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "Course
                // Number"))
                // .testThen(Restriction.byExtFldRefTable("FL-PGM-CAPE",
                // "pgm-cape-district",
                // "Cape Program Code")),
                // ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]\\S{6}$"),
                // Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "Course
                // Number"))
                // .testThen(Restriction.or(
                // Restriction.equalsFieldValue("Cape Program Code", "Course Number",
                // String.class),
                // Restriction.equals("Cape Program Code", "0000000")))
                ),
                        "If Course Number equals 0000000 and Grade Level is KG-08, then the Career and Technical Education/Adult General Education Program Code must be zero."
                                + "If Course Number begins with a number; and"
                                + " Is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730); and"
                                + " Is not 0000000, 1006300, 2001310, 2001340, 2003310, 2102360, 2102365, 2102370, 3027010, 3027020, 2000350, or 2000360;"
                                + "Then the Career and Technical Education/Adult General Education Program Code must be a valid program number for the Course Number submitted, "
                                + "as listed in file F61730 for School Year - Course Taken."
                                + "If the Course Number begins with three alphabetic characters followed by a zero in the fourth position, and"
                                + " Is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730);"
                                + "Then the Career and Technical Education/Adult General Education Program Code must be a valid program number for the Course Number submitted, "
                                + "as listed in file F61730 for School Year - Course Taken."
                                + "If Course Number begins with one alphabetic character, and"
                                + " Is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730);"
                                + "Then the Career and Technical Education/Adult General Education Program Code must be the same as the Course Number for School Year - Course Taken."
                                + "Otherwise the Career and Technical Education/Adult General Education Program Code must be zero.")),
                new FLValidationRule("CAPEIC", "12", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Cource School Year", "0708"),
                                        Restriction.lessThanOrEqualsFieldValue("Cource School Year", "Fiscal Year",
                                                String.class)))),
                        "The School Year - Course Taken must be a valid school year greater than or equal to 0708 and less than or equal to the current school year.")),
                new FLValidationRule("CAPEIC", "13", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Industry Cert Id", "^\\S{5}8\\S{3}$"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-8])$"))),
                        "If position 6 of Industry Certification Identifier is 8 (Type = 3 on Appendix Z), then Grade Level must be KG-08.")),
                new FLValidationRule("CAPEIC", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Industry Cert Id", "^\\S{5}8\\S{3}$"))
                                .testThen(Restriction.equalsFieldValue("Cource School Year", "Fiscal Year",
                                        String.class))),
                        "If position 6 of the Industry Certification Identifier is 8 (Type = 3 on Appendix Z) "
                                + "then the School Year - Course Taken must be equal to the School Year - Record Submission.")),
                new FLValidationRule("CAPEIC", "15",
                        new ValidateRegularExpression("Student ID Local", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "The Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                new FLValidationRule("CAPEIC", "16", new ValidateRegularExpression(
                        "Grade Level", "^(KG|0[1-9]|1[0-2])$", "Grade Level must be KG-12.")),
                new FLValidationRule("CAPEIC", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("Student Number", "District Number CIS",
                                        "School Number CIS", "Survey Period", "Fiscal Year",
                                        "Course Number", "Industry Cert Id", "Industry Cert Outcom"))),
                        "Each record must be unique based on Student Number Identifier, Florida; School Year - Record Submission; "
                                + "District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Course Number; Industry Certification Identifier; Industry Certification Outcome and Survey Period Code.")),
                new FLValidationRule("CAPEIC", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number",
                                "^(?!99[89])\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not N998 or N999, then it must exist on the "
                                + "Master School Identification File as a valid active number in the District Number, Current Enrollment.")),
                new FLValidationRule("CAPEIC", "20", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number CIS",
                                "^(?!C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|P000|P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.byActiveSchool("School Number CIS"))),
                        "If the School Number, Current Instruction/Service is not C901-C928, U970-U981, or P001-P999, then it must exist as "
                                + "a valid active number in the District Number, Current Instruction/Service on the Master School Identification File.")),
                new FLValidationRule("CAPEIC", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-industry-cert",
                                "Industry Cert Id"))
                                .testThen(Restriction.pattern("Grade Level", "^(0[1-6]|1[0-2])$"))),
                        "If the Industry Certification Identifier on Appendix Z is indicated to be a CAPE Industry Certification (Type = 1) "
                                + "or CAPE Acceleration Industry Certification (Type = 2), then Grade Level must be 06-12.")),
                new FLValidationRule("CAPEIC", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!P000|P\\d{3})\\S{1,4}$"),
                                Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-industry-cert",
                                        "Industry Cert Id"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        "If the Industry Certification Identifier on Appendix Z is indicated to be a CAPE Industry Certification (Type = 1) or "
                                + "CAPE Acceleration Industry Certification (Type = 2), then Course Number must be a valid number on the Course Code Directory or "
                                + "the Statewide Course Numbering System file for the School Year - Course Taken, unless School Number, Current Instruction/Service equals P001-P999.")),
                new FLValidationRule("CAPEIC", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(
                                        Restriction.pattern("Cape Id", "ZZZ"),
                                        Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-identifier",
                                                "Cape Id")))),
                        "Career and Professional Academy Identifier must exist on Appendix Y of the FDOE Information Database Requirements: "
                                + "Volume I -- Automated Student Information System Manual as a valid number for the District Number, Current Instruction/Service for the "
                                + "School Year - Course Taken or it must be ZZZ.")),
                // TODO add correct ref table for Cape Program Code
                // new FLValidationRule("CAPEIC", "24", new FLValidationRuleSet(new RuleSet(
                // ValidationRule.testIf(Restriction.alwaysTrue())
                // .testThen(Restriction.or(
                // Restriction.pattern("Cape Program Code", "0000000"),
                // Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district",
                // "Cape Program Code")))),
                // "The Career and Technical Education/Adult General Education Program Code must be
                // a program number from the Career and Technical Education/Adult "
                // + "General Education Program Edit file (F61730) for School Year - Course Taken or
                // must be zero-filled.")),
                new FLValidationRule("CAPEIC", "25", new ValidateRegularExpression("Fl Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("CAPEIC", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Industry Cert Id", "CAPEI001"))
                                .testThen(Restriction.pattern("Course Number", "^(2102365|1001425|0109355|0200325)$"))),
                        "If the Industry Certification Identifier is CAPEI001, then Course Number must be 2102365, 1001425, 0109355, or 0200325.")),
                new FLValidationRule("CAPEIC", "27",
                        new ValidateRegularExpression("Dual Enrollment Type", "^([A-D]|Z)$",
                                "Dual Enrollment Institution Type must be A, B, C, D or Z.")),
                new FLValidationRule("CAPEIC", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]\\S{6}$"))
                                .testThen(Restriction.pattern("Dual Enrollment Type", "^[A-D]$"))),
                        "If Course Number begins with an alphabetic character then Dual Enrollment Institution Type must be A, B, C or D.")),
                new FLValidationRule("CAPEIC", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number CIS", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Industry Certification record must have a matching Student Demographic Information record based on "
                                + "District Number, Current Instruction; Student Number Identifier, Florida; Survey Period Code; and Year.")),
                new FLValidationRule("CAPEIC", "41", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number",
                                "^(?!0000000|1006300|2001310|2001340|2003310|2102360|2102365|2102370|3027010|3027020|2000350|2000360)\\d\\S{6}$"),
                                Restriction.equalsFieldValue("Cource School Year", "Fiscal Year", String.class)
                        // TODO add correct ref table for Cape Program Code
                        // ,
                        // Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "Cape
                        // Program Code")
                        )
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-CTESSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number CIS", "Instruct District"),
                                        new KeyValuePair("School Number CIS", "Instruct School"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Cape Program Code", "CTE Program Code"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Course Number begins with a number and school year-course taken = school year-record submitted; and"
                                + " Is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730) for the School Year - Course Taken; and"
                                + " Is not 0000000, 1006300, 2001310, 2001340, 2003310, 2102360, 2102365, 2102370, 3027010, 3027020, 2000350, or 2000360;"
                                + "Then there must be a matching record on the Career and Technical Education Student Course Schedule format based on "
                                + "Student Number Identifier, Florida; District Number, Current Instruction; School Number, Current Instruction; "
                                + "Survey Period Code; Career and Technical Education/Adult General Education Program Code; Course Number; School Year")),
                new FLValidationRule("CAPEIC", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number CIS", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Grade Level", "Grade Level")))),
                        "The student's Grade Level code on the Industry Certification record must agree with the Grade Level code on the Student Demographic record submitted by "
                                + "the district of instruction. Matched based on District Number, Current Instruction; Student Number Identifier, Florida; Survey Period Code; and Year.")),
        });
    }
}
