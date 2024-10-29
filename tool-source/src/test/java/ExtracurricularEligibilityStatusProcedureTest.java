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
import static com.follett.fsc.core.k12.test.AspenIntegrationTestManager.getBroker;
import static com.follett.fsc.core.k12.test.AspenIntegrationTestManager.getUser;
import static com.follett.fsc.core.k12.test.AspenIntegrationTestManager.getUserDataContainer;
import static com.follett.fsc.core.k12.test.AspenIntegrationTestManager.saveTemporaryBeanForced;
import static com.follett.fsc.core.k12.test.AspenIntegrationTestManager.saveTemporaryBeanWithErrorOutput;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.test.AspenIntegrationTestRule;
import com.follett.fsc.core.k12.test.BeanStorageHelperStaticInit;
import com.follett.fsc.core.k12.test.TestUtils;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolInputParameter;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.expressions.ConditionExpression;
import com.x2dev.sis.model.beans.ExcParticipationActivity;
import com.x2dev.sis.model.beans.ExtracurricularCriteria;
import com.x2dev.sis.model.beans.ExtracurricularProgram;
import com.x2dev.sis.model.beans.ExtracurricularSchoolProgram;
import com.x2dev.sis.model.beans.ExtracurricularStudent;
import com.x2dev.sis.model.business.criteriaCategory.CriteriaCategory.Category;
import com.x2dev.sis.model.business.extracurricular.ExtracurricularCodeHelper;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

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

/**
 * @author Follett Software Company
 * @copyright 2018
 */
@RunWith(Enclosed.class)
public class ExtracurricularEligibilityStatusProcedureTest {

    //@formatter:off
    public static UserDataContainer s_userData;
    public static X2Broker s_broker;
    static ExtracurricularEligibilityStatusProcedure proc;
    static ExtracurricularProgram districtProgram, districtProgram_A, districtProgram_B, districtProgram_C;
    static ExtracurricularSchoolProgram schoolProgram, schoolProgram2, schoolProgram_A, schoolProgram_B, schoolProgram_C;
    static ExtracurricularStudent studentMemberA, studentMemberB, studentMemberC, studentMemberD, studentMemberE;
    static ExtracurricularStudent studentMemberF, studentMemberG, studentMemberH, studentMemberI, studentMemberJ;
    static ExtracurricularCriteria criteria1;
    static ExtracurricularCriteria criteria2;
    static DistrictSchoolYearContext schoolYear;
    static Student studentA, studentB, studentC, studentD, studentE, studentF, studentG, studentH, studentI, studentJ;
    static School school;
    static final String eligibleCode = ExtracurricularCodeHelper.ExtracurricularLocalCode.ELIGIBILITY_ELIGIBLE.getLocalCode();
    static final String ineligibleCode = ExtracurricularCodeHelper.ExtracurricularLocalCode.ELIGIBILITY_INELIGIBLE.getLocalCode();
    static final String pendingCode = ExtracurricularCodeHelper.ExtracurricularLocalCode.ELIGIBILITY_PENDING.getLocalCode();
    static final String activityAddedCode = ExtracurricularCodeHelper.ExtracurricularLocalCode.ACTIVITY_ADDED.getLocalCode();
    static final String defaultStatus = ExtracurricularCodeHelper.ExtracurricularLocalCode.STATUS_DEFAULT.getLocalCode();

    static Procedure eligibilityProcedure;
    static ToolSourceCode procSourceCode;
    static String athletics, culinary, vball, soccer, pastry, chef;


    //@formatter:on

    @ClassRule
    public static AspenIntegrationTestRule aspenIntegrationTestRule =
    new AspenIntegrationTestRule(ExtracurricularEligibilityStatusProcedureTest.class);


    @BeforeClass
    public static void dataSetup() throws Exception {
        s_userData = getUserDataContainer();
        s_broker = getBroker();

        proc = new ExtracurricularEligibilityStatusProcedure() {
            @Override
            protected X2Broker getBroker() {
                return s_broker;
            }
        };

        // We have to create some Programs first
        schoolYear = BeanStorageHelperStaticInit.createSchoolYear(s_broker);
        school = BeanStorageHelperStaticInit.createSchool(s_broker, schoolYear);

        // Note, the hard coded OIDs are in the REFCODE.TXT and are part of the defaults
        athletics = "rcdX2F04008530";
        culinary =
                BeanStorageHelperStaticInit.createReferenceCode(s_broker, "rtbExcPgmType", school.getOid(),
                        Ownable.OWNER_TYPE_SCHOOL, "Culinary", "Culinary", false).getOid();
        vball = "rcdX2F04008558";
        soccer = "rcdX2F04008551";
        pastry =
                BeanStorageHelperStaticInit.createReferenceCode(s_broker, "rtbExcSubType", school.getOid(),
                        Ownable.OWNER_TYPE_SCHOOL, "Pastry", "Pastry", false).getOid();
        chef =
                BeanStorageHelperStaticInit.createReferenceCode(s_broker, "rtbExcSubType", school.getOid(),
                        Ownable.OWNER_TYPE_SCHOOL, "Chef", "Chef", false).getOid();

        // Create a school program
        districtProgram =
                BeanStorageHelperStaticInit.createExtracurricularProgram(s_broker, "junitId", "districtProgram",
                        schoolYear);
        districtProgram.setType("Athletics");
        districtProgram.setSubType("Soccer");
        saveTemporaryBeanWithErrorOutput(s_broker, districtProgram);

        districtProgram_A =
                BeanStorageHelperStaticInit.createExtracurricularProgram(s_broker, "junitIdA", "districtProgramA",
                        schoolYear);
        districtProgram_A.setType("Athletics");
        districtProgram_A.setSubType("Volleyball");
        saveTemporaryBeanWithErrorOutput(s_broker, districtProgram_A);

        districtProgram_B =
                BeanStorageHelperStaticInit.createExtracurricularProgram(s_broker, "junitIdB", "districtProgramB",
                        schoolYear);
        districtProgram_B.setType("Culinary");
        districtProgram_B.setSubType("Chef");
        saveTemporaryBeanWithErrorOutput(s_broker, districtProgram_B);

        districtProgram_C =
                BeanStorageHelperStaticInit.createExtracurricularProgram(s_broker, "junitIdC", "districtProgramC",
                        schoolYear);
        districtProgram_C.setType("Culinary");
        districtProgram_C.setSubType("Pastry");
        saveTemporaryBeanWithErrorOutput(s_broker, districtProgram_C);

        School school2 = BeanStorageHelperStaticInit.createSchool(s_broker, schoolYear);
        schoolProgram =
                BeanStorageHelperStaticInit.createExtracurricularSchoolProgram(s_broker, districtProgram, school);
        schoolProgram2 =
                BeanStorageHelperStaticInit.createExtracurricularSchoolProgram(s_broker, districtProgram, school2);
        schoolProgram_A =
                BeanStorageHelperStaticInit.createExtracurricularSchoolProgram(s_broker, districtProgram_A, school);
        schoolProgram_B =
                BeanStorageHelperStaticInit.createExtracurricularSchoolProgram(s_broker, districtProgram_B, school);
        schoolProgram_C =
                BeanStorageHelperStaticInit.createExtracurricularSchoolProgram(s_broker, districtProgram_C, school);

        //@formatter:off
        studentA = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentA"), school);
        studentB = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentB"), school);
        studentC = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentC"), school);
        studentD = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentD"), school);
        studentE = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentE"), school);
        studentF = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentF"), school, "NOTActive");
        studentG = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentG"), school, "NOTActive");
        studentH = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentH"), school, "NOTActive");
        studentI = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentI"), school, "NOTActive");
        studentJ = BeanStorageHelperStaticInit.createStudent(s_broker, BeanStorageHelperStaticInit.createPerson(s_broker, "studentJ"), school, "NOTActive");

        //@formatter:on
        try {
            // Create the procedure
            eligibilityProcedure = X2BaseBean.newInstance(Procedure.class, getUser().getPersistenceKey());
            eligibilityProcedure.setId("EC_ELIG");
            eligibilityProcedure.setOrganization1Oid("*dst");
            saveTemporaryBeanWithErrorOutput(s_broker, eligibilityProcedure);

            // Create the inputDef
            InputStream xml_source = ExtracurricularEligibilityStatusProcedureTest.class.getResourceAsStream("/ExtracurricularEligibilityStatusProcedureInput.xml");
            assertNotNull("Cannot find ExtracurricularEligibilityStatusProcedureTest.xml on path", xml_source);

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            StreamUtils.copyStream(xml_source, outputStream);

            // Create the toolSourceCode
            procSourceCode = X2BaseBean.newInstance(ToolSourceCode.class, getUser().getPersistenceKey());
            procSourceCode.setInputDefinition(outputStream.toString());
            saveTemporaryBeanForced(s_broker, procSourceCode);
            eligibilityProcedure.setSourceCodeOid(procSourceCode.getOid());
            saveTemporaryBeanWithErrorOutput(s_broker, eligibilityProcedure);

        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    @AfterClass
    public static void dataTeardown() throws Exception {
        // clear all fields for garbage collection
    }

    @Ignore("Shared class")
    public static class Shared {
        @Before
        public void setup() {
            studentMemberA = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentA,
                    "studentAComment", defaultStatus, pendingCode);
            studentMemberB = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentB,
                    "studentBComment", defaultStatus, eligibleCode);
            studentMemberC = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentC,
                    "studentCComment", defaultStatus, pendingCode);
            studentMemberD = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentD,
                    "studentDComment", defaultStatus, ineligibleCode);
            studentMemberE = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentE,
                    "studentEComment", defaultStatus, eligibleCode);
            studentMemberF = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentF,
                    "studentFComment", defaultStatus, pendingCode);
            studentMemberG = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentG,
                    "studentGComment", defaultStatus, eligibleCode);
            studentMemberH = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentH,
                    "studentHComment", defaultStatus, pendingCode);
            studentMemberI = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentI,
                    "studentIComment", defaultStatus, ineligibleCode);
            studentMemberJ = BeanStorageHelperStaticInit.createExtracurricularStudent(s_broker, schoolProgram, studentJ,
                    "studentJComment", defaultStatus, eligibleCode);

            proc.clearProcedureMessages();
            proc.extracurrSchoolProgs.clear();
            proc.clearProcedureParameters();

            criteria1 =
                    BeanStorageHelperStaticInit.createExtracurricularCriteria(s_broker, districtProgram, "Test1",
                            Category.SQL.getId(), "This is message 1", "where STD_ENROLLMENT_STATUS='Active'");

            setCurrentCriteriaOnToolJavaSource(new X2Criteria());
        }

        @After
        public void tearDown() {
            try {
                s_broker.deleteByQuery(new QueryByCriteria(ExtracurricularStudent.class, new Criteria()));
                s_broker.deleteByQuery(new QueryByCriteria(ExtracurricularCriteria.class, new Criteria()));
            } catch (Exception err) {
                LoggerUtils.getLogger().severe("Unable to clean up tests" + err);
                err.printStackTrace();
            }
        }

        public void setCurrentCriteriaOnToolJavaSource(X2Criteria criteria) {
            try {
                Field currentCriteriaField =
                        ToolJavaSource.class.getDeclaredField("m_currentCriteria");
                currentCriteriaField.setAccessible(true);
                currentCriteriaField.set(proc, criteria);
            } catch (Exception e) {
                fail(e.getMessage());
            }
        }
    }

    public static class ToolInputValidation extends Shared {

        @Test
        public void validateToolInputsFor_distrcit() throws X2BaseException {

            s_userData.openRootOrganizationContext();
            TestUtils.setCurrentNode("tools.prc.list.detail", false, null, s_userData);
            s_userData.setToolInput(new ToolInput(eligibilityProcedure, null, s_userData, Locale.getDefault()));

            ToolInput input = s_userData.getToolInput();

            // Validate these are the 5 we expect
            Assert.assertEquals(5, input.getElements().size());
            ToolInputParameter schoolOid = (ToolInputParameter) input.getElement("schoolOid");
            assertNotNull(schoolOid);
            ToolInputParameter queryBy = (ToolInputParameter) input.getElement("queryBy");
            assertNotNull(queryBy);
            ToolInputParameter programType = (ToolInputParameter) input.getElement("programType");
            assertNotNull(programType);
            ToolInputParameter programSubType = (ToolInputParameter) input.getElement("programSubType");
            assertNotNull(programSubType);

            // Then we can get the conditions and assert the boolean
            Assert.assertTrue(ConditionExpression.evaluateDisplay(schoolOid.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(queryBy.getConditions(), s_userData));
            Assert.assertTrue(ConditionExpression.evaluateDisplay(programType.getConditions(), s_userData));
            Assert.assertTrue(ConditionExpression.evaluateDisplay(programSubType.getConditions(), s_userData));

        }

        @Test
        public void validateToolInputsFor_schoolProgramList() throws X2BaseException {

            s_userData.openContext(ApplicationContext.SCHOOL, school.getOid(), false);
            TestUtils.setCurrentNode("extracurricular.eck.list", false, null, s_userData);
            s_userData.setToolInput(new ToolInput(eligibilityProcedure, null, s_userData, Locale.getDefault()));

            ToolInput input = s_userData.getToolInput();

            // Validate these are the 5 we expect
            Assert.assertEquals(5, input.getElements().size());
            ToolInputParameter schoolOid = (ToolInputParameter) input.getElement("schoolOid");
            assertNotNull(schoolOid);
            ToolInputParameter queryBy = (ToolInputParameter) input.getElement("queryBy");
            assertNotNull(queryBy);
            ToolInputParameter programType = (ToolInputParameter) input.getElement("programType");
            assertNotNull(programType);
            ToolInputParameter programSubType = (ToolInputParameter) input.getElement("programSubType");
            assertNotNull(programSubType);

            // Then we can get the conditions and assert the boolean
            Assert.assertTrue(ConditionExpression.evaluateDisplay(schoolOid.getConditions(), s_userData));
            Assert.assertTrue(ConditionExpression.evaluateDisplay(queryBy.getConditions(), s_userData));
            Assert.assertTrue(ConditionExpression.evaluateDisplay(programType.getConditions(), s_userData));
            Assert.assertTrue(ConditionExpression.evaluateDisplay(programSubType.getConditions(), s_userData));

        }

        @Test
        public void validateToolInputsFor_schoolProgramDetail() throws X2BaseException {
            s_userData.openContext(ApplicationContext.SCHOOL, school.getOid(), false);
            TestUtils.setCurrentNode("extracurricular.eck.list.detail", false, schoolProgram, s_userData);
            s_userData.setToolInput(new ToolInput(eligibilityProcedure, null, s_userData, Locale.getDefault()));

            ToolInput input = s_userData.getToolInput();

            // Validate these are the 5 we expect
            Assert.assertEquals(5, input.getElements().size());
            ToolInputParameter schoolOid = (ToolInputParameter) input.getElement("schoolOid");
            assertNotNull(schoolOid);
            ToolInputParameter queryBy = (ToolInputParameter) input.getElement("queryBy");
            assertNotNull(queryBy);
            ToolInputParameter programType = (ToolInputParameter) input.getElement("programType");
            assertNotNull(programType);
            ToolInputParameter programSubType = (ToolInputParameter) input.getElement("programSubType");
            assertNotNull(programSubType);

            // Then we can get the conditions and assert the boolean
            Assert.assertTrue(ConditionExpression.evaluateDisplay(schoolOid.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(queryBy.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(programType.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(programSubType.getConditions(), s_userData));
        }

        @Test
        public void validateToolInputsFor_schoolProgramStudentList() throws X2BaseException {
            s_userData.openContext(ApplicationContext.SCHOOL, school.getOid(), false);
            TestUtils.setCurrentNode("extracurricular.eck.list.students", false, schoolProgram, s_userData);
            s_userData.setToolInput(new ToolInput(eligibilityProcedure, null, s_userData, Locale.getDefault()));

            ToolInput input = s_userData.getToolInput();

            // Validate these are the 5 we expect
            Assert.assertEquals(5, input.getElements().size());
            ToolInputParameter schoolOid = (ToolInputParameter) input.getElement("schoolOid");
            assertNotNull(schoolOid);
            ToolInputParameter queryBy = (ToolInputParameter) input.getElement("queryBy");
            assertNotNull(queryBy);
            ToolInputParameter programType = (ToolInputParameter) input.getElement("programType");
            assertNotNull(programType);
            ToolInputParameter programSubType = (ToolInputParameter) input.getElement("programSubType");
            assertNotNull(programSubType);

            // Then we can get the conditions and assert the boolean
            Assert.assertTrue(ConditionExpression.evaluateDisplay(schoolOid.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(queryBy.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(programType.getConditions(), s_userData));
            Assert.assertFalse(ConditionExpression.evaluateDisplay(programSubType.getConditions(), s_userData));
        }
    }

    public static class PopulateSchoolProgramList extends Shared {

        @Override
        @Before
        public void setup() {
            super.setup();
            setSchool(school);
        }

        @Test
        public void currentCriteria_school() throws Exception {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, school.getOid());
            setCurrentCriteriaOnToolJavaSource(schoolCriteria);

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);

            proc.populateSchoolProgramList();

            Assert.assertEquals(0, proc.extracurrSchoolProgs.size());
        }

        @Test
        public void nullParams() throws Exception {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, districtProgram.getOid());
            setCurrentCriteriaOnToolJavaSource(schoolCriteria);

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, null);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_SUB_TYPE, null);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, null);

            proc.populateSchoolProgramList();

            Assert.assertEquals(4, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_B));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));

        }

        @Test
        public void currentCriteria_program() throws Exception {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, districtProgram.getOid());
            setCurrentCriteriaOnToolJavaSource(schoolCriteria);

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);

            proc.populateSchoolProgramList();

            Assert.assertEquals(0, proc.extracurrSchoolProgs.size());
        }

        @Test
        public void currentCriteria_null() throws Exception {

            setCurrentCriteriaOnToolJavaSource(null);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);

            proc.populateSchoolProgramList();

            Assert.assertEquals(0, proc.extracurrSchoolProgs.size());

        }

        @Test
        public void listContainsSchools() {
            proc.extracurrSchoolProgs.add(schoolProgram);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics);

            proc.populateSchoolProgramList();

            Assert.assertEquals(1, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
        }

        @Test
        public void queryBy_All_NoType_NoSubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);

            proc.populateSchoolProgramList();

            Assert.assertEquals(4, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_B));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));

        }

        @Test
        public void queryBy_All_Type_NoSubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics);

            proc.populateSchoolProgramList();

            Assert.assertEquals(2, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));

        }

        @Test
        public void queryBy_All_2TypeBadType_NoSubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics + ",abc123");

            proc.populateSchoolProgramList();

            Assert.assertEquals(0, proc.extracurrSchoolProgs.size());

            List<String> messages = proc.getProcedureMessages();
            Assert.assertTrue(messages.contains("Invalid reference codes chosen."));
        }

        @Test
        public void queryBy_All_BadType_NoSubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, "abc123");

            proc.populateSchoolProgramList();

            Assert.assertEquals(0, proc.extracurrSchoolProgs.size());

            List<String> messages = proc.getProcedureMessages();
            Assert.assertTrue(messages.contains("Invalid reference codes chosen."));
        }

        @Test
        public void queryBy_All_2Type_NoSubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics + "," + culinary);

            proc.populateSchoolProgramList();

            Assert.assertEquals(4, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_B));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));

        }

        @Test
        public void queryBy_All_NoType_SubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_SUB_TYPE, pastry);

            proc.populateSchoolProgramList();

            Assert.assertEquals(1, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));

        }

        @Test
        public void queryBy_All_NoType_2SubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_SUB_TYPE, pastry + "," + chef);

            proc.populateSchoolProgramList();

            Assert.assertEquals(2, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_B));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));

        }

        @Test
        public void queryBy_All_Type_SubType() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_SUB_TYPE, soccer);

            proc.populateSchoolProgramList();

            Assert.assertEquals(1, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
        }

        private X2Criteria getSchoolProgramListCriteria() {
            X2Criteria schoolProgramListCriteria = new X2Criteria();

            schoolProgramListCriteria.addEqualTo(ExtracurricularSchoolProgram.REL_SCHOOL + "." +
                    X2BaseBean.COL_OID,
                    school.getOid());
            return schoolProgramListCriteria;
        }

        @Test
        public void queryBy_Selection_NoType_NoSubType() {

            // All programs for school
            setCurrentCriteriaOnToolJavaSource(getSchoolProgramListCriteria());

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);

            proc.populateSchoolProgramList();

            Assert.assertEquals(4, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_B));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));

        }

        @Test
        public void queryBy_Selection_Type_NoSubType() {
            // All programs for school
            setCurrentCriteriaOnToolJavaSource(getSchoolProgramListCriteria());

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics);

            proc.populateSchoolProgramList();

            Assert.assertEquals(2, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));

        }

        @Test
        public void queryBy_Selection_NoType_SubType() {
            // All programs for school
            setCurrentCriteriaOnToolJavaSource(getSchoolProgramListCriteria());

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_SUB_TYPE, pastry);

            proc.populateSchoolProgramList();

            Assert.assertEquals(1, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));
        }

        @Test
        public void queryBy_Selection_Type_SubType() {
            // All programs for school
            setCurrentCriteriaOnToolJavaSource(getSchoolProgramListCriteria());

            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 1);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_TYPE, athletics);
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_PROGRAM_SUB_TYPE, vball);

            proc.populateSchoolProgramList();

            Assert.assertEquals(1, proc.extracurrSchoolProgs.size());

            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));
        }

        @Test
        public void queryWithNoSchool() {
            setParam(ExtracurricularEligibilityStatusProcedure.PARAM_QUERY_BY, 0);
            setSchool(null);

            proc.populateSchoolProgramList();

            Assert.assertEquals(5, proc.extracurrSchoolProgs.size());
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram2));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_A));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_B));
            Assert.assertTrue(proc.extracurrSchoolProgs.contains(schoolProgram_C));
        }

        private void setSchool(School school) {
            try {
                Field currentCriteriaField =
                        ToolJavaSource.class.getDeclaredField("m_school");
                currentCriteriaField.setAccessible(true);
                currentCriteriaField.set(proc, school);
            } catch (Exception e) {
                fail(e.getMessage());
            }
        }

        private void setParam(String param, Object value) {
            try {
                Method addParameterMethod =
                        ToolJavaSource.class.getDeclaredMethod("addParameter", String.class, Object.class);
                addParameterMethod.setAccessible(true);
                addParameterMethod.invoke(proc, param, value);
            } catch (Exception e) {
                fail(e.getMessage());
            }
        }

    }

    public static class RunEligibilityForSchoolProgram extends Shared {

        @Test
        public void nullProgram() throws Exception {
            proc.runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, null);
            List<String> messages = proc.getProcedureMessages();
            Assert.assertEquals(messages.toString(), 0, messages.size());
        }

        @Test
        public void studentList() throws Exception {

            Collection<String> criteriaMessages = Arrays.asList(criteria1.getMessage());

            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addIn(ExtracurricularStudent.COL_STUDENT_OID,
                    Arrays.asList(new String[] {studentA.getOid(), studentB.getOid(), studentC.getOid(),
                            studentD.getOid(), studentE.getOid(), studentF.getOid(), studentG.getOid()}));
            setCurrentCriteriaOnToolJavaSource(studentCriteria);

            proc.dataClass = ExtracurricularStudent.class;
            proc.runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, schoolProgram);
            List<String> messages = proc.getProcedureMessages();

            // There should be 6 eligibility messages and 2 informational messages
            Assert.assertEquals(messages.toString(), 6, messages.size());
            //@formatter:off
            Assert.assertTrue(messages.contains("Student: studentA, studentA became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentB, studentB")); //StudentB is already eligible
            Assert.assertTrue(messages.contains("Student: studentC, studentC became Eligible."));
            Assert.assertTrue(messages.contains("Student: studentD, studentD became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentE, studentE")); //StudentE is already eligible
            Assert.assertTrue(messages.contains("Student: studentF, studentF became Ineligible. This is message 1"));
            Assert.assertFalse(messages.contains("Student: studentG, studentG")); //StudentG is already eligible
            Assert.assertFalse(messages.contains("Student: studentH, studentH"));//not included in list
            Assert.assertFalse(messages.contains("Student: studentI, studentI"));//not included in list
            Assert.assertFalse(messages.contains("Student: studentJ, studentJ")); //not included in list

            validateParticipationActivities(studentMemberA.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberB.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberC.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberD.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberE.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberF.getOid(), ineligibleCode, true,criteriaMessages );
            validateParticipationActivities(studentMemberG.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberH.getOid(), pendingCode, false, null);
            validateParticipationActivities(studentMemberI.getOid(), ineligibleCode, false, null);
            validateParticipationActivities(studentMemberJ.getOid(), eligibleCode, false, null);
            //@formatter:on
        }

        @Test
        public void nullCriteria() throws Exception {
            setCurrentCriteriaOnToolJavaSource(null);

            proc.dataClass = ExtracurricularStudent.class;
            proc.runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, schoolProgram);
            List<String> messages = proc.getProcedureMessages();

            // Information message
            Assert.assertEquals(messages.toString(), 1, messages.size());
        }

        @Test
        public void nonStudentList() throws Exception {
            Collection<String> criteriaMessages = Arrays.asList(criteria1.getMessage());

            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, new QueryByCriteria(Student.class, new X2Criteria()));
            setCurrentCriteriaOnToolJavaSource(studentCriteria);

            proc.dataClass = Student.class;
            proc.runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, schoolProgram);
            List<String> messages = proc.getProcedureMessages();

            // There should be 6 eligibility messages and 2 informational messages
            Assert.assertEquals(messages.toString(), 8, messages.size());
            //@formatter:off
            Assert.assertTrue(messages.contains("Student: studentA, studentA became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentB, studentB")); //StudentB is already eligible
            Assert.assertTrue(messages.contains("Student: studentC, studentC became Eligible."));
            Assert.assertTrue(messages.contains("Student: studentD, studentD became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentE, studentE")); //StudentE is already eligible
            Assert.assertTrue(messages.contains("Student: studentF, studentF became Ineligible. This is message 1"));
            Assert.assertFalse(messages.contains("Student: studentG, studentG")); //StudentG is already eligible
            Assert.assertTrue(messages.contains("Student: studentH, studentH became Ineligible. This is message 1"));
            Assert.assertTrue(messages.contains("Student: studentI, studentI became Ineligible. This is message 1"));
            Assert.assertFalse(messages.contains("Student: studentJ, studentJ")); //StudentJ is already eligible

            validateParticipationActivities(studentMemberA.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberB.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberC.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberD.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberE.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberF.getOid(), ineligibleCode, true,criteriaMessages );
            validateParticipationActivities(studentMemberG.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberH.getOid(), ineligibleCode, true, criteriaMessages);
            validateParticipationActivities(studentMemberI.getOid(), ineligibleCode, true, criteriaMessages);
            validateParticipationActivities(studentMemberJ.getOid(), eligibleCode, false, null);
            //@formatter:on
        }

        @Test
        public void runExecuteValidateActivitesAreCreated() throws Exception {

            Collection<String> criteriaMessages = Arrays.asList(criteria1.getMessage());

            proc.runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, schoolProgram);
            List<String> messages = proc.getProcedureMessages();

            // There should be 6 eligibility messages and 2 informational messages
            Assert.assertEquals(messages.toString(), 8, messages.size());
            //@formatter:off
            Assert.assertTrue(messages.contains("Student: studentA, studentA became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentB, studentB")); //StudentB is already eligible
            Assert.assertTrue(messages.contains("Student: studentC, studentC became Eligible."));
            Assert.assertTrue(messages.contains("Student: studentD, studentD became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentE, studentE")); //StudentE is already eligible
            Assert.assertTrue(messages.contains("Student: studentF, studentF became Ineligible. This is message 1"));
            Assert.assertFalse(messages.contains("Student: studentG, studentG")); //StudentG is already eligible
            Assert.assertTrue(messages.contains("Student: studentH, studentH became Ineligible. This is message 1"));
            Assert.assertTrue(messages.contains("Student: studentI, studentI became Ineligible. This is message 1"));
            Assert.assertFalse(messages.contains("Student: studentJ, studentJ")); //StudentJ is already eligible

            validateParticipationActivities(studentMemberA.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberB.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberC.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberD.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberE.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberF.getOid(), ineligibleCode, true,criteriaMessages );
            validateParticipationActivities(studentMemberG.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberH.getOid(), ineligibleCode, true, criteriaMessages);
            validateParticipationActivities(studentMemberI.getOid(), ineligibleCode, true, criteriaMessages);
            validateParticipationActivities(studentMemberJ.getOid(), eligibleCode, false, null);
            //@formatter:on
        }

        @Test
        public void runExecuteValidateMultipleActivitesAreCreated() throws Exception {
            criteria2 =
                    BeanStorageHelperStaticInit.createExtracurricularCriteria(s_broker, districtProgram, "Test2",
                            Category.SQL.getId(), "This is message 2", "where STD_ENROLLMENT_STATUS='Active'");

            Collection<String> criteriaMessages = Arrays.asList(criteria1.getMessage(), criteria2.getMessage());

            proc.runEligibilityForSchoolProgram(eligibleCode, ineligibleCode, schoolProgram);
            List<String> messages = proc.getProcedureMessages();

            // There should be 9 eligibility messages and 2 informational messages
            Assert.assertEquals(messages.toString(), 11, messages.size());
            //@formatter:off
            Assert.assertTrue(messages.contains("Student: studentA, studentA became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentB, studentB")); //StudentB is already eligible
            Assert.assertTrue(messages.contains("Student: studentC, studentC became Eligible."));
            Assert.assertTrue(messages.contains("Student: studentD, studentD became Eligible."));
            Assert.assertFalse(messages.contains("Student: studentE, studentE")); //StudentE is already eligible
            Assert.assertTrue(messages.contains("Student: studentF, studentF became Ineligible. This is message 2"));
            Assert.assertFalse(messages.contains("Student: studentG, studentG")); //StudentG is already eligible
            Assert.assertTrue(messages.contains("Student: studentH, studentH became Ineligible. This is message 2"));
            Assert.assertTrue(messages.contains("Student: studentI, studentI became Ineligible. This is message 2"));
            Assert.assertFalse(messages.contains("Student: studentJ, studentJ")); //StudentJ is already eligible

            validateParticipationActivities(studentMemberA.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberB.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberC.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberD.getOid(), eligibleCode, true, null);
            validateParticipationActivities(studentMemberE.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberF.getOid(), ineligibleCode, true,criteriaMessages );
            validateParticipationActivities(studentMemberG.getOid(), eligibleCode, false, null);
            validateParticipationActivities(studentMemberH.getOid(), ineligibleCode, true, criteriaMessages);
            validateParticipationActivities(studentMemberI.getOid(), ineligibleCode, true, criteriaMessages);
            validateParticipationActivities(studentMemberJ.getOid(), eligibleCode, false, null);
            //@formatter:on
        }


        private void validateParticipationActivities(String studentMembershipOid,
                                                     String eligiblityStatus,
                                                     boolean procedureMarkedStatus,
                                                     Collection<String> messages) {
            // First load the record again to make sure we have it from the DB
            ExtracurricularStudent studentRecord =
                    (ExtracurricularStudent) s_broker.getBeanByOid(ExtracurricularStudent.class, studentMembershipOid);
            Assert.assertEquals(eligiblityStatus, studentRecord.getEligibilityStatus());

            Collection<ExcParticipationActivity> participationActivities = studentRecord.getParticipationActivities();
            boolean foundAddedRecord = false;
            boolean foundProcedureRecord = false;
            for (ExcParticipationActivity activity : participationActivities) {
                if (activity.getAction().equals(eligiblityStatus)) {
                    if (eligiblityStatus == ineligibleCode) {
                        assertTrue(messages.contains(activity.getComment()));
                    }
                    foundProcedureRecord = true;
                }
                if (activity.getAction().equals(activityAddedCode)) {
                    foundAddedRecord = true;
                }
            }

            // Note: if the procedure marked the status, we should have a procedure record. If we
            // didn't need to update the status for a record, we should NOT have a procedure record
            Assert.assertTrue(foundAddedRecord);
            if (procedureMarkedStatus) {
                Assert.assertTrue(foundProcedureRecord);
            } else {
                Assert.assertFalse(foundProcedureRecord);
            }
        }

        protected Collection getStudentParticipationActivities(String studentMembershipOid) {
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(X2BaseBean.COL_OID, s_broker.getPersistenceKey());
            criteria.addEqualTo(ExcParticipationActivity.COL_EXC_STUDENT_MEMBERSHIP_OID, studentMembershipOid);

            SubQuery subQuery =
                    new SubQuery(ExcParticipationActivity.class, X2BaseBean.COL_OID, criteria);
            subQuery.setDistinct(true);
            return s_broker.getCollectionByQuery(subQuery);
        }
    }

    public static class GetStudentsCriteria extends Shared {

        @Test
        public void invalidCurrentNode() throws Exception {
            proc.dataClass = String.class;
            X2Criteria studentsCriteria = proc.getStudentsCriteria(pendingCode, null);
            Assert.assertNull(studentsCriteria);
            List<String> messages = proc.getProcedureMessages();
            Assert.assertEquals("[Invalid location to run procedure.]", messages.toString());
        }

        @Test
        public void validCurrentNodeNullCriteria() throws Exception {
            proc.dataClass = ExtracurricularStudent.class;
            X2Criteria studentsCriteria = proc.getStudentsCriteria(pendingCode, null);
            Assert.assertNull(studentsCriteria);
            List<String> messages = proc.getProcedureMessages();
            Assert.assertEquals("[Invalid location to run procedure.]", messages.toString());
        }

        @Test
        public void validCurrentNode() throws Exception {

            setCurrentCriteriaOnToolJavaSource(new X2Criteria());
            proc.dataClass = ExtracurricularStudent.class;

            X2Criteria studentsCriteria = proc.getStudentsCriteria(pendingCode, schoolProgram);
            Assert.assertNotNull(studentsCriteria);
            List<String> messages = proc.getProcedureMessages();
            Assert.assertEquals("[]", messages.toString());

            Assert.assertEquals(
                    "[extracurricularSchoolProgOid = " + schoolProgram.getOid() + ", [eligibilityStatus <> "
                            + pendingCode
                            + ", [eligibilityStatus IS NULL ]]]",
                            studentsCriteria.toString());
        }

        @Test
        public void validCurrentRecord() throws Exception {
            X2Criteria studentsCriteria = proc.getStudentsCriteria(eligibleCode, schoolProgram);
            Assert.assertNotNull(studentsCriteria);
            List<String> messages = proc.getProcedureMessages();
            Assert.assertEquals("[]", messages.toString());
            Assert.assertEquals(
                    "[extracurricularSchoolProgOid = " + schoolProgram.getOid() + ", [eligibilityStatus <> "
                            + eligibleCode
                            + ", [eligibilityStatus IS NULL ]]]",
                            studentsCriteria.toString());
        }
    }
}
