# Aspen Customer Tools

This project contains: 

 * `tool-source` contains all of the standard and state specific source and common files that are deployed for the state-reporting team and new installs
 * `bundle-builder` contains the source to build the specific bundle of your choice to deploy to a customer
 
## System Setup

 * See the aspen-runtime README.md `http://stash.fsc.follett.com/projects/ASPEN/repos/aspen-runtime/browse` or aspen README.md `http://stash.fsc.follett.com/projects/ASPEN/repos/aspen/browse` for system setup instructions (wildfly, maven, eclipse-red hat, java)
 
## Project Setup

1. Clone this project from this repo: `git clone http://stash.fsc.follett.com/scm/aspen/aspen-runtime.git`.
1. To run 6.4: Create  a `local-test-config.properties` files in the root of the project. This file is the same that live in the aspen/aspen-config and are unique to you (they do NOT get committed into source control).  You can copy them over from project to project.
1. To run 6.5: Create  a `local-build.properties` and `local-test.properties` files in the tool-source/aspen-config project. These files are the same that live in the aspen/aspen-config and are unique to you (they do NOT get committed into source control).  You can copy them over from project to project.
  
## Maven run commands (while 6.4 is in production!)

* `mvn -f clean initialize process-classes install` runs 6.5 and **WILL** run tests
 
## Run a Single bundle Maven Command (the default!!!)

In order to create a SINGLE bundle, you will need to add a file `bundle.properties` under the `bundle-builder` project.  The bundle.properties file looks like this:

    state.dir=MA
    export.dir=MA-FULL

 * state.dir is the state you wish to create the bundle for, it corresponds to a folder in the `bundle-builder\States` directory
 * export.dir is the export you wish to create the bundle for, it corresponds to a folder in the `bundle-builder\States\${state.dir}` directory
 
* The artifacts are built in the `bundle-builder/output/bundleOutput`, this directory is created for you!  The output of the maven run shows what file is created and where.

 * `mvn clean install`  this will clean the project, compile the source, run the tests and then create the bundle as per the `bundle.properties` file.  The creating of the bundle is tied to the install command.  You can add in the `-DskipTests` option to skip running the tests

 
## Run all states and standard bundles Maven Command
The default command runs all of the STATE-ALL files.

* `mvn clean install -Drun-default`  this will clean the project, compile the source, run the tests and then create all the state-ALL bundles.  The creating of the bundle is tied to the install 
command.  You can add in the `-DskipTests` option to skip running the tests
 
 
## Run all the bundles Maven Command

 * `mvn clean install -Drun-all`  this will clean the project, compile the source, run the tests and then create the ALL the bundles.  The creating of the bundles is tied to the install command.  You can add in the `-DskipTests` option to skip running the tests
 
## Maven Eclipse setup

 * right-click on the pom.xml file in the aspen-customer-tools-parent directory and select "Run As > Maven install", which will run the full install build.
 * right-click on the pom.xml file in the aspen-customer-tools-parent directory and select "Run As > Maven build...", will bring up the run configuration.  You can specify the goals you want (clean, compile, install etc...) and you can CHECK the skipTests check-box if you would like
