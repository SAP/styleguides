# 4. Tools
Various tools can be used to orchestrate the code review process, this section introduces the tooling required for conducting code reviews in the context of ABAP development.

## 4.1. Git
[Git](https://en.wikipedia.org/wiki/Git) is a popular [distributed version control system](https://en.wikipedia.org/wiki/Distributed_version_control), which helps developers work on the same codebase. Unlike ABAP, Git works with files instead of objects in a database, which allows editing any file with
any editor, and keeping multiple copies (branches) of files.

At a technical level Git is a protocol, and is typically used in connection with cloud-based hosting platforms like:

* [GitHub](https://github.com)
* [GitLab](https://gitlab.com)
* [Bitbucket](https://bitbucket.com)
* [Azure Repos](https://azure.microsoft.com/en-us/services/devops/repos/)

For an introduction to Git see:

* [Git Documentation](https://git-scm.com/doc)
* [Git Handbook](https://docs.github.com/en/get-started/using-git/about-git)

## 4.2. Code Review Platforms
Git helps keeping track of changes to files, and the Git hosting platforms add extra functionality on top of that. As explained in the [introduction](https://github.com/priyasingh199885/styleguides/blob/main/abap-code-review/src/introduction.md), the most important feature is **proposed commits**, which is the basis for code reviews.
The general idea is to keep all commits of a feature in a separate branch, propose to merge this branch to a parent or main branch, and to conduct a review conversation over this proposal.

For instance, GitHub provides [Code Reviews via Pull Requests](https://github.com/features/code-review/). Other git platforms offer similar features.

## 4.3. CTS
The Change and Transport System (CTS) helps organizing development projects in transports, both for objects in the ABAP Workbench and in Customizing. After the development is completed the change is transported between the SAP systems in the system landscape.

The complete documentation on how to configure CTS and how to set up transport landscapes is available on  [SAP Help Portal](https://help.sap.com/viewer/4a368c163b08418890a406d413933ba7/201809.latest/en-US/48c4300fca5d581ce10000000a42189c.html).

CTS also offers the possibility to integrate custom logic in the flow of transport requests via the BAdI `CTS_REQUEST_CHECK`.
In particular, the method `CHECK_BEFORE_RELEASE` is useful for vetoing the release of transports based on external conditions, like code reviews.

## 4.4. ABAP and Git
ABAP objects are stored in the underlying database, and git works on files, to enable git in ABAP, the
objects must be serialized to files and moved to the git server via the git protocol. Currently two different
tools exists that brings git and ABAP together.

### 4.4.1. abapGit
[abapGit](https://abapGit.org) is a git client written in ABAP for use with ABAP, it serializes the ABAP objects to files, and provides access to the most common git operations.

It can be [installed](https://docs.abapgit.org/guide-install.html) on any SAP system version 702 and up, requiring only a ABAP developer key for installation.

abapGit was initially released in [2014](https://blogs.sap.com/2014/07/17/git-client-for-abap-alpha-release/) and is continuously being enhanced, users can update to the latest version at any time.
[Multiple open source projects](https://dotabap.org) use abapGit for development and installation into systems.

### 4.4.2. gCTS
A first set of features of git-enabled CTS (gCTS) is available with SAP S/4HANA 1909. You can find the documentation on how to configure and use gCTS on the SAP Help Portal: [Git-enabled Change and Transport System](https://help.sap.com/viewer/4a368c163b08418890a406d413933ba7/201909.000/en-US/f319b168e87e42149e25e13c08d002b9.html). A good starting point is Karin Spiegel’s blog post [gCTS is here](https://blogs.sap.com/2019/11/14/gcts-is-here/).

To get the idea of gCTS and what is planned for future releases, please refer to its [statement of direction](https://support.sap.com/content/dam/support/en_us/library/ssp/tools/Software-logistic-tools/Ideas_CI_ABAP_V3.pdf).

## 4.5. Static Analysis and Tests

To make code reviews effective, it is useful to conduct automated checks upfront. For instance, machines are much cheaper and more effective at finding typical slip errors by static code analysis than human reviewers. Hence it is desirable to perform such checks upfront before peer code reviewers are bothered with a change.

Git platforms usually allow developers to connect continuous integration pipelines to their repositories so that arbitrary automated checks can be configured.
In the following, we will look into tools running ABAP-specific checks in such pipelines.

### 4.5.1. Code Inspector (ATC)
The developer can use [ATC](https://help.sap.com/viewer/c238d694b825421f940829321ffa326a/7.51.7/en-US/4ec5711c6e391014adc9fffe4e204223.html) to check the working copy in an ABAP system.

If the state of the ABAP system matches the git state, ATC can be executed locally for the scope given in the transport or pull request to help the code reviewer.

In addition to the standard checks provided by SAP, there are open source projects that provide additional checks:

* [code pal for ABAP](https://github.com/SAP/code-pal-for-abap)
* [abapOpenChecks](https://abapopenchecks.org)

It is however not so trivial (yet) to execute ATC as part of a continuous integration pipeline.

One possibility is to use BAdI `CTS_REQUEST_CHECK` to execute ATC programmatically during the release of a transport.

In case the SAP ABAP Development Tools for Eclipse are already configured for your system, you could use its REST APIs to trigger ATC and ABAP Unit externally. This is what, for instance, the pipeline step [gctsExecuteABAPUnitTests](https://www.project-piper.io/steps/gctsExecuteABAPUnitTests/) of project Piper does.

With SAP BTP ABAP Environment (a.k.a. [Steampunk](https://blogs.sap.com/2019/08/20/its-steampunk-now/)) and newer SAP S/4 HANA releases, there is also the possibility to use dedicated REST end-points for such continuous integration scenarios. There is even a dedicated [ABAP Environment Pipeline](https://www.project-piper.io/pipelines/abapEnvironment/introduction/) for that.
These newer features are of course not available to customers working on older releases.

To call the REST endpoints on the ABAP system, the pipelines must have network access to the ABAP system.

```mermaid
%%{ init: { 'flowchart': { 'curve': 'bumpX' } } }%%
 flowchart LR 
    A(abap1) -->|1: push| B([git])
    B -->|2: trigger| C[pipeline]
    C -->|3: REST| D(abap2)
    D --> |5: ATC| D
    B -->|4: pull| D
 ```

### 4.5.2. abaplint
[abaplint](https://abaplint.org) is a open source static analysis tool for ABAP, it works on files serialized by abapGit.
abaplint is "cloud native" and built to [run on CI](https://github.com/abaplint/abaplint/blob/main/docs/ci/README.md) pipelines, and only requires that the pipeline can access the files in git. There are no specific system requirements, as abaplint can run on the lowest/free pipeline tiers.

Multiple [rules](https://rules.abaplint.org) can be configured and the corresponding configuration file  `abaplint.json` is stored in the repository along with the code, following the same change process as the code.

The [abaplint.app](https://abaplint.app) can be installed via one-click from [Github Marketplace](https://github.com/marketplace/abaplint), and provides extra features like automatic suggestions and code insights.

The developers can check their working copy using [abaplint-sci-client](https://github.com/abaplint/abaplint-sci-client) which integrates the rules into Code Inspector/ATC. And the linter also works in [vscode](https://marketplace.visualstudio.com/items?itemName=larshp.vscode-abaplint) or running [standalone](https://playground.abaplint.org) in a browser window.

```mermaid
 %%{ init: { 'flowchart': { 'curve': 'natural' } } }%%
 flowchart LR 
    A(abap) -->|1: push| B([git])
    B -->|2: trigger| C([pipeline])
    B -->|3: pull| C
    C --> |4: abaplint| C
 ```

### 4.5.3. Third party checks

There are other tools from third party vendors that can be also used for ABAP code analysis. 

For instance, [SonarSource ABAP](https://rules.sonarsource.com/abap/) can be used to check ABAP source serialized by abapGit. 

## 4.6. abap-openapi-client

External check services often have an [OpenAPI](https://swagger.io/docs/specification/about/) definition. Unfortunately, there is no standard way of consuming such OpenAPI services in ABAP yet. An emerging open source OpenAPI client for ABAP can be found at https://github.com/abap-openapi/abap-openapi-client
    
