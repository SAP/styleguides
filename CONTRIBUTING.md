# Contributing to SAP Styleguides

You want to contribute to the SAP Style Guides? Welcome! Please read this document to understand what you can do:
- [Contributing to SAP Styleguides](#contributing-to-sap-styleguides)
    - [Help Others](#help-others)
    - [Analyze Issues](#analyze-issues)
        - [Quick Checklist for Issues and Change Proposals](#quick-checklist-for-issues-and-change-proposals)
        - [Issue Handling Process](#issue-handling-process)
        - [Usage of Labels](#usage-of-labels)
        - [Issue Reporting Disclaimer](#issue-reporting-disclaimer)
    - [Pull Requests](#pull-requests)
        - [Developer Certificate of Origin (DCO)](#developer-certificate-of-origin-dco)
    - [Issues](#issues)
        - [How to Contribute - the Process](#how-to-contribute---the-process)
        - [Decision Making by the Commmiters](#decision-making-by-the-commmiters)
    - [Discussions](#discussions)
    - [Forks](#forks)
    - [Use English](#use-english)

## Help Others

You can help Clean ABAP by helping others to learn Clean Code, Clean ABAP, Test Automation, Code Reviews and ABAP. You will find more resources and tools to foster clean code in the [introduction, motivation and overview blog post](https://blogs.sap.com/2022/12/21/clean-code-writing-maintainable-readable-and-testable-code/).

## Analyze Issues

Analyzing issues can be a lot of effort. Any help is welcome!
Go to [the Github issue tracker](https://github.com/SAP/styleguides/issues) and find issues and express your thoughts by up-voting, down-voting, sharing a comment and help to develop the issue further.

Maybe you can [contribute](#pull-requests) a change proposal via pull request, which could close the issue?

If the committers have time to look at issues, they will first look at the  [most highly ranked issues](https://github.com/SAP/styleguides/issues?q=is%3Aissue+is%3Aopen+sort%3Areactions-%2B1-desc).

### Quick Checklist for Issues and Change Proposals

Issue report checklist:
* No duplicate
* Good summary
* Well-documented
* Minimal example
* Use an existing template

### Issue Handling Process

When an issue is reported, the community can participate in the discussion express their thoughts by up-voting, down-voting, sharing a comment and help to develop the issue further.

A committer will sooner or later look at it (can take some time) and either confirm it as a real issue (by giving the "in progress" label), close it if it is not an issue, or ask for more details. In-progress issues are then either assigned to a committer in GitHub, or left open as "contribution welcome" for easy or not so urgent fixes. If a committer has time they usually look first at the open pull requests and the [most highly ranked issues](https://github.com/SAP/styleguides/issues?q=is%3Aissue+is%3Aopen+sort%3Areactions-%2B1-desc).

An issue requesting changes in the style guide is closed as soon as a change (pull request) that resolves it is committed to the main branch or if consensus is reached that no such change will be made.

### Usage of Labels

GitHub offers labels to categorize issues. We use the following labels:

Status of open issues:
* unconfirmed: This report needs confirmation whether it is a valid or invalid request (no label; this is the default status).
* in progress: This issue has been reviewed and a commiter has been assigned.
* author action: The author is required to provide more information.
* contribution welcome: The requested change is generally seen as desirable, and everyone is invited to contribute a possible solution via pull request. If you are working on a fix for such an issue yourself, it is good to leave a comment in the issue or ask for it to be assigned to you so that people don't start work on something someone else is already working on

Status/resolution of closed issues:
* fixed: a fix for the issue was provided
* duplicate: the problem is also reported in a different issue and is handled there
* invalid: for some reason or another this issue report will not be handled further (maybe lack of information or issue does not apply anymore)

The labels can only be set and modified by committers.

### Issue Reporting Disclaimer

We want to improve the quality of SAP Style Guides and change proposals are welcome! But our capacity is limited, so we cannot handle questions or consultation requests not directly related to the guides. So we reserve the right to close or to not process off-topic issues or issues/pull requests with insufficient information in favor of those that are clearly documented and address actual issues with the guide. Even though we would like to address each well-documented issue, there is always a chance that it won't happen - remember: SAP Style Guides are Open Source, and your help in processing issues or improving the guides is always welcome!

## Pull Requests

Create [pull requests](https://github.com/SAP/clean-abap/compare) when you know exactly what you want to change, and how, in particular when you are addressing an open issue. They are most welcome, and may range from minor formatting corrections to adding whole sections. Of course, entirely new rules and sections may take longer to review and simple corrections.

When creating a pull request, we will ask you to accept the Developer Certificate of Origin (DCO).

### Developer Certificate of Origin (DCO)

Due to legal reasons, contributors will be asked to accept a DCO before they submit the first pull request to this project. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).  This happens in an automated fashion during the submission process: the CLA assistant tool will add a comment to the pull request. Click it to check the DCO, then accept it on the following screen. CLA assistant will save this decision for upcoming contributions from the same Github account.

This DCO replaces the previously used CLA ("Contributor License Agreement") as well as the "Corporate Contributor License Agreement" with new terms which are well-known standards and hence easier to approve by legal departments. Contributors who had already accepted the CLA in the past may be asked once to accept the new DCO.

## Issues

Create [issues](https://github.com/SAP/clean-abap/issues/new) if you have a problem with the guides' content but do not immediately have a solution in form of a pull request or if you want to discuss your issue with the community first. Feel free to comment on existing issues and add your voice - every contribution counts, and since our style guides should be applicable to a wide range of different developers, we always welcome hearing opinions from new contributors.

We will ask you for permission before taking over text and examples proposed in issues unchanged or ask you to contribute them in a pull request yourself.

### How to contribute - the Process

1. Make sure your intended change would be welcome - if you have any doubts at all, open an issue discussing the change first. If you are sure that the change is welcome by the community, you can also directly open a pull request.
2. Create a fork of this repository and do your changes in a branch on your fork, using legible and meaningful commit messages.
3. Create a pull request with a description summarizing your change, in particular (for changes that are not obvious corrections) the motivation behind it and any prior discussion.
   If your change resolves an existing GitHub issue, add the following line to the message:
    - ```Fixes https://github.com/SAP/styleguides/issues/(issueNumber)```
4. If contributing for the first time, see the [chapter on Developer Certificate of Origin (DCO)](#developer-certificate-of-origin-dco) for more detail
5. Wait for code reviews to come in and be ready to respond to requested changes or questions by the reviewers
    - Depending on the effort required for reviewing and clarification this process may take a while
    - We also intentionally often delay merging new proposals for up to 3 months to allow the community some time to provide feedback before it is merged. Corrections that do not introduce new rules may be merged faster.
6. Once the review process is finished, a committer will merge your pull request. You can delete your branch and/or fork after this.

### Decision Making by the Commmiters

To be able to make decisions and represent the huge diversity of ABAP developers, we have defined a [decision making process](GOVERNANCE.md).

One reason for the process was to increase to the confidence in making decisions by representing different groups and set it up to be more sustainable (i.e. independent from the original creators). The committers usually meet every second week. The first priority of the meeting is to go through open pull requests. The discussion about every single PR takes some time, but this means you can usually expect an initial reaction to your pull request from someone within two weeks.

## Discussions

We encourage you to participate in the discussions of the issues and pull requests. To get notification you can star or watch the repository.

## Forks

If you generally agree with this styleguide, but want to adapt certain things to your team's personal needs, we recommend to fork this repository and add your changes there.

## Use English

Please use English for issues, pull requests, and discussions. Due to the wide-spread presence of English as a second language, SAP uses English as its common language to facilitate cross-culture communication.
