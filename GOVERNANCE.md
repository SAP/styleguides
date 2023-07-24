# Clean Code Community - Contribution, Discussion, Review and Decision Process

With the success of the style guide, there comes the need for a more formal process when it comes to reviewing issues and improvement proposals to be able to form a common understanding and be effective in making decisions.
Clarity on how those should be opened, are processed and the typical timeframe. Besides, especially for changes with a larger impact (e.g. incompatible changes) we need a formal process for decision making.

**Note: The document replicates the parts of the SAP internal governance document, which can be made public - more than 95% of the text is the same.**

## Principles for Decision Making

The style guides are based on some universal principles. So when we make the decision between different alternatives we keep those principles in mind:

- Testability: Every piece of code needs to be testable in an efficient, fast and robust way
- Readability and Maintainability is important and determines developer productivity
  - [Studies](https://www.researchgate.net/publication/359129462_Code_Red_The_Business_Impact_of_Code_Quality_--_A_Quantitative_Study_of_39_Proprietary_Production_Codebases) found that developers spend about 60% of their development time on program comprehension activities and at least 10 times the amount of time is used for reading compared to writing the same piece of code.
- Readability and Modularity over premature performance optimization, but performance is important (e.g. mass read over single read)
- Tradeoffs: If we cannot come up with a consensus, we list the tradeoffs and explain the consequences of the alternatives.
- Same terminology and principles across programming languages (e.g. [testing terminology](https://blogs.sap.com/2021/12/06/shared-language-for-talking-about-test-strategy/) is the same for JavaScript and ABAP)

## Review and Discussion

Compared to an open source software project, the rate of change for a style guide should be much slower. Due to the adoption by many projects and usage by S4HANA as official programming guideline changes can have an impact on many projects and can result in an adjustment of code check profiles. Therefore, we want to give the community of readers and contributors enough time to have a conversation on the issues and pull requests.

When moderating the discussion and more clarity is needed for the discussed topic you should consider the following. Discussions are unlikely to give a clear picture of the community as a whole for several reasons. They favor people who like to argue, those who are more articulate or have more experience in English (or whatever language the discussion is in), and those who feel like they're in the "in" crowd and will have a popular opinion. Finally, they hide how many people are in agreement with someone who speaks up. With this in mind, use the discussion to explore different sides of the issue. Raise awareness of the discussion by announcing in the community channels (if appropriate) to make sure that the entire community is able to weigh in.

## Labeling

For deciding how to proceed with a pull request, we want to classify the different issues and pull requests into categories. Therefore, we have the following labels:
- Type of Change
  - Clarity of Text, e.g. misunderstanding in text
  - `Formatting`, e.g. missing `.` in code, or wrong "," in text
  - Content Error, e.g. bad link, `me->` where it doesn't belong (= consistency issue)
  - New Rule, e.g. add a new rule
  - Adjustment of Rule, e.g. change content of rule, such as generalizing a rule
  - Idea - Usually an issue e.g. request to add a guide for clean EML
- Status
  - Ready for Decision

We use these labels to organize our collaboration. Externals cannot add/remove them.

## Review and Decision Process

### Review by Maintainers and Owners

Every team member with triage rights can approve pull requests or request changes. Only Code Owners can merge the pull request.
To join the team of clean code moderators, you can join the respective group:
- [Clean ABAP review and discussion moderators](https://github.com/orgs/SAP/teams/clean-abap-review-and-discussion-moderation/members?query=) - - Due to technical reasons only visible to members of the SAP GitHub organization.

**Meetings:** For discussing changes and other issues the **Clean ABAP review and discussion moderators meet every second week**. To ensure efficient processing an issue or pull request is presented by an advocate with a proposal how to proceed with the issue. Most of the issues and pull requests can be closed or merged within this meeting, but for reviewing changes with impact (e.g. new rules or adjustment of rules) there is an additional community meeting with a larger amount of participants:
- **Once every quarter, the agile software engineering community** with members from application development, tools and abap meet to discuss about important changes
- An advocate presents the change to the community
- In order to ensure efficient decision making the following checklist needs to be fulfilled to bring the issue or pull request to the community:
  - The issue or pull request is older than 3 month so that the external and internal community had time for a conversation
  - The external and internal community expressed their opinion

For more details on process for important changes see the [chapter on decision making for important changes](#decision-making-for-important-changes).

### Action by Author and Request for Changes

When asked to provide feedback or the pull request is set to request for changes, the author has 2 month time. After the 2 month, the issue or pull request can be closed or the code owner take over the necessary steps to include the proposal into the guide.

### Advocate

An advocate takes the ownership to drive the process for a specific issue or pull request towards closing an issue / pull request or merging a pull request and represent the topic in the different community meetings. When picking up the topic, an advocate assigns himself to the issue or pull request.

### Decision Board for ABAP

The decision board should represent the diversity of business applications and technology:

- ASE Application Development Community - 6 colleagues representing large business application areas
- ABAP group - 3 colleagues representing areas of the ABAP foundation

Everyone in the community can participate and express his opinion, but only the decision board members have the power to block a change.

### Decision Making for important changes

In the Clean Code Projects a discussion toward a decision follows this process. For important decisions about new rules or change of existing rules (labeled as "New Rule" or "Change of rule"), we strive for consensus among the decision board members. It only becomes a consensus when at least 2/3 of the members of the decision board - i.e. 6 or more of the 9 people in the decision board - have given a +1 and no additional member voted with -1. Therefore, we ask whether all those entitled to vote agree and aim for a consensus. Consensus means that no one has an objection, that a person responsibly shares the decision, if he decides not to veto that for him the decision is ok. So consensus looks at the veto, at the opposition.

1. A proposal is put forth and a check for consensus is made.
  - Consensus is signified through a +1 vote.
2. A check is made for any dissent on the proposal.
  - Reservations
    - Reservations about the proposal are worked through, seeking consensus to resolve the reservations.
    - A reservation is not a vote against the proposal, but may turn into a vote against if unresolved. It is often expressed with an initial -1 vote to indicate reservations and concerns. This indicates there is still discussion to be had.
  - Stand aside? No comment, or state concerns without a -1 reservation
    - This option allows a member to have issues with the proposal without choosing to block the proposal, by instead standing aside with a +/-0 vote.
    - The stated concerns may influence other people to have or release reservations.
  - Block? Vote -1 with reasons for the block.
    - This is a complete block on a proposal, refusing to let it pass. A block is a -1 vote and must be accompanied with substantive arguments that are rooted in the merit criteria of the Project protecting the community, the upstream, technical reasons, and so forth.

Block (-1) votes used as a veto are typically used only when consensus cannot otherwise be met, and are effectively a veto that any sitting Board member can utilize with sufficient substantiation. A block should only happen if there are very strong concerns about the proposal (e.g. creates potential security problem or severe performance degradation).

A way to deal with reservations, concerns and vetos is to document the tradeoffs for the different alternatives.

In Github, this process is represented as follows: Any important change is represented by a pull request. The votes in the above description correspond to reviews of this pull request, with:

- +1 votes corresponding to reviews that approve the changes
- -1 votes (both blocks and reservations) corresponding to reviews that request changes
- Standing aside or concerns without reservation corresponding to comments or commenting reviews

### Notify Code Owners about the need for decision making

If there is a pull request, which needs the decision from the code owners, write a short comment in the [discussion section of the code owner team](https://github.com/orgs/SAP/teams/clean-abap-code-owner/discussions).

### Code Owners

Code Owners put the decision of the group into practice and can merge smaller changes without going through the decision making process. Every code owner commits to follow the clean code community process description and to agile software engineering principles.

Code Owner Teams:
- [Clean ABAP Code Owner](https://github.com/orgs/SAP/teams/clean-abap-code-owner) - Due to technical reasons only visible to members of the SAP GitHub organization. 

### Merge of Pull Request

Pull Request are merged with squash and merge:
- Example: [Commit Message](https://github.com/SAP/styleguides/commit/81e2d9e601c3f4e4a2c806737e161bb9826332b9)
  - title: recommend avoiding manual versioning through comments
  - description: #278, no vetos, 7 approvals

## Process after Important Changes

**Change Log:** The new rules or adjusted rules can be found in the [change log](https://github.com/SAP/styleguides/pulls?q=is%3Apr+is%3Aclosed+label%3A%22Adjustment+Of+Rule%22%2C%22New+Rule%22+is%3Amerged+)

The **Code Pal+* team is watching the merged pull request and elaborates if static code check adjustments are needed.

**Update of the translations** is not directly triggered after the change is merged. The translators are asked for watching the changes and update the translation from time.

## Related Topics

**How are new language features considered for an update of Clean ABAP?**

Several members of the clean abap team are part of the ABAP team and also involved when rolling out those new ABAP features. These colleagues will create a pull request to update Clean ABAP if necessary.  


