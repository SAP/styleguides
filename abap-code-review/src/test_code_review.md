# 3. Best Practices

This section contains suggestions on how to conduct code reviews effectively. Other code review guides have already dealt with this topic generically, for instance, [Google’s Engineering Practices documentation](https://google.github.io/eng-practices/). We will focus on ABAP specific considerations.

## 3.1. Why perform Code Reviews?

Code reviews can detect issues in the software that are harder to find via other methods:

* Foster [Clean code](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md) to improve **maintainability** and **readability** of code, including comments, and to avoid unnecessary **complexity**.
* Ensure the code is **well designed**.
* Consider behavioral aspects like **performance** and **security**.
* Improve **testability** of code, including test design and adequateness of tests.

Code reviews are one possibility to **share experience and knowledge** between developers. It works a bit like "asynchronous pair programming", so that colleagues can learn from each other, see different perspectives, and share know-how about all parts of the code.

Code reviews help to prevent that **code health** deteriorates over time.

## 3.2. Size and Speed of Reviews

Change-based code reviews are these days considered more effective than formal [Fagan-style code inspections](https://en.wikipedia.org/wiki/Fagan_inspection) as they were practiced some years ago. Conducted continuously, they require less effort while still finding a considerable number of defects at an earlier time. The main reason is the **smaller size** of code deltas to digest. This ensures that reviews can be performed both, more **quickly** and **thoroughly**, while keeping the process lean, i.e., less waste if changes are rejected and lower probability to block other development. Changes should be self-contained to help reviewers focus on the task at hand.

An **ABAP Transport Request** typically fulfills these criteria.

Code reviews need to be done **fast**. Hence, you need to balance between the interruption of your own work against the probability of blocking the release of other developers' transport. Rule of thumb for the maximum delay should be **one day**. Usually, the speed of code reviews will improve over time.

## 3.3. How to Select Reviewers?

In general, reviewers can be selected manually or automatically. Manual selection happens either by the author explicitly requesting reviews from colleagues with matching expertise, or by interested reviewers actively picking open changes for review. The latter requires a certain "meritocracy" culture in an organization, where novice developers may gain reputation by reviewing open changes. Some organisations may even chose to let team leads assign reviewers to changes. 

**Automated selection** of code reviewers is also possible, for instance, via round robin routing or advanced concepts like [Code Owners](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/about-code-owners). The latter enables you to define (groups of) default reviewers for certain files and folders on git level. This can be useful for critical coding parts or known hot spots.

Attributes qualifying colleagues as reviewers include:

* team assignment
* experience and seniority (also outside of the team)
* consumer role (to check if something was implemented as required)

## 3.4. Required Code Reviews

An important decision is how many code reviews are required to release a change. Most projects require at least the so-called 4-eyes-principle for every change. It is important to note the culture aspect of this configuration: This is not meant as an additional hurdle to harass developers that should be generally mistrusted. It is rather an opportunity and safety net that prevents slip errors from spreading unnoticed, because they can happen anytime to anyone. If reviews from **Code Owners** are required, individual approvals will not suffice.

In case of pair-programming, code reviews can of course be conducted much faster. Usually, a remark documenting the pair-programming should suffice.

Moreover, one could imagine also dynamic conditions that make code reviews mandatory, for instance, a significant drop in test coverage, a high number of low priority ATC messages, or changes of critical objects like DDIC or CDS, security checks, APIs and package interfaces, just to name a few. The practicability of such requirements of course depends on the capabilities of the chosen code review platform.

## 3.5. Code Formatting

The code reviewer will look through the textual changes in the source code, and even if the actual change is small,
there might be a lot of changes due to reformatting of the code.

Having a consistently formatted codebase will help keeping the size of textual changes small, and make reviewing easier.

The SAP ABAP style guide provides multiple recommendations which can be used in the development team,

* [Use your pretty printer team settings](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-your-pretty-printer-team-settings)
* [Stick to a reasonable line length](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#stick-to-a-reasonable-line-length)
* [Indent and snap to tab](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#indent-and-snap-to-tab)
* [Align type clauses](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-align-type-clauses)
* [Close brackets at line end](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#close-brackets-at-line-end)
* [Align parameters](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#align-parameters)

Objects can be mass pretty printed via [abappretty](https://www.npmjs.com/package/abappretty), and developers reminded to follow the team conventions via static analysis tooling.
