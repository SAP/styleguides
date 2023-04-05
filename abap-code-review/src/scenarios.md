# 5. Scenarios

In this section we look at typical scenarios how git-based code reviews and classical ABAP CTS transports can be combined.
We assume the SAP ABAP development application servers have network access to the code review platform, e.g., GitHub.

## 5.1. One Way Synchronization

By one-way we mean that CTS is the primary source of truth of your component. It is in full control of your software lifecycle. Git is only used for code review purposes as a secondary persistence or USB-stick-like export channel.
Developers work normally, releasing transports via CTS as configured per transport layer.

This setup is recommended for scnearios where multiple developers work on the same repostiory in the same system,
pulling code into the system might overwrite ongoing changes by other developers, allowing only pushes will avoid this problem.

```mermaid
%%{ init: { 'flowchart': { 'curve': 'basis' } } }%%
graph TD
    A(fa:fa-user Developer 1) --> B(abap)
    C(fa:fa-user Developer 2) --> B
    D(fa:fa-user Developer 3) --> B
    B -->|push| E([git])
 ```
 The only difference are the new review steps, which are conducted via an ABAP git client (either abapGit or gCTS) on a code review platform like GitHub.
Even though code reviews could also be conducted independently from transport releases, it is more efficient to synchronize both tightly because their granularity typically captures a logical unit of work.

This scenario requires minimal git knowledge, as developers will continue working with normal CTS requests/tasks.

## 5.2. Two Way Synchronization

In contrast, open source projects or reusable assets developed by partners for multiple clients usually have a lifecycle of their own, independent of the target systems they are deployed to. Git becomes the primary persistence. Classical CTS is only used optionally for software logistics of the consuming target project (or you rely completely on gCTS).
All git operations are manually performed by the developer using the ABAP git client (either abapGit or gCTS), including the pull of changes back from git into AS ABAP.

One developer would typically work on one repository in a system at a time, utilizing both the push and pull features in the git client.

 ```mermaid
%%{ init: { 'flowchart': { 'curve': 'cardinal' } } }%%
 graph LR
    A(fa:fa-user One Developer) --> B(abap)
    B --->|push| C([git])
    C -->|pull| B
```
Conflicts resulting from concurrent modification of the same objects on different systems need to be resolved on git level.

