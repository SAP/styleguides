```mermaid
%%{ init: { 'flowchart': { 'curve': 'basis' } } }%%
graph TD
    A[Developer 1] --> B[abap]
    C[Developer 2] --> B
    D[Developer 3] --> B
    B -->|push| E([git])
 ```
 <div style="background-color:black, height:25px">
 <img src="img_girl.jpg" alt="Girl in a jacket" width="50" height="50">   
 ```mermaid
%%{ init: { 'flowchart': { 'curve': 'cardinal' } } }%%
 graph LR
    A[One Developer] --> B[abap]
    B --->|push| C[git]
    C -->|pull| B
```
</div>
