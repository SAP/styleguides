```mermaid
graph LR
    A[Custom Shape] --> B
    style A fill:#f9f,stroke:#333,stroke-width:4px
    style B fill:#fff,stroke:#333,stroke-width:4px
    A((0,20))--(10,0)--(20,20)--(10,40)
```

```mermaid
flowchart LR

subgraph Frontend
    rect[React App]
    rect2[HTML]
    rect3[CSS]
end

subgraph Backend
    cloud((API))
    db(Database)
end

subgraph Infrastructure
    rectangle(DNS)
    rectangle2(Gateway)
    rectangle3(Firewall)
end

subgraph "Communication Channel"
    circle(Websocket)
end
rect --> cloud
rect2 --> cloud
rect3 --> cloud
cloud --> db
db --> circle
circle --> rectangle
rectangle --> rectangle2
rectangle2 --> rectangle3
cloud:::cloud
cloud((0,15))--(10,0)--(20,15)--(10,40)--cycle

classDef cloud fill:#f9f,stroke:#666,stroke-width:2px,shape:polygon,rounded:1,points:0,15 10,0 20,15 10,30;
classDef db fill:#fff,stroke:#666,stroke-width:2px;
classDef rectangle fill:#fff,stroke:#666,stroke-width:2px;
classDef circle fill:#fff,stroke:#666,stroke-width:2px;
```
