# Voting contract

## Requirements

Java JDK 11, sbt (1.x), Docker

## How to build

1. Build uber-jar file for contract:

```
sbt clean assembly
```

2. Build docker image:

```
docker build -t voting-contract .
```

*If you change the code you have to rebuild jar file!*
