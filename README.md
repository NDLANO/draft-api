# Draft API 
[![Build Status](https://travis-ci.org/NDLANO/draft-api.svg?branch=master)](https://travis-ci.org/NDLANO/draft-api)

Creates, updates and returns an Article draft`. Implements Elasticsearch for search within the article database.

## Developer documentation
**Compile**: sbt compile

**Run tests:** sbt test

**Create Docker Image:** sbt docker

### IntegrationTest Tag and sbt run problems
Tests that need a running elasticsearch outside of component, e.g. in your local docker are marked with selfdefined java
annotation test tag  ```IntegrationTag``` in ```/ndla/article-api/src/test/java/no/ndla/tag/IntegrationTest.java```. 
As of now we have no running elasticserach or tunnel to one on Travis and need to ignore these tests there or the build will fail.  
Therefore we have the
 ```testOptions in Test += Tests.Argument("-l", "no.ndla.tag.IntegrationTest")``` in ```build.sbt```  

    sbt "test-only -- -n no.ndla.tag.IntegrationTest"

