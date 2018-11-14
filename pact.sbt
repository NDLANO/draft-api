pactBrokerAddress := "http://pact-broker.ndla-local"
pactContractVersion := git.gitHeadCommit.value
  .map(sha => sha.take(7))
  .get

providerName := "draft-api"
consumerNames := Seq("article-api")
