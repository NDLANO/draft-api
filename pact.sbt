pactBrokerAddress := sys.env.get("PACT_BROKER_URL").get
pactContractVersion := git.gitHeadCommit.value
  .map(sha => sha.take(7))
  .get
pactContractTags := List(git.gitCurrentBranch.value)

