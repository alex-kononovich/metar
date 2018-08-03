STACK_PROFILE=stack --work-dir .stack-work-profile
PROFILE_OPTS=+RTS -p -hc -L100 -RTS
N=30000
TEST_OUTPUT=test.txt

all: profile

clean: profile-clean

profile: profile-metar-generate profile-metar

profile-metar: install-hp2pretty profile-build
	$(STACK_PROFILE) exec metar -- $(PROFILE_OPTS) -f $(TEST_OUTPUT)
	hp2pretty metar.hp

profile-metar-generate: install-hp2pretty profile-build
	@rm -f $(TEST_OUTPUT)
	$(STACK_PROFILE) exec metar-generate -- $(PROFILE_OPTS) -n $(N) -f $(TEST_OUTPUT)
	hp2pretty metar-generate.hp

profile-build:
	$(STACK_PROFILE) --profile build


install-hp2pretty:
	stack install hp2pretty --silent

profile-clean:
	@rm -f $(TEST_OUTPUT)
	@rm -f metar-generate.hp
	@rm -f metar-generate.svg
	@rm -f metar-generate.prof
	@rm -f metar.hp
	@rm -f metar.svg
	@rm -f metar.prof

