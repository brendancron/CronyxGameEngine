E := $(word 2,$(MAKECMDGOALS))

.PHONY: build run

build:
	dotnet build Cronyx/Cronyx.fsproj

run:
	dotnet run --project Examples/$(E)/$(E).fsproj

%:
	@:
