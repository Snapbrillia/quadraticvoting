
build-QvfGenerateScriptsFunction:
	true

build-QvfGenerateScriptsStaticFunction:
	./nix-build-function.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts-static.x86_64-linux

build-QvfGenerateScriptsDynamicFunction:
	./nix-build-function.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts.x86_64-linux
