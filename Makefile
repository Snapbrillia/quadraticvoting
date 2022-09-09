build-QvfGenerateScriptsStaticFunction:
	./build-function.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts-static.x86_64-linux

build-QvfGenerateScriptsDynamicFunction:
	./build-function.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts.x86_64-linux
