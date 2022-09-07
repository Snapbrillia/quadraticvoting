build-QvfGenerateScriptsStaticFunction:
	./build-tools/build-function.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts-static.x86_64-linux

build-QvfGenerateScriptsDynamicFunction:
	./build-tools/build-function.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts.x86_64-linux

build-QvfGenerateScriptsLayer:
	./build-tools/build-layer.sh "$(ARTIFACTS_DIR)" qvf-generate-scripts .#qvf-generate-scripts.x86_64-linux
