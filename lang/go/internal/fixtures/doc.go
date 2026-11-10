// Package fixtures holds the models models-gen's GoEmitter makes of its fixture schemas (schemas/*.cddl): constructs
// of the profile the shared schemas do not use, but which change the Go output. Compiling them, and the tests beside
// them, is the test. Regenerate with
// tools/sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain lang/go/internal/fixtures/schemas - - lang/go internal/fixtures".
package fixtures
