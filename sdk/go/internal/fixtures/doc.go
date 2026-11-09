// Package fixtures holds the models models-gen's GoEmitter makes of its fixture schemas (schemas/*.cddl): constructs
// of the profile the shared schemas do not use, but which change the Go output. Compiling them, and the tests beside
// them, is the test. Regenerate with
// sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain sdk/go/internal/fixtures/schemas - - sdk/go internal/fixtures".
package fixtures
