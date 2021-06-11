package yin

import (
	"fmt"
	"os"
	"testing"
)

func TestACF(t *testing.T) {
	if os.Getenv("PRINT_PLOTS") == "" {
		t.Skip("skipping print test")
	}

	floats, err := FileToSlice("../samples/F-4_48000_classicalguitar.txt")
	die(err)

	acorr := ACF(&floats)

	for _, sample := range acorr {
		fmt.Printf("%v\n", sample)
	}
}

func TestDF(t *testing.T) {
	if os.Getenv("PRINT_PLOTS") == "" {
		t.Skip("skipping print test")
	}

	floats, err := FileToSlice("../samples/F-4_48000_classicalguitar.txt")
	die(err)

	df := DF(&floats)

	for _, sample := range df {
		fmt.Printf("%v\n", sample)
	}
}

func TestCMNDF(t *testing.T) {
	if os.Getenv("PRINT_PLOTS") == "" {
		t.Skip("skipping print test")
	}

	floats, err := FileToSlice("../samples/F-4_48000_classicalguitar.txt")
	die(err)

	df := DF(&floats)
	CMNDF(&df)

	for _, sample := range df {
		fmt.Printf("%v\n", sample)
	}
}

func TestCMNDFViolaClean(t *testing.T) {
	if os.Getenv("PRINT_PLOTS") == "" {
		t.Skip("skipping print test")
	}

	floats, err := FileToSlice("../samples/E3_44100_viola_degraded_0.txt")
	die(err)

	df := DF(&floats)
	CMNDF(&df)

	for _, sample := range df {
		fmt.Printf("%v\n", sample)
	}
}

func TestCMNDFViolaDirty(t *testing.T) {
	if os.Getenv("PRINT_PLOTS") == "" {
		t.Skip("skipping print test")
	}

	floats, err := FileToSlice("../samples/E3_44100_viola_degraded_4.txt")
	die(err)

	df := DF(&floats)
	CMNDF(&df)

	for _, sample := range df {
		fmt.Printf("%v\n", sample)
	}
}

func TestYinPitchManyWays(t *testing.T) {
	samples, err := FileToSlice("../samples/F-4_48000_classicalguitar.txt")
	die(err)

	t.Log("Method1")
	pitch, lagIncr := YinPitch1(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method2")
	pitch, lagIncr = YinPitch2(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method3")
	pitch, lagIncr = YinPitch3(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method4")
	pitch, lagIncr = YinPitch4(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method5")
	pitch, lagIncr = YinPitch5(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method6")
	pitch, lagIncr = YinPitch6(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method7")
	pitch, lagIncr = YinPitch7(&samples, 48000.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)
}

func TestACFPitch(t *testing.T) {
	samples, err := FileToSlice("../samples/F-4_48000_classicalguitar.txt")
	die(err)

	t.Log("Method1")
	pitch, lagIncr := ACFPitch1(&samples, 48000.0)
	t.Log("Pitch: ", pitch)
	t.Log("Lag increment: ", lagIncr)

	t.Log("Method2")
	pitch, lagIncr = ACFPitch2(&samples, 48000.0)
	t.Log("Pitch: ", pitch)
	t.Log("Lag increment: ", lagIncr)
}

func TestYinPitchManyWays82Hz(t *testing.T) {
	samples, err := FileToSlice("../samples/E2_44100_acousticguitar.txt")
	die(err)

	t.Log("Method1")
	pitch, lagIncr := YinPitch1(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method2")
	pitch, lagIncr = YinPitch2(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method3")
	pitch, lagIncr = YinPitch3(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method4")
	pitch, lagIncr = YinPitch4(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method5")
	pitch, lagIncr = YinPitch5(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method6")
	pitch, lagIncr = YinPitch6(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)

	t.Log("Method7")
	pitch, lagIncr = YinPitch7(&samples, 44100.0)
	t.Log("\tPitch: ", pitch)
	t.Log("\tLag increment: ", lagIncr)
}

func BenchmarkYinTimeDomainHugeSamples(b *testing.B) {
	samples, err := FileToSlice("../samples/A4_44100_violin.txt")
	die(err)

	for n := 0; n < b.N; n++ {
		YinPitch5(&samples, 441000)
	}
}
