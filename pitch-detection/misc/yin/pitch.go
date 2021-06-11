package yin

func YinPitch1(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	ath := AbsoluteThreshold(&df)
	return sampleRate / ath, ath
}

func YinPitch2(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	CMNDF(&df)
	ath := AbsoluteThreshold(&df)
	return sampleRate / ath, ath
}

func YinPitch3(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	CMNDF(&df)
	lagIncr := NaivePeakPick(&df)
	return sampleRate / lagIncr, lagIncr
}

func YinPitch4(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	lagIncr := NaivePeakPick(&df)
	return sampleRate / lagIncr, lagIncr
}

func YinPitch5(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	CMNDF(&df)
	ath := AbsoluteThreshold(&df)
	ath = ParabolicInterpolation(&df, ath)
	return sampleRate / ath, ath
}

func YinPitch6(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	ath := AbsoluteThreshold(&df)
	ath = ParabolicInterpolation(&df, ath)
	return sampleRate / ath, ath
}

func YinPitch7(samples *[]float64, sampleRate float64) (float64, float64) {
	df := DF(samples)
	CMNDF(&df)
	lagIncr := NaivePeakPick(&df)
	lagIncr = ParabolicInterpolation(&df, lagIncr)
	return sampleRate / lagIncr, lagIncr
}

func ACFPitch1(samples *[]float64, sampleRate float64) (float64, float64) {
	acf := ACF(samples)
	lagIncr := NaivePeakPick(&acf)
	return sampleRate / lagIncr, lagIncr
}

func ACFPitch2(samples *[]float64, sampleRate float64) (float64, float64) {
	acf := ACF(samples)
	lagIncr := NaivePeakPick(&acf)
	lagIncr = ParabolicInterpolation(&acf, lagIncr)
	return sampleRate / lagIncr, lagIncr
}

func NaivePeakPick(f *[]float64) float64 {
	occurences := 0.0
	totalPeakBinIndex := 0

	for i := 1; i < len(*f)-1; i++ {
		if (*f)[i] > (*f)[i-1] && (*f)[i] > (*f)[i+1] {
			occurences += 1.0
			totalPeakBinIndex = i
		}
	}

	lagIncrement := float64(totalPeakBinIndex) / occurences

	return lagIncrement
}
