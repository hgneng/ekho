package yin

func ACF(x *[]float64) []float64 {
	N := len(*x)
	r := make([]float64, N)

	for tau := 0; tau < N; tau++ {
		entry := 0.0
		for j := 1; j < N-tau; j++ {
			entry += (*x)[j] * (*x)[j+tau]
		}
		r[tau] = entry
	}
	return r
}

func DF(x *[]float64) []float64 {
	N := len(*x)
	d := make([]float64, N)

	for tau := 0; tau < N; tau++ {
		entry := 0.0
		for j := 0; j < N-tau; j++ {
			entry += ((*x)[j] - (*x)[j+tau]) * ((*x)[j] - (*x)[j+tau])
		}
		d[tau] = entry
	}
	return d
}

func CMNDF(df *[]float64) {
	(*df)[0] = 1
	runningSum := 0.0

	for tau := 1; tau < len(*df); tau++ {
		runningSum += (*df)[tau]
		(*df)[tau] *= float64(tau) / runningSum
	}
}

var YIN_DEFAULT_THRESHOLD = 0.20

func AbsoluteThreshold(cmndf *[]float64) float64 {
	N := len(*cmndf)

	tau := 0

	for tau = 2; tau < N; tau++ {
		if (*cmndf)[tau] < YIN_DEFAULT_THRESHOLD {
			for tau+1 < N && (*cmndf)[tau+1] < (*cmndf)[tau] {
				tau++
			}
			break
		}
	}
	if tau == N || (*cmndf)[tau] >= YIN_DEFAULT_THRESHOLD {
		return -1
	}
	return float64(tau)
}
