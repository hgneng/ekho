package yin

func ParabolicInterpolation(samples *[]float64, x_ float64) float64 {
	if x_ < 0 {
		return x_
	}
	x := int(x_)
	xAdjusted := 0

	if x < 1 {
		if (*samples)[x] <= (*samples)[x+1] {
			xAdjusted = x
		} else {
			xAdjusted = x + 1
		}
	} else if x > len(*samples)-1 {
		if (*samples)[x] <= (*samples)[x-1] {
			xAdjusted = x
		} else {
			xAdjusted = x - 1
		}
	} else {
		den := (*samples)[x+1] + (*samples)[x-1] - 2*(*samples)[x]
		delta := (*samples)[x-1] - (*samples)[x+1]
		if den == 0.0 {
			return x_
		} else {
			return x_ + delta/(2*den)
		}
	}

	return float64(xAdjusted)
}
