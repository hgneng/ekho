package yin

import (
	"io/ioutil"
	"strconv"
	"strings"
)

func FileToSlice(fname string) ([]float64, error) {
	b, err := ioutil.ReadFile(fname)
	die(err)

	lines := strings.Split(string(b), "\n")
	nums := make([]float64, 0, len(lines))

	for _, l := range lines {
		if len(l) == 0 {
			continue
		}

		n, err := strconv.ParseFloat(l, 64)
		die(err)

		nums = append(nums, n)
	}

	return nums, nil
}

func die(e error) {
	if e != nil {
		panic(e)
	}
}
