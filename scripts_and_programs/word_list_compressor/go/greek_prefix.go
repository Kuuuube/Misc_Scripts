package main

import (
	"strconv"
)

func greekPrefix(numberStr string) string {
	// Define the Greek prefix mappings
	prefixes := map[int]string{
		0:       "miden",
		1:       "mono",
		2:       "di",
		3:       "tri",
		4:       "tetra",
		5:       "penta",
		6:       "hexa",
		7:       "hepta",
		8:       "octo",
		9:       "ennea",
		10:      "deca",
		11:      "hendeca",
		12:      "dodeca",
		13:      "trideca",
		14:      "tetradeca",
		15:      "pentadeca",
		16:      "hexadeca",
		17:      "heptadeca",
		18:      "octodeca",
		19:      "enneadeca",
		20:      "icosi",
		30:      "triconta",
		40:      "tetraconta",
		50:      "pentaconta",
		60:      "hexaconta",
		70:      "heptaconta",
		80:      "octaconta",
		90:      "enneaconta",
		100:     "hecato",
		200:     "diacosia",
		300:     "tricosia",
		400:     "tetracosia",
		500:     "pentacosia",
		600:     "hexacosia",
		700:     "heptacosia",
		800:     "octocosia",
		900:     "enneacosia",
		1000:    "chili",
		2000:    "dischili",
		3000:    "trischili",
		4000:    "tetraischili",
		5000:    "pentaischili",
		6000:    "hexaischili",
		7000:    "heptaischili",
		8000:    "octoischili",
		9000:    "enneaischili",
		10000:   "myria",
		20000:   "dicismyria",
		30000:   "tricismyria",
		40000:   "tetracismyria",
		50000:   "pentacismyria",
		60000:   "hexacismyria",
		70000:   "heptacismyria",
		80000:   "octocismyria",
		90000:   "enneacismyria",
		100000:  "decakismyria",
		200000:  "dodecakismyria",
		300000:  "tridecakismyria",
		400000:  "tetradecakismyria",
		500000:  "pentadecakismyria",
		600000:  "hexadecakismyria",
		700000:  "heptadecakismyria",
		800000:  "octodecakismyria",
		900000:  "enneadecakismyria",
		1000000: "hecatomyria",
	}

	// Reverse the number string
	reversedNumberStr := reverseString(numberStr)

	// Convert each digit to its corresponding prefix
	var result string
	for i, digit := range reversedNumberStr {
		digitInt, _ := strconv.Atoi(string(digit))
		power := intPow(10, i)
		prefix := prefixes[digitInt*power]
		if i == 0 {
			if digitInt == 1 && len(reversedNumberStr) > 1 {
				nextDigit, _ := strconv.Atoi(string(reversedNumberStr[1]))
				if nextDigit > 0 && nextDigit < 3 {
					result = prefixes[nextDigit*10] + result
				}
			}

			if digitInt == 0 {
				prefix = ""
			} else if digitInt == 1 {
				prefix = "hen"
			} else if digitInt == 2 {
				prefix = "do"
			}
		}
		result = prefix + result
	}

	return result
}

func intPow(base, exponent int) int {
	result := 1
	for i := 0; i < exponent; i++ {
		result *= base
	}
	return result
}

func reverseString(str string) string {
	runes := []rune(str)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}
