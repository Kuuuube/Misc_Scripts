package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"time"
)

func main() {
	var inputFilepath string
	fmt.Print("Input file: ")
	_, err := fmt.Scanln(&inputFilepath)
	if err != nil {
		panic("Failed to read input.")
	}
	inputFilepath = strings.TrimSpace(inputFilepath)

	var filterCaseInput string
	fmt.Print("Ignore uppercase and lowercase (y/n): ")
	fmt.Scanln(&filterCaseInput)
	filterCase := strings.ToLower(strings.TrimSpace(filterCaseInput)) == "y"

	var ngramSize string
	fmt.Print("Size of ngram to calculate (Default: 3): ")
	fmt.Scanln(&ngramSize)
	ngramSizeInt := 3
	if ngramSize != "" {
		var err error
		ngramSizeInt, err = strconv.Atoi(ngramSize)
		if err != nil {
			panic("Invalid ngram size")
		}
	}

	// benchmarking code
	startTime := time.Now()

	rawFileBytes, err := ioutil.ReadFile(inputFilepath)
	if err != nil {
		panic("Failed to read input")
	}
	rawFileString := string(rawFileBytes)
	rawWords := parseJSON(string(rawFileString))
	if rawWords == nil {
		panic("Unsupported JSON key")
	}

	paddedWords := make([]string, len(rawWords))
	for i, word := range rawWords {
		paddedWords[i] = " " + word + " "
	}

	perWordNgrams := make([][]string, len(paddedWords))
	for i, word := range paddedWords {
		ngrams := getTrigrams(word, ngramSizeInt, filterCase)
		if ngrams != nil {
			perWordNgrams[i] = ngrams
		} else {
			perWordNgrams[i] = make([]string, 0)
		}
	}

	flattenedNgrams := make([]string, 0)
	for _, ngrams := range perWordNgrams {
		flattenedNgrams = append(flattenedNgrams, ngrams...)
	}

	ngramsHashMap := make(map[string]int)
	for _, ngram := range flattenedNgrams {
		ngramsHashMap[ngram]++
	}

	removalIndexes := make([]int, 0)
	for i, wordNgrams := range perWordNgrams {
		foundZero := false
		for _, ngram := range wordNgrams {
			if ngramsHashMap[ngram] <= 1 {
				foundZero = true
				break
			}
		}
		if !foundZero {
			for _, ngram := range wordNgrams {
				ngramsHashMap[ngram]--
			}
			removalIndexes = append(removalIndexes, i)
		}
	}

	for i := len(removalIndexes) - 1; i >= 0; i-- {
		index := removalIndexes[i]
		rawWords = append(rawWords[:index], rawWords[index+1:]...)
	}

	outputFilenames := getOutputFilename(inputFilepath, filterCase, greekPrefix(fmt.Sprint(ngramSizeInt)))
	plaintextWrite(rawWords, outputFilenames.PlaintextFilename)
	jsonWrite(rawWords, outputFilenames.JSONFilename)

	// benchmarking code
	timeElapsed := time.Since(startTime)
	fmt.Printf("Generated in: %s\n", timeElapsed)
	//
}

func getTrigrams(inputString string, ngram int, filterCase bool) []string {
	if len(inputString) < ngram {
		return nil
	}

	workingInputString := inputString
	if filterCase {
		workingInputString = strings.ToLower(inputString)
	}

	rangeStart := 0
	rangeEnd := len(workingInputString) - (ngram - 1)

	middleMapSlices := make([]string, 0, rangeEnd-rangeStart)
	for i := rangeStart; i < rangeEnd; i++ {
		middleMapSlices = append(middleMapSlices, workingInputString[i:i+ngram])
	}

	startSlice := "|" + workingInputString[1:ngram/2]
	endSlice := workingInputString[len(workingInputString)-ngram+(ngram-1)/2:] + "|"

	slices := append(middleMapSlices, startSlice, endSlice)

	return slices
}
