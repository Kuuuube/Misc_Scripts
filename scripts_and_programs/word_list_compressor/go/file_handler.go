package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type OutputFilenames struct {
	PlaintextFilename string
	JSONFilename      string
}

func parseJSON(json string) []string {
	replacedString := strings.NewReplacer(" ", "", "\t", "", "\"", "", "\n", "", "\r", "").Replace(json)
	filteredStringWords := strings.Split(replacedString, "words:[")
	filteredStringTexts := strings.Split(replacedString, "texts:[")

	wordList := make([]string, 0)
	if len(filteredStringWords) > 1 {
		wordList = filteredStringWords
	} else if len(filteredStringTexts) > 1 {
		wordList = filteredStringTexts
	} else {
		return nil
	}

	splitEnd := strings.Split(wordList[1], "]")
	splitCommas := strings.Split(splitEnd[0], ",")

	return splitCommas
}

func plaintextWrite(inputWords []string, filepath string) {
	plaintextString := strings.Join(inputWords, " ")
	err := ioutil.WriteFile(filepath, []byte(plaintextString), 0644)
	if err != nil {
		panic("Couldn't open file.")
	}
}

func jsonWrite(inputWords []string, filepath string) {
	jsonString := fmt.Sprintf("{\n    \"total\": %d,\n    \"texts\": [\n", len(inputWords))
	for i, word := range inputWords {
		jsonString += fmt.Sprintf("        \"%s\"", word)
		if i < len(inputWords)-1 {
			jsonString += ",\n"
		}
	}
	jsonString += "\n    ]\n}"
	err := ioutil.WriteFile(filepath, []byte(jsonString), 0644)
	if err != nil {
		panic("Couldn't open file.")
	}
}

func getOutputFilename(inputFilename string, filterCase bool, ngramGreekPrefix string) OutputFilenames {
	splitString := strings.Split(inputFilename, ".")
	splitLen := len(splitString)
	if splitLen > 1 {
		splitString = splitString[:splitLen-1]
	}

	ignorecaseString := ""
	if filterCase {
		ignorecaseString = "_ignorecase"
	}

	baseFilename := strings.Join(splitString, ".")
	plaintextFilename := fmt.Sprintf("%s_%sgram_compressed%s.txt", baseFilename, ngramGreekPrefix, ignorecaseString)
	jsonFilename := fmt.Sprintf("%s_%sgram_compressed%s.json", baseFilename, ngramGreekPrefix, ignorecaseString)

	return OutputFilenames{
		PlaintextFilename: plaintextFilename,
		JSONFilename:      jsonFilename,
	}
}
