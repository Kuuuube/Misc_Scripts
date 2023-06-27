import 'dart:convert';
import 'dart:io';

class OutputFilenames {
  final String plaintextFilename;
  final String jsonFilename;

  OutputFilenames(
      {required this.plaintextFilename, required this.jsonFilename});
}

List<String> parseJSON(String json) {
  String replacedString = json.replaceAll(RegExp(r'\s+|\\t+|"+|\n+\r+'), '');
  List<String> filteredStringWords = replacedString.split('words:[');
  List<String> filteredStringTexts = replacedString.split('texts:[');

  List<String> wordList = [];
  if (filteredStringWords.length > 1) {
    wordList = filteredStringWords;
  } else if (filteredStringTexts.length > 1) {
    wordList = filteredStringTexts;
  } else {
    return [];
  }

  List<String> splitEnd = wordList[1].split(']');
  List<String> splitCommas = splitEnd[0].split(',');

  return splitCommas;
}

void plaintextWrite(List<String> inputWords, String filepath) {
  String plaintextString = inputWords.join(' ');
  File(filepath).writeAsStringSync(plaintextString);
}

void jsonWrite(List<String> inputWords, String filepath) {
  Map<String, dynamic> jsonData = {
    'total': inputWords.length,
    'texts': inputWords,
  };

  var encoder = JsonEncoder.withIndent("    ");
  String jsonString = encoder.convert(jsonData);
  File(filepath).writeAsStringSync(jsonString);
}

OutputFilenames getOutputFilename(
    String inputFilename, bool filterCase, String ngramGreekPrefix) {
  List<String> splitString = inputFilename.split('.');
  int splitLen = splitString.length;
  if (splitLen > 1) {
    splitString = splitString.sublist(0, splitLen - 1);
  }

  String ignorecaseString = "";
  if (filterCase) {
    ignorecaseString = "_ignorecase";
  }

  String baseFilename = splitString.join('.');
  String plaintextFilename = '$baseFilename'
      '_$ngramGreekPrefix'
      'gram_compressed$ignorecaseString.txt';
  String jsonFilename = '$baseFilename'
      '_$ngramGreekPrefix'
      'gram_compressed$ignorecaseString.json';

  return OutputFilenames(
      plaintextFilename: plaintextFilename, jsonFilename: jsonFilename);
}
