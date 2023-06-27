import 'dart:io';
import 'package:wordcompressor/file_handler.dart';
import 'package:wordcompressor/greekprefix.dart';

void main() {
  stdout.write('Input file: ');
  String inputFilepath = stdin.readLineSync()!.trim();

  stdout.write('Ignore uppercase and lowercase (y/n): ');
  String filterCaseInput = stdin.readLineSync()!.trim().toLowerCase();
  bool filterCase = filterCaseInput == 'y';

  stdout.write('Size of ngram to calculate (Default: 3): ');
  String ngramSize = stdin.readLineSync()!.trim();
  int ngramSizeInt = ngramSize.isNotEmpty ? int.parse(ngramSize) : 3;

  // Benchmarking code
  var startTime = DateTime.now();

  var rawFileBytes = File(inputFilepath).readAsBytesSync();
  var rawFileString = String.fromCharCodes(rawFileBytes);
  var rawWords = parseJSON(rawFileString);
  if (rawWords.isEmpty) {
    throw Exception('Unsupported JSON key');
  }

  var paddedWords =
      List<String>.generate(rawWords.length, (i) => ' ${rawWords[i]} ');

  var perWordNgrams = List<List<String>>.filled(paddedWords.length, []);
  for (var i = 0; i < paddedWords.length; i++) {
    var ngrams = getTrigrams(paddedWords[i], ngramSizeInt, filterCase);
    if (ngrams.isNotEmpty) {
      perWordNgrams[i] = ngrams;
    } else {
      perWordNgrams[i] = [];
    }
  }

  var flattenedNgrams = <String>[];
  for (var ngrams in perWordNgrams) {
    flattenedNgrams.addAll(ngrams);
  }

  var ngramsHashMap = <String, int>{};
  for (var ngram in flattenedNgrams) {
    ngramsHashMap[ngram] = (ngramsHashMap[ngram] ?? 0) + 1;
  }

  var removalIndexes = <int>[];
  for (var i = 0; i < perWordNgrams.length; i++) {
    var wordNgrams = perWordNgrams[i];
    var foundZero = false;
    for (var ngram in wordNgrams) {
      if (ngramsHashMap[ngram]! <= 1) {
        foundZero = true;
        break;
      }
    }
    if (!foundZero) {
      for (var ngram in wordNgrams) {
        ngramsHashMap[ngram] = ngramsHashMap[ngram]! - 1;
      }
      removalIndexes.add(i);
    }
  }

  for (var i = removalIndexes.length - 1; i >= 0; i--) {
    var index = removalIndexes[i];
    rawWords.removeAt(index);
  }

  var outputFilenames = getOutputFilename(
      inputFilepath, filterCase, greekPrefix(ngramSizeInt.toString()));
  plaintextWrite(rawWords, outputFilenames.plaintextFilename);
  jsonWrite(rawWords, outputFilenames.jsonFilename);

  // Benchmarking code
  var timeElapsed = DateTime.now().difference(startTime);
  print('Generated in: ${timeElapsed.inMilliseconds}ms');
}

List<String> getTrigrams(String inputString, int ngram, bool filterCase) {
  if (inputString.length < ngram) {
    return [];
  }

  var workingInputString = inputString;
  if (filterCase) {
    workingInputString = inputString.toLowerCase();
  }

  var rangeStart = 0;
  var rangeEnd = workingInputString.length - (ngram - 1);

  var middleMapSlices = <String>[];
  for (var i = rangeStart; i < rangeEnd; i++) {
    middleMapSlices.add(workingInputString.substring(i, i + ngram));
  }

  var startSlice = '|${workingInputString.substring(1, (ngram ~/ 2) + 1)}';
  var endSlice =
      '${workingInputString.substring(workingInputString.length - ngram + (ngram - 1) ~/ 2)}|';

  var slices = [...middleMapSlices, startSlice, endSlice];

  return slices;
}
