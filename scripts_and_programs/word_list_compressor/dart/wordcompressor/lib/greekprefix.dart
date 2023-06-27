import 'dart:math';

String greekPrefix(String numberStr) {
  Map<int, String> prefixes = {
    0: 'miden',
    1: 'mono',
    2: 'di',
    3: 'tri',
    4: 'tetra',
    5: 'penta',
    6: 'hexa',
    7: 'hepta',
    8: 'octo',
    9: 'ennea',
    10: 'deca',
    11: 'hendeca',
    12: 'dodeca',
    13: 'trideca',
    14: 'tetradeca',
    15: 'pentadeca',
    16: 'hexadeca',
    17: 'heptadeca',
    18: 'octodeca',
    19: 'enneadeca',
    20: 'icosi',
    30: 'triconta',
    40: 'tetraconta',
    50: 'pentaconta',
    60: 'hexaconta',
    70: 'heptaconta',
    80: 'octaconta',
    90: 'enneaconta',
    100: 'hecato',
    200: 'diacosia',
    300: 'tricosia',
    400: 'tetracosia',
    500: 'pentacosia',
    600: 'hexacosia',
    700: 'heptacosia',
    800: 'octocosia',
    900: 'enneacosia',
    1000: 'chili',
    2000: 'dischili',
    3000: 'trischili',
    4000: 'tetraischili',
    5000: 'pentaischili',
    6000: 'hexaischili',
    7000: 'heptaischili',
    8000: 'octoischili',
    9000: 'enneaischili',
    10000: 'myria',
    20000: 'dicismyria',
    30000: 'tricismyria',
    40000: 'tetracismyria',
    50000: 'pentacismyria',
    60000: 'hexacismyria',
    70000: 'heptacismyria',
    80000: 'octocismyria',
    90000: 'enneacismyria',
    100000: 'decakismyria',
    200000: 'dodecakismyria',
    300000: 'tridecakismyria',
    400000: 'tetradecakismyria',
    500000: 'pentadecakismyria',
    600000: 'hexadecakismyria',
    700000: 'heptadecakismyria',
    800000: 'octodecakismyria',
    900000: 'enneadecakismyria',
    1000000: 'hecatomyria',
  };

  int intNumber = int.parse(numberStr);
  if (intNumber < 20) {
    return prefixes[intNumber]!;
  }

  // Reverse the number string
  String reversedNumberStr = numberStr.split('').reversed.join('');

  // Convert each digit to its corresponding prefix
  String result = '';
  for (int i = 0; i < reversedNumberStr.length; i++) {
    int digitInt = int.parse(reversedNumberStr[i]);
    int power = pow(10, i).toInt();
    String prefix = prefixes[digitInt * power]!;
    if (i == 0) {
      if (digitInt == 1 && reversedNumberStr.length > 1) {
        int nextDigit = int.parse(reversedNumberStr[1]);
        if (nextDigit > 0 && nextDigit < 3) {
          result = prefixes[nextDigit * 10]! + result;
        }
      }

      if (digitInt == 0) {
        prefix = '';
      } else if (digitInt == 1) {
        prefix = 'hen';
      } else if (digitInt == 2) {
        prefix = 'do';
      }
    }
    result = prefix + result;
  }

  return result;
}
