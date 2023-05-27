import 'dart:io';
import 'dart:math';

var t9 = {
  '0': ['0'],
  '1': ['1'],
  '2': ['A', 'B', 'C'],
  '3': ['D', 'E', 'F'],
  '4': ['G', 'H', 'I'],
  '5': ['J', 'K', 'L'],
  '6': ['M', 'N', 'O'],
  '7': ['P', 'Q', 'R', 'S'],
  '8': ['T', 'U', 'V'],
  '9': ['W', 'X', 'Y', 'Z'],
};

const version = '1.0.0';

List<String> numeric(String digits) {
  // Build the digit base map and character mapping
  // based on the characters mapped from the original string.
  var maximum = 1;
  var columns = List.generate(digits.length, (i) {
    var focus = digits[i];
    var chars = t9[focus];
    chars ??= [focus];
    maximum *= chars.length;
    return chars;
  });

  List<String> result = List.empty(growable: true);

  // Now just count.
  for (var i = 0; i < maximum; i++) {
    var number = i;
    List<String> temp = List.empty(growable: true);

    // From right to left pick character based on modulus by divisor
    // and divide number by divisor.
    for (var column in columns.reversed) {
      var divisor = column.length;
      temp.insert(0, column[number % divisor]);
      number ~/= divisor;
    }

    result.add(temp.join());
  }

  return result;
}

void main(List<String> arguments) {
  stderr.write("### Starting numeric.go $version\n");

  var line = stdin.readLineSync();
  while (line != null) {
    var digits = line.trim();
    var results = numeric(digits);
    var resultWord = results.length == 1 ? "result" : "results";

    // Dart has no sprintf for some reason (though it's available as a 3rd part package).
    // The following uses the Dart string interpolation mechanism.
    stdout.write(
        "${digits.padRight(25)}${results.length.toString().padLeft(7, " ")} $resultWord\n");

    var count = 79 ~/ (digits.length + 1);
    var r = 0;
    while (r < results.length) {
      stdout.write(' ');
      for (var i = 0; i <= count; i++) {
        if (r < results.length) {
          stdout.write(" ${results[r++]}");
        } else {
          break;
        }
      }
      stdout.write("\n");
    }
    line = stdin.readLineSync();
  }

  stderr.write("### Finished numeric.go $version\n");
}
