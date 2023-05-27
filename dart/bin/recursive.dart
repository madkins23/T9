import 'dart:io';

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

List<String> recursive(String remaining, [String starting = '']) {
  var focus = remaining[0];

  String tail = '';
  if (remaining.isNotEmpty) {
    tail = remaining.substring(1);
  }

  List<String> result = List.empty(growable: true);

  var chars = t9[focus];
  chars ??= [focus];
  for (var char in chars) {
    var stem = starting + char;

    if (tail.isNotEmpty) {
      result.addAll(recursive(tail, stem));
    } else {
      result.add(stem);
    }
  }

  return result;
}

void main(List<String> arguments) {
  stderr.write("### Starting recursive.go $version\n");

  var line = stdin.readLineSync();
  while (line != null) {
    var digits = line.trim();
    var results = recursive(digits);
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

  stderr.write("### Finished recursive.go $version\n");
}
