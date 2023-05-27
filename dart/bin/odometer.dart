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

class Wheel {
  Wheel(this.chars);
  int position = 0;
  List<String> chars;

  bool click() {
    if (++position < chars.length) {
      return false;
    } else {
      position = 0;
      return true;
    }
  }

  String get current => chars[position];
}

List<String> odometer(String digits) {
  List<Wheel> wheels = List.generate(digits.length, (i) {
    var focus = digits[i];
    var chars = t9[focus];
    chars ??= [focus];
    return Wheel(chars);
  });

  List<String> result = List.empty(growable: true);

  while (true) {
    //  Next result string is from current state of 'odometer'.
    result.add(
        List.generate(wheels.length, (index) => wheels[index].current).join());

    var carryOver = true;
    for (var wheel in wheels.reversed) {
      carryOver = wheel.click();
      if (!carryOver) {
        break;
      }
    }
    if (carryOver) {
      return result;
    }
  }
}

void main(List<String> arguments) {
  stderr.write("### Starting odometer.go $version\n");

  var line = stdin.readLineSync();
  while (line != null) {
    var digits = line.trim();
    var results = odometer(digits);
    var resultWord = results.length == 1 ? "result" : "results";

    // Dart has no sprintf for some reason (though it's available as a 3rd part package).
    // The following uses the Dart string interpolation mechanism just to be Dart-ish.
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

  stderr.write("### Finished odometer.go $version\n");
}
