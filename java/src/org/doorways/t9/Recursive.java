package org.doorways.t9;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Recursive {
    static final String VERSION = "1.0.0";
    static final Map<String, List<String>> T9 = Map.of(
            "0", List.of("0"),
            "1", List.of("1"),
            "2", List.of("A", "B", "C"),
            "3", List.of("D", "E", "F"),
            "4", List.of("G", "H", "I"),
            "5", List.of("J", "K", "L"),
            "6", List.of("M", "N", "O"),
            "7", List.of("P", "Q", "R", "S"),
            "8", List.of("T", "U", "V"),
            "9", List.of("W", "X", "Y", "Z"));

    private static LinkedList<String> recursive(String starting, String remaining) {
        assert remaining.length() > 0;

        String focus = remaining.substring(0, 1);
        String tail = remaining.substring(1);
        List<String> chars = T9.getOrDefault(focus, List.of(focus));
        LinkedList<String> result = new LinkedList<>();

        for (String t9char : chars) {
            String stem = starting + t9char;

            if (tail.length() > 0) {
                result.addAll(recursive(stem, tail));
            } else {
                result.add(stem);
            }
        }

        return result;
    }

    public static void main(String[] args) {
        System.err.printf("### Starting Recursive.java %s\n", VERSION);

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();

                LinkedList<String> result = recursive("", line);

                System.out.printf("%-25s%7d result%s\n", line, result.size(), result.size() == 1 ? "" : "s");

                double count = 79.0 / (line.length() + 1.0);

                while (!result.isEmpty()) {
                    System.out.print(" ");

                    for (int i = 0; i < count; i++) {
                        if (result.isEmpty()) {
                            break;
                        }

                        System.out.printf(" %s", result.remove());
                    }

                    System.out.println();
                }
            }
        } catch (IOException e) {
            System.err.printf("*** Error reading from STDIN:  %s\n", e);
        }

        System.err.printf("### Finished Recursive.java %s\n", VERSION);
    }
}
