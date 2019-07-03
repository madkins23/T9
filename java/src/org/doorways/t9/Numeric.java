package org.doorways.t9;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class Numeric {
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

    private static class Wheel {
        int click = 0;
        List<String> chars;

        Wheel(List<String> chars) {
            this.chars = chars;
        }
    }

    private static Queue<String> numeric(String digits) {
        List<List<String>> columns = new ArrayList<>(digits.length());
        int maximum = 1;

        //  Build the digit base map and character mapping
        //  based on the characters mapped from the original string.
        for (int i = 0; i < digits.length(); i++) {
            String focus = digits.substring(i, i + 1);
            List<String> chars = T9.getOrDefault(focus, List.of(focus));

            columns.add(chars);
            maximum *= chars.size();
        }

        Queue<String> result = new LinkedList<>();

        // Now just count.
        for (int i = 0; i < maximum; i++) {
            LinkedList<String> alpha = new LinkedList<>();
            int number = i;

            for (int col = columns.size() - 1; col >= 0; col--) {
                List<String> chars = columns.get(col);

                alpha.addFirst(chars.get(number % chars.size()));
                number /= chars.size();
            }

            result.add(String.join("", alpha));
        }

        return result;
    }

    public static void main(String[] args) {
        System.err.printf("### Starting Numeric.java %s\n", VERSION);

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();

                Queue<String> result = numeric(line);

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

        System.err.printf("### Finished Numeric.java %s\n", VERSION);
    }
}
