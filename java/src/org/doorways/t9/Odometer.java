package org.doorways.t9;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Collectors;

public class Odometer {
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

    private static Queue<String> odometer(String digits) {
        Wheel[] wheels = new Wheel[digits.length()];

        //  Build the 'odometer' with different wheels at each position
        //  based on the chars mapped from the original string.
        for (int i = 0; i < digits.length(); i++) {
            String focus = digits.substring(i, i + 1);

            wheels[i] = new Wheel(T9.getOrDefault(focus, List.of(focus)));
        }

        Queue<String> result = new LinkedList<>();

        while (true) {
            // Next string is from current state of 'odometer'.
            result.add(Arrays.stream(wheels)
                    .map(w -> w.chars.get(w.click))
                    .collect(Collectors.joining("")));

            // Start clicking wheels from the right.
            int which = wheels.length - 1;

            while (which >= 0) {
                Wheel wheel = wheels[which];

                wheel.click++;

                if (wheel.click < wheel.chars.size()) {
                    // Click value still within range, stop clicking.
                    break;
                }

                // Zero this wheel and move on the one to the left.
                wheel.click = 0;
                which--;
            }

            if (which < 0) {
                // No more wheels to click, we're done.
                break;
            }
        }

        return result;
    }

    public static void main(String[] args) {
        System.err.printf("### Starting Odometer.java %s\n", VERSION);

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();

                Queue<String> result = odometer(line);

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

        System.err.printf("### Finished Odometer.java %s\n", VERSION);
    }
}
