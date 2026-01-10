package de.tntinteractive.solar;

import java.io.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class ExtractConsumption {

    public static void main(String[] args) throws IOException {
        Map<String, String> consumption = new HashMap<String, String>();
        try (FileInputStream fis = new FileInputStream("filtered_data.csv");
                Reader r = new InputStreamReader(fis);
                BufferedReader br = new BufferedReader(r)) {
            String header = br.readLine();
            List<String> headerParts = Arrays.asList(header.split(","));
            int dateIdx = headerParts.indexOf("\"date\"");
            int consIdx = headerParts.indexOf("\"consumption\"");
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(",");
                if (parts[consIdx].equals("NA")) {
                    continue;
                }
                consumption.put(
                        strip(parts[dateIdx], 6, 4),
                        parts[consIdx]);
            }
        }
        try (FileWriter w = new FileWriter("consumption.csv");) {
            Random rand = new Random(123);
            LocalDateTime date = LocalDateTime.parse("2024-01-01T00:00:00");
            while (date.getYear() == 2024) {
                w.write(getValueForDate(consumption, date, rand) + "\n");
                date = date.plusMinutes(15);
            }
        }
    }

    private static String getValueForDate(Map<String, String> consumption, LocalDateTime date, Random r) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM-dd hh:mm");
        String fullKey = date.format(formatter);
        String value = consumption.get(fullKey);
        if (value != null) {
            return value;
        }
        List<String> similarValues = new ArrayList<String>();
        similarValues.add(consumption.get(date.plusMinutes(15).format(formatter)));
        similarValues.add(consumption.get(date.plusMinutes(-15).format(formatter)));
        similarValues.add(consumption.get(date.plusDays(1).format(formatter)));
        similarValues.add(consumption.get(date.plusDays(-1).format(formatter)));
        similarValues.add(consumption.get(date.plusDays(2).format(formatter)));
        similarValues.add(consumption.get(date.plusDays(-2).format(formatter)));
        similarValues.add(new ArrayList<>(consumption.values()).get(r.nextInt(consumption.size())));
        while (similarValues.remove(null)) {
        }
        return similarValues.get(r.nextInt(similarValues.size()));
    }

    private static String strip(String input, int start, int end) {
        return input.substring(start, input.length() - end);
    }
}
