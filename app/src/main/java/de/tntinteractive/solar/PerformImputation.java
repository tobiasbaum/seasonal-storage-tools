package de.tntinteractive.solar;

import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Fügt fehlende Datensätze hinzu, indem diese aus benachbarten Datensätzen
 * zufällig abgeleitet werden.
 */
public class PerformImputation {

    private static final long MS_PER_QUARTER = 15 * 60 * 1000L;

    private static final class Day {

        private Date startOfDay;
        private QuarterHourRecord[] records;

        public Day(Date startOfDay) {
            this.startOfDay = startOfDay;
            this.records = new QuarterHourRecord[4 * 24];
        }

        public void setRecord(Date time, QuarterHourRecord record) {
            long diff = time.getTime() - this.startOfDay.getTime();
            assert diff % MS_PER_QUARTER == 0;
            this.records[(int) (diff / MS_PER_QUARTER)] = record;
        }

        public boolean isComplete() {
            for (QuarterHourRecord record : records) {
                if (record == null) {
                    return false;
                }
            }
            return true;
        }

        public boolean isDefinedForAllGaps(Day day) {
            for (int i = 0; i < day.records.length; i++) {
                if (day.isUndefinedFor(i) && this.isUndefinedFor(i)) {
                    return false;
                }
            }
            return true;
        }

        private boolean isUndefinedFor(int i) {
            return this.records[i] == null || this.records[i].containsNaN();
        }

        public void fillGapsWith(Day selected) {
            for (int i = 0; i < this.records.length; i++) {
                if (this.records[i] == null) {
                    this.records[i] = selected.records[i];
                }
            }
        }

        public boolean containsNaNs() {
            for (QuarterHourRecord record : records) {
                if (record != null && record.containsNaN()) {
                    return true;
                }
            }
            return false;
        }

        public void fillNaNsWith(Day selected) {
            for (int i = 0; i < this.records.length; i++) {
                if (this.records[i].containsNaN()) {
                    this.records[i] = this.records[i].fillNaNsWith(selected.records[i]);
                }
            }
        }
    }

    private record QuarterHourRecord(
            double consumption,
            double selfConsumption,
            double export,
            double storagePower,
            double storageEnergyLevel,
            double solarProduction,
            double imported) {

        public boolean containsNaN() {
            return Double.isNaN(consumption)
                || Double.isNaN(selfConsumption)
                || Double.isNaN(export)
                || Double.isNaN(storagePower)
                || Double.isNaN(storageEnergyLevel)
                || Double.isNaN(solarProduction)
                || Double.isNaN(imported);
        }

        public QuarterHourRecord fillNaNsWith(QuarterHourRecord record) {
            return new QuarterHourRecord(
                    replaceNaN(this.consumption, record.consumption),
                    replaceNaN(this.selfConsumption, record.selfConsumption),
                    replaceNaN(this.export, record.export),
                    replaceNaN(this.storagePower, record.storagePower),
                    replaceNaN(this.storageEnergyLevel, record.storageEnergyLevel),
                    replaceNaN(this.solarProduction, record.solarProduction),
                    replaceNaN(this.imported, record.imported)
            );
        }

        private static double replaceNaN(double value, double otherValue) {
            return Double.isNaN(value) ? otherValue : value;
        }
    }

    private static final class IncompleteData {

        private static final long MS_PER_HOUR = 60 * 60 * 1000L;
        private static final long MS_PER_DAY = 24 * MS_PER_HOUR;

        private TreeMap<Date, Day> days = new TreeMap<>();

        public void fillDay(int i, Random random) {
            Day day = getOrCreateDay(i);
            if (!day.isComplete()) {
                imputeWholeRecords(i, random, day);
            }
            if (day.containsNaNs()) {
                imputeNaNs(i, random, day);
            }
        }

        private void imputeWholeRecords(int i, Random random, Day day) {
            List<Day> templates = pickTemplates(i, day);
            Day selected = templates.get(random.nextInt(templates.size()));
            day.fillGapsWith(selected);
        }

        private List<Day> pickTemplates(int i, Day day) {
            List<Day> templates = new ArrayList<>();
            // zwei Beispiele für den gleichen Wochentag aus der näheren Vergangenheit
            int weeksBack = 0;
            do {
                weeksBack++;
                Day potentialTemplate = getOrCreateDay(i - 7 * weeksBack);
                if (potentialTemplate.isDefinedForAllGaps(day)) {
                    templates.add(potentialTemplate);
                }
            } while (templates.size() < 2 && weeksBack < 56);
            // zwei Beispiele für den gleichen Wochentag aus der näheren Zukunft
            int weeksForth = 0;
            do {
                weeksForth++;
                Day potentialTemplate = getOrCreateDay(i + 7 * weeksForth);
                if (potentialTemplate.isDefinedForAllGaps(day)) {
                    templates.add(potentialTemplate);
                }
            } while (templates.size() < 4 && weeksForth < 56);
            return templates;
        }

        private void imputeNaNs(int i, Random random, Day day) {
            List<Day> templates = pickTemplates(i, day);
            Day selected = templates.get(random.nextInt(templates.size()));
            day.fillNaNsWith(selected);
            assert !day.containsNaNs();
        }

        public Day getOrCreateDay(int i) {
            int dayCount = this.days();
            int iBound = ((i % dayCount) + dayCount) % dayCount;
            Date first = days.firstKey();
            Date key = new Date(first.getTime() + iBound * MS_PER_DAY);
            Day day = days.get(key);
            if (day == null) {
                day = new Day(key);
                days.put(key, day);
            }
            return day;
        }

        public int days() {
            Date first = days.firstKey();
            Date last = days.lastKey();
            long diff = last.getTime() - first.getTime();
            return (int) (diff / MS_PER_DAY);
        }

        public void add(Date dateTime, QuarterHourRecord record) {
            // Tagesbeginn immer in GMT+1 (d.h. ohne Sommerzeitumstellung), damit jeder Tag gleich viele Records hat
            Date startOfDay = new Date((dateTime.getTime() + MS_PER_HOUR) / MS_PER_DAY * MS_PER_DAY - MS_PER_HOUR);
            Day day = days.get(startOfDay);
            if (day == null) {
                day = new Day(startOfDay);
                days.put(startOfDay, day);
            }
            day.setRecord(dateTime, record);
        }

        public void write(FileOutputStream fos) throws IOException {
            Writer w = new OutputStreamWriter(fos);
            BufferedWriter bw = new BufferedWriter(w);
            bw.write("dateHuman;dateMs;consumption;selfConsumption;export;storagePower;storageEnergyLevel;solarProduction;imported\n");
            for (Day day : days.values()) {
                for (int i = 0; i < day.records.length; i++) {
                    QuarterHourRecord r = day.records[i];
                    Date d = new Date(day.startOfDay.getTime() + i * MS_PER_QUARTER);
                    bw.write("\"" + DATE_FORMAT.format(d) + "\";" +
                            d.getTime() + ";" +
                            r.consumption + ";" +
                            r.selfConsumption + ";" +
                            r.export + ";" +
                            r.storagePower + ";" +
                            r.storageEnergyLevel + ";" +
                            r.solarProduction + ";" +
                            r.imported + "\n");
                }
            }
            bw.flush();
        }
    }

    public static void main(String[] args) throws Exception {
        for (int seed = 0; seed < 5; seed++) {
            Random random = new Random(seed);
            IncompleteData data = readData();
            for (int i = 0; i < data.days(); i++) {
                data.fillDay(i, random);
            }
            writeData(data, seed);
        }
    }

    private static void writeData(IncompleteData data, int i) throws IOException {
        try (FileOutputStream fos = new FileOutputStream("imputed_data_" + i + ".csv")) {
            data.write(fos);
        }
    }

    private static IncompleteData readData() throws IOException, ParseException {
        IncompleteData ret = new IncompleteData();
        try (FileInputStream fis = new FileInputStream("filtered_data.csv");
             Reader r = new InputStreamReader(fis);
             BufferedReader br = new BufferedReader(r)) {
            String header = br.readLine();
            List<String> headerParts = Arrays.asList(header.split(","));
            int dateIdx = headerParts.indexOf("\"date\"");
            int consumptionIdx = headerParts.indexOf("\"consumption\"");
            int selfConsumptionIdx = headerParts.indexOf("\"selfConsumption\"");
            int exportIdx = headerParts.indexOf("\"export\"");
            int storagePowerIdx = headerParts.indexOf("\"storagePower\"");
            int storageEnergyLevelIdx = headerParts.indexOf("\"storageEnergyLevel\"");
            int solarProductionIdx = headerParts.indexOf("\"solarProduction\"");
            int importedIdx = headerParts.indexOf("\"imported\"");
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(",");
                Date date = parseDate(parts[dateIdx]);
                QuarterHourRecord record = new QuarterHourRecord(
                        parse(parts[consumptionIdx]),
                        parse(parts[selfConsumptionIdx]),
                        parse(parts[exportIdx]),
                        parse(parts[storagePowerIdx]),
                        parse(parts[storageEnergyLevelIdx]),
                        parse(parts[solarProductionIdx]),
                        parse(parts[importedIdx])
                );
                ret.add(date, record);
            }
        }
        return ret;
    }

    private static SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private static Date parseDate(String part) throws ParseException {
        assert part.startsWith("\"") && part.endsWith("\"");
        return DATE_FORMAT.parse(part.substring(1, part.length() - 1));
    }

    private static double parse(String part) {
        if (part.equals("NA")) {
            return Double.NaN;
        }
        return Double.parseDouble(part);
    }
}
