package de.tntinteractive.solar;

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonValue;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.BiConsumer;

/**
 * Konvertiert von JSON nach CSV.
 */
public class Solar {
    private static final class DataPoint {
        private double production = Double.NaN;
        private double consumption = Double.NaN;
        private double selfConsumption = Double.NaN;
        private double export = Double.NaN;
        private double storagePower = Double.NaN;
        private double storageEnergyLevel = Double.NaN;
        private double solarProduction = Double.NaN;
        private double imported = Double.NaN;

        public static String header() {
            return ";production" +
                    ";consumption" +
                    ";selfConsumption" +
                    ";export" +
                    ";storagePower" +
                    ";storageEnergyLevel" +
                    ";solarProduction" +
                    ";imported";
        }

        public String toCsv() {
            return ";" + production +
                    ";" + consumption +
                    ";" + selfConsumption +
                    ";" + export +
                    ";" + storagePower +
                    ";" + storageEnergyLevel +
                    ";" + solarProduction +
                    ";" + imported;
        }

        public void setProduction(double production) {
            this.production = production;
        }

        public void setConsumption(double consumption) {
            this.consumption = consumption;
        }

        public void setSelfConsumption(double selfConsumption) {
            this.selfConsumption = selfConsumption;
        }

        public void setExport(double export) {
            this.export = export;
        }

        public void setStoragePower(double storagePower) {
            this.storagePower = storagePower;
        }

        public void setStorageEnergyLevel(double storageEnergyLevel) {
            this.storageEnergyLevel = storageEnergyLevel;
        }

        public void setSolarProduction(double solarProduction) {
            this.solarProduction = solarProduction;
        }

        public void setImported(double imported) {
            this.imported = imported;
        }

    }

    public static void main(String[] args) throws IOException, ParseException {
        System.out.println("Solar");
        JsonArray arr;
        try (FileReader fr = new FileReader("solaredge.json")) {
            arr = Json.parse(fr).asArray();
        }
        TreeMap<String, DataPoint> map = new TreeMap<>();
        for (JsonValue v : arr) {
            addData(map, v, "systemProduction", DataPoint::setProduction);
            addData(map, v, "consumption", DataPoint::setConsumption);
            addData(map, v, "selfConsumption", DataPoint::setSelfConsumption);
            addData(map, v, "export", DataPoint::setExport);
            addData(map, v, "solarProduction", DataPoint::setSolarProduction);
            addData(map, v, "storageEnergyLevel", DataPoint::setStorageEnergyLevel);
            addData(map, v, "storagePower", DataPoint::setStoragePower);
            addData(map, v, "import", DataPoint::setImported);
        }
        try (FileWriter fw = new FileWriter("solar.csv")) {
            fw.write("date;daysSummer;minutesNoon;dow" + DataPoint.header() + "\n");
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            for (Map.Entry<String, DataPoint> e : map.entrySet()) {
                Date d = df.parse(e.getKey());
                fw.write(e.getKey() + ";" +
                        daysSummer(d) + ";" +
                        minutesNoon(d) + ";" +
                        dow(d) +
                        e.getValue().toCsv() + "\n");
            }
        }
    }

    private static int daysSummer(Date d) {
        Date summerStart = new Date(d.getYear(), 5, 21, d.getHours(), d.getMinutes());
        long diff = d.getTime() - summerStart.getTime();
        return (int) (diff / 1000 / 60 / 60 / 24);
    }

    private static int minutesNoon(Date d) {
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(d);
        final int dstOff = cal.getTimeZone().inDaylightTime(d) ? 60 : 0;
        return cal.get(GregorianCalendar.HOUR_OF_DAY) * 60 + cal.get(GregorianCalendar.MINUTE) - 31 - 12 * 60 - dstOff;
    }

    private static String dow(Date d) {
        switch (d.getDay()) {
            case 0:
                return "7_SUN";
            case 1:
                return "1_MON";
            case 2:
                return "2_TUE";
            case 3:
                return "3_WED";
            case 4:
                return "4_THU";
            case 5:
                return "5_FRI";
            case 6:
                return "6_SAT";
            default:
                throw new AssertionError("Invalid day: " + d.getDay());
        }
    }

    private static void addData(
            Map<String, DataPoint> map, JsonValue v, String name, BiConsumer<DataPoint, Double> dp) {
        JsonArray data = v.asObject().get(name).asObject().get("dateSeries").asArray();
        for (JsonValue d : data) {
            String date = d.asObject().get("date").asString();
            JsonValue valueRaw = d.asObject().get("value");
            DataPoint dpt = getOrCreate(map, date);
            if (valueRaw.isNumber()) {
                double value = valueRaw.asDouble();
                dp.accept(dpt, value);
            }
        }
    }

    private static DataPoint getOrCreate(Map<String, DataPoint> map, String date) {
        DataPoint dpt = map.get(date);
        if (dpt == null) {
            dpt = new DataPoint();
            map.put(date, dpt);
        }
        return dpt;
    }
}

