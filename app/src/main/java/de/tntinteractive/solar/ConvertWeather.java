package de.tntinteractive.solar;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.GregorianCalendar;

public class ConvertWeather {

    public static void main(String[] args) throws IOException {
        FileWriter out = new FileWriter("cloudCover.csv");
        BufferedImage image2024 = ImageIO.read(new File("C:\\Source\\Solar\\cloudCover2024.png"));
        DateTimeFormatter fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        out.write("time;color;cloud_cover\n");
        for (int x = 0; x < image2024.getWidth(); x++) {
            for (int y = image2024.getHeight() - 1; y >= 0; y--) {
                int pixel = image2024.getRGB(x, y);
                GregorianCalendar calendar = new GregorianCalendar();
                calendar.set(GregorianCalendar.YEAR, 2024);
                calendar.set(GregorianCalendar.DAY_OF_YEAR, x + 1);
                calendar.set(GregorianCalendar.HOUR_OF_DAY, 23 - y);
                calendar.set(GregorianCalendar.MINUTE, 0);
                out.write(calendar.toZonedDateTime().format(fmt) + ";" + pixel + ";" + mapPixel(pixel) + "\n");
            }
        }
        BufferedImage image2025 = ImageIO.read(new File("C:\\Source\\Solar\\cloudCover2025.png"));
        for (int x = 0; x < 100; x++) {
            for (int y = image2025.getHeight() - 1; y >= 0; y--) {
                int pixel = image2025.getRGB(x, y);
                GregorianCalendar calendar = new GregorianCalendar();
                calendar.set(GregorianCalendar.YEAR, 2025);
                calendar.set(GregorianCalendar.DAY_OF_YEAR, x + 1);
                calendar.set(GregorianCalendar.HOUR_OF_DAY, 23 - y);
                calendar.set(GregorianCalendar.MINUTE, 0);
                out.write(calendar.toZonedDateTime().format(fmt) + ";" + pixel + ";" + mapPixel(pixel) + "\n");
            }
        }
        out.close();
    }

    private static String mapPixel(int pixel) {
        switch (pixel) {
        case 0:
            return "none";
        case -5459786:
            return "mostly_cloudy";
        case -5126947:
            return "mostly_clear";
        case -6775647:
            return "overcast";
        case -3551015:
            return "partly_cloudy";
        case -13073:
            return "no_significant_cloud";
        case -13116:
            return "no_cloud_detected";
        default:
            return "unknown";
        }
    }

}
