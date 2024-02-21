package pre_process;
import java.nio.file.*;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalTime;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.stream.Collectors;

public class ObadCSVTimeProcessor {

    private static final LocalTime MIDNIGHT = LocalTime.MIDNIGHT;
    private static final LocalTime THRESHOLD_TIME = LocalTime.of(0, 1); // 1 minute after midnight
    private static final String TIME_FORMAT = "HH:mm:ss";

    public static void main(String[] args) {
        String inputFilePath = "path/to/your/input.csv";
        String outputFilePath = "path/to/your/output.csv";

        List<Map<String, String>> records = readCsvFile(inputFilePath);
        List<Map<String, String>> updatedRecords = updateTimesInRecords(records);

        writeCsvFile(updatedRecords, outputFilePath);
    }

    private static List<Map<String, String>> readCsvFile(String filePath) {
        List<Map<String, String>> records = new ArrayList<>();
        Path pathToFile = Paths.get(filePath);

        try (BufferedReader br = Files.newBufferedReader(pathToFile)) {
            String[] header = br.readLine().split(","); // Read the header
            String line;
            while ((line = br.readLine()) != null) {
                String[] attributes = line.split(",");
                Map<String, String> record = new HashMap<>();
                for (int i = 0; i < attributes.length && i < header.length; i++) {
                    record.put(header[i], attributes[i]);
                }
                records.add(record);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return records;
    }

    private static List<Map<String, String>> updateTimesInRecords(List<Map<String, String>> records) {
        for (int i = 0; i < records.size(); i++) {
            Map<String, String> currentRecord = records.get(i);
            LocalTime stopTime = parseTime(currentRecord.get("stop_time"));
            LocalTime doorClosingTime = parseTime(currentRecord.get("door_closing_time"));

            boolean stopTimeAtMidnight = stopTime.equals(MIDNIGHT);
            boolean doorClosingTimeAtMidnight = doorClosingTime.equals(MIDNIGHT);

            if (stopTimeAtMidnight) {
                currentRecord.put("stop_time", "null");
            }

            if (doorClosingTimeAtMidnight) {
                currentRecord.put("door_closing_time", "null");
            }

            if (i > 0) { // Check previous record only if not the first
                Map<String, String> previousRecord = records.get(i - 1);
                LocalTime previousStopTime = parseTime(previousRecord.get("stop_time"));
                LocalTime previousDoorClosingTime = parseTime(previousRecord.get("door_closing_time"));

                if (isTimeCloseToMidnight(previousStopTime) || isTimeCloseToMidnight(previousDoorClosingTime)) {
                    // Handle the logic for times close to midnight here
                    // For now, let's print a message
                    System.out.println("Previous times are close to midnight. Adjusting current times accordingly.");
                    // Implement the required logic
                }
            }
        }
        return records;
    }

    private static LocalTime parseTime(String timeString) {
        try {
            return LocalTime.parse(timeString);
        } catch (DateTimeParseException e) {
            return MIDNIGHT; // If parsing fails, return midnight by default
        }
    }

    private static boolean isTimeCloseToMidnight(LocalTime time) {
        return time.isAfter(MIDNIGHT) && time.isBefore(THRESHOLD_TIME);
    }

    private static void writeCsvFile(List<Map<String, String>> records, String filePath) {
        Path pathToFile = Paths.get(filePath);
        try (BufferedWriter bw = Files.newBufferedWriter(pathToFile)) {
            String header = String.join(",", records.get(0).keySet());
            bw.write(header);
            bw.newLine();

            for (Map<String, String> record : records) {
                String line = record.values().stream()
                        .collect(Collectors.joining(","));
                bw.write(line);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}