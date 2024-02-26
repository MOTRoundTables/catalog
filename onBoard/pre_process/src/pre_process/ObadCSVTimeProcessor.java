package pre_process;
import java.io.*;
import java.util.*;

public class ObadCSVTimeProcessor {
    public static void main(String[] args) {
        String inputFilePath = "E:\\git\\catalog\\onBoard\\pre_process\\data\\in\\obad.csv";
        String outputFilePath = "E:\\git\\catalog\\onBoard\\pre_process\\data\\out\\obad_proccesed.csv";

        // Define the list of trip_id_unique values to skip
        Set<String> skipTripIds = new HashSet<>(Arrays.asList(
            "110837�", "115419�", "121187�", "220344�", "221569�", "221604�",
            "310371�", "310475�", "310500�", "310952�", "311079�", "311375�",
            "311499�", "311544�", "311763�", "311786�", "311797�", "313193�",
            "313829�", "313843�", "314229�", "317230�", "320031�", "412840�",
            "415407�", "415541�", "416744�", "416997�", "417018�", "417288�",
            "417853�"));

        // Map to hold the last station index for each trip_id_unique
        Map<String, Integer> lastStationIndexMap = checkLastStation(inputFilePath);

        readFileChangeToNull(inputFilePath, outputFilePath, skipTripIds, lastStationIndexMap);
    }

	private static void readFileChangeToNull(String inputFilePath, String outputFilePath, Set<String> skipTripIds,
			Map<String, Integer> lastStationIndexMap) {
		try (
            BufferedReader reader = new BufferedReader(new FileReader(inputFilePath));
            BufferedWriter writer = new BufferedWriter(new FileWriter(outputFilePath))
        ) {
            String line;
            boolean isHeader = true;

            // Read the file line by line
            while ((line = reader.readLine()) != null) {
                if (isHeader) {
                    // Write the header to the output file and add the new field
                    writer.write(line + ",skipped_trip_id_unique\n");
                    isHeader = false;
                    continue;
                }
                
                int skipped = 0;
                String[] values = line.split(",");
                String tripIdUnique = values[2/* Index of trip_id_unique in CSV */];
                
                if (skipTripIds.contains(tripIdUnique)){
                	skipped = 1;
                    System.out.println("Found trip_id_unique = "+ tripIdUnique + " to skip");
                    writer.write(String.join(",", values) + "," + skipped + "\n");
                }
                else
                {
                	int stationIndex = Integer.parseInt(values[4/* Index of station_index in CSV */]);
                    String stopTime = values[7/* Index of stop_time in CSV */];
                    
                    int passengersUp = Integer.parseInt(values[11/* Index of passengers_up in CSV */]);
                    int passengersDown = Integer.parseInt(values[12/* Index of passengers_down in CSV */]);
                    
                    // check if current station is last tation
                    boolean isLastStation = stationIndex == lastStationIndexMap.get(tripIdUnique);
                    

                    // Null which skips first stop and last stop station
                    if (stationIndex != 1 && (!isLastStation) ) {
                        // Logic to modify stop_time based on given conditions time 00:00 + pass up/down = 0
                        if(stopTime.equals("00:00")  && 
                            passengersUp == 0 && passengersDown == 0) {
                            stopTime = "Null";
                            values[7] = stopTime;
                        }
                    }


                    // Write the modified record to the output file
                    writer.write(String.join(",", values) + "," + skipped + "\n");
                }


            }
        } catch (IOException e) {
            e.printStackTrace();
        }
	}

	private static Map<String, Integer> checkLastStation(String inputFilePath) {
		Map<String, Integer> lastStationIndexMap = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
            String line;
            boolean isHeader = true;

            // First pass: identify the last station index for each trip_id_unique
            while ((line = reader.readLine()) != null) {
                if (isHeader) {
                    isHeader = false; // Skip header
                    continue;
                }

                String[] values = line.split(",");
                String tripIdUnique = values[2/* Index of trip_id_unique in CSV */];
                int stationIndex = Integer.parseInt(values[4/* Index of station_index in CSV */]);

                // Update the last station index for each trip_id_unique
                lastStationIndexMap.put(tripIdUnique, Math.max(lastStationIndexMap.getOrDefault(tripIdUnique, 0), stationIndex));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
		return lastStationIndexMap;
	}
}
