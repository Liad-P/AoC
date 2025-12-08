package Y2025.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

public class Utils {

    public static Path getInputFileOfDay(int year, int day){
        Path inputDirPath = Path.of("AoC" + year,"input");
        if (!inputDirPath.toFile().exists()){
            throw new IllegalStateException("No input data directory: " + inputDirPath.toAbsolutePath());
        }
        try (var inputFiles = Files.walk(inputDirPath)){
            Optional<Path> daysInputFile = inputFiles
                .filter(p -> !p.toFile().isDirectory())
                .filter(p -> p.toString().endsWith(day + ".txt")).findFirst();
            if (daysInputFile.isPresent()){
                return daysInputFile.get();
            }
            else{
                throw new IllegalStateException("Could not file data file for this day, the file should be in the format \"*{day}.txt\"");
            }
        }
        catch (IOException e){
            throw new RuntimeException("Error while finding input file for day: " + day, e.fillInStackTrace());
        }
        
    }
    public static Path getInputFileOfName(int year, String fileName){
        Path inputDirPath = Path.of("AoC" + year,"input");
        if (!inputDirPath.toFile().exists()){
            throw new IllegalStateException("No input data directory: " + inputDirPath.toAbsolutePath());
        }
        try (var inputFiles = Files.walk(inputDirPath)){
            Optional<Path> daysInputFile = inputFiles
                .filter(p -> !p.toFile().isDirectory())
                .filter(p -> p.toString().endsWith(fileName)).findFirst();
            if (daysInputFile.isPresent()){
                return daysInputFile.get();
            }
            else{
                throw new IllegalStateException("Could not file data file for this day, the file should be in the format \"*{day}.txt\"");
            }
        }
        catch (IOException e){
            throw new RuntimeException("Error while finding input file for : " + fileName, e.fillInStackTrace());
        }
        
    }

}
