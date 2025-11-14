
import java.lang.reflect.InvocationTargetException;

import Y2025.days.*;

public class Main {
    public static void main(String[] args) {
        if (args.length <2) {
            System.out.println("Usage: java Main <year> <day>");
            System.out.println("Example: java Main 2025 1");
            return;
        }

        int year = Integer.parseInt(args[0]);
        int day = Integer.parseInt(args[1]);
        System.out.println("Running Advent of Code 2025 - Year" + year + " - Day " + day);

        try {
            executeDayOfAdvent(year, day);
        } catch (Exception e) {
            System.out.println("Unable to run this year and day.");
            e.printStackTrace();
        }
    }


    public static void executeDayOfAdvent(int year, int day) throws ClassNotFoundException, InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
        Class<?> dayToExecuteClass = ClassLoader.getSystemClassLoader().loadClass("Y"+year+".days."+"Day"+day);
        IDay dayToExecute = (IDay) dayToExecuteClass.getDeclaredConstructor().newInstance();
        dayToExecute.solve();
    }
}
