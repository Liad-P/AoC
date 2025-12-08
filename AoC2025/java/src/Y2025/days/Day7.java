package Y2025.days;

import Y2025.utils.Utils;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class Day7 implements IDay{

    @Override
    public void solve() {
        System.out.println("Part 1: ");
        part1();

        System.out.println("Part 2: ");
        part2();
        
    }

    public static void part2(){
        List<String> lines;
        try {
            lines = Files.readAllLines(Utils.getInputFileOfDay(2025, 7));
            var indexOfStart = lines.getFirst().indexOf("S");
            var tachyon = new Tachyon(new Tachyon.Beam(new Coord(indexOfStart, 0)));
            lines.remove(lines.getFirst());
            int y = 1;
            for (var line : lines){
                var chars = line.chars().toArray();
                for (int x = 0; x < chars.length;x++){
                    if (chars[x] == '^'){
                        tachyon.splitAllBeamsWithX(x, y);
                    }
                }
                y++;
            }

            System.out.println(tachyon.beamToNumOfPathsToBeam.values().stream().reduce(0L, (l1, l2) -> l1 + l2));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public static void part1(){

        List<String> lines;
        try {
            lines = Files.readAllLines(Utils.getInputFileOfDay(2025, 7));
            var indexOfStart = lines.getFirst().indexOf("S");
            var tachyon = new Tachyon(new Tachyon.Beam(new Coord(indexOfStart, 0)));
            lines.remove(lines.getFirst());
            int y = 1;
            for (var line : lines){
                var chars = line.chars().toArray();
                for (int x = 0; x < chars.length;x++){
                    if (chars[x] == '^'){
                        tachyon.splitAllBeamsWithX(x, y);
                    }
                }
                y++;
            }
            
            System.out.println(tachyon.splitCount.get());
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    public static class Tachyon {

        Tachyon (Beam beam){
            beams.add(beam);
            this.beamToNumOfPathsToBeam.put(beam.start().x(), 1L);
        }

        public AtomicInteger splitCount = new AtomicInteger(0); 

        public Map<Integer, Long> beamToNumOfPathsToBeam = new ConcurrentHashMap<>();

        private Set<Beam> beams = new HashSet<>();

        public List<Integer> getXsOfBeams(){
            return beams.stream().map(b -> b.start.x).collect(Collectors.toList());
        }

        public void splitAllBeamsWithX(int x, int y) {
            this.beams = beams.stream().flatMap((Beam b) -> {
                        if (b.start.x == x){
                            splitCount.incrementAndGet();
                            final Long numberOfPathsToThisX = this.beamToNumOfPathsToBeam.get(x);
                            this.beamToNumOfPathsToBeam.remove(x);
                            this.beamToNumOfPathsToBeam.compute(x-1, (k,v) -> {
                                if (v == null){
                                    return numberOfPathsToThisX;
                                } else{
                                    return v+numberOfPathsToThisX;
                                }
                            });
                            this.beamToNumOfPathsToBeam.compute(x+1, (k,v) -> {
                                if (v == null){
                                    return numberOfPathsToThisX;
                                } else{
                                    return v+numberOfPathsToThisX;
                                }
                            });
                            return List.of(new Beam(new Coord(x-1, y)), new Beam(new Coord(x+1, y))).stream();
                        }
                        else {
                            return List.of(b).stream();
                        }
                    }).collect(Collectors.toSet());
        }

        

        public static record Beam(Coord start){
            @Override
            public final boolean equals(Object b) {
                if (b instanceof Beam beam){
                    return beam.start().x() == this.start().x();
                }
                return false;
            }

            @Override
            public final int hashCode() {
                return Objects.hash(this.start().x());
            }
        }; 
    }

    static class Splitter{
        private final Coord coord;
        Splitter (Coord c){
            this.coord = c;
        }
    }

    static record Coord(int x, int y){};
    
}
