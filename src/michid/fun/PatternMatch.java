package michid.fun;

import static java.lang.String.format;

import java.util.function.Function;

import michid.fun.PatternMatch.Vehicle.Bicycle;
import michid.fun.PatternMatch.Vehicle.Rocket;
import michid.fun.PatternMatch.Vehicle.Ship;

public class PatternMatch {
    sealed interface Vehicle {

        record Ship(String name, int weight) implements Vehicle {
            @Override
            public <T> T match(
                    Function<Ship, T> ship,
                    Function<Rocket, T> rocket,
                    Function<Bicycle, T> bicycle) {
                return ship.apply(this);
            }
        }

        record Rocket(String name, int height) implements Vehicle {
            @Override
            public <T> T match(
                    Function<Ship, T> ship,
                    Function<Rocket, T> rocket,
                    Function<Bicycle, T> bicycle) {
                return rocket.apply(this);
            }
        }

        record Bicycle(String name, double speed) implements Vehicle {
            @Override
            public <T> T match(
                    Function<Ship, T> ship,
                    Function<Rocket, T> rocket,
                    Function<Bicycle, T> bicycle) {
                return bicycle.apply(this);
            }
        }

        <T> T match(
                Function<Ship, T> ship,
                Function<Rocket, T> rocket,
                Function<Bicycle, T> bicycle);
    }

    public static void main(String[] args) {
        Vehicle vehicle = new Ship("Moby Dick", 42);

        String result = vehicle.match (
            (Ship ship) -> format("The ship named %s weighs %d tons", ship.name, ship.weight),
            (Rocket rocket) -> format("The rocket of height %d is called %s", rocket.height, rocket.name),
            (Bicycle bicycle) -> format("My bicycle is called %s and is speeding at %f kmh", bicycle.name, bicycle.speed));

        System.out.println(result);
    }
}
