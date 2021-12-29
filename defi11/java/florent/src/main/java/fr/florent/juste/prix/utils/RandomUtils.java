package fr.florent.juste.prix.utils;

import java.util.Random;

/**
 * This classe provide common method to get random value
 */
public class RandomUtils {

    private static Random random = new Random();

    /**
     * Return a random integer bewtween min and max value provide
     *
     * @param min Minimum value can be reach
     * @param max Maximum value can be reach
     * @return Random value
     */
    public static int getRandom(int min, int max) {
        synchronized (random) {
            return random.nextInt(max - min) + min;
        }
    }

}
