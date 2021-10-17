package fr.florent.juste.prix.game;

import fr.florent.juste.prix.ai.AbstractPlayer;
import fr.florent.juste.prix.ai.MiddlePlayer;
import fr.florent.juste.prix.utils.RandomUtils;

public class Game {
    private static final int MAX_ATTENT = 50;
    private static final int MIN_NUMBER = 1;
    private static final int MAX_NUMBER = 1000000;

    public int play(){
        int count = 0;
        boolean endGame;
        EnumHint hint = null;

        int value = RandomUtils.getRandom(MIN_NUMBER, MAX_NUMBER);

        AbstractPlayer player = new MiddlePlayer(MIN_NUMBER, MAX_NUMBER);
        do {

            hint = trouver(value, player.findPrice(hint));
            count++;
            endGame = count > MAX_ATTENT || hint == EnumHint.EQUALS;
        } while (!endGame);

        if (hint == EnumHint.EQUALS) {
            return 1;
        } else {
            return 0;
        }
    }

    private EnumHint trouver(int value, int proposition) {
        if (value == proposition) {
            return EnumHint.EQUALS;
        } else if (value < proposition) {
            return EnumHint.LOWER;
        } else {
            return EnumHint.HEIGER;
        }
    }

}
