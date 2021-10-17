package fr.florent.juste.prix.ai;

import fr.florent.juste.prix.game.EnumHint;

/**
 * Player who send always a middle value
 */
public class MiddlePlayer extends AbstractPlayer {

    Integer lastValue;

    public MiddlePlayer(int min, int max) {
        super(min, max);
    }

    @Override
    public int findPrice(EnumHint hint) {

        if (lastValue != null && hint != null) {
            switch (hint) {
                case LOWER:
                    max = lastValue - 1;
                    break;
                case HEIGER:
                    min = lastValue + 1;
                    break;
            }
        }

        lastValue = (max - min )/ 2 + min;
        return lastValue;
    }
}
