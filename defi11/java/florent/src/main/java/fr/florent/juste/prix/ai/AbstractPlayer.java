package fr.florent.juste.prix.ai;

import fr.florent.juste.prix.game.EnumHint;

/**
 * Abstract game player
 */
public abstract class AbstractPlayer {

    protected int min;
    protected int max;

    public AbstractPlayer(int min, int max) {
        this.min = min;
        this.max = max;
    }


    /**
     * Find price between min and max
     *
     * @param hint send information if it's lower or heigher
     * @return Proposition value
     */
    public abstract int findPrice(EnumHint hint);
}
