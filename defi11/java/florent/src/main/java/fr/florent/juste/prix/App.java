package fr.florent.juste.prix;

import fr.florent.juste.prix.game.Game;

public class App {


    public static void main(String[] args) {

        Game game = new Game();
        int win = 0;
        int loose = 0;

        for (int i = 0; i < 10000000; i++) {
            int result = game.play();

            if (result == 1) {
                win++;
            } else {
                loose++;
            }
        }

        System.out.println("You win : " + win + " and loose : " + loose);

    }


}
