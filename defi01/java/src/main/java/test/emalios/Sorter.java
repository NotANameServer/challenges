package test.emalios;

public class Sorter {

    public static int[] sortArray(int[] array, int length) {
        /*
        Implémentation du tri par fusion
         */
        //si le tableau a 1 élément ou moins alors il est déjà trié.
        if(length <= 1)
            return array;
        else {
            var tuple = Sorter.cutArray(array);
            return Sorter.arraysFusion(Sorter.sortArray(tuple._1(), tuple._1().length), Sorter.sortArray(tuple._2(), tuple._2().length));
        }
    }


    public static int[] arraysFusion(int[] a, int[] b) {
        /*
        Méthode qui s'occupe d'à partir de deux tableaux de retourner un tableau trié de ceux-ci
        arraysFusion([3, 4], [2]) -> [2, 3, 4]
         */
        //si a est vide on renvoie b
        if(a.length == 0) return b;
        //si b est vide on renvoie a
        int[] result;
        if(b.length == 0) return a;
        if(a[0] <= b[0]) {
            int number = a[0];
            a = Sorter.leftShift(a);
            result = Sorter.concatenate(number, Sorter.arraysFusion(a, b));
        } else {
            int number = b[0];
            b = Sorter.leftShift(b);
            result = Sorter.concatenate(number, Sorter.arraysFusion(b, a));
        }
        return result;
    }

    public static int[] leftShift(int[] array) {
        /*
        Méthode qui s'occupe de décaler tous les éléments d'un tableau d'un rang sur la gauche
        [1, 2, 3, 4, 5] -> [2, 3, 4, 5]
         */
        int[] newArray = new int[array.length-1];
        for (int i = 0; i < array.length-1; i++) {
            newArray[i] = array[i+1];
        }
        return newArray;
    }

    public static int[] concatenate(int number, int[] array) {
        /*
        Méthode qui ajoute en tête de tableau un nombre
        concatenate(1, [2, 3]) -> [1, 2, 3]
         */
        int[] newArray = new int[array.length+1];
        newArray[0] = number;
        for (int i = 0; i < array.length; i++) {
            newArray[i+1] = array[i];
        }
        return newArray;
    }

    public static Tuple2<int[], int[]> cutArray(int[] array) {
        /*
        Méthode qui coupe équitablement un tableau en deux tableaux
        {1, 2, 3, 4, 5} -> ({1, 2, 3}, {4, 5})
        {1, 2, 3, 4} -> ({1, 2}, {3, 4})
         */
        int middle = array.length/2;
        int[] last = new int[middle];
        int[] first;
        if(array.length % 2 == 0) {
            first = new int[middle];
            for (int i = 0; i < middle; i++) {
                first[i] = array[i];
            }
            for (int i = 0; i < middle; i++) {
                last[i] = array[i+middle];
            }
        } else {
            first = new int[middle+1];
            for (int i = 0; i <= middle; i++) {
                first[i] = array[i];
            }
            for (int i = 0; i < middle; i++) {
                last[i] = array[i+middle+1];
            }
        }
        return new Tuple2<>(first, last);
    }
}
