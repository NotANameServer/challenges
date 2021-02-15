package test.emalios;

public class Tuple2<T, S> {

    private final T t;
    private final S s;

    public Tuple2(T t, S s) {
        this.t = t;
        this.s = s;
    }

    public T _1() { return this.t; }
    public S _2() { return this.s; }
}