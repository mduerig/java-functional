package michid.fun.monad;

import java.util.function.Function;

import static java.util.function.Function.identity;
import static michid.fun.ConsList.*;
import static michid.fun.monad.State.Tuple.tuple;

public interface State<V, S> extends Function<S, State.Tuple<V, S>> {
    record Tuple<V, S>(V value, S state) {
        <R> Tuple<R, S> then(Function<V, State<R, S>> f) {
            return f.apply(value).apply(state);
        }

        static <V, S> Tuple<V, S> tuple(V value, S state) {
            return new Tuple<>(value, state);
        }
    }

    static <V, S> State<V, S> init(V value) {
        return state -> tuple(value, state);
    }

    default <R> State<R, S> then(Function<V, State<R, S>> f) {
        return state -> apply(state).then(f);
    }

    static <V, S> State<V, S> step(Function<S, Tuple<V, S>> f) {
        return f::apply;
    }

    static <S> State<S, S> next(Function<S, S> f) {
        return state -> tuple(state, f.apply(state));
    }

    static <S> State<S, S> get() {
        return next(identity());
    }

    static <S> State<S, S> put(S s) {
        return state -> tuple(null, s);
    }

    default V getValue(S state) {
        return apply(state).value;
    }

    static void main(String[] args) {
        List<String> list = cons("c", cons("b", cons("a", nil())));
        State<List<String>, Integer> sList = relabel(list);
        List<String> list0 = sList.getValue(0);
        System.out.println(list0);

        State<String, Integer> r = State.<String, Integer>
            init("v")
                .then(v ->
                    step(s -> tuple(s + v, s + 1)))

                .then(v ->
                    next((Integer s) -> s + 1)
                .then(s ->
                    init(s + v)))

                .then(v ->
                    State.<Integer>get()
                .then(s ->
                    put(s + 1)
                .then(_ ->
                    init(s + v))))

                .then(v ->
                    State.<Integer>get()
                .then(s ->
                    init(s + v)))
            ;

        System.out.println(r.getValue(0));
    }

    static State<List<String>, Integer> relabel(List<String> list) {
        return length(list) == 0
            ? init(nil())
            : next((Integer n) -> n + 1)
                .then(headLabel ->
                    relabel(tail(list))
                .then(tailLabels ->
                    init(cons(head(list) + "->" + headLabel, tailLabels))));
    }

}
