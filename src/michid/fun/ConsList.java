package michid.fun;

import java.util.function.BiFunction;
import java.util.function.Function;

import michid.fun.ConsList.List.Cons;
import michid.fun.ConsList.List.Nil;

public class ConsList {

    // List a = Nil | Cons a ( List a )
    public sealed interface List<T> {

        record Nil<T>() implements List<T> {
            public <S> S match(
                    Function<Nil<T>, S> nil,
                    BiFunction<Cons<T>, S, S> cons) {
                return nil.apply(this);
            }
        }

        record Cons<T>(T t, List<T> ts) implements List<T> {
            public <S> S match(
                    Function<Nil<T>, S> nil,
                    BiFunction<Cons<T>, S, S> cons) {
                return cons.apply(this, ts.match(nil, cons));
            }
        }

        <S> S match(
            Function<Nil<T>, S> nil,
            BiFunction<Cons<T>, S, S> cons);
    }

    public static <T> List<T> nil() {
        return new Nil<>();
    }

    public static <T> List<T> cons(T t, List<T> ts) {
        return new Cons<>(t, ts);
    }

    public static <T> int length(List<T> list) {
        return list.match(
            nil -> 0,
            (cons, length) -> length + 1);
    }

    public static <T> List<T> append(List<T> l1, List<T> l2) {
        return l1.match(
            nil -> l2,
            (cons, list) -> cons(cons.t, list));
    }

    public static <T, S> List<S> map(Function<T, S> f, List<T> list) {
        return list.match (
            nil -> nil(),
            (cons, mapped) -> cons(f.apply(cons.t), mapped));
    }

    public static <T, S> List<S> flatMap(Function<T, List<S>> f, List<T> list) {
        return list.match (
            nil -> nil(),
            (cons, mapped) -> append(f.apply(cons.t), mapped));
    }

    public static <T, R> R foldl(R r, BiFunction<R, T, R> op, List<T> list) {
        return list.match (
            nil -> r,
            (cons, acc) -> op.apply(acc, cons.t));
    }

    public static void main(String[] args) {
        List<Integer> l1 = cons(3, cons(2, cons(1, nil())));
        System.out.println(l1);
        System.out.println(length(l1));
        System.out.println(foldl(0, Integer::sum, l1));

        List<String> l2 = cons("a", cons("ab", nil()));
        System.out.println(l2);
        System.out.println(map(String::length, l2));

        List<String> l3 = flatMap(s -> map(i -> s + i, l1), l2);
        System.out.println(l3);
    }

}

