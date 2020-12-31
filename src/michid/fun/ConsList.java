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
                    Function<Cons<T>, S> cons) {
                return nil.apply(this);
            }
        }

        record Cons<T>(T t, List<T> ts) implements List<T> {
            public <S> S match(
                    Function<Nil<T>, S> nil,
                    Function<Cons<T>, S> cons) {
                return cons.apply(this);
            }
        }

        <S> S match(
            Function<Nil<T>, S> nil,
            Function<Cons<T>, S> cons);
    }

    public static <T> List<T> nil() {
        return new Nil<>();
    }

    public static <T> List<T> cons(T t, List<T> ts) {
        return new Cons<>(t, ts);
    }

    public static <T> int length(List<T> list) {
        return list.match (
            nil -> 0,
            cons -> length(cons.ts) + 1);
    }

    public static <T> List<T> append(List<T> l1, List<T> l2) {
        return l1.match(
            nil -> l2,
            cons -> cons(cons.t, append(cons.ts, l2)));
    }

    public static <T, S> List<S> map(Function<T, S> f, List<T> list) {
        return list.match (
            nil -> nil(),
            cons -> cons(f.apply(cons.t), map(f, cons.ts())));
    }

    public static <T, S> List<S> flatMap(Function<T, List<S>> f, List<T> list) {
        return list.match (
            nil -> nil(),
            cons -> append(f.apply(cons.t), flatMap(f, cons.ts))
        );
    }

    public static <T, R> R foldl(R r, BiFunction<R, T, R> op, List<T> list) {
        return list.match (
            nil -> r,
            cons -> foldl(op.apply(r, cons.t), op, cons.ts));
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

