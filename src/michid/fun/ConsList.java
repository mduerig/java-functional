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

    public static <T> List<T> pure(T t) {
        return cons(t, nil());
    }

    public static <T> T head(List<T> ts) {
        return ts.match(
            nil -> null,
            (cons, __) -> cons.t);
    }

    public static <T> List<T> tail(List<T> ts) {
        return ts.match(
            nil -> nil(),
            (cons, __) -> cons.ts);
    }

    public static <T, S> List<S> map(Function<T, S> f, List<T> list) {
        return list.match (
            nil -> nil(),
            (cons, mapped) -> cons(f.apply(cons.t), mapped));
    }

    public static <T> List<T> reverse(List<T> list) {
        return foldl(nil(), (ts, t) -> cons(t, ts), list);
    }

    public static <T, S> List<S> ap(List<Function<T, S>> functions, List<T> list) {
        return
            flatMap(list, t ->
            flatMap(functions, f ->
                pure(f.apply(t))));
    }

    public static <T, S> List<S> apZip(List<Function<T, S>> functions, List<T> list) {
        record Acc<T, S>(List<Function<T, S>> fs, List<S> ss) {}

        return foldr(
            new Acc<>(reverse(functions), nil()),
            (t, acc) -> new Acc<>(tail(acc.fs), cons(head(acc.fs).apply(t), acc.ss)),
            list)
        .ss;
    }

    public static <T, S> List<S> flatMap(List<T> list, Function<T, List<S>> f) {
        return list.match (
            nil -> nil(),
            (cons, mapped) -> append(f.apply(cons.t), mapped));
    }

    public static <T, R> R foldl(R r, BiFunction<R, T, R> op, List<T> list) {
        return list.match(
            nil -> Function.<R>identity(),
            (cons, f) -> rr -> f.apply(op.apply(rr, cons.t)))
        .apply(r);
    }


    public static <T, R> R foldr(R r, BiFunction<T, R, R> op, List<T> list) {
        return list.match (
            nil -> r,
            (cons, acc) -> op.apply(cons.t, acc));
    }

    public static void main(String[] args) {
        List<Integer> l1 = cons(3, cons(2, cons(1, nil())));
        System.out.println(l1);
        System.out.println(length(l1));
        System.out.println(foldr(0, Integer::sum, l1));

        List<String> l2 = cons("a", cons("ab", nil()));
        System.out.println(l2);
        System.out.println(map(String::length, l2));

        List<String> l3 = flatMap(l2, s -> map(i -> s + i, l1));
        System.out.println(l3);

        List<Function<String, Character>> functions =
            cons(s -> s.charAt(0),
            cons(s -> s.charAt(1),
            cons(s -> s.charAt(2),
            nil())));

        System.out.println(foldr(nil(), ConsList::cons, cons("a", cons("b", cons("c", nil())))));
        System.out.println(foldl(nil(), (s2, s1) -> cons(s1, s2), cons("a", cons("b", cons("c", nil())))));

        List<String> strings = cons("abc", cons("ABC", cons("xyz", nil())));
        System.out.println(ap(functions, strings));
        System.out.println(apZip(functions, strings));
    }

}

