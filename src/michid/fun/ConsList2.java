package michid.fun;

import java.util.function.BiFunction;
import java.util.function.Function;

import michid.fun.ConsList2.List2.Cons2;
import michid.fun.ConsList2.List2.Nil2;
import michid.fun.ConsList2.List2.Visitor2;

// Same as ConsList but with explicit Visitor
public class ConsList2 {

    // List a = Nil | Cons a ( List a )
    public sealed interface List2<T> {
        interface Visitor2<T, S> {
            S visit(Nil2<T> nil);
            S visit(Cons2<T> cons, S t);
        }

        record Nil2<T>() implements List2<T> {
            public <S> S accept(Visitor2<T, S> visitor) {
                return visitor.visit(this);
            }
        }

        record Cons2<T>(T t, List2<T> ts) implements List2<T> {
            public <S> S accept(Visitor2<T, S> visitor) {
                return visitor.visit(this, ts.accept(visitor));
            }
        }

        <S> S accept(Visitor2<T, S> visitor);
    }

    public static <T> List2<T> nil2() {
        return new Nil2<>();
    }

    public static <T> List2<T> cons2(T t, List2<T> ts) {
        return new Cons2<>(t, ts);
    }

    public static <T> int length2(List2<T> list) {
        return list.accept(new Visitor2<>() {
            @Override
            public Integer visit(Nil2<T> nil) {
                return 0;
            }

            @Override
            public Integer visit(Cons2<T> cons, Integer t) {
                return t + 1;
            }
        });
    }

    public static <T> List2<T> append2(List2<T> l1, List2<T> l2) {
        return l1.accept(new Visitor2<>() {
            @Override
            public List2<T> visit(Nil2<T> nil) {
                return l2;
            }

            @Override
            public List2<T> visit(Cons2<T> cons, List2<T> t) {
                return cons2(cons.t, t);
            }
        });
    }

    public static <T, R> List2<R> map2(Function<T, R> f, List2<T> list) {
        return list.accept(new Visitor2<>() {
            @Override
            public List2<R> visit(Nil2<T> nil) {
                return nil2();
            }

            @Override
            public List2<R> visit(Cons2<T> cons, List2<R> t) {
                return cons2(f.apply(cons.t), t);
            }
        });
    }

    public static <T, S> List2<S> flatMap2(Function<T, List2<S>> f, List2<T> list) {
        return list.accept(new Visitor2<>() {
            @Override
            public List2<S> visit(Nil2<T> nil) {
                return nil2();
            }

            @Override
            public List2<S> visit(Cons2<T> cons, List2<S> t) {
                return append2(f.apply(cons.t), t);
            }
        });
    }

    public static <T, R> R foldl2(R r, BiFunction<R, T, R> op, List2<T> list) {
        return list.accept(new Visitor2<>() {
            @Override
            public R visit(Nil2<T> nil) {
                return r;
            }

            @Override
            public R visit(Cons2<T> cons, R t) {
                return op.apply(t, cons.t);
            }
        });
    }

    public static void main(String[] args) {
        List2<Integer> l1 = cons2(3, cons2(2, cons2(1, nil2())));
        System.out.println(l1);
        System.out.println(length2(l1));
        System.out.println(foldl2(0, Integer::sum, l1));

        List2<String> l2 = cons2("a", cons2("ab", nil2()));
        System.out.println(l2);
        System.out.println(map2(String::length, l2));

        List2<String> l3 = flatMap2(s -> map2(i -> s + i, l1), l2);
        System.out.println(l3);
    }

}

