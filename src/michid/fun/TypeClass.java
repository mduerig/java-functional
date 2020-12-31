package michid.fun;

import static michid.fun.ConsList.cons;
import static michid.fun.ConsList.foldl;
import static michid.fun.ConsList.map;
import static michid.fun.ConsList.nil;

import java.util.function.BiFunction;
import java.util.function.Function;

import michid.fun.ConsList.List;
import michid.fun.TypeClass.HList.LIST;

public class TypeClass {

/*
 * Monoid
 */
    public interface Monoid<T> {
        T unit();
        T append(T t1, T t2);
    }

    public static <T> Monoid<T> monoid(T unit, BiFunction<T, T, T> append) {
        return new Monoid<>() {
            @Override
            public T unit() {
                return unit;
            }

            @Override
            public T append(T t1, T t2) {
                return append.apply(t1, t2);
            }
        };
    }

    public static <T> T fold(List<T> ts, Monoid<T> monoid) {
        return foldl(monoid.unit(), monoid::append, ts);
    }

/*
 * Functor
 */
    public interface H<K, T> {}

    public static record HList<T>(List<T> list) implements H<LIST, T> {
        public static class LIST { }

        public static <T> HList<T> of(List<T> list) {
            return new HList<>(list);
        }

        public static <T> List<T> list(H<LIST, T> h) {
            return ((HList<T>) h).list;
        }
    }

    public interface Functor<K> {
        <T, R> Function<H<K, T>, H<K, R>> lift(Function<T, R> f);

        default <T, R> H<K, R> fmap(Function<T, R> f, H<K, T> h) {
            return lift(f).apply(h);
        }
    }

    public static class ListFunctor implements Functor<LIST> {
        @Override
        public <T, R> Function<H<LIST, T>, H<LIST, R>> lift(Function<T, R> f) {
            return h -> HList.of(map(f, HList.list(h)));
        }
    }

    public static ListFunctor listFunctor = new ListFunctor();

    public static void main(String[] args) {
        List<Integer> intList = cons(3, cons(2, cons(1, nil())));
        Monoid<Integer> intMonoid = monoid(0, Integer::sum);
        Integer sum = fold(intList, intMonoid);
        System.out.println(sum);

        List<String> stringList = HList.list(listFunctor.fmap(i -> "int: " + i, HList.of(intList)));
        System.out.println(stringList);
    }
}
