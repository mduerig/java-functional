package michid.fun.fix;

import static java.util.stream.Stream.concat;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import michid.fun.fix.Free.Algebra;
import michid.fun.fix.Free.CoAlgebra;
import michid.fun.fix.Free.Functor;
import michid.fun.fix.Hylomorphism.Tree.Branch;
import michid.fun.fix.Hylomorphism.Tree.Leaf;

public class Hylomorphism {

    public sealed interface Tree<T> extends Functor<T> {
        record Leaf<T>() implements Tree<T> { }
        record Branch<T>(T left, int value, T right) implements Tree<T> { }

        @Override
        default <R> Functor<R> map(Function<T, R> f) {
            return switch (this) {
                case Leaf<T>() -> new Leaf<>();
                case Branch<T>(var left, var value, var right) -> new Branch<>(f.apply(left), value, f.apply(right));
            };
        }
    }

    record Split() implements CoAlgebra<List<Integer>> {
        @Override
        public Functor<List<Integer>> apply(List<Integer> values) {
            if (values.isEmpty()) {
                return new Leaf<>();
            } else {
                var middle = values.get(0);
                var left = values.stream().skip(1).filter(v -> v < middle).toList();
                var right = values.stream().skip(1).filter(v -> v > middle).toList();
                return new Branch<>(left, middle, right);
            }
        }
    }

    record Join() implements Algebra<List<Integer>> {
        @Override
        public List<Integer> apply(Functor<List<Integer>> hTree) {
            return switch (hTree) {
                case Leaf() -> List.of();
                case Branch(var left, var value, var right) ->
                    concat(concat(left.stream(), Stream.of(value)), right.stream()).toList();
                default -> throw new IllegalStateException();
            };
        }
    }

    /**
     * Hylomorphism
     * hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
     * hylo f g = f . fmap (hylo f g) . g
     */
    public static <F extends Functor<A>, A, B> Function<A, B> hylo(Algebra<B> f, CoAlgebra<A> g) {
        return a -> f.apply(g.apply(a).map(hylo(f, g)));
    }

    public static Function<List<Integer>, List<Integer>> quicksort = hylo(new Join(), new Split());
}
