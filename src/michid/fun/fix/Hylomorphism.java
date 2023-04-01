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

    public sealed interface Tree<T> extends Functor<Tree<T>, T> {
        record Leaf<T>() implements Tree<T> { }
        record Branch<T>(T left, int value, T right) implements Tree<T> { }

        @Override
        default <S> Tree<S> map(Function<T, S> f) {
            return switch (this) {
                case Leaf<T>() -> new Leaf<>();
                case Branch<T>(var left, var value, var right) -> new Branch<>(f.apply(left), value, f.apply(right));
            };
        }
    }

    record Split() implements CoAlgebra<Tree<List<Integer>>, List<Integer>> {
        @Override
        public Tree<List<Integer>> apply(List<Integer> values) {
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

    record Join() implements Algebra<Tree<List<Integer>>, List<Integer>> {
        @Override
        public List<Integer> apply(Tree<List<Integer>> tree) {
            return switch (tree) {
                case Leaf() -> List.of();
                case Branch(var left, var value, var right) ->
                    concat(concat(left.stream(), Stream.of(value)), right.stream()).toList();
            };
        }
    }

    /**
     * Hylomorphism
     * hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
     * hylo f g = f . fmap (hylo f g) . g
     */
    public static <A, B> Function<A, B> hylo(Algebra<Tree<B>, B> alg, CoAlgebra<Tree<A>, A> coAlg) {
        return a -> alg.apply(coAlg.apply(a).map(hylo(alg, coAlg)));
    }

    public static Function<List<Integer>, List<Integer>> quicksort = hylo(new Join(), new Split());
}
