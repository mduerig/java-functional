package michid.fun.fix;

import static michid.fun.fix.Hylomorphism.quicksort;

import java.util.List;
import java.util.function.Function;

import michid.fun.fix.Naturals.Nat;
import michid.fun.fix.Naturals.Tuple;

public class Free {

    public interface Functor<T> {
        <R> Functor<R> map(Function<T, R> f);
    }

    /**
     * Fixpoint of functor
     * Fix   :: f (Fix f) -> Fix f
     * unfix :: Fix f     -> f (Fix f)
     * newtype Fix f = Fix {unfix::f (Fix f)}
     */
    public record Fix<F extends Functor<T>, T>(F f) {
        public static <T> Fix fix(Functor<T> f) {
            return new Fix<>(f);
        }

        public Functor<Fix<F, T>> unfix() {
            return (Functor<Fix<F, T>>) f;
        }
    }

    /**
     * type Algebra f a = f a -> a
     * type CoAlgebra f a = a -> fa
     */
    public interface Algebra<T> extends Function<Functor<T>, T> {}
    public interface CoAlgebra<T> extends Function<T, Functor<T>> {}

    /**
     * Catamorphism
     * cata :: Functor f => Algebra f a -> Fix f -> a
     * cata alg = alg . fmap (cata alg) . unfix
     */
    public static <F extends Functor<T>, T> Function<Fix<F, T>, T> cata(Algebra<T> alg) {
        return fix -> alg.apply(fix.unfix().map(cata(alg)));
    }


    public static void main(String[] args) {
        Fix<Nat<Integer>, Integer> natFive = Naturals.toNat(5);
        System.out.println("five=" + natFive);
        System.out.println("evalNat(five)=" + Naturals.evalNat.apply(natFive));

        Fix<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>> fibFive = Naturals.toFibNat(5);
        System.out.println("evalFib(five)=" + Naturals.evalFib.apply(fibFive));

        Fix expr = Expression.addFix(Expression.mulFix(Expression.constFix(2), Expression.constFix(3)), Expression.constFix(4));
        System.out.println("expr=" + expr);
        System.out.println("evalExpr(expr)=" + Expression.evalExpr.apply(expr));
        System.out.println("evalPrintExpr(expr)=" + Expression.evalPrintExpr.apply(expr));

        List<Integer> sorted = quicksort.apply(List.of(5, 3, 9, 2, 1));
        System.out.println("sorted=" + sorted);
    }

}
