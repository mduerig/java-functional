package michid.fun.fix;

import static michid.fun.fix.Hylomorphism.quicksort;

import java.util.List;
import java.util.function.Function;

import michid.fun.fix.Expression.Expr;
import michid.fun.fix.Naturals.Nat;
import michid.fun.fix.Naturals.Tuple;

public class Free {

    interface Functor<F extends Functor<F, T>, T> {
        <R> Functor<?, R> map(Function<T, R> f);
    }

    /**
     * Fixpoint of functor
     * Fix   :: f (Fix f) -> Fix f
     * unfix :: Fix f     -> f (Fix f)
     * newtype Fix f = Fix {unfix::f (Fix f)}
     */
    public record Fix<F extends Functor<F, T>, T>(F f) {
        public static <F extends Functor<F, T>, T> Fix<F, T> fix(F f) {
            return new Fix<>(f);
        }

        public Functor<?, Fix<F, T>> unfix() {
            return (Functor<?, Fix<F, T>>) f;
        }
    }

    /**
     * type Algebra f a = f a -> a
     * type CoAlgebra f a = a -> fa
     */
    public interface Algebra<F extends Functor<F, T>, T> extends Function<F, T> {}
    public interface CoAlgebra<F extends Functor<F, T>, T> extends Function<T, F> {}

    /**
     * Catamorphism
     * cata :: Functor f => Algebra f a -> Fix f -> a
     * cata alg = alg . fmap (cata alg) . unfix
     */
    public static <F extends Functor<F, T>, T> Function<Fix<F, T>, T> cata(Algebra<F, T> alg) {
        return fix -> alg.apply((F) fix.unfix().map(cata(alg)));
    }


    public static void main(String[] args) {
        Fix<Nat<Integer>, Integer> natFive = Naturals.toNat(5);
        System.out.println("five=" + natFive);
        System.out.println("evalNat(five)=" + Naturals.evalNat.apply(natFive));

        Fix<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>> fibFive = Naturals.toFibNat(5);
        System.out.println("evalFib(five)=" + Naturals.evalFib.apply(fibFive));

        Fix<Expr<Integer>, Integer> exprInt = Expression.addFix(Expression.mulFix(Expression.constFix(2), Expression.constFix(3)), Expression.constFix(4));
        System.out.println("expr=" + exprInt);
        System.out.println("evalExpr(expr)=" + Expression.evalExpr.apply(exprInt));

        Fix<Expr<String>, String> exprString = Expression.addFix(Expression.mulFix(Expression.constFix(2), Expression.constFix(3)), Expression.constFix(4));
        System.out.println("evalPrintExpr(expr)=" + Expression.evalPrintExpr.apply(exprString));

        List<Integer> sorted = quicksort.apply(List.of(5, 3, 9, 2, 1));
        System.out.println("sorted=" + sorted);
    }

}
