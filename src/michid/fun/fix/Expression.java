package michid.fun.fix;

import static michid.fun.fix.Free.Fix.fix;
import static michid.fun.fix.Free.cata;

import java.util.function.Function;

import michid.fun.fix.Expression.Expr.Add;
import michid.fun.fix.Expression.Expr.Const;
import michid.fun.fix.Expression.Expr.Mul;
import michid.fun.fix.Free.Algebra;
import michid.fun.fix.Free.Fix;
import michid.fun.fix.Free.Functor;

public class Expression {

    /**
     * A simple expression algebra
     * data ExprF a
     * = Const Int
     * | Add a a
     * | Mul a a
     * deriving (Functor, Show)
     */
    public sealed interface Expr<T> extends Functor<Expr<T>, T> {
        record Const<T>(int n) implements Expr<T> { }
        record Add<T>(T t1, T t2) implements Expr<T> { }
        record Mul<T>(T t1, T t2) implements Expr<T> { }

        @Override
        default <S> Expr<S> map(Function<T, S> f) {
            return switch (this) {
                case Const<T>(var n) -> new Const<>(n);
                case Add<T>(var t1, var t2) -> new Add<>(f.apply(t1), f.apply(t2));
                case Mul<T>(var t1, var t2) -> new Mul<>(f.apply(t1), f.apply(t2));
            };
        }
    }

    /**
     * constFix :: Int -> Fix ExprF
     * constFix n = Fix (Const n)
     *
     * addFix :: Fix ExprF -> Fix ExprF -> Fix ExprF
     * addFix a b = Fix (Add a b)
     *
     * mulFix :: Fix ExprF -> Fix ExprF -> Fix ExprF
     * mulFix a b = Fix (Mul a b)
     */
    public static <T> Fix<Expr<T>, T> constFix(int n) {
        return fix(new Const<>(n));
    }

    public static <T> Fix<Expr<T>, T> addFix(Fix<?, T> t1, Fix<?, T> t2) {
        return fix((Expr<T>) new Add<>(t1, t2));
    }

    public static <T> Fix<Expr<T>, T> mulFix(Fix<?, T> t1, Fix<?, T> t2) {
        return fix((Expr<T>) new Mul<>(t1, t2));
    }

    /**
     * evalExprF :: Algebra ExprF Int
     * evalExprF (Const n) = n
     * evalExprF (Add m n) = m + n
     * evalExprF (Mul m n) = m * n
     */
    public static class ExprAlg implements Algebra<Expr<Integer>, Integer> {
        @Override
        public Integer apply(Expr<Integer> expr) {
            return switch (expr) {
                case Const(var n) -> n;
                case Add(var e1, var e2) -> e1 + e2;
                case Mul(var e1, var e2) -> e1 * e2;
            };
        }
    }

    /**
     * evalExpr :: Fix ExprF -> Int
     * evalExpr = cata evalExprF
     */
    public static Function<Fix<Expr<Integer>, Integer>, Integer> evalExpr
        = cata(new ExprAlg());

    public static class PrintExprAlg implements Algebra<Expr<String>, String> {
        @Override
        public String apply(Expr<String> expr) {
            return switch (expr) {
                case Const(var n) -> Integer.toString(n);
                case Add(var e1, var e2) -> e1 + " + " + e2;
                case Mul(var e1, var e2) -> e1 + " * " + e2;
            };
        }
    }

    public static Function<Fix<Expr<String>, String>, String> evalPrintExpr
        = cata(new PrintExprAlg());
}
