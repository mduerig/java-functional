package michid.fun;

import java.util.function.Function;

public class Free {

    /*
     * Higher order parametricity and Functor
     */
    public interface H<F, T> { }

    public interface Functor<F, T> {
        <R> H<F, R> map(Function<T, R> f);
    }

    /*
     * -- Fix   :: f (Fix f) -> Fix f
     * -- unfix :: Fix f     -> f (Fix f)
     * newtype Fix f = Fix {unfix::f (Fix f)}
     */
    public static record Fix<F extends H<F, T> & Functor<F, T>, T>(F f) {
        public Functor<F, Fix<F, T>> unfix() {
            return (Functor<F, Fix<F, T>>) f;
        }
    }

    /*
     * type Algebra f a = f a -> a
     */
    public interface Algebra<F, T> extends Function<H<F, T>, T> {}

    /*
     * -- Catamorphism
     * cata :: Functor f => Algebra f a -> Fix f -> a
     * cata alg = alg . fmap (cata alg) . unfix
     */
    public static <F extends H<F, T> & Functor<F, T>, T> Function<Fix<F, T>, T> cata(Algebra<F, T> alg) {
        return fix -> alg.apply(fix.unfix().map(cata(alg)));
    }

// -- Natural numbers as fixed point

    /*
     * data NatF a = ZeroF | SuccF a
     *  deriving (Functor, Show)
     *
     * zeroFix :: Fix NatF
     * zeroFix = Fix ZeroF
     *
     * succFix :: Fix NatF -> Fix NatF
     * succFix n = Fix (SuccF n)
     */
    public sealed interface Nat<T> extends H<Nat, T>, Functor<Nat, T> {
        static <S> Nat<S> nat(H<Nat, S> hNat) {
            return (Nat<S>) hNat;
        }

        <R> R match(Function<Zero<T>, R> zero, Function<Succ<T>, R> succ);
    }

    public static record Zero<T>() implements Nat<T> {
        @Override
        public <R> Nat<R> map(Function<T, R> f) {
            return new Zero<>();
        }

        @Override
        public <R> R match(Function<Zero<T>, R> zero, Function<Succ<T>, R> succ) {
            return zero.apply(this);
        }
    }

    public static record Succ<T>(T n) implements Nat<T> {
        @Override
        public <R> Nat<R> map(Function<T, R> f) {
            return new Succ<>(f.apply(n));
        }

        @Override
        public <R> R match(Function<Zero<T>, R> zero, Function<Succ<T>, R> succ) {
            return succ.apply(this);
        }
    }

    public static <T> Fix<?, T> zeroF() {
        return new Fix<>(new Zero<>());
    }

    public static <T> Fix<?, T> succF(Fix<?, T> n) {
        return new Fix<>(new Succ<>(n));
    }

    /*
     * toNatF :: Int -> Fix NatF
     * toNatF 0 = zeroFix
     * toNatF n = succFix (toNatF (n - 1))
     */
    public static <T> Fix<?, T> toNat(int n) {
        return n == 0
           ? zeroF()
           : succF(toNat(n - 1));
    }

    /*
     * -- Algebra back to natural numbers
     * nat :: Algebra NatF Int
     * nat ZeroF = 0
     * nat (SuccF n) = n + 1
     */
    public static class NatAlg implements Algebra<Nat, Integer> {
        @Override
        public Integer apply(H<Nat, Integer> hNat) {
            return Nat.nat(hNat).match(
                zero -> 0,
                succ -> succ.n + 1);
        }
    }

    /*
     * evalNat :: Fix NatF -> Int
     * evalNat = cata nat
     */
    public static Function<Fix<?, Integer>, Integer> evalNat = cata(new NatAlg());

    /*
     * -- Fibonacci algebra over NatF with carrier (Int, Int)
     * fib :: Algebra NatF (Int, Int)
     * fib ZeroF = (1, 1)
     * fib (SuccF (m, n)) = (n, n + m)
     */
    public record Tuple<P, Q>(P p, Q q) {}

    public static class FibAlg implements Algebra<Nat, Tuple<Integer, Integer>> {
        @Override
        public Tuple<Integer, Integer> apply(H<Nat, Tuple<Integer, Integer>> hNat) {
            return Nat.nat(hNat).match(
                zero -> new Tuple<>(1, 1),
                succ -> new Tuple<>(succ.n.q, succ.n.p + succ.n.q));
        }
    }

    /*
     * evalFib :: Fix NatF -> (Int, Int)
     * evalFib = cata fib
     */
    public static Function<Fix<?, Integer>, Tuple<Integer, Integer>> evalFib = cata(new FibAlg());


// -- Simple expression algebra

   /*
    * data ExprF a
    * = Const Int
    * | Add a a
    * | Mul a a
    * deriving (Functor, Show)
    */
    public sealed interface Expr<T> extends H<Expr, T>, Functor<Expr, T> {
        static <S> Expr<S> expr(H<Expr, S> hExpr) {
            return (Expr<S>) hExpr;
        }

        <R> R match(Function<Const<T>, R> conzt, Function<Add<T>, R> add, Function<Mul<T>, R> mul);
    }

    public record Const<T>(int n) implements Expr<T> {
        @Override
        public <R> R match(Function<Const<T>, R> conzt,
                           Function<Add<T>, R> add,
                           Function<Mul<T>, R> mul) {
            return conzt.apply(this);
        }

        @Override
        public <R> H<Expr, R> map(Function<T, R> f) {
            return new Const<>(n);
        }
    }

    public record Add<T>(T t1, T t2) implements Expr<T> {
        @Override
        public <R> R match(Function<Const<T>, R> conzt,
                           Function<Add<T>, R> add,
                           Function<Mul<T>, R> mul) {
            return add.apply(this);
        }

        @Override
        public <R> H<Expr, R> map(Function<T, R> f) {
            return new Add<>(f.apply(t1), f.apply(t2));
        }
    }

    public record Mul<T>(T t1, T t2) implements Expr<T> {
        @Override
        public <R> R match(Function<Const<T>, R> conzt,
                           Function<Add<T>, R> add,
                           Function<Mul<T>, R> mul) {
            return mul.apply(this);
        }

        @Override
        public <R> H<Expr, R> map(Function<T, R> f) {
            return new Mul<>(f.apply(t1), f.apply(t2));
        }
    }

    /*
    * constFix :: Int -> Fix ExprF
    * constFix n = Fix (Const n)
    *
    * addFix :: Fix ExprF -> Fix ExprF -> Fix ExprF
    * addFix a b = Fix (Add a b)
    *
    * mulFix :: Fix ExprF -> Fix ExprF -> Fix ExprF
    * mulFix a b = Fix (Mul a b)
    */
    public static <T> Fix<?, T> constFix(int n) {
        return new Fix<>(new Const<>(n));
    }

    public static <T> Fix<?, T> addFix(Fix<?, T> t1, Fix<?, T> t2) {
        return new Fix<>(new Add<>(t1, t2));
    }

    public static <T> Fix<?, T> mulFix(Fix<?, T> t1, Fix<?, T> t2) {
        return new Fix<>(new Mul<>(t1, t2));
    }

   /*
    * evalExprF :: Algebra ExprF Int
    * evalExprF (Const n) = n
    * evalExprF (Add m n) = m + n
    * evalExprF (Mul m n) = m * n
    */
    public static class ExprAlg implements Algebra<Expr, Integer> {
        @Override
        public Integer apply(H<Expr, Integer> hExpr) {
            return Expr.expr(hExpr).match(
                conzt -> conzt.n,
                add   -> add.t1 + add.t2,
                mul   -> mul.t1 * mul.t2);
        }
    }

   /*
    * evalExpr :: Fix ExprF -> Int
    * evalExpr = cata evalExprF
    */
    public static Function<Fix<?, Integer>, Integer> evalExpr = cata(new ExprAlg());

    public static class PrintExprAlg implements Algebra<Expr, String> {
        @Override
        public String apply(H<Expr, String> hExpr) {
            return Expr.expr(hExpr).match(
                conzt -> Integer.toString(conzt.n),
                add   -> add.t1 + " + " + add.t2,
                mul   -> mul.t1 + " * " + mul.t2);
        }
    }

    public static Function<Fix<?, String>, String> evalPrintExpr = cata(new PrintExprAlg());


    public static void main(String[] args) {
        Fix<?, Integer> five = toNat(5);
        System.out.println(five);
        System.out.println(evalNat.apply(five));
        System.out.println(evalFib.apply(five));

        Fix expr = addFix(mulFix(constFix(2), constFix(3)), constFix(4));
        System.out.println(expr);
        System.out.println(evalExpr.apply(expr));
        System.out.println(evalPrintExpr.apply(expr));
    }

}
