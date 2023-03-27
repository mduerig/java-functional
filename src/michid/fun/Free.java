package michid.fun;

import static java.util.stream.Stream.concat;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public class Free {

    public interface Functor<F, T> {
        <R> Functor<F, R> map(Function<T, R> f);
    }

    /*
     * -- Fix   :: f (Fix f) -> Fix f
     * -- unfix :: Fix f     -> f (Fix f)
     * newtype Fix f = Fix {unfix::f (Fix f)}
     */
    public record Fix<F extends Functor<F, T>, T>(F f) {
        public Functor<F, Fix<F, T>> unfix() {
            return (Functor<F, Fix<F, T>>) f;
        }
    }

    /*
     * type Algebra f a = f a -> a
     */
    public interface Algebra<F, T> extends Function<Functor<F, T>, T> {}
    public interface CoAlgebra<T, F> extends Function<T, Functor<F, T>> {}

    /*
     * -- Catamorphism
     * cata :: Functor f => Algebra f a -> Fix f -> a
     * cata alg = alg . fmap (cata alg) . unfix
     */
    public static <F extends Functor<F, T>, T> Function<Fix<F, T>, T> cata(Algebra<F, T> alg) {
        return fix -> alg.apply(fix.unfix().map(cata(alg)));
    }

    /*
     * -- Hylomorphism
     * hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
     * hylo f g = f . fmap (hylo f g) . g
     */
    public static <F extends Functor<F, A>, A, B> Function<A, B> hylo(Algebra<F, B> f, CoAlgebra<A, F> g) {
        return a -> f.apply(g.apply(a).map(hylo(f, g)));
    }

    public sealed interface Tree<T> extends Functor<Tree, T> { }

    public record Leaf<T>() implements Tree<T> {
        @Override
        public <R> Tree<R> map(Function<T, R> f) {
            return new Leaf<>();
        }
    }

    public record Branch<T>(T left, int value, T right) implements Tree<T> {
        @Override
        public <R> Branch<R> map(Function<T, R> f) {
            return new Branch<>(f.apply(left), value, f.apply(right));
        }
    }

    static class Join implements Algebra<Tree, List<Integer>> {
        @Override
        public List<Integer> apply(Functor<Tree, List<Integer>> hTree) {
            return switch (hTree) {
                case Leaf() -> List.of();
                case Branch(var left, var value, var right) ->
                    concat(concat(left.stream(), Stream.of(value)), right.stream()).toList();
                default -> throw new IllegalStateException();
            };
        }
    }

    static class Split implements CoAlgebra<List<Integer>, Tree> {
        @Override
        public Functor<Tree, List<Integer>> apply(List<Integer> values) {
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
    public sealed interface Nat<T> extends Functor<Nat, T> { }

    public record Zero<T>() implements Nat<T> {
        @Override
        public <R> Nat<R> map(Function<T, R> f) {
            return new Zero<>();
        }
    }

    public record Succ<T>(T n) implements Nat<T> {
        @Override
        public <R> Nat<R> map(Function<T, R> f) {
            return new Succ<>(f.apply(n));
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
        public Integer apply(Functor<Nat, Integer> hNat) {
            return switch (hNat) {
                case Zero() -> 0;
                case Succ(var n) -> n + 1;
                default -> throw new IllegalStateException();
            };
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
        public Tuple<Integer, Integer> apply(Functor<Nat, Tuple<Integer, Integer>> hNat) {
            return switch (hNat) {
                case Zero() -> new Tuple<>(1, 1);
                case Succ(var n) -> new Tuple<>(n.q, n.p + n.q);
                default -> throw new IllegalStateException();
            };
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
    public sealed interface Expr<T> extends Functor<Expr, T> { }

    public record Const<T>(int n) implements Expr<T> {
        @Override
        public <R> Expr<R> map(Function<T, R> f) {
            return new Const<>(n);
        }
    }

    public record Add<T>(T t1, T t2) implements Expr<T> {
        @Override
        public <R> Expr<R> map(Function<T, R> f) {
            return new Add<>(f.apply(t1), f.apply(t2));
        }
    }

    public record Mul<T>(T t1, T t2) implements Expr<T> {
        @Override
        public <R> Expr<R> map(Function<T, R> f) {
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
        public Integer apply(Functor<Expr, Integer> hExpr) {
            return switch (hExpr) {
                case Const(var n) -> n;
                case Add(var e1, var e2) -> e1 + e2;
                case Mul(var e1, var e2) -> e1 * e2;
                default -> throw new IllegalStateException();
            };
        }
    }

   /*
    * evalExpr :: Fix ExprF -> Int
    * evalExpr = cata evalExprF
    */
    public static Function<Fix<?, Integer>, Integer> evalExpr = cata(new ExprAlg());

    public static class PrintExprAlg implements Algebra<Expr, String> {
        @Override
        public String apply(Functor<Expr, String> hExpr) {
            return switch (hExpr) {
                case Const(var n) -> Integer.toString(n);
                case Add(var e1, var e2) -> e1 + " + " + e2;
                case Mul(var e1, var e2) -> e1 + " * " + e2;
                default -> throw new IllegalStateException();
            };
        }
    }

    public static Function<Fix<?, String>, String> evalPrintExpr = cata(new PrintExprAlg());


    public static void main(String[] args) {
        Fix<?, Integer> five = toNat(5);
        System.out.println("five=" + five);
        System.out.println("evalNat(five)=" + evalNat.apply(five));
        System.out.println("evalFib(five)=" + evalFib.apply(five));

        Fix expr = addFix(mulFix(constFix(2), constFix(3)), constFix(4));
        System.out.println("expr=" + expr);
        System.out.println("evalExpr(expr)=" + evalExpr.apply(expr));
        System.out.println("evalPrintExpr(expr)=" + evalPrintExpr.apply(expr));

        Function<List<Integer>, List<Integer>> hylo = hylo(new Join(), new Split());
        List<Integer> sorted = hylo.apply(List.of(5, 3, 9, 2, 1));
        System.out.println("sorted=" + sorted);
    }

}
