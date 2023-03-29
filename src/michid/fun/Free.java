package michid.fun;

import static java.util.stream.Stream.concat;
import static michid.fun.Free.Fix.fix;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import michid.fun.Free.Expr.Add;
import michid.fun.Free.Expr.Const;
import michid.fun.Free.Expr.Mul;
import michid.fun.Free.Nat.Succ;
import michid.fun.Free.Nat.Zero;
import michid.fun.Free.Tree.Branch;
import michid.fun.Free.Tree.Leaf;

public class Free {

    public interface Functor<T> {
        <R> Functor<R> map(Function<T, R> f);
    }

    /*
     * -- Fix   :: f (Fix f) -> Fix f
     * -- unfix :: Fix f     -> f (Fix f)
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

    /*
     * type Algebra f a = f a -> a
     * type CoAlgebra f a = a -> fa
     */
    public interface Algebra<T> extends Function<Functor<T>, T> {}
    public interface CoAlgebra<T> extends Function<T, Functor<T>> {}

    /*
     * -- Catamorphism
     * cata :: Functor f => Algebra f a -> Fix f -> a
     * cata alg = alg . fmap (cata alg) . unfix
     */
    public static <F extends Functor<T>, T> Function<Fix<F, T>, T> cata(Algebra<T> alg) {
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
    public sealed interface Nat<T> extends Functor<T> {
        record Zero<T>() implements Nat<T> {
            @Override
            public <R> Nat<R> map(Function<T, R> f) {
                return new Zero<>();
            }
        }
        record Succ<T>(T n) implements Nat<T> {
            @Override
            public <R> Nat<R> map(Function<T, R> f) {
                return new Succ<>(f.apply(n));
            }
        }
    }

    public static <T> Fix<Nat<T>, T> toNat(int n) {
        return n == 0
           ? fix(new Zero<>())
           : fix(new Succ<>(toNat(n - 1)));
    }

    /*
     * -- Algebra back to natural numbers
     * nat :: Algebra NatF Int
     * nat ZeroF = 0
     * nat (SuccF n) = n + 1
     */
    public record NatAlg() implements Algebra<Integer> {
        @Override
        public Integer apply(Functor<Integer> nat) {
            return switch (nat) {
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
    public static Function<Fix<Nat<Integer>, Integer>, Integer> evalNat = cata(new NatAlg());

    /*
     * -- Fibonacci algebra over NatF with carrier (Int, Int)
     * fib :: Algebra NatF (Int, Int)
     * fib ZeroF = (1, 1)
     * fib (SuccF (m, n)) = (n, n + m)
     */
    public record Tuple<P, Q>(P p, Q q) {}

    public static <T> Fix<Nat<Tuple<T, T>>, Tuple<T, T>> toFibNat(int n) {
        return n == 0
           ? fix(new Zero<>())
           : fix(new Succ<>(toNat(n - 1)));
    }

    public record FibAlg() implements Algebra<Tuple<Integer, Integer>> {
        @Override
        public Tuple<Integer, Integer> apply(Functor<Tuple<Integer, Integer>> hNat) {
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
    public static Function<Fix<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>>, Tuple<Integer, Integer>> evalFib
        = cata(new FibAlg());


    // -- Simple expression algebra

   /*
    * data ExprF a
    * = Const Int
    * | Add a a
    * | Mul a a
    * deriving (Functor, Show)
    */
    public sealed interface Expr<T> extends Functor<T> {
       record Const<T>(int n) implements Expr<T> {
           @Override
           public <R> Expr<R> map(Function<T, R> f) {
               return new Const<>(n);
           }
       }

       record Add<T>(T t1, T t2) implements Expr<T> {
           @Override
           public <R> Expr<R> map(Function<T, R> f) {
               return new Add<>(f.apply(t1), f.apply(t2));
           }
       }

       record Mul<T>(T t1, T t2) implements Expr<T> {
           @Override
           public <R> Expr<R> map(Function<T, R> f) {
               return new Mul<>(f.apply(t1), f.apply(t2));
           }
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
        return fix(new Const<>(n));
    }

    public static <T> Fix<?, T> addFix(Fix<?, T> t1, Fix<?, T> t2) {
        return fix(new Add<>(t1, t2));
    }

    public static <T> Fix<?, T> mulFix(Fix<?, T> t1, Fix<?, T> t2) {
        return fix(new Mul<>(t1, t2));
    }

   /*
    * evalExprF :: Algebra ExprF Int
    * evalExprF (Const n) = n
    * evalExprF (Add m n) = m + n
    * evalExprF (Mul m n) = m * n
    */
    public static class ExprAlg implements Algebra<Integer> {
        @Override
        public Integer apply(Functor<Integer> hExpr) {
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
    public static Function<Fix<Expr<Integer>, Integer>, Integer> evalExpr = cata(new ExprAlg());

    public static class PrintExprAlg implements Algebra<String> {
        @Override
        public String apply(Functor<String> hExpr) {
            return switch (hExpr) {
                case Const(var n) -> Integer.toString(n);
                case Add(var e1, var e2) -> e1 + " + " + e2;
                case Mul(var e1, var e2) -> e1 + " * " + e2;
                default -> throw new IllegalStateException();
            };
        }
    }

    public static Function<Fix<Expr<String>, String>, String> evalPrintExpr = cata(new PrintExprAlg());

    /*
     * -- Hylomorphism
     * hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
     * hylo f g = f . fmap (hylo f g) . g
     */
    public static <F extends Functor<A>, A, B> Function<A, B> hylo(Algebra<B> f, CoAlgebra<A> g) {
        return a -> f.apply(g.apply(a).map(hylo(f, g)));
    }

    public sealed interface Tree<T> extends Functor<T> {
        record Leaf<T>() implements Tree<T> {
            @Override
            public <R> Tree<R> map(Function<T, R> f) {
                return new Leaf<>();
            }
        }

        record Branch<T>(T left, int value, T right) implements Tree<T> {
            @Override
            public <R> Branch<R> map(Function<T, R> f) {
                return new Branch<>(f.apply(left), value, f.apply(right));
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

    public static void main(String[] args) {
        Fix<Nat<Integer>, Integer> natFive = toNat(5);
        System.out.println("five=" + natFive);
        System.out.println("evalNat(five)=" + evalNat.apply(natFive));

        Fix<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>> fibFive = toFibNat(5);
        System.out.println("evalFib(five)=" + evalFib.apply(fibFive));

        Fix expr = addFix(mulFix(constFix(2), constFix(3)), constFix(4));
        System.out.println("expr=" + expr);
        System.out.println("evalExpr(expr)=" + evalExpr.apply(expr));
        System.out.println("evalPrintExpr(expr)=" + evalPrintExpr.apply(expr));

        Function<List<Integer>, List<Integer>> hylo = hylo(new Join(), new Split());
        List<Integer> sorted = hylo.apply(List.of(5, 3, 9, 2, 1));
        System.out.println("sorted=" + sorted);
    }

}
