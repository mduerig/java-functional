package michid.fun.fix;

import static michid.fun.fix.Free.Fix.fix;
import static michid.fun.fix.Free.cata;

import java.util.function.Function;

import michid.fun.fix.Free.Algebra;
import michid.fun.fix.Free.Fix;
import michid.fun.fix.Free.Functor;
import michid.fun.fix.Naturals.Nat.Succ;
import michid.fun.fix.Naturals.Nat.Zero;

public class Naturals {

    /**
     * data NatF a = ZeroF | SuccF a
     *   deriving (Functor, Show)
     *
     * zeroFix :: Fix NatF
     * zeroFix = Fix ZeroF
     *
     * succFix :: Fix NatF -> Fix NatF
     * succFix n = Fix (SuccF n)
     */
    public sealed interface Nat<T> extends Functor<T> {
        record Zero<T>() implements Nat<T> { }
        record Succ<T>(T n) implements Nat<T> { }

        @Override
        default <R> Nat<R> map(Function<T, R> f) {
            return switch(this) {
                case Zero<T>() -> new Zero<>();
                case Succ<T>(var n) -> new Succ<>(f.apply(n));
            };
        }
    }

    /**
     * Algebra back to natural numbers
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

    public static <T> Fix<Nat<T>, T> toNat(int n) {
        return n == 0
               ? fix(new Zero<>())
               : fix(new Succ<>(toNat(n - 1)));
    }

    /**
     * evalNat :: Fix NatF -> Int
     * evalNat = cata nat
     */
    public static Function<Fix<Nat<Integer>, Integer>, Integer>
        evalNat = cata(new NatAlg());


    /**
     * Fibonacci algebra over NatF with carrier (Int, Int)
     * fib :: Algebra NatF (Int, Int)
     * fib ZeroF = (1, 1)
     * fib (SuccF (m, n)) = (n, n + m)
     */
    public record Tuple<P, Q>(P p, Q q) {}
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

    public static <T> Fix<Nat<Tuple<T, T>>, Tuple<T, T>> toFibNat(int n) {
        return n == 0
               ? fix(new Zero<>())
               : fix(new Succ<>(toNat(n - 1)));
    }

    /**
     * evalFib :: Fix NatF -> (Int, Int)
     * evalFib = cata fib
     */
    public static Function<Fix<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>>, Tuple<Integer, Integer>>
        evalFib = cata(new FibAlg());
}
