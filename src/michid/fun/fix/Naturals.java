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
    public sealed interface Nat<T> extends Functor<Nat<T>, T> {
        record Zero<T>() implements Nat<T> { }
        record Succ<T>(T n) implements Nat<T> { }

        @Override
        default <R> Nat<R> map(Function<T, R> f) {
            return switch (this) {
                case Zero() -> new Zero<>();
                case Succ(var n) -> new Succ<>(f.apply(n));
            };
        }
    }

    /**
     * Algebra back to natural numbers
     * nat :: Algebra NatF Int
     * nat ZeroF = 0
     * nat (SuccF n) = n + 1
     */
    public record NatAlg() implements Algebra<Nat<Integer>, Integer> {
        @Override
        public Integer apply(Nat<Integer> nat) {
            return switch (nat) {
                case Zero<Integer>() -> 0;
                case Succ<Integer>(var n) -> n + 1;
            };
        }
    }

    public static <T> Fix<Nat<T>, T> toNat(int n) {
        return n == 0
           ? fix(new Zero<>())
           : fix((Nat<T>) new Succ<>(toNat(n - 1)));
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
    public record FibAlg() implements Algebra<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>> {
        @Override
        public Tuple<Integer, Integer> apply(Nat<Tuple<Integer, Integer>> nat) {
            return switch (nat) {
                case Zero() -> new Tuple<>(1, 1);
                case Succ(var n) -> new Tuple<>(n.q, n.p + n.q);
            };
        }
    }

    public static <T> Fix<Nat<Tuple<T, T>>, Tuple<T, T>> toFibNat(int n) {
        return n == 0
               ? fix(new Zero<>())
               : fix((Nat) new Succ<>(toFibNat(n - 1)));
    }

    /**
     * evalFib :: Fix NatF -> (Int, Int)
     * evalFib = cata fib
     */
    public static Function<Fix<Nat<Tuple<Integer, Integer>>, Tuple<Integer, Integer>>, Tuple<Integer, Integer>>
        evalFib = cata(new FibAlg());
}
