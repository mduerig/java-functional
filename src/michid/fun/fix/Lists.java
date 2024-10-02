package michid.fun.fix;

import static michid.fun.fix.Free.Fix.fix;
import static michid.fun.fix.Free.cata;

import java.util.function.Function;

import michid.fun.fix.Free.Algebra;
import michid.fun.fix.Free.Fix;
import michid.fun.fix.Free.Functor;
import michid.fun.fix.Lists.ConsList.Nil;

public class Lists {

    /**
     * A algebra for an integer list
     * data List a = Nil | Cons Int a
     * deriving (Functor, Show)
     */
    public sealed interface ConsList<T> extends Functor<ConsList<T>, T> {
        record Nil<T>() implements ConsList<T> { }
        record Cons<T>(int n, T ns) implements ConsList<T> { }

        @Override
        default <S> ConsList<S> map(Function<T, S> f) {
            return switch (this) {
                case Nil<T>() -> new Nil<>();
                case Cons<T>(var n, var ns) -> new Cons<>(n, f.apply(ns));
            };
        }
    }

    public static <T> Fix<ConsList<T>, T> nilFix() {
        return fix(new Nil<>());
    }

    public static <T> Fix<ConsList<T>, T> consFix(int n, Fix<?, T> ns) {
        return fix((ConsList<T>) new ConsList.Cons<>(n ,ns));
    }

    public static class ListSumAlg implements Algebra<ConsList<Integer>, Integer> {
        @Override
        public Integer apply(ConsList<Integer> list) {
            return switch (list) {
                case Nil() -> 0;
                case ConsList.Cons(var n, var ns) -> n + ns;
            };
        }
    }

    public static Function<Fix<ConsList<Integer>, Integer>, Integer> evalListSum
        = cata(new ListSumAlg());

}
