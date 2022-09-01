(module srfi-141
  (ceiling/ ceiling-quotient ceiling-remainder
   floor/ floor-quotient floor-remainder
   truncate/ truncate-quotient truncate-remainder
   round/ round-quotient round-remainder
   euclidean/ euclidean-quotient euclidean-remainder
   balanced/ balanced-quotient balanced-remainder)

  (import scheme
          (only (chicken base) include receive exact-integer? unless)
          (chicken condition)
          (chicken type))

  (include "exceptions.scm")
  (include "srfi-141-impl.scm")
)
