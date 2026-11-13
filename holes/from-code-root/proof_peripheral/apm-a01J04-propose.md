**THE KEY INSIGHT**
- Subtract the principal part: write \(f(z) = \lambda/(z-1) + g(z)\) where \(g\) extends holomorphically across \(1\), hence is entire. The pole term has derivatives \( (\lambda/(z-1))^{(n)}(2) = (-1)^n n! \, \lambda\). By Cauchy’s integral formula, we can bound the remainder uniformly: for any chosen radius \(R>T\), \(|g^{(n)}(2)|/n! \le M_R R^{-n}\). Multiplying by \(T^n\) gives \(M_R (T/R)^n\to 0\). Thus the entire sequence collapses to the residue regardless of the prescribed geometric factor.

**THE NAIVE APPROACH THAT FAILS**
- Expanding \(f\) in a Taylor series about \(2\) and differentiating term-by-term ignores the pole. The coefficients mix the singular and regular parts, so you cannot isolate \(\lambda\) or control the remainder’s derivatives. Without removing the singular component, Cauchy’s estimate cannot be applied on a disk that excludes the pole, and you fail to ensure \(T^n\) drives the remainder to zero.
