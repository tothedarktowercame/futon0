**WHAT IS REALLY BEING ASKED**
- We examine a function \(f : \mathbb C \setminus \{1\} \to \mathbb C\) that is holomorphic everywhere except at a simple pole at \(z=1\) with residue \(\lambda\). For each nonnegative integer \(n\), we look at the \(n\)-th derivative \(f^{(n)}(2)\). The goal is to prove that, for every fixed \(T>0\), the sequence
  \[
    a_n := T^n\left|(-1)^n \frac{f^{(n)}(2)}{n!} - \lambda\right|
  \]
  tends to zero as \(n\to\infty\). Equivalently, after factoring out the factorial growth and the alternating sign, the high-order derivatives at \(2\) converge to the residue faster than any geometric rate. The real question: why does the local Laurent data at the pole entirely govern the asymptotics of derivatives taken far away?

**WHY IT IS HARD**
- A naive Taylor expansion about \(2\) intermingles the singular and analytic parts, obscuring the residue. The right strategy is to split \(f(z)=\lambda/(z-1)+g(z)\), where \(g\) extends holomorphically across \(1\) and is therefore entire. The pole term alone produces \((-1)^n n!\,\lambda\), while Cauchy estimates give \(|g^{(n)}(2)|/n! \le M_R R^{-n}\) for any radius \(R\) with \(|2-1|<R\). Students often forget to subtract the principal part or to pick \(R>T\), so they cannot make \(M_R(T/R)^n \to 0\). The problem tests fluency with residues/Laurent expansions plus Cauchy’s integral bounds for derivatives.
