# hofl-typing-in-prolog
higher order functional language typing in prolog

## HOFL

### Terms
 t ::= x | n | t0 op t1 | if t then t0 else t1
| (t0, t1) | fst(t) | snd(t)
| x. t | t0 t1
| rec x. t 

rec x. t stands for let x = t in x

### Types

τ ::= int | τ0 * τ1 | τ0 → τ1

\Tau set of alla variables
Ide = {Ide\tau }\tau \in \Tau
\widehat{\bullet} : Ide → \Tau
\widehat{x} denotes the type of x

### Type judgements
t : τ

types are assigned to pre-terms using a set of inference rules
(structural induction of HOFL syntax)

### Type system
<-- add rules -->

#### Well Formed term
a pre-term t is well formed if \exists \tau \in \Tau. t : \tau
i.e. if we can assign a type to it also called well-typed or typeable

### Type inference
types of variables are not given
type rules are used to derive type constraints (type equations) whose solutions (via unification) define the principal type

#### Unification algorithm
